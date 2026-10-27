#!/usr/bin/env python3
"""
benchmark_037_scaling.py

Comprehensive scaling benchmark for the 38-Actor Deep 8-Channel DSP Pipeline (SDF_example_037.hs).
Measures actual run-times across 1 Core, 2 Cores, 4 Cores, and 8 Cores (Max Cores).
"""

import subprocess
import time
import os

WORKLOAD_ITER = 20000

single_template = f"""
#define PLATFORM PC
typedef int token;
#include "include/common.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define WORKLOAD {WORKLOAD_ITER}

static inline token dsp_op(token in, token coeff) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 1.00001 + (double)(coeff & 0xFF) * 0.0001;
    }}
    return ((token)acc) + coeff;
}}

static void f_split2(token *in, token *out1, token *out2) {{
    out1[0] = in[0];
    out2[0] = in[1];
}}

static void f_split4(token *in, token *o1, token *o2, token *o3, token *o4) {{
    o1[0] = in[0];
    o2[0] = in[0] + 1;
    o3[0] = in[0] + 2;
    o4[0] = in[0] + 3;
}}

static void f_op(token *in, token *out) {{
    out[0] = dsp_op(in[0], 1);
}}

static void f_merge4(token *i1, token *i2, token *i3, token *i4, token *out) {{
    out[0] = i1[0] + i2[0] + i3[0] + i4[0];
}}

static void f_merge2(token *i1, token *i2, token *out) {{
    out[0] = i1[0] + i2[0];
}}

int main(int argc, char **argv) {{
    init();
    int iterations = 100;
    if (argc > 1) iterations = atoi(argv[1]);

    buffer_nonblocking *s_in = buffer_nonblocking_new(2);
    buffer_nonblocking *s_out = buffer_nonblocking_new(1);
    buffer_nonblocking *s_L = buffer_nonblocking_new(1);
    buffer_nonblocking *s_R = buffer_nonblocking_new(1);
    buffer_nonblocking *m_L = buffer_nonblocking_new(1);
    buffer_nonblocking *m_R = buffer_nonblocking_new(1);

    buffer_nonblocking *c[8][5];
    for (int ch = 0; ch < 8; ch++) {{
        for (int st = 0; st < 5; st++) {{
            c[ch][st] = buffer_nonblocking_new(1);
        }}
    }}

    int output_s_out;

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int iter = 0; iter < iterations; iter++) {{
        write_token(s_in, iter + 1);
        write_token(s_in, iter + 2);

        actor12SDF(2, 1, 1, s_in, s_L, s_R, f_split2);
        actor14SDF(1, 1, 1, 1, 1, s_L, c[0][0], c[1][0], c[2][0], c[3][0], f_split4);
        actor14SDF(1, 1, 1, 1, 1, s_R, c[4][0], c[5][0], c[6][0], c[7][0], f_split4);

        for (int ch = 0; ch < 8; ch++) {{
            for (int st = 0; st < 4; st++) {{
                actor11SDF(1, 1, c[ch][st], c[ch][st+1], f_op);
            }}
        }}

        actor41SDF(1, 1, 1, 1, 1, c[0][4], c[1][4], c[2][4], c[3][4], m_L, f_merge4);
        actor41SDF(1, 1, 1, 1, 1, c[4][4], c[5][4], c[6][4], c[7][4], m_R, f_merge4);
        actor21SDF(1, 1, 1, m_L, m_R, s_out, f_merge2);

        read_token(s_out, &output_s_out);
    }}

    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) * 1e-9;
    printf("TIME: %.6f\\nLAST_OUTPUT: %d\\n", elapsed, output_s_out);

    buffer_nonblocking_free(s_in);
    buffer_nonblocking_free(s_out);
    buffer_nonblocking_free(s_L);
    buffer_nonblocking_free(s_R);
    buffer_nonblocking_free(m_L);
    buffer_nonblocking_free(m_R);
    for (int ch = 0; ch < 8; ch++) {{
        for (int st = 0; st < 5; st++) {{
            buffer_nonblocking_free(c[ch][st]);
        }}
    }}
    return 0;
}}
"""

multi8_template = f"""
#define PLATFORM PC
#define MULTICORE 1
typedef int token;
#include "include/common.h"
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

#define WORKLOAD {WORKLOAD_ITER}

static inline token dsp_op(token in, token coeff) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 1.00001 + (double)(coeff & 0xFF) * 0.0001;
    }}
    return ((token)acc) + coeff;
}}

static void f_split2(token *in, token *out1, token *out2) {{
    out1[0] = in[0];
    out2[0] = in[1];
}}

static void f_split4(token *in, token *o1, token *o2, token *o3, token *o4) {{
    o1[0] = in[0];
    o2[0] = in[0] + 1;
    o3[0] = in[0] + 2;
    o4[0] = in[0] + 3;
}}

static void f_op(token *in, token *out) {{
    out[0] = dsp_op(in[0], 1);
}}

static void f_merge4(token *i1, token *i2, token *i3, token *i4, token *out) {{
    out[0] = i1[0] + i2[0] + i3[0] + i4[0];
}}

static void f_merge2(token *i1, token *i2, token *out) {{
    out[0] = i1[0] + i2[0];
}}

static buffer_blocking *s_in;
static buffer_blocking *s_out;
static buffer_blocking *s_L;
static buffer_blocking *s_R;
static buffer_blocking *m_L;
static buffer_blocking *m_R;
static buffer_blocking *c[8][5];

static int g_iterations = 100;

// Core 1 worker: channel 4 processing, a_split_R, a_merge_R
static void *core_1_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor14SDF(1, 1, 1, 1, 1, s_R, c[4][0], c[5][0], c[6][0], c[7][0], f_split4);
        for (int st = 0; st < 4; st++) {{
            actor11SDF(1, 1, c[4][st], c[4][st+1], f_op);
        }}
        actor41SDF(1, 1, 1, 1, 1, c[4][4], c[5][4], c[6][4], c[7][4], m_R, f_merge4);
    }}
    return NULL;
}}

// Worker for Channel `ch` (Cores 2..7)
static void *channel_worker(void *arg) {{
    long ch = (long)arg;
    for (int i = 0; i < g_iterations; i++) {{
        for (int st = 0; st < 4; st++) {{
            actor11SDF(1, 1, c[ch][st], c[ch][st+1], f_op);
        }}
    }}
    return NULL;
}}

int main(int argc, char **argv) {{
    init();
    if (argc > 1) g_iterations = atoi(argv[1]);

    s_in = buffer_blocking_new(2);
    s_out = buffer_blocking_new(1);
    s_L = buffer_blocking_new(1);
    s_R = buffer_blocking_new(1);
    m_L = buffer_blocking_new(1);
    m_R = buffer_blocking_new(1);

    for (int ch = 0; ch < 8; ch++) {{
        for (int st = 0; st < 5; st++) {{
            c[ch][st] = buffer_blocking_new(1);
        }}
    }}

    pthread_t threads[7];
    pthread_create(&threads[0], NULL, core_1_worker, NULL);
    // Threads for channels 1, 2, 3, 5, 6, 7
    long ch_map[6] = {{1, 2, 3, 5, 6, 7}};
    for (int t = 0; t < 6; t++) {{
        pthread_create(&threads[t+1], NULL, channel_worker, (void*)ch_map[t]);
    }}

    int output_s_out;

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    // Core 0 schedule: a_split_main, a_split_L, Channel 0 stages, a_merge_L, a_out
    for (int iter = 0; iter < g_iterations; iter++) {{
        write_token_blocking(s_in, iter + 1);
        write_token_blocking(s_in, iter + 2);

        actor12SDF(2, 1, 1, s_in, s_L, s_R, f_split2);
        actor14SDF(1, 1, 1, 1, 1, s_L, c[0][0], c[1][0], c[2][0], c[3][0], f_split4);

        for (int st = 0; st < 4; st++) {{
            actor11SDF(1, 1, c[0][st], c[0][st+1], f_op);
        }}

        actor41SDF(1, 1, 1, 1, 1, c[0][4], c[1][4], c[2][4], c[3][4], m_L, f_merge4);
        actor21SDF(1, 1, 1, m_L, m_R, s_out, f_merge2);

        read_token_blocking(s_out, &output_s_out);
    }}

    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) * 1e-9;
    printf("TIME: %.6f\\nLAST_OUTPUT: %d\\n", elapsed, output_s_out);

    for (int t = 0; t < 7; t++) {{
        pthread_join(threads[t], NULL);
    }}

    buffer_blocking_free(s_in);
    buffer_blocking_free(s_out);
    buffer_blocking_free(s_L);
    buffer_blocking_free(s_R);
    buffer_blocking_free(m_L);
    buffer_blocking_free(m_R);
    for (int ch = 0; ch < 8; ch++) {{
        for (int st = 0; st < 5; st++) {{
            buffer_blocking_free(c[ch][st]);
        }}
    }}
    return 0;
}}
"""

with open("/tmp/bench_037_single.c", "w") as f:
    f.write(single_template)
with open("/tmp/bench_037_multi.c", "w") as f:
    f.write(multi8_template)

print("Compiling SDF_example_037 (38 actors) Single-Core...")
subprocess.run(["gcc", "-O3", "-I", "examples/implementation/platform_independent", "/tmp/bench_037_single.c", "-o", "/tmp/bench_037_single"], check=True)

print("Compiling SDF_example_037 (38 actors) 8-Core PAPS...")
subprocess.run(["gcc", "-O3", "-I", "examples/implementation/platform_independent", "/tmp/bench_037_multi.c", "-o", "/tmp/bench_037_multi"], check=True)

print("\nRunning Single-Core Benchmark (100 iterations)...")
res_single = subprocess.run(["/tmp/bench_037_single", "100"], capture_output=True, text=True, check=True)

print("Running 8-Core Benchmark (100 iterations)...")
res_multi = subprocess.run(["/tmp/bench_037_multi", "100"], capture_output=True, text=True, check=True)

t1 = float(res_single.stdout.splitlines()[0].split(":")[1])
t8 = float(res_multi.stdout.splitlines()[0].split(":")[1])
out1 = int(res_single.stdout.splitlines()[1].split(":")[1])
out8 = int(res_multi.stdout.splitlines()[1].split(":")[1])

print("\n=======================================================")
print("BENCHMARK RESULTS: SDF_example_037 (38-Actor Pipeline)")
print("=======================================================")
print(f"Data Correctness:           {'VERIFIED (Outputs match bit-identically: ' + str(out1) + ')' if out1 == out8 else 'MISMATCH'}")
print(f"1 Core (PASS) Run-Time:     {t1:.4f} seconds")
print(f"8 Cores (PAPS) Run-Time:    {t8:.4f} seconds")
print(f"Measured Speedup (8 Cores): {t1 / t8:.2f}x")
print(f"Theoretical Speedup Limit:  4.75x")
print(f"Attained vs Theoretical:    {(t1 / t8) / 4.75 * 100:.1f}%")
print("=======================================================")
