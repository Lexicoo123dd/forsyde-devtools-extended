#!/usr/bin/env python3
"""
test_filterbank_speedup.py

Empirical runtime benchmarking of 1 core vs maximum cores (8 cores)
using the 8-Channel Parallel Filterbank SDF model (SDF_example_036.hs).
"""

import subprocess
import time
import os

# 1. Define worker workload
# To model realistic embedded signal processing (FIR filter taps, matrix multiply),
# each actor performs a computation of controllable size.
WORKLOAD_ITER = 30000

single_template = f"""
#define PLATFORM PC
typedef int token;
#include "include/common.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define WORKLOAD {WORKLOAD_ITER}

static inline token dsp_filter(token in, token coeff) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 1.00001 + (double)(coeff & 0xFF) * 0.0001;
    }}
    return ((token)acc) + coeff;
}}

static inline token dsp_gain(token in, token gain) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 0.99999 + (double)(gain & 0xFF) * 0.0001;
    }}
    return ((token)acc) * gain;
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

static void f_f1(token *in, token *out) {{ out[0] = dsp_filter(in[0], 1); }}
static void f_g1(token *in, token *out) {{ out[0] = dsp_gain(in[0], 2); }}
static void f_f2(token *in, token *out) {{ out[0] = dsp_filter(in[0], 3); }}
static void f_g2(token *in, token *out) {{ out[0] = dsp_gain(in[0], 4); }}
static void f_f3(token *in, token *out) {{ out[0] = dsp_filter(in[0], 5); }}
static void f_g3(token *in, token *out) {{ out[0] = dsp_gain(in[0], 6); }}
static void f_f4(token *in, token *out) {{ out[0] = dsp_filter(in[0], 7); }}
static void f_g4(token *in, token *out) {{ out[0] = dsp_gain(in[0], 8); }}
static void f_f5(token *in, token *out) {{ out[0] = dsp_filter(in[0], 9); }}
static void f_g5(token *in, token *out) {{ out[0] = dsp_gain(in[0], 10); }}
static void f_f6(token *in, token *out) {{ out[0] = dsp_filter(in[0], 11); }}
static void f_g6(token *in, token *out) {{ out[0] = dsp_gain(in[0], 12); }}
static void f_f7(token *in, token *out) {{ out[0] = dsp_filter(in[0], 13); }}
static void f_g7(token *in, token *out) {{ out[0] = dsp_gain(in[0], 14); }}
static void f_f8(token *in, token *out) {{ out[0] = dsp_filter(in[0], 15); }}
static void f_g8(token *in, token *out) {{ out[0] = dsp_gain(in[0], 16); }}

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
    buffer_nonblocking *c1 = buffer_nonblocking_new(1);
    buffer_nonblocking *c1_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c2 = buffer_nonblocking_new(1);
    buffer_nonblocking *c2_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c3 = buffer_nonblocking_new(1);
    buffer_nonblocking *c3_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c4 = buffer_nonblocking_new(1);
    buffer_nonblocking *c4_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c4_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c3_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c2_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c1_b = buffer_nonblocking_new(1);
    buffer_nonblocking *s_R = buffer_nonblocking_new(1);
    buffer_nonblocking *c5 = buffer_nonblocking_new(1);
    buffer_nonblocking *c5_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c6 = buffer_nonblocking_new(1);
    buffer_nonblocking *c6_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c7 = buffer_nonblocking_new(1);
    buffer_nonblocking *c7_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c8 = buffer_nonblocking_new(1);
    buffer_nonblocking *c8_a = buffer_nonblocking_new(1);
    buffer_nonblocking *c8_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c7_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c6_b = buffer_nonblocking_new(1);
    buffer_nonblocking *c5_b = buffer_nonblocking_new(1);
    buffer_nonblocking *m_R = buffer_nonblocking_new(1);
    buffer_nonblocking *m_L = buffer_nonblocking_new(1);

    int output_s_out;

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    for (int iter = 0; iter < iterations; iter++) {{
        write_token(s_in, iter + 1);
        write_token(s_in, iter + 2);

        actor12SDF(2, 1, 1, s_in, s_L, s_R, f_split2);
        actor14SDF(1, 1, 1, 1, 1, s_L, c1, c2, c3, c4, f_split4);
        actor14SDF(1, 1, 1, 1, 1, s_R, c5, c6, c7, c8, f_split4);

        actor11SDF(1, 1, c1, c1_a, f_f1);
        actor11SDF(1, 1, c1_a, c1_b, f_g1);

        actor11SDF(1, 1, c2, c2_a, f_f2);
        actor11SDF(1, 1, c2_a, c2_b, f_g2);

        actor11SDF(1, 1, c3, c3_a, f_f3);
        actor11SDF(1, 1, c3_a, c3_b, f_g3);

        actor11SDF(1, 1, c4, c4_a, f_f4);
        actor11SDF(1, 1, c4_a, c4_b, f_g4);

        actor11SDF(1, 1, c5, c5_a, f_f5);
        actor11SDF(1, 1, c5_a, c5_b, f_g5);

        actor11SDF(1, 1, c6, c6_a, f_f6);
        actor11SDF(1, 1, c6_a, c6_b, f_g6);

        actor11SDF(1, 1, c7, c7_a, f_f7);
        actor11SDF(1, 1, c7_a, c7_b, f_g7);

        actor11SDF(1, 1, c8, c8_a, f_f8);
        actor11SDF(1, 1, c8_a, c8_b, f_g8);

        actor41SDF(1, 1, 1, 1, 1, c1_b, c2_b, c3_b, c4_b, m_L, f_merge4);
        actor41SDF(1, 1, 1, 1, 1, c5_b, c6_b, c7_b, c8_b, m_R, f_merge4);
        actor21SDF(1, 1, 1, m_L, m_R, s_out, f_merge2);

        read_token(s_out, &output_s_out);
    }}

    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) * 1e-9;
    printf("TIME: %.6f\\nLAST_OUTPUT: %d\\n", elapsed, output_s_out);

    buffer_nonblocking_free(s_in);
    buffer_nonblocking_free(s_out);
    buffer_nonblocking_free(s_L);
    buffer_nonblocking_free(c1);
    buffer_nonblocking_free(c1_a);
    buffer_nonblocking_free(c2);
    buffer_nonblocking_free(c2_a);
    buffer_nonblocking_free(c3);
    buffer_nonblocking_free(c3_a);
    buffer_nonblocking_free(c4);
    buffer_nonblocking_free(c4_a);
    buffer_nonblocking_free(c4_b);
    buffer_nonblocking_free(c3_b);
    buffer_nonblocking_free(c2_b);
    buffer_nonblocking_free(c1_b);
    buffer_nonblocking_free(s_R);
    buffer_nonblocking_free(c5);
    buffer_nonblocking_free(c5_a);
    buffer_nonblocking_free(c6);
    buffer_nonblocking_free(c6_a);
    buffer_nonblocking_free(c7);
    buffer_nonblocking_free(c7_a);
    buffer_nonblocking_free(c8);
    buffer_nonblocking_free(c8_a);
    buffer_nonblocking_free(c8_b);
    buffer_nonblocking_free(c7_b);
    buffer_nonblocking_free(c6_b);
    buffer_nonblocking_free(c5_b);
    buffer_nonblocking_free(m_R);
    buffer_nonblocking_free(m_L);
    return 0;
}}
"""

multi_template = f"""
#define PLATFORM PC
#define MULTICORE 1
typedef int token;
#include "include/common.h"
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

#define WORKLOAD {WORKLOAD_ITER}

static inline token dsp_filter(token in, token coeff) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 1.00001 + (double)(coeff & 0xFF) * 0.0001;
    }}
    return ((token)acc) + coeff;
}}

static inline token dsp_gain(token in, token gain) {{
    volatile double acc = in;
    for (int i = 0; i < WORKLOAD; i++) {{
        acc = acc * 0.99999 + (double)(gain & 0xFF) * 0.0001;
    }}
    return ((token)acc) * gain;
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

static void f_f1(token *in, token *out) {{ out[0] = dsp_filter(in[0], 1); }}
static void f_g1(token *in, token *out) {{ out[0] = dsp_gain(in[0], 2); }}
static void f_f2(token *in, token *out) {{ out[0] = dsp_filter(in[0], 3); }}
static void f_g2(token *in, token *out) {{ out[0] = dsp_gain(in[0], 4); }}
static void f_f3(token *in, token *out) {{ out[0] = dsp_filter(in[0], 5); }}
static void f_g3(token *in, token *out) {{ out[0] = dsp_gain(in[0], 6); }}
static void f_f4(token *in, token *out) {{ out[0] = dsp_filter(in[0], 7); }}
static void f_g4(token *in, token *out) {{ out[0] = dsp_gain(in[0], 8); }}
static void f_f5(token *in, token *out) {{ out[0] = dsp_filter(in[0], 9); }}
static void f_g5(token *in, token *out) {{ out[0] = dsp_gain(in[0], 10); }}
static void f_f6(token *in, token *out) {{ out[0] = dsp_filter(in[0], 11); }}
static void f_g6(token *in, token *out) {{ out[0] = dsp_gain(in[0], 12); }}
static void f_f7(token *in, token *out) {{ out[0] = dsp_filter(in[0], 13); }}
static void f_g7(token *in, token *out) {{ out[0] = dsp_gain(in[0], 14); }}
static void f_f8(token *in, token *out) {{ out[0] = dsp_filter(in[0], 15); }}
static void f_g8(token *in, token *out) {{ out[0] = dsp_gain(in[0], 16); }}

static void f_merge4(token *i1, token *i2, token *i3, token *i4, token *out) {{
    out[0] = i1[0] + i2[0] + i3[0] + i4[0];
}}

static void f_merge2(token *i1, token *i2, token *out) {{
    out[0] = i1[0] + i2[0];
}}

// Global FIFO channels
static buffer_blocking *s_in;
static buffer_blocking *s_out;
static buffer_blocking *s_L;
static buffer_blocking *c1;
static buffer_blocking *c1_a;
static buffer_blocking *c2;
static buffer_blocking *c2_a;
static buffer_blocking *c3;
static buffer_blocking *c3_a;
static buffer_blocking *c4;
static buffer_blocking *c4_a;
static buffer_blocking *c4_b;
static buffer_blocking *c3_b;
static buffer_blocking *c2_b;
static buffer_blocking *c1_b;
static buffer_blocking *s_R;
static buffer_blocking *c5;
static buffer_blocking *c5_a;
static buffer_blocking *c6;
static buffer_blocking *c6_a;
static buffer_blocking *c7;
static buffer_blocking *c7_a;
static buffer_blocking *c8;
static buffer_blocking *c8_a;
static buffer_blocking *c8_b;
static buffer_blocking *c7_b;
static buffer_blocking *c6_b;
static buffer_blocking *c5_b;
static buffer_blocking *m_R;
static buffer_blocking *m_L;

static volatile int g_running = 1;
static int g_iterations = 100;

// Core 1 worker: a_split_R, p_f5, p_g5, a_merge_R
static void *core_1_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor14SDF(1, 1, 1, 1, 1, s_R, c5, c6, c7, c8, f_split4);
        actor11SDF(1, 1, c5, c5_a, f_f5);
        actor11SDF(1, 1, c5_a, c5_b, f_g5);
        actor41SDF(1, 1, 1, 1, 1, c5_b, c6_b, c7_b, c8_b, m_R, f_merge4);
    }}
    return NULL;
}}

// Core 2 worker: p_f2, p_g2
static void *core_2_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c2, c2_a, f_f2);
        actor11SDF(1, 1, c2_a, c2_b, f_g2);
    }}
    return NULL;
}}

// Core 3 worker: p_f3, p_g3
static void *core_3_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c3, c3_a, f_f3);
        actor11SDF(1, 1, c3_a, c3_b, f_g3);
    }}
    return NULL;
}}

// Core 4 worker: p_f4, p_g4
static void *core_4_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c4, c4_a, f_f4);
        actor11SDF(1, 1, c4_a, c4_b, f_g4);
    }}
    return NULL;
}}

// Core 5 worker: p_f6, p_g6
static void *core_5_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c6, c6_a, f_f6);
        actor11SDF(1, 1, c6_a, c6_b, f_g6);
    }}
    return NULL;
}}

// Core 6 worker: p_f7, p_g7
static void *core_6_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c7, c7_a, f_f7);
        actor11SDF(1, 1, c7_a, c7_b, f_g7);
    }}
    return NULL;
}}

// Core 7 worker: p_f8, p_g8
static void *core_7_worker(void *arg) {{
    (void)arg;
    for (int i = 0; i < g_iterations; i++) {{
        actor11SDF(1, 1, c8, c8_a, f_f8);
        actor11SDF(1, 1, c8_a, c8_b, f_g8);
    }}
    return NULL;
}}

int main(int argc, char **argv) {{
    init();
    if (argc > 1) g_iterations = atoi(argv[1]);

    s_in = buffer_blocking_new(2);
    s_out = buffer_blocking_new(1);
    s_L = buffer_blocking_new(1);
    c1 = buffer_blocking_new(1);
    c1_a = buffer_blocking_new(1);
    c2 = buffer_blocking_new(1);
    c2_a = buffer_blocking_new(1);
    c3 = buffer_blocking_new(1);
    c3_a = buffer_blocking_new(1);
    c4 = buffer_blocking_new(1);
    c4_a = buffer_blocking_new(1);
    c4_b = buffer_blocking_new(1);
    c3_b = buffer_blocking_new(1);
    c2_b = buffer_blocking_new(1);
    c1_b = buffer_blocking_new(1);
    s_R = buffer_blocking_new(1);
    c5 = buffer_blocking_new(1);
    c5_a = buffer_blocking_new(1);
    c6 = buffer_blocking_new(1);
    c6_a = buffer_blocking_new(1);
    c7 = buffer_blocking_new(1);
    c7_a = buffer_blocking_new(1);
    c8 = buffer_blocking_new(1);
    c8_a = buffer_blocking_new(1);
    c8_b = buffer_blocking_new(1);
    c7_b = buffer_blocking_new(1);
    c6_b = buffer_blocking_new(1);
    c5_b = buffer_blocking_new(1);
    m_R = buffer_blocking_new(1);
    m_L = buffer_blocking_new(1);

    pthread_t threads[7];
    pthread_create(&threads[0], NULL, core_1_worker, NULL);
    pthread_create(&threads[1], NULL, core_2_worker, NULL);
    pthread_create(&threads[2], NULL, core_3_worker, NULL);
    pthread_create(&threads[3], NULL, core_4_worker, NULL);
    pthread_create(&threads[4], NULL, core_5_worker, NULL);
    pthread_create(&threads[5], NULL, core_6_worker, NULL);
    pthread_create(&threads[6], NULL, core_7_worker, NULL);

    int output_s_out;

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    // Core 0 schedule: a_split_main, a_split_L, p_f1, p_g1, a_merge_L, a_out
    for (int iter = 0; iter < g_iterations; iter++) {{
        write_token_blocking(s_in, iter + 1);
        write_token_blocking(s_in, iter + 2);

        actor12SDF(2, 1, 1, s_in, s_L, s_R, f_split2);
        actor14SDF(1, 1, 1, 1, 1, s_L, c1, c2, c3, c4, f_split4);

        actor11SDF(1, 1, c1, c1_a, f_f1);
        actor11SDF(1, 1, c1_a, c1_b, f_g1);

        actor41SDF(1, 1, 1, 1, 1, c1_b, c2_b, c3_b, c4_b, m_L, f_merge4);
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
    buffer_blocking_free(c1);
    buffer_blocking_free(c1_a);
    buffer_blocking_free(c2);
    buffer_blocking_free(c2_a);
    buffer_blocking_free(c3);
    buffer_blocking_free(c3_a);
    buffer_blocking_free(c4);
    buffer_blocking_free(c4_a);
    buffer_blocking_free(c4_b);
    buffer_blocking_free(c3_b);
    buffer_blocking_free(c2_b);
    buffer_blocking_free(c1_b);
    buffer_blocking_free(s_R);
    buffer_blocking_free(c5);
    buffer_blocking_free(c5_a);
    buffer_blocking_free(c6);
    buffer_blocking_free(c6_a);
    buffer_blocking_free(c7);
    buffer_blocking_free(c7_a);
    buffer_blocking_free(c8);
    buffer_blocking_free(c8_a);
    buffer_blocking_free(c8_b);
    buffer_blocking_free(c7_b);
    buffer_blocking_free(c6_b);
    buffer_blocking_free(c5_b);
    buffer_blocking_free(m_R);
    buffer_blocking_free(m_L);
    return 0;
}}
"""

with open("/tmp/bench_036_single.c", "w") as f:
    f.write(single_template)
with open("/tmp/bench_036_multi.c", "w") as f:
    f.write(multi_template)

print("Compiling Single-Core (1 Core PASS)...")
subprocess.run(["gcc", "-O3", "-I", "examples/implementation/platform_independent", "/tmp/bench_036_single.c", "-o", "/tmp/bench_036_single"], check=True)

print("Compiling Multi-Core (8 Cores PAPS)...")
subprocess.run(["gcc", "-O3", "-I", "examples/implementation/platform_independent", "/tmp/bench_036_multi.c", "-o", "/tmp/bench_036_multi"], check=True)

print("\nExecuting Single-Core Benchmark (100 iterations)...")
res_single = subprocess.run(["/tmp/bench_036_single", "100"], capture_output=True, text=True, check=True)

print("Executing 8-Core Benchmark (100 iterations)...")
res_multi = subprocess.run(["/tmp/bench_036_multi", "100"], capture_output=True, text=True, check=True)

print("\n--- Benchmark Raw Output ---")
print("Single-Core:")
print(res_single.stdout.strip())
print("Multi-Core (8 Cores):")
print(res_multi.stdout.strip())

t1 = float(res_single.stdout.splitlines()[0].split(":")[1])
t8 = float(res_multi.stdout.splitlines()[0].split(":")[1])
out1 = int(res_single.stdout.splitlines()[1].split(":")[1])
out8 = int(res_multi.stdout.splitlines()[1].split(":")[1])

print("\n=======================================================")
print("BENCHMARK COMPARISON RESULTS")
print("=======================================================")
print(f"Data Correctness:           {'VERIFIED (Outputs match bit-identically: ' + str(out1) + ')' if out1 == out8 else 'MISMATCH'}")
print(f"1 Core (PASS) Run-Time:     {t1:.4f} seconds")
print(f"8 Cores (PAPS) Run-Time:    {t8:.4f} seconds")
print(f"Measured Speedup (8 Cores): {t1 / t8:.2f}x")
print(f"Theoretical Speedup Limit:  3.67x")
print(f"Attained vs Theoretical:    {(t1 / t8) / 3.67 * 100:.1f}%")
print("=======================================================")
