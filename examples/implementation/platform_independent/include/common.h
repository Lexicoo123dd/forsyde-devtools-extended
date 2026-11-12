#ifndef COMMON_H
#define COMMON_H

#include "buffer.h"
#include "actor_templates.h"

/** @brief Selected target platform is a Linux PC */
#define PC 1

/** @brief Selected target platform is a Pico 2 board */
#define PICO2 2

/**  @brief Platform modes, use this parameter to select platform.
 *   Available platforms so far:
 *  @param PC - Linux PC (pthreads)
 *  @param PICO2 - PICO 2 board
 */
#ifndef PLATFORM
#define PLATFORM PC
#endif

#if PLATFORM == PC
    static void init(void) {}
#endif

#if PLATFORM == PICO2
    #include <bsp.h>
    static void init(void) {
        BSP_Init();
    }
#endif

#ifdef BENCHMARK_TIMING
#include <time.h>
#include <stdio.h>
static struct timespec _bench_start, _bench_end;
#define BENCH_START() clock_gettime(CLOCK_MONOTONIC, &_bench_start)
#define BENCH_END() do { \
    clock_gettime(CLOCK_MONOTONIC, &_bench_end); \
    double _elapsed = (_bench_end.tv_sec - _bench_start.tv_sec) + (_bench_end.tv_nsec - _bench_start.tv_nsec) * 1e-9; \
    fprintf(stderr, "EXECUTION_TIME_SEC: %.9f\n", _elapsed); \
} while(0)
#else
#define BENCH_START() ((void)0)
#define BENCH_END() ((void)0)
#endif

#endif

