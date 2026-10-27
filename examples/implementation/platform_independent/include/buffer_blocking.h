#ifndef BUFFER_BLOCKING_H
#define BUFFER_BLOCKING_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <sched.h>
#include "spsc_queue.h"

/* -------------------------------------------------------------------------- */
/* Low-power CPU pause during spin-wait (cross-platform: PC, ARM, and Pico 2)  */
/* -------------------------------------------------------------------------- */
#if defined(PICO_ON_DEVICE) || defined(PICO_BOARD)
  #include <pico/platform.h>
  #define SPSC_CPU_PAUSE() tight_loop_contents()
#elif defined(__x86_64__) || defined(_M_X64)
  #include <immintrin.h>
  #define SPSC_CPU_PAUSE() _mm_pause()
#elif defined(__aarch64__) || defined(__arm__)
  #define SPSC_CPU_PAUSE() __asm__ volatile("yield")
#else
  #define SPSC_CPU_PAUSE() sched_yield()
#endif

/* -------------------------------------------------------------------------- */
/* Round up arbitrary SDF buffer size to the nearest power of two             */
/* -------------------------------------------------------------------------- */
static inline size_t spsc_next_power_of_two(size_t n) {
    if (n <= 2) return 2;
    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    #if SIZE_MAX > 0xFFFFFFFF
    n |= n >> 32;
    #endif
    return n + 1;
}

/* -------------------------------------------------------------------------- */
/* Unified ForSyDe buffer_blocking adapter using spsc_queue                   */
/* -------------------------------------------------------------------------- */
typedef struct spsc_queue buffer_blocking;

static inline void buffer_blocking_init(buffer_blocking *buf, size_t size) {
    size_t cap = spsc_next_power_of_two(size);
    spsc_queue_init(buf, cap);
}

static inline buffer_blocking *buffer_blocking_new(size_t size) {
    buffer_blocking *buf = (buffer_blocking *)malloc(sizeof(buffer_blocking));
    if (!buf) return NULL;
    size_t cap = spsc_next_power_of_two(size);
    if (!spsc_queue_init(buf, cap)) {
        free(buf);
        return NULL;
    }
    return buf;
}

static inline void buffer_blocking_free(buffer_blocking *buf) {
    if (buf) {
        spsc_queue_free(buf);
        free(buf);
    }
}

static inline void write_token_blocking(buffer_blocking *buf, token data) {
    while (!spsc_queue_push(buf, (void *)(intptr_t)data)) {
        SPSC_CPU_PAUSE();
    }
}

static inline void read_token_blocking(buffer_blocking *buf, token *data) {
    void *item = NULL;
    while (!spsc_queue_pop(buf, &item)) {
        SPSC_CPU_PAUSE();
    }
    *data = (token)(intptr_t)item;
}

#endif // BUFFER_BLOCKING_H
