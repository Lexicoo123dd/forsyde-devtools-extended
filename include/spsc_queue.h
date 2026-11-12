#ifndef SPSC_QUEUE_H
#define SPSC_QUEUE_H

#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#if defined(_MSC_VER)
#define SPSC_ALIGN(x) __declspec(align(x))
#else
#define SPSC_ALIGN(x) __attribute__((aligned(x)))
#endif

/* -------------------------------------------------------------------------- */
/* Standalone Lock-Free Single-Producer Single-Consumer (SPSC) Queue          */
/* -------------------------------------------------------------------------- */
struct spsc_queue {
    size_t capacity;
    size_t mask;
    void **buffer;

    // Separate cache lines (typically 64 bytes) to prevent false sharing
    SPSC_ALIGN(64) atomic_size_t head;
    SPSC_ALIGN(64) atomic_size_t tail;
};

static inline bool spsc_queue_init(struct spsc_queue *q, size_t capacity) {
    // Capacity must be a power of two
    if (capacity == 0 || (capacity & (capacity - 1)) != 0) {
        return false;
    }
    
    q->buffer = (void **)malloc(sizeof(void *) * capacity);
    if (!q->buffer) {
        return false;
    }

    q->capacity = capacity;
    q->mask = capacity - 1;
    atomic_init(&q->head, 0);
    atomic_init(&q->tail, 0);
    return true;
}

static inline void spsc_queue_free(struct spsc_queue *q) {
    if (q) {
        free(q->buffer);
    }
}

static inline bool spsc_queue_push(struct spsc_queue *q, void *item) {
    const size_t head = atomic_load_explicit(&q->head, memory_order_relaxed);
    const size_t tail = atomic_load_explicit(&q->tail, memory_order_acquire);

    if ((head - tail) >= q->capacity) {
        return false; // Queue is full
    }

    q->buffer[head & q->mask] = item;
    
    // Release the write to the consumer
    atomic_store_explicit(&q->head, head + 1, memory_order_release);
    return true;
}

static inline bool spsc_queue_pop(struct spsc_queue *q, void **item) {
    const size_t tail = atomic_load_explicit(&q->tail, memory_order_relaxed);
    const size_t head = atomic_load_explicit(&q->head, memory_order_acquire);

    if (tail == head) {
        return false; // Queue is empty
    }

    *item = q->buffer[tail & q->mask];
    
    // Release the consumed slot back to the producer
    atomic_store_explicit(&q->tail, tail + 1, memory_order_release);
    return true;
}

#endif // SPSC_QUEUE_H
