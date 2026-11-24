#ifndef BUFFER_H
#define BUFFER_H

#include <stdlib.h>
#include <stdint.h>
#include "buffer_nonblocking.h"
#include "buffer_blocking.h"

typedef enum {
    BUFFER_NONBLOCKING = 0,
    BUFFER_BLOCKING = 1
} buffer_type_t;

typedef struct buffer {
    buffer_type_t type;
    union {
        buffer_nonblocking *nb;
        buffer_blocking *b;
    } handle;
} buffer_t;

static inline buffer_t *buffer_nonblocking_new(size_t size) {
    buffer_t *buf = (buffer_t *)malloc(sizeof(buffer_t));
    if (!buf) return NULL;
    buf->type = BUFFER_NONBLOCKING;
    buf->handle.nb = buffer_nonblocking_raw_new(size);
    return buf;
}

static inline buffer_t *buffer_blocking_new(size_t size) {
    buffer_t *buf = (buffer_t *)malloc(sizeof(buffer_t));
    if (!buf) return NULL;
    buf->type = BUFFER_BLOCKING;
    buf->handle.b = buffer_blocking_raw_new(size);
    return buf;
}

static inline void buffer_free(buffer_t *buf) {
    if (!buf) return;
    if (buf->type == BUFFER_NONBLOCKING) {
        buffer_nonblocking_raw_free(buf->handle.nb);
    } else {
        buffer_blocking_raw_free(buf->handle.b);
    }
    free(buf);
}

static inline void write_token(buffer_t *buf, token data) {
    if (buf->type == BUFFER_NONBLOCKING) {
        write_token_nonblocking(buf->handle.nb, data);
    } else {
        write_token_blocking(buf->handle.b, data);
    }
}

static inline void read_token(buffer_t *buf, token *data) {
    if (buf->type == BUFFER_NONBLOCKING) {
        read_token_nonblocking(buf->handle.nb, data);
    } else {
        read_token_blocking(buf->handle.b, data);
    }
}

static inline size_t contained_tokens(buffer_t *buf) {
    if (buf->type == BUFFER_NONBLOCKING) {
        return contained_tokens_nonblocking(buf->handle.nb);
    } else {
        return contained_tokens_blocking(buf->handle.b);
    }
}

#define buffer_nonblocking_free(buf) buffer_free(buf)
#define buffer_blocking_free(buf) buffer_free(buf)

#endif // BUFFER_H
