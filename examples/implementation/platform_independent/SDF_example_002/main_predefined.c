#include <pthread.h>
typedef int token;

#include "include/common.h"
#include "input.h"
#include <stdio.h>
#include <stdlib.h>

/* Buffers */
buffer_blocking *s_in_a;
buffer_blocking *s_in_b;
buffer_blocking *s_out;
buffer_blocking *a_1;
buffer_blocking *a_2;
buffer_blocking *a_3;
buffer_blocking *a_4;

/* Actor Functions */
// A = actor11SDF 2 1 f_a where f_a [x, y] = [x + y]
static void f_a(token *in, token *out) {
    out[0] = in[0] + in[1];
}

// B = actor11SDF 1 2 f_b where f_b [x] = [x, x+1]
static void f_b(token *in, token *out) {
    out[0] = in[0];
    out[1] = in[0] + 1;
}

// C = actor21SDF (2,1) 1 f_c where f_c [x,y] [z] = [x + y + z]
static void f_c(token *in1, token *in2, token *out) {
    out[0] = in1[0] + in1[1] + in2[0];
}

// D = actor22SDF (2,1) (1,2) f_d where f_d [x,y] [z] = ([x + y + z], [x + y, x + y + z])
static void f_d(token *in1, token *in2, token *out1, token *out2) {
    out1[0] = in1[0] + in1[1] + in2[0];
    out2[0] = in1[0] + in1[1];
    out2[1] = in1[0] + in1[1] + in2[0];
}

/* Core 0: Runs Actor A (twice) and Actor C */
void *run_core_0(void *arg) {
    (void)arg;
    printf("[Core 0] Native Thread ID: %p started\n", (void *)pthread_self());
    for (int iter = 0; iter < NUM_ITERATIONS; iter++) {
        // First firing of A: write 2 tokens from static array
        for (int i = 0; i < 2; i++) {
            write_token_blocking(s_in_a, input_s_in_a[iter * 4 + i]);
        }
        actor11SDF(2, 1, s_in_a, a_1, f_a);

        // Second firing of A: write next 2 tokens from static array
        for (int i = 0; i < 2; i++) {
            write_token_blocking(s_in_a, input_s_in_a[iter * 4 + 2 + i]);
        }
        actor11SDF(2, 1, s_in_a, a_1, f_a);

        // Firing of C (consumes 2 from a_1, 1 from a_4, produces 1 into a_3)
        actor21SDF(2, 1, 1, a_1, a_4, a_3, f_c);
    }
    return NULL;
}

/* Core 1: Runs Actor B (once) and Actor D */
void *run_core_1(void *arg) {
    (void)arg;
    printf("[Core 1] Native Thread ID: %p started\n", (void *)pthread_self());
    token output;
    for (int iter = 0; iter < NUM_ITERATIONS; iter++) {
        // Firing of B: write 1 token from static array
        for (int i = 0; i < 1; i++) {
            write_token_blocking(s_in_b, input_s_in_b[iter * 1 + i]);
        }
        actor11SDF(1, 2, s_in_b, a_2, f_b);

        // Firing of D (consumes 2 from a_2, 1 from a_3, produces 1 into a_4, 2 into s_out)
        actor22SDF(2, 1, 1, 2, a_2, a_3, a_4, s_out, f_d);

        // Read and print output tokens
        printf("Output: ");
        for (int i = 0; i < 2; i++) {
            read_token_blocking(s_out, &output);
            printf("%d ", output);
        }
        printf("\n");
    }
    return NULL;
}

int main(void) {
    init();

    // Create FIFO-Buffers for signals
    s_in_a = buffer_blocking_new(4);
    s_in_b = buffer_blocking_new(1);
    s_out  = buffer_blocking_new(2);
    a_1    = buffer_blocking_new(2);
    a_2    = buffer_blocking_new(2);
    a_3    = buffer_blocking_new(1);
    a_4    = buffer_blocking_new(1);

    // Initial delay token on channel a_4
    write_token_blocking(a_4, 0);

    // Launch worker threads
    pthread_t thread1;
    pthread_create(&thread1, NULL, run_core_1, NULL);

    // Run Core 0 on main thread
    run_core_0(NULL);

    // Wait for Core 1 to complete all iterations
    pthread_join(thread1, NULL);

    // Free all buffers
    buffer_blocking_free(s_in_a);
    buffer_blocking_free(s_in_b);
    buffer_blocking_free(s_out);
    buffer_blocking_free(a_1);
    buffer_blocking_free(a_2);
    buffer_blocking_free(a_3);
    buffer_blocking_free(a_4);

    return EXIT_SUCCESS;
}
