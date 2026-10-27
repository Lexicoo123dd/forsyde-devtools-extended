#include <stdlib.h>
typedef int token;
#include "include/common.h"
#include <stdio.h>

buffer_blocking *s_in_1;
buffer_blocking *s_in_2;
buffer_blocking *s_out;
buffer_blocking *s_1;
buffer_blocking *s_2;
buffer_blocking *s_3;
buffer_blocking *s_4;
token input_a[4];
token input_b[1];
token output;

pthread_mutex_t scan_mutex = PTHREAD_MUTEX_INITIALIZER;

// A = actor11SDF 2 1 f_a where
//  f_a [x, y] = [x + y]
static void f_a(token *in, token *out) { out[0] = in[0] + in[1]; }

// B = actor11SDF 1 2 f_a where
//  f_b [x] = [x, x+1]
static void f_b(token *in, token *out) {
    out[0] = in[0];
    out[1] = in[0] + 1;
}

// C = actor21SDF (2,1) 1 f_c where
//  f_c [x,y] [z] = [x + y + z]
static void f_c(token *in1, token *in2, token *out) {
    out[0] = in1[0] + in1[1] + in2[0];
}

// D = actor22SDF (2,1) (1,2) f_c where
//  f_d [x,y] [z] = ([x + y + z], [x + y, x + y + z])
static void f_d(token *in1, token *in2, token *out1, token *out2) {
    out1[0] = in1[0] + in1[1] + in2[0];
    out2[0] = in1[0] + in1[1];
    out2[1] = in1[0] + in1[1] + in2[0];
}

void *run_core_0() {
    int ret;
    int i;
    while (1) {
        pthread_mutex_lock(&scan_mutex);
        printf("Read 2 input tokens in s_in_1: ");
        for (i = 0; i < 2; i++) {
            ret = scanf("%d", &input_a[i]);
        }
        pthread_mutex_unlock(&scan_mutex);
        if (ret < 1) {
            break;
        }
        for (i = 0; i < 2; i++) {
            write_token_blocking(s_in_1, input_a[i]);
        }
        // A
        actor11SDF(2, 1, s_in_1, s_1, f_a);
        pthread_mutex_lock(&scan_mutex);
        printf("Read 2 input tokens in s_in_1: ");
        for (i = 0; i < 2; i++) {
            ret = scanf("%d", &input_a[i]);
        }
        pthread_mutex_unlock(&scan_mutex);
        if (ret < 1) {
            break;
        }
        for (i = 0; i < 2; i++) {
            write_token_blocking(s_in_1, input_a[i]);
        }
        // A
        actor11SDF(2, 1, s_in_1, s_1, f_a);
        // C
        actor21SDF(2, 1, 1, s_1, s_4, s_3, f_c);
    }
    return NULL;
}

void *run_core_1() {
    int ret;
    int i;
    while (1) {
        pthread_mutex_lock(&scan_mutex);
        printf("Write 1 token on s_in_2");
        for (i = 0; i < 1; i++) {
            ret = scanf("%d", &input_b[i]);
        }
        pthread_mutex_unlock(&scan_mutex);
        if (ret < 1) {
            break;
        }
        for (i = 0; i < 1; i++) {
            write_token_blocking(s_in_2, input_b[i]);
        }
        // B
        actor11SDF(1, 2, s_in_2, s_2, f_b);
        // D
        actor22SDF(2, 1, 1, 2, s_2, s_3, s_4, s_out, f_d);
        // Write output tokens
        printf("Output: ");
        for (i = 0; i < 2; i++) {
            read_token_blocking(s_out, &output);
            printf("%d ", output);
        }
        printf("\n");
    }
    return NULL;
}

int main(int argc, char *argv[]) {
    // Create FIFO-Buffers for signals

    // Buffer s_in_1: Size: 4
    s_in_1 = buffer_blocking_new(4);
    // Buffer s_in_2: Size: 1
    s_in_2 = buffer_blocking_new(1);
    // Buffer s_out: Size: 2

    s_out = buffer_blocking_new(2);

    // Buffer s_1: Size 2
    s_1 = buffer_blocking_new(2);
    // Buffer s_2: Size 2
    s_2 = buffer_blocking_new(2);
    // Buffer s_3: Size 1 BLOCKING
    s_3 = buffer_blocking_new(1);
    // Buffer s_4: Size 1
    // BLOCKING
    s_4 = buffer_blocking_new(1);

    // delay token
    write_token_blocking(s_4, 0);
    // for int i = 1; i < nproc; i++)
    //  run_core_i()

    pthread_t thread;
    pthread_create(&thread, NULL, run_core_1, NULL);

    run_core_0();

    pthread_join(thread, NULL);

    // Free all buffers
    buffer_blocking_free(s_in_1);
    buffer_blocking_free(s_in_2);
    buffer_blocking_free(s_out);
    buffer_blocking_free(s_1);
    buffer_blocking_free(s_2);
    buffer_blocking_free(s_3);
    buffer_blocking_free(s_4);
    return EXIT_SUCCESS;
}
