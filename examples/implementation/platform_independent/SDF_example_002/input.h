#ifndef INPUT_H
#define INPUT_H

#define NUM_ITERATIONS 7

/* Static input arrays matching the tokens in test.in */
/* Actor A consumes 4 tokens per iteration (2 firings x 2 tokens) */
static const token input_s_in_a[NUM_ITERATIONS * 4] = {
    1, 1, 2, 4,   // Iteration 0
    2, 2, 2, 2,   // Iteration 1
    4, 1, 2, 1,   // Iteration 2
    1, 1, 1, 1,   // Iteration 3
    0, 0, 0, 0,   // Iteration 4
    0, 0, 0, 0,   // Iteration 5
    1, 5, 2, 3    // Iteration 6
};

/* Actor B consumes 1 token per iteration (1 firing x 1 token) */
static const token input_s_in_b[NUM_ITERATIONS * 1] = {
    1,            // Iteration 0
    2,            // Iteration 1
    1,            // Iteration 2
    1,            // Iteration 3
    0,            // Iteration 4
    0,            // Iteration 5
    4             // Iteration 6
};

#endif // INPUT_H
