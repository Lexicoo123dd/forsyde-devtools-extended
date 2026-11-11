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

#endif

