#ifndef H_COMMON
#define H_COMMON

#include <limits.h> // ULLONG_MAX
#include <stdint.h> // intptr_t
#include <inttypes.h> // PRI

// https://stackoverflow.com/questions/51616057/how-to-determine-pointer-size-preprocessor-c
#if UINTPTR_MAX >= ULLONG_MAX
#define PTR64
#else
#define PTR32
#endif // UINTPTR_MAX >= ULLONG_MAX

// https://scaryreasoner.wordpress.com/2009/02/28/checking-sizeof-at-compile-time/
#define BUILD_BUG_ON(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

// https://stackoverflow.com/a/3219471
#define ANSI_RED     "\x1b[31m"
#define ANSI_GREEN   "\x1b[32m"
#define ANSI_YELLOW  "\x1b[33m"
//#define ANSI_BLUE    "\x1b[34m"
//#define ANSI_MAGENTA "\x1b[35m"
#define ANSI_CYAN    "\x1b[36m"
#define ANSI_WHITE   "\x1b[0m"

// https://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
#define printInfo(format, ...)    printf(ANSI_GREEN  "[INFO] "    ANSI_WHITE format "\n", __VA_ARGS__)
#define printWarning(format, ...) printf(ANSI_YELLOW "[WARNING] " ANSI_WHITE format "\n", __VA_ARGS__)
#define printError(format, ...)   printf(ANSI_RED    "[ERROR] "   ANSI_WHITE format "\n", __VA_ARGS__)
#define printDebug(format, ...)   printf(ANSI_CYAN   "[DEBUG] "   ANSI_WHITE format "\n", __VA_ARGS__)

#endif // !H_COMMON
