#ifndef H_COMMON
#define H_COMMON

#include <stdio.h>
#include <stdarg.h> // va_list // https://stackoverflow.com/questions/695982/passing-an-ellipsis-to-another-variadic-function?noredirect=1&lq=1
#include <limits.h> // ULLONG_MAX
#include <stdint.h> // intptr_t
#include <inttypes.h> // PRI

#define LANGUAGE_VERSION "b.1.1.4"

// https://stackoverflow.com/questions/51616057/how-to-determine-pointer-size-preprocessor-c
#if UINTPTR_MAX >= ULLONG_MAX
#define PTR64
#else
#define PTR32
#endif // UINTPTR_MAX >= ULLONG_MAX

// https://scaryreasoner.wordpress.com/2009/02/28/checking-sizeof-at-compile-time/
#define BUILD_BUG_ON(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

// https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
#define STRINGIZING(s) #s

// https://stackoverflow.com/a/3219471
#define ANSI_RED     "\x1b[31m"
#define ANSI_GREEN   "\x1b[32m"
#define ANSI_YELLOW  "\x1b[33m"
//#define ANSI_BLUE    "\x1b[34m"
//#define ANSI_MAGENTA "\x1b[35m"
#define ANSI_CYAN    "\x1b[36m"
#define ANSI_WHITE   "\x1b[0m"

extern const char* fERROR;

typedef unsigned short lin_num;
#define fLIN PRIu16
typedef unsigned short col_num;
#define fCOL PRIu16

void printLanguageError(const char* type, const char* subtype, const char* filename, lin_num line, col_num column, const char* format, va_list argp);

#endif // !H_COMMON
