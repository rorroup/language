#ifndef H_COMMON
#define H_COMMON

#include <stdint.h>

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

// https://stackoverflow.com/a/36935966
typedef intptr_t s_lang;

enum LANGUAGE_TYPE : s_lang
{
	LANGUAGE_INVALID = 0,
	LANGUAGE_BUFFER = 0x00010000,
	LANGUAGE_TOKEN = 0x00020000,
	LANGUAGE_VALUE = 0x00040000,
	LANGUAGE_OPERATOR = 0x00080000,
	LANGUAGE_KEYWORD = 0x00100000,
	LANGUAGE_DELIMITER = 0x00200000,
	LANGUAGE_EXPRESSION = 0x00400000,
	LANGUAGE_ERROR = 0x00800000,

	LANGUAGE_TYPE = 0xFFFF0000,
	LANGUAGE_MASK = 0x0000FFFF,
};

#endif // !H_COMMON
