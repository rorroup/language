#include "common.h"

const char* fERROR = ANSI_RED "[Error] " ANSI_WHITE;

const char* fFILEPOS = "'%s'<%" fLIN ",%" fCOL "> ";

void printLanguageError(const char* type, const char* subtype, const char* filename, lin_num line, col_num column, const char* format, va_list argp)
{
    printf(fERROR);
    printf("%s ", type);
    printf(fFILEPOS, filename, line, column);
    printf("%s: ", subtype);
    vprintf(format, argp);
    printf("\n");
}
