#include "Interpreter.h"

int main()
{
    LANGUAGE_initialize();

    SOLVE_RESULT result = file_import("example/import.txt");

    if (result == SOLVE_OK)
    {
    }

    LANGUAGE_terminate();

    return 0;
}
