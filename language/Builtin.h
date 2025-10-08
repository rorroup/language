#ifndef H_BUILTIN
#define H_BUILTIN

#include <algorithm>
#include "common.h"
#include "Interpreter.h"

#define BUILTIN_DEFINE(name) SOLVE_RESULT name(std::vector<Token>& arguments, std::vector<Token>& solution, Thread_tL* thread)
#define BUILTIN_REGISTER(name) VALUE_TABLE.insert({ NAME_TABLE_id(#name), Token(name) })

int register_function();

#endif // !H_BUILTIN
