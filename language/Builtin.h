#ifndef H_BUILTIN
#define H_BUILTIN

#include "common.h"
#include "Interpreter.h"

#define BUILTIN_DEFINE(name) s_lang name(Result& result, std::vector<Result>& args)
#define BUILTIN_REGISTER(name) VALUE_TABLE.insert({ #name, Result{EXPRESSION_BUILTIN, (t_value)(new BuiltIn{ #name, name }) } })

int register_function();

#endif // !H_BUILTIN
