#include <algorithm>
#include "common.h"
#include "Interpreter.h"

void builtinErrorVA(const char* format, va_list argp)
{
	vprintf(format, argp);
}

void builtinError_(const char* filename, const char* builtin_name, const char* f, ...)
{
	printf(fERROR);
	printf("Builtin <%s> '%s'. ", builtin_name, filename);
	va_list argp;
	va_start(argp, f);
	builtinErrorVA(f, argp);
	va_end(argp);
	printf("\n");
}

#define file_name() thread->executing.back().pFunction->source->name.c_str()

// https://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
#define builtinError(builtin_name, format, ...) builtinError_(file_name(), builtin_name, format, __VA_ARGS__)

#define BUILTIN_DEFINE(name) SOLVE_RESULT name(std::vector<Token>& arguments, std::vector<Token>& solution, Thread_tL* thread)
#define BUILTIN_REGISTER(name) VALUE_TABLE.insert({ NAME_TABLE_id(#name), Token(name) })

BUILTIN_DEFINE(import)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::STRING)
	{
		builtinError("import", "Argument must be a single '%s'.", tag_name(Token::STRING));
		return SOLVE_ERROR;
	}

	SOLVE_RESULT solve = file_import(arguments[0].u_string->string_get());

	if (solve == SOLVE_ERROR)
		builtinError("import", "Failed to import file '%s'.", arguments[0].u_string->string_get());

	solution.emplace_back(0, 0, (int_tL)solve);

	return solve;
}

BUILTIN_DEFINE(load)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::STRING)
	{
		builtinError("load", "Argument must be a single '%s'.", tag_name(Token::STRING));
		return SOLVE_ERROR;
	}

	Function_tL* func = file_load(arguments[0].u_string->string_get());

	if (!func)
	{
		builtinError("load", "Failed to load file '%s'.", arguments[0].u_string->string_get());
		return SOLVE_ERROR;
	}

	solution.emplace_back(0, 0, func);

	return SOLVE_OK;
}

BUILTIN_DEFINE(version)
{
	if (!arguments.empty())
	{
		builtinError("version", "This function takes no arguments.");
		return SOLVE_ERROR;
	}

	solution.emplace_back(0, 0, String_tL_external(VERSION));
	return SOLVE_OK;
}

BUILTIN_DEFINE(print)
{
	if (arguments.size() != 1)
	{
		builtinError("print", "This function takes a single argument.");
		return SOLVE_ERROR;
	}

	arguments[0].print();
	printf("\n");

	return SOLVE_OK;
}

BUILTIN_DEFINE(max)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::ARRAY || arguments[0].u_array->array.empty())
	{
		builtinError("max", "Argument must be a single non-empty '%s'.", tag_name(Token::ARRAY));
		return SOLVE_ERROR;
	}

	if (std::find_if(
		arguments[0].u_array->array.begin(),
		arguments[0].u_array->array.end(),
		[](const Token& element) { return element.tag != Token::INT && element.tag != Token::FLOAT; }) ==
		arguments[0].u_array->array.end())
	{
		solution.push_back(
			std::move(
				*std::max_element(
					arguments[0].u_array->array.begin(),
					arguments[0].u_array->array.end(),
					[](const Token& left, const Token& right) {
						return ((left.tag == Token::INT) ? left.u_int : left.u_float) < ((right.tag == Token::INT) ? right.u_int : right.u_float);
					}
				)
			)
		);

		return SOLVE_OK;
	}

	builtinError("max", "'%s' must contain only '%s' and '%s'.", tag_name(Token::ARRAY), tag_name(Token::INT), tag_name(Token::FLOAT));
	return SOLVE_ERROR;
}

int_tL NAME_TABLE_id(const char* name)
{
	char* key = (char*)name;
	if (!NAME_TABLE.count(name)) {
		const size_t len = strlen(name) + 1;
		key = new char[len];
		std::memcpy(key, name, len);
	}
	return NAME_TABLE.insert({ key, NAME_TABLE.size() + 1 }).first->second;
}

int register_function()
{
	BUILTIN_REGISTER(import);
	BUILTIN_REGISTER(load);
	BUILTIN_REGISTER(version);
	BUILTIN_REGISTER(print);
	BUILTIN_REGISTER(max);

	return 0;
}
