#include "Builtin.h"

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

	return 0;
}
