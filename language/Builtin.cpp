#include "Builtin.h"

BUILTIN_DEFINE(duplicate)
{
	if (arguments.size() != 1)
	{
		printError("duplicate: incorrect number of arguments.");
		return SOLVE_ERROR;
	}
	Token& ev = arguments[0];
	if (ev.tag != Token::INT)
	{
		printError("duplicate: incorrect type of argument.");
		return SOLVE_ERROR;
	}
	solution.emplace_back(2 * ev.intu);
	return SOLVE_OK;
}

BUILTIN_DEFINE(print)
{
	if (arguments.size() != 1)
	{
		printError("print: incorrect number of arguments.");
		return SOLVE_ERROR;
	}
	Token& ev = arguments[0];
	if (ev.tag != Token::STRING)
	{
		printError("print: incorrect type of argument.");
		return SOLVE_ERROR;
	}
	printf(ANSI_YELLOW "[BUILTIN::PRINT] %s\n", ev.str->string);
	solution.push_back(TOKEN_TRUE); // To keep the pattern.
	return SOLVE_OK;
}

BUILTIN_DEFINE(max)
{
	if (arguments.size() != 2)
	{
		printError("max: incorrect number of arguments.");
		return SOLVE_ERROR;
	}
	Token& arg0 = arguments[0];
	if (arg0.tag != Token::INT)
	{
		printError("max: incorrect type of argument 0.");
		return SOLVE_ERROR;
	}
	Token& arg1 = arguments[1];
	if (arg1.tag != Token::INT)
	{
		printError("max: incorrect type of argument 1.");
		return SOLVE_ERROR;
	}
	solution.emplace_back(arg0.intu > arg1.intu ? arg0.intu : arg1.intu);
	return SOLVE_AWAIT;
}

intt NAME_TABLE_id(const char* name)
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
	BUILTIN_REGISTER(duplicate);
	BUILTIN_REGISTER(print);
	BUILTIN_REGISTER(max);

	return 0;
}
