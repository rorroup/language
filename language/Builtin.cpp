#include "Builtin.h"

BUILTIN_DEFINE(duplicate)
{
	if (arguments.size() != 1)
	{
		printError("duplicate: incorrect number of arguments.");
		return false;
	}
	Token& ev = arguments[0];
	if (ev.type_ != Token::INT)
	{
		printError("duplicate: incorrect type of argument.");
		return false;
	}
	solution.push_back({ Token::INT, (long int)(2 * ev.num) });
	return true;
}

BUILTIN_DEFINE(print)
{
	if (arguments.size() != 1)
	{
		printError("print: incorrect number of arguments.");
		return false;
	}
	Token& ev = arguments[0];
	if (ev.type_ != Token::STRING)
	{
		printError("print: incorrect type of argument.");
		return false;
	}
	printf(ANSI_YELLOW "[BUILTIN::PRINT] %s\n", STR_OWN_STR(ev.str));
	solution.push_back({ Token::INT, (long int)1 }); // To keep the pattern.
	return true;
}

BUILTIN_DEFINE(max)
{
	if (arguments.size() != 2)
	{
		printError("max: incorrect number of arguments.");
		return SOLVE_ERROR;
	}
	Token& arg0 = arguments[0];
	if (arg0.type_ != Token::INT)
	{
		printError("max: incorrect type of argument 0.");
		return SOLVE_ERROR;
	}
	Token& arg1 = arguments[1];
	if (arg1.type_ != Token::INT)
	{
		printError("max: incorrect type of argument 1.");
		return SOLVE_ERROR;
	}
	solution.push_back({ Token::INT, (long int)(arg0.num > arg1.num ? arg0.num : arg1.num) });
	return SOLVE_AWAIT;
}

int register_function()
{
	BUILTIN_REGISTER(duplicate);
	BUILTIN_REGISTER(print);
	BUILTIN_REGISTER(max);

	return 0;
}
