#include "Builtin.h"

BUILTIN_DEFINE(duplicate)
{
	if (args.size() != 1)
	{
		printError("duplicate: incorrect number of arguments.");
		return ERROR_ARGUMENT_MISMATCH;
	}
	Result ev = args[0];
	if (ev.type_ != VALUE_INT)
	{
		printError("duplicate: incorrect type of argument.");
		return ERROR_TYPE_MISMATCH;
	}
	result = { VALUE_INT, (t_value)((intptr_t)(2 * (intptr_t)ev.value)) };
	return ERROR_NONE;
}

int register_function()
{
	BUILTIN_REGISTER(duplicate);

	return 0;
}
