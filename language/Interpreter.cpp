#include "Interpreter.h"

Token::Token() { tag = Token::NONE; intu = 0; }
Token::Token(intt i) { tag = Token::INT; intu = i; }
Token::Token(floatt f) { tag = Token::FLOAT; floatu = f; }
Token::Token(char* s) { tag = Token::IDENTIFIER; var = s; }
Token::Token(StringShared* s) { tag = Token::STRING; str = s; str->owners++; }
Token::Token(SharedArray* v) { tag = Token::ARRAY; vec = v; vec->owners++; }
Token::Token(Function* f) { tag = Token::FUNCTION; fx = f; }
Token::Token(fBuiltin* f) { tag = Token::BUILTIN; func = f; }
Token::Token(Execution* e) { tag = Token::YIELDED; exe = e; }

Token::Token(tok_tag t, intt i) { tag = t; intu = i; }

Token::Token(const Token& token)
{
	switch (token.tag)
	{
	case Token::NONE:
	case Token::INT:
	case Token::CALL:
	case Token::RETURN:
	case Token::ARRAY_INIT:
	case Token::YIELD:
	case Token::AWAIT:
	case Token::GOTO:
	case Token::JUMP:
	case Token::JUMP_ON_FALSE:
	case Token::JUMP_ON_TRUE:
	case Token::JUMP_NEXT:
		intu = token.intu;
		break;
	case Token::FLOAT:
		floatu = token.floatu;
		break;
	case Token::STRING:
		str = token.str;
		str->owners++;
		break;
	case Token::IDENTIFIER:
		//printError("TOKEN COPY CONSTRUCTOR FOR VARIABLE SHOULD NEVER BE CALLED!!! '%s'.", token.var);
		var = new char[strlen(token.var) + 1];
		std::memcpy(var, token.var, strlen(token.var) + 1);
		break;
	case Token::ARRAY:
		vec = token.vec;
		vec->owners++;
		break;
	case Token::FUNCTION:
		fx = token.fx;
		break;
	case Token::BUILTIN:
		func = token.func;
		break;
	case Token::YIELDED:
		exe = token.exe;
		break;
	default:
		intu = token.intu;
		break;
	}
	tag = token.tag;
}

Token::Token(Token&& token) noexcept
{
	tag = token.tag;
	switch (tag)
	{
	case Token::FLOAT:
		floatu = token.floatu;
		break;
	case Token::IDENTIFIER:
		var = token.var;
		token.var = nullptr;
		break;
	case Token::STRING:
		str = token.str;
		token.str = nullptr;
		break;
	case Token::ARRAY:
		vec = token.vec;
		token.vec = nullptr;
		break;
	case Token::FUNCTION:
		fx = token.fx;
		break;
	case Token::BUILTIN:
		func = token.func;
		break;
	case Token::YIELDED:
		exe = token.exe;
		break;
	default:
		intu = token.intu;
		break;
	}
}

Token& Token::operator=(const Token& token)
{
	if (this != &token) {
		switch (tag)
		{
		case Token::STRING:
			if (str != nullptr) {
				str->owners--;
				if (str->owners <= 0) {
					delete str;
				}
			}
			break;
		case Token::ARRAY:
			if (vec != nullptr) {
				vec->owners--;
				if (vec->owners == 0) {
					delete vec;
				}
			}
			break;
		case Token::IDENTIFIER:
			if (var != nullptr)
				delete[] var;
			break;
		case Token::YIELDED:
			delete exe;
			break;
		default:
			break;
		}

		tag = token.tag;
		switch (tag)
		{
		case Token::NONE:
		case Token::INT:
		case Token::CALL:
		case Token::RETURN:
		case Token::YIELD:
		case Token::ARRAY_INIT:
		case Token::AWAIT:
		case Token::GOTO:
		case Token::JUMP:
		case Token::JUMP_ON_FALSE:
		case Token::JUMP_ON_TRUE:
		case Token::JUMP_NEXT:
			intu = token.intu;
			break;
		case Token::FLOAT:
			floatu = token.floatu;
			break;
		case Token::STRING:
			str = token.str;
			str->owners++;
			break;
		case Token::IDENTIFIER:
			var = token.var;
			break;
		case Token::ARRAY:
			vec = token.vec;
			vec->owners++;
			break;
		case Token::FUNCTION:
			fx = token.fx;
			break;
		case Token::BUILTIN:
			func = token.func;
			break;
		case Token::YIELDED:
			exe = token.exe;
			break;
		default:
			intu = token.intu;
			break;
		}
	}
	return *this;
}

Token& Token::operator=(Token&& token) noexcept
{
	switch (tag)
	{
	case Token::STRING:
		if (str != nullptr) {
			str->owners--;
			if (str->owners <= 0) {
				delete str;
			}
		}
		break;
	case Token::IDENTIFIER:
		if (var != nullptr)
			delete[] var;
		break;
	case Token::ARRAY:
		if (vec != nullptr) {
			vec->owners--;
			if (vec->owners == 0) {
				delete vec;
			}
		}
		break;
	case Token::YIELDED:
		delete exe;
		break;
	default:
		break;
	}
	
	tag = token.tag;

	switch (tag)
	{
	case Token::FLOAT:
		floatu = token.floatu;
		break;
	case Token::STRING:
		str = token.str;
		token.str = nullptr;
		break;
	case Token::IDENTIFIER:
		var = token.var;
		token.var = nullptr;
		break;
	case Token::ARRAY:
		vec = token.vec;
		token.vec = nullptr;
		break;
	case Token::FUNCTION:
		fx = token.fx;
		break;
	case Token::BUILTIN:
		func = token.func;
		break;
	case Token::YIELDED:
		exe = token.exe;
		break;
	default:
		intu = token.intu;
		break;
	}

	return *this;
}

Token::~Token()
{
	switch (tag) {
	case Token::STRING:
		if (str != nullptr) {
			str->owners--;
			if (str->owners <= 0) {
				delete str;
			}
		}
		break;
	case Token::IDENTIFIER:
		if (var != nullptr)
			delete[] var;
		break;
	case Token::ARRAY:
		if (vec != nullptr) {
			vec->owners--;
			if (vec->owners == 0) {
				delete vec;
			}
		}
		break;
	default:
		break;
	}
}

NAME_TABLE_TYPE NAME_TABLE;
VALUE_TABLE_TYPE VALUE_TABLE;

bool GET_VARIABLE_VALUE(Token& variable, Execution& state, bool raw)
{
	const VALUE_TABLE_TYPE::iterator& local = state.LOCALS.find(variable.intu);
	if (local != state.LOCALS.end()) {
		printDebug("Variable id = %lld found of type '%hhd'", variable.intu, local->second.tag);
		variable = (!raw && local->second.tag == Token::YIELDED) ? Token(local->second.exe->pFunction) : local->second;
		return variable.tag != Token::FUNCTION || variable.fx->loaded;
	}

	const VALUE_TABLE_TYPE::iterator& global = VALUE_TABLE.find(variable.intu);
	if (global != VALUE_TABLE.end()) {
		printDebug("Variable id = %lld found of type '%hhd'", variable.intu, global->second.tag);
		variable = (!raw && global->second.tag == Token::YIELDED) ? Token(global->second.exe->pFunction) : global->second;
		return variable.tag != Token::FUNCTION || variable.fx->loaded;
	}

	printError("Variable id = %lld not initialized.", variable.intu);
	return false;
}

bool GET_VARIABLE_VALUE_GLOBAL(Token& variable, Execution& state, bool raw)
{
	// Global scope File Functions should ALWAYS operate only on the GLOBAL VALUE_TABLE.
	const VALUE_TABLE_TYPE::iterator& global = VALUE_TABLE.find(variable.intu);
	if (global != VALUE_TABLE.end()) {
		printDebug("Variable id = %lld found of type '%hhd'", variable.intu, global->second.tag);
		variable = (!raw && global->second.tag == Token::YIELDED) ? Token(global->second.exe->pFunction) : global->second; // return yielded execution or its function instead.
		return variable.tag != Token::FUNCTION || variable.fx->loaded; // Check unloaded function.
	}

	printError("Variable id = %lld not initialized.", variable.intu);
	return false;
}

VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE(Execution& state)
{
	return state.LOCALS;
}

VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE_GLOBAL(Execution& state)
{
	return VALUE_TABLE;
}

#define tagCOUPLE(l, r) (((l) << 8) | (r))

SOLVE_RESULT run(Thread& thread)
{
	while (!thread.calling.empty()) {
		Execution& state = thread.calling.back();

		while (true)
		{
			if (!state.pFunction->loaded) {
				printError("Function '%s' UNLOADED!", state.pFunction->name);
				SOLVE_FAILED;
			}
			if (state.CP < 0 || state.pFunction->program.size() <= state.CP) {
				break;
			}
			const Token token = state.pFunction->program[state.CP];
			printInfo("program_counter = %d.", state.CP);
			state.CP++;

			switch (token.tag)
			{
				//case Token::NONE:
			case Token::INT:
			case Token::FLOAT:
			case Token::STRING:
			case Token::ARRAY:
			case Token::VARIABLE:
				state.solution.push_back(token);
				break;
			case Token::FUNCTION:
				if (token.fx->name != nullptr) {
					VALUE_TABLE.insert_or_assign(token.fx->variable_id, token);
					printInfo("Registered global function '%s'(id = %lld).", token.fx->name, token.fx->variable_id);
					//break; // Push the Token to keep the pattern.
				}
				state.solution.push_back(token);
				break;

			case Token::UNARY_FLIP:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(arg, state, true)) SOLVE_FAILED;
				}
				if (arg.tag == Token::INT) {
					arg.intu = ~arg.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.tag, arg.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_NEGATION:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(arg, state, true)) SOLVE_FAILED;
				}
				if (arg.tag == Token::INT) {
					arg.intu = !arg.intu;
				}
				else if (arg.tag == Token::FLOAT) {
					arg.floatu = !arg.floatu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.tag, arg.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_POSITIVE:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 1, state.solution.size());
					SOLVE_FAILED;
				}
				const Token& arg = state.solution.back();
				if (arg.tag != Token::INT && arg.tag != Token::FLOAT) {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.tag, arg.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_NEGATIVE:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(arg, state, true)) SOLVE_FAILED;
				}
				if (arg.tag == Token::INT) {
					arg.intu = -arg.intu;
				}
				else if (arg.tag == Token::FLOAT) {
					arg.floatu = -arg.floatu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.tag, arg.tag);
					SOLVE_FAILED;
				}
			}
			break;

			case Token::BINARY_ADD:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu += right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token((floatt)left.intu + right.floatu); // Constructor + Move Assignment + Destructor. // TODO: Bypass by assigning tag and value?
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.floatu += (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.floatu += right.floatu;
					break;
				case tagCOUPLE(Token::INT, Token::STRING):
				{
					const size_t l0 = intlen(left.intu);
					const size_t l1 = strlen(right.str->string);
					left = Token(StringShared_init(left.intu, l0, right.str->string, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::INT):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = intlen(right.intu);
					left = Token(StringShared_init(left.str->string, l0, right.intu, l1)); // This must trigger constructor on left so its str_tok gets decremented!
				}
				break;
				case tagCOUPLE(Token::FLOAT, Token::STRING):
				{
					const size_t l0 = snprintf(NULL, 0, "%f", left.floatu);
					const size_t l1 = strlen(right.str->string);
					left = Token(StringShared_init(left.floatu, l0, right.str->string, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::FLOAT):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = snprintf(NULL, 0, "%f", right.floatu);
					left = Token(StringShared_init(left.str->string, l0, right.floatu, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::STRING):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = strlen(right.str->string);
					left = Token(StringShared_init(left.str->string, l0, right.str->string, l1));
				}
				break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_SUBSTRACT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu -= right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token((floatt)left.intu - right.floatu);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.floatu -= (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.floatu -= right.floatu;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_MULTIPLY:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu *= right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token((floatt)left.intu * right.floatu);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.floatu *= (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.floatu *= right.floatu;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_DIVIDE:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					if (right.intu == LANGUAGE_ZERO_INT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.intu /= right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					if (right.floatu == LANGUAGE_ZERO_FLOAT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left = Token((floatt)left.intu / right.floatu);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					if (right.intu == LANGUAGE_ZERO_INT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.floatu /= (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					if (right.floatu == LANGUAGE_ZERO_FLOAT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.floatu /= right.floatu;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_MODULUS:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				if (left.tag == Token::INT && right.tag == Token::INT) {
					if (right.intu == LANGUAGE_ZERO_INT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.intu %= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_SHIFT_LEFT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.intu <<= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_SHIFT_RIGHT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.intu >>= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_LESSER:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu < right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu < right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu < (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu < right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_GREATER:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu > right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu > right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu > (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu > right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_LESSER_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu <= right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu <= right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu <= (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu <= right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_GREATER_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu >= right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu >= right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu >= (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu >= right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL_DOUBLE:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhu' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu == right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu == right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu == (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu == right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL_NOT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu = (left.intu != right.intu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.intu = ((floatt)left.intu != right.floatu) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.floatu != (floatt)right.intu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.floatu != right.floatu) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.tag, left.tag, right.tag);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhu' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, false)) SOLVE_FAILED; // Never assign a yielded Execution: 'raw' = false; Only ever assign the underlying function.
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					printInfo("Registered variable id = %lld of type '%hhd'.", left.intu, right.tag);
					state.GET_ASSIGNMENT_TABLE_(state).insert_or_assign(left.intu, right);
					left = std::move(right);
				}
				else {
					printError("Can not assign a value to a type '%hhd'.", left.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::INDEX:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.tag, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(right, state, true)) SOLVE_FAILED;
				}
				Token& left = state.solution.back();
				if (left.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(left, state, true)) SOLVE_FAILED;
				}
				if (right.tag != Token::INT) {
					printError("Index must be an integer but found a '%hhd' instead.", right.tag);
					SOLVE_FAILED;
				}
				if (right.intu < 0) {
					printError("%d is not a valid index.", right.intu);
					SOLVE_FAILED;
				}
				// TODO: Check for assingment. array[index] = value; If assignment, make sure the assigned value is never YIELDED but its FUNCTION.
				if (left.tag == Token::STRING) {
					if (right.intu < strlen(left.str->string)) {
						char cc[2]{ left.str->string[right.intu], '\0' };
						state.solution.emplace_back(StringShared::init(cc, 1));
					}
					else {
						printError("Index [%d] is out of bounds [%llu]'%s'.", right.intu, strlen(left.str->string), left.str->string);
						SOLVE_FAILED;
					}
				}
				else if (left.tag == Token::ARRAY) {
					if (right.intu < left.vec->a.size()) {
						state.solution.push_back(left.vec->a[right.intu]);
					}
					else {
						printError("Index '%d' is out of bounds '%llu'.", right.intu, left.vec->a.size());
						SOLVE_FAILED;
					}
				}
				else {
					printError("Unable to index a value of type '%hhd'.", left.tag);
					SOLVE_FAILED;
				}
			}
			break;

			//else if (nArgs == 3) {} // TODO: Ternary.

			case Token::CALL:
			{
				const int nArgs = token.intu;
				if (nArgs < 0) {
					printError("Number of arguments must be at least 0 but it was '%d'.", nArgs);
					SOLVE_FAILED;
				}
				if (state.solution.size() < nArgs + 1) {
					printError("Not enough arguments. Call requires '%d' but the stack only has '%llu' elements (function x1 + ARGUMENT NUMBER).", nArgs, state.solution.size());
					SOLVE_FAILED;
				}
				Token calling = state.solution[state.solution.size() - 1 - nArgs]; // TODO: This may be a reference.
				state.last_called = 0;
				if (calling.tag == Token::VARIABLE) {
					state.last_called = calling.intu;
					if (!state.GET_VARIABLE_VALUE_(calling, state, true)) SOLVE_FAILED;
				}
				if (calling.tag == Token::BUILTIN) {
					std::vector<Token> arguments{};
					arguments.reserve(nArgs);
					for (int i = state.solution.size() - nArgs; i < state.solution.size(); i++) {
						if (state.solution[i].tag == Token::VARIABLE) {
							if (!state.GET_VARIABLE_VALUE_(state.solution[i], state, false)) SOLVE_FAILED; // Dereference variables.
						}
						arguments.emplace_back(std::move(state.solution[i]));
					}
					state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Take the function and its arguments out of the solution stack.
					SOLVE_RESULT answer = calling.func(arguments, state.solution);
					if (answer != SOLVE_OK) {
						return answer;
					}
				}
				else if (calling.tag == Token::FUNCTION) {
					Function* func = calling.fx;
					if (!func->loaded) {
						printError("Function unloaded!");
						SOLVE_FAILED;
					}
					if (func->arg_id.size() != nArgs) {
						printError("Incorrect number of arguments. Expected '%llu' but '%d' were given.", func->arg_id.size(), nArgs);
						SOLVE_FAILED;
					}
					Execution exe{ func, (int)thread.calling.size() };
					for (int i = state.solution.size() - nArgs, j = 0; i < state.solution.size(); i++, j++) {
						if (state.solution[i].tag == Token::VARIABLE) { // Dereference variables.
							if (!state.GET_VARIABLE_VALUE_(state.solution[i], state, false)) SOLVE_FAILED;
						}
						state.GET_ASSIGNMENT_TABLE_(exe).insert_or_assign(func->arg_id[j], std::move(state.solution[i]));
					}
					state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Take the function and its arguments out of the solution stack.
					thread.calling.emplace_back(std::move(exe)); // Modifying the thread as the last step since it invalidates references.
					goto execution_end;
				}
				else if (calling.tag == Token::YIELDED) {
					Function* func = calling.exe->pFunction;
					if (!func->loaded) {
						printError("Function unloaded!");
						SOLVE_FAILED;
					}
					state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Not pass in the arguments again.
					//calling.exe->index = thread.calling.size(); // Reassign the Execution index to the last one. // Unnecessary.
					thread.calling.emplace_back(std::move(*calling.exe));
					if (state.last_called != 0) {
						state.GET_ASSIGNMENT_TABLE_(state).insert_or_assign(state.last_called, Token(func)); // Replace locally stored Execution with its Function. // TODO: Check if this is necessary.
					}
					goto execution_end;
					break;
				}
				else {
					printError("Element of type '%hhd' is not callable.", calling.tag);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::RETURN:
				if (token.intu > 0) {
					if (state.index <= 0) {
						printError("Outermost function has nowhere to return to.");
						SOLVE_FAILED;
					}
					if (state.solution.size() <= 0) {
						printError("Expected to return results but the solution stack was empty.");
						SOLVE_FAILED;
					}
					Token& result = state.solution.back();
					if (result.tag == Token::VARIABLE) {
						if (!state.GET_VARIABLE_VALUE_(result, state, false)) SOLVE_FAILED;
					}
					thread.calling[state.index - 1].solution.push_back(std::move(result));
				}
				goto execution_terminate; // This could just be state.CP = -1; to break out of the loop, but goto is probably better here.
				break;
			case Token::YIELD:
			{
				if (state.index <= 0) {
					printError("Yielding function has no outer function to come back to.");
					SOLVE_FAILED;
				}
				if (state.solution.size() > 0) {
					Token& result = state.solution.back();
					if (result.tag == Token::VARIABLE) {
						if (!state.GET_VARIABLE_VALUE_(result, state, false)) SOLVE_FAILED;
					}
					thread.calling[state.index - 1].solution.push_back(std::move(result));
				}
				state.solution.clear();
				if (thread.calling[state.index - 1].last_called != 0) {
					thread.calling[state.index - 1].GET_ASSIGNMENT_TABLE_(thread.calling[state.index - 1]).insert_or_assign(thread.calling[state.index - 1].last_called, Token(new Execution(std::move(state))));
				}
				goto execution_terminate;
			}
			break;
			case Token::AWAIT:
				state.solution.emplace_back((intt)1); // To keep the pattern.
				return SOLVE_AWAIT;
				break;
			case Token::ARRAY_INIT:
			{
				const int nArgs = token.intu;
				if (nArgs < 0) {
					printError("Number of array elements must be at least 0 but it was '%d'.", nArgs);
					SOLVE_FAILED;
				}
				if (state.solution.size() < nArgs) {
					printError("Not enough elements. Array requires '%d' but the stack only has '%llu' elements.", nArgs, state.solution.size());
					SOLVE_FAILED;
				}
				SharedArray* v = new SharedArray{ 0 }; // Build Array.
				for (int i = state.solution.size() - nArgs; i < state.solution.size(); i++) {
					if (state.solution[i].tag == Token::VARIABLE) { // Dereference variables.
						if (!state.GET_VARIABLE_VALUE_(state.solution[i], state, false)) SOLVE_FAILED;
					}
					v->a.emplace_back(std::move(state.solution[i]));
				}
				state.solution.erase(state.solution.end() - nArgs, state.solution.end());
				state.solution.emplace_back(v);
			}
			break;
			case Token::GOTO:
			{
				if (state.solution.size() != 1) {
					printError("Token::GOTO requires only '1' value on the stack but '%llu' were found.", state.solution.size());
					SOLVE_FAILED;
				}
				Token& label_name = state.solution.back();
				if (label_name.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(label_name, state, true)) SOLVE_FAILED;
				}
				if (label_name.tag != Token::STRING) {
					printError("Token::GOTO label must be identified by a string type but found '%hhd'.", label_name.tag);
					SOLVE_FAILED;
				}
				const LABEL_TABLE_TYPE::iterator& label_num = state.pFunction->labels.find(label_name.str->string);
				if (label_num == state.pFunction->labels.end()) {
					printError("No label named '%s'.", label_name.str->string);
					SOLVE_FAILED;
				}
				state.CP = label_num->second;
				state.solution.clear();
			}
			break;
			case Token::JUMP:
				state.solution.emplace_back(token.intu); // TODO: Remove
				state.CP = token.intu;
			case Token::JUMP_NEXT:
				// TODO: Remove.
				if (state.solution.size() == 1) {
					if (state.solution[0].tag == Token::INT) {
						printInfo("Result is = '%lld'.", state.solution[0].intu);
					}
					else if (state.solution[0].tag == Token::FLOAT) {
						printInfo("Result is = '%f'.", state.solution[0].floatu);
					}
					else if (state.solution[0].tag == Token::STRING) {
						printInfo("Result is = '%s'.", state.solution[0].str->string);
					}
					state.ES++;
					if (state.ES >= 60) {
						printInfo("EMERGENCY STOP!!!");
						return SOLVE_ERROR;
					}
				}
				else {
					printError("HAHAHAHAHA!!");
					return SOLVE_ERROR;
				}
				state.solution.clear();
				break;
			case Token::JUMP_ON_FALSE:
			case Token::JUMP_ON_TRUE:
			{
				if (state.solution.size() != 1) {
					printError("JUMP_ON should evaluate a single condition but the solution stack contains '%llu' elements instead.", state.solution.size());
					SOLVE_FAILED;
				}
				Token& condition = state.solution.back();
				if (condition.tag == Token::VARIABLE) {
					if (!state.GET_VARIABLE_VALUE_(condition, state, true)) SOLVE_FAILED;
				}
				if (condition.tag == Token::INT) {
					if ((condition.intu == LANGUAGE_FALSE) == (token.tag != Token::JUMP_ON_TRUE)) {
						state.CP = token.intu;
					}
					state.solution.clear();
				}
				else if (condition.tag == Token::FLOAT) {
					if ((condition.floatu == LANGUAGE_ZERO_FLOAT) == (token.tag != Token::JUMP_ON_TRUE)) {
						state.CP = token.intu;
					}
					state.solution.clear();
				}
				else {
					printError("Can not resolve a '%hhd' type to a boolean.", condition.tag);
					SOLVE_FAILED;
				}
			}
			break;
			default:
				printError("AAAAAAAAAAAAAAAHHHH!!!");
				SOLVE_FAILED;
			}
		}
	execution_terminate:
		thread.calling.pop_back();
	execution_end:
		; // Empty statement to allow compiling.
	}

	return SOLVE_OK;
}
