#include "Interpreter.h"

size_t kpCHAR_HASH::operator()(const char* k) const
{
	size_t hash = -1;
	static const int s = (sizeof(size_t) - sizeof(char)) * 8; // TODO: + 1?
	int i = 0;
	while (k[i] != '\0') {
		hash = (size_t)k[i] ^ (hash << (i % s));
		i++;
	}
	return hash;
}

bool kpCHAR_EQUAL::operator()(const char* lhs, const char* rhs) const
{
	if (lhs == nullptr || rhs == nullptr) {
		return false;
	}
	return strlen(lhs) == strlen(rhs) && strcmp(lhs, rhs) == 0;
}

UMAP_kpCHAR(const char*, Token) VALUE_TABLE;


Token::Token()
{
	type_ = Token::NONE;
	intu = 0;
}

Token::Token(tok_tag t, int i)
{
	type_ = t;
	intu = i;
}

Token::Token(tok_tag t, intt i)
{
	type_ = t;
	intu = i;
}

Token::Token(tok_tag t, floatt f)
{
	type_ = t;
	floatu = f;
}

Token::Token(tok_tag t, char* s)
{
	type_ = t;
	var = s;
}

Token::Token(tok_tag t, StringShared* s)
{
	type_ = t;
	str = s;
	str->owners++;
}

Token::Token(tok_tag t, SharedArray* v)
{
	type_ = t;
	vec = v;
	vec->owners++;
}

Token::Token(tok_tag t, Function* f)
{
	type_ = t;
	fx = f;
}

Token::Token(tok_tag t, char(*f)(std::vector<Token>&, std::vector<Token>&))
{
	type_ = t;
	func = f;
}

Token::Token(tok_tag t, Execution* e)
{
	type_ = t;
	exe = e;
}

Token::Token(const Token& token)
{
	switch (token.type_)
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
	type_ = token.type_;
}

Token::Token(Token&& token) noexcept
{
	type_ = token.type_;
	switch (type_)
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
		switch (type_)
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
		default:
			break;
		}

		type_ = token.type_;
		switch (type_)
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
	switch (type_)
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
	default:
		break;
	}
	
	type_ = token.type_;

	switch (type_)
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
	switch (type_) {
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

#define tagCOUPLE(l, r) (((l) << 8) | (r))

#define GET_VARIABLE_VALUE(variable) \
const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(variable.var); \
if (local == state.LOCALS.end()) { \
	const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(variable.var); \
	if (iter == VALUE_TABLE.end()) { \
		printError("Variable '%s' not initialized.", variable.var); \
		SOLVE_FAILED; \
	} \
	printDebug("Variable '%s' found of type '%hhd'", variable.var, iter->second.type_); \
	variable = iter->second; \
} \
else { \
	printDebug("Variable '%s' found of type '%hhd'", variable.var, local->second.type_); \
	variable = local->second; \
} \


char run(Thread& thread)
{
	while (!thread.calling.empty()) {
		Execution& state = thread.calling.back();
		const std::vector<Token>& tokens = state.pFunction->program;

		while (0 <= state.CP && state.CP < tokens.size()) {
			printInfo("program_counter = %d.", state.CP);
			const Token& token = tokens[state.CP];
			state.CP++;

			switch (token.type_)
			{
				//case Token::NONE:
			case Token::INT:
			case Token::FLOAT:
			case Token::STRING:
			case Token::ARRAY:
			case Token::IDENTIFIER:
				state.solution.push_back(token);
				break;
			case Token::FUNCTION:
				if (token.fx->name != nullptr) {
					size_t len = strlen(token.fx->name) + 1;
					char* func_name = new char[len];
					memcpy(func_name, token.fx->name, len);
					VALUE_TABLE.insert_or_assign(func_name, token);
					printInfo("Registered global function '%s'.", func_name);
					//break; // Push the Token to keep the pattern.
				}
				state.solution.push_back(token);
				break;

			case Token::UNARY_FLIP:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(arg);
				}
				if (arg.type_ == Token::INT) {
					arg.intu = ~arg.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.type_, arg.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_NEGATION:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(arg);
				}
				if (arg.type_ == Token::INT) {
					arg.intu = !arg.intu;
				}
				else if (arg.type_ == Token::FLOAT) {
					arg.floatu = !arg.floatu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.type_, arg.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_POSITIVE:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 1, state.solution.size());
					SOLVE_FAILED;
				}
				const Token& arg = state.solution.back();
				if (arg.type_ != Token::INT && arg.type_ != Token::FLOAT) {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.type_, arg.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::UNARY_NEGATIVE:
			{
				if (state.solution.size() < 1) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 1, state.solution.size());
					SOLVE_FAILED;
				}
				Token& arg = state.solution.back();
				if (arg.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(arg);
				}
				if (arg.type_ == Token::INT) {
					arg.intu = -arg.intu;
				}
				else if (arg.type_ == Token::FLOAT) {
					arg.floatu = -arg.floatu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd'.", token.type_, arg.type_);
					SOLVE_FAILED;
				}
			}
			break;

			case Token::BINARY_ADD:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu += right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (floatt)left.intu + right.floatu); // Constructor + Move Assignment + Destructor. // TODO: Bypass by assigning tag and value?
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
					left = Token(Token::STRING, StringShared_init(left.intu, l0, right.str->string, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::INT):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = intlen(right.intu);
					left = Token(Token::STRING, StringShared_init(left.str->string, l0, right.intu, l1)); // This must trigger constructor on left so its str_tok gets decremented!
				}
				break;
				case tagCOUPLE(Token::FLOAT, Token::STRING):
				{
					const size_t l0 = snprintf(NULL, 0, "%f", left.floatu);
					const size_t l1 = strlen(right.str->string);
					left = Token(Token::STRING, StringShared_init(left.floatu, l0, right.str->string, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::FLOAT):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = snprintf(NULL, 0, "%f", right.floatu);
					left = Token(Token::STRING, StringShared_init(left.str->string, l0, right.floatu, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::STRING):
				{
					const size_t l0 = strlen(left.str->string);
					const size_t l1 = strlen(right.str->string);
					left = Token(Token::STRING, StringShared_init(left.str->string, l0, right.str->string, l1));
				}
				break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_SUBSTRACT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu -= right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (floatt)left.intu - right.floatu);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.floatu -= (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.floatu -= right.floatu;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_MULTIPLY:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.intu *= right.intu;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (floatt)left.intu * right.floatu);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.floatu *= (floatt)right.intu;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.floatu *= right.floatu;
					break;
				default:
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_DIVIDE:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					left = Token(Token::FLOAT, (floatt)left.intu / right.floatu);
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_MODULUS:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				if (left.type_ == Token::INT && right.type_ == Token::INT) {
					if (right.intu == LANGUAGE_ZERO_INT) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.intu %= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_SHIFT_LEFT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				if (left.type_ == Token::INT && right.type_ == Token::INT) {
					left.intu <<= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_SHIFT_RIGHT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				if (left.type_ == Token::INT && right.type_ == Token::INT) {
					left.intu >>= right.intu;
				}
				else {
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::BINARY_LESSER:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_GREATER:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_LESSER_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_GREATER_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL_DOUBLE:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhu' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL_NOT:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				switch (tagCOUPLE(left.type_, right.type_))
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
					printError("Operator '%hhd' may not operate on values of type '%hhd' and '%hhd'.", token.type_, left.type_, right.type_);
					SOLVE_FAILED;
					break;
				}
			}
			break;
			case Token::BINARY_EQUAL:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhu' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					const UMAP_kpCHAR(const char*, Token)::iterator& val = state.LOCALS.find(left.var);
					printInfo("Registered variable '%s' of type '%hhd'.", left.var, right.type_);
					if (val == state.LOCALS.end()) {
						state.LOCALS.insert({ left.var, right });
						left.var = nullptr;
					}
					else {
						val->second = right;
					}
					left = std::move(right);
				}
				else {
					printError("Can not assign a value to a type '%hhd'.", left.type_);
					SOLVE_FAILED;
				}
			}
			break;
			case Token::INDEX:
			{
				if (state.solution.size() < 2) {
					printError("Operator '%hhd' takes '%d' arguments but only '%llu' are available.", token.type_, 2, state.solution.size());
					SOLVE_FAILED;
				}
				Token right = std::move(state.solution.back());
				state.solution.pop_back();
				if (right.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(right);
				}
				Token& left = state.solution.back();
				if (left.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(left);
				}
				if (right.type_ != Token::INT) {
					printError("Index must be an integer but found a '%hhd' instead.", right.type_);
					SOLVE_FAILED;
				}
				if (right.intu < 0) {
					printError("%d is not a valid index.", right.intu);
					SOLVE_FAILED;
				}
				// TODO: Check for assingment. array[index] = value;
				if (left.type_ == Token::STRING) {
					if (right.intu < strlen(left.str->string)) {
						char cc[2]{ left.str->string[right.intu], '\0' };
						state.solution.emplace_back(Token::STRING, StringShared::init(cc, 1));
					}
					else {
						printError("Index [%d] is out of bounds [%llu]'%s'.", right.intu, strlen(left.str->string), left.str->string);
						SOLVE_FAILED;
					}
				}
				else if (left.type_ == Token::ARRAY) {
					if (right.intu < left.vec->a.size()) {
						state.solution.push_back(left.vec->a[right.intu]);
					}
					else {
						printError("Index '%d' is out of bounds '%llu'.", right.intu, left.vec->a.size());
						SOLVE_FAILED;
					}
				}
				else {
					printError("Unable to index a value of type '%hhd'.", left.type_);
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
				Token calling = state.solution[state.solution.size() - 1 - nArgs];
				if (calling.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(calling);
				}
				if (calling.type_ == Token::BUILTIN) {
					std::vector<Token> arguments{};
					for (int i = state.solution.size() - nArgs; i < state.solution.size(); i++) {
						if (state.solution[i].type_ == Token::IDENTIFIER) { // Dereference variables.
							const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].var);
							if (local == state.LOCALS.end()) {
								const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].var);
								if (iter == VALUE_TABLE.end()) {
									printError("Variable '%s' not initialized.", state.solution[i].var);
									SOLVE_FAILED;
								}
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, iter->second.type_);
								arguments.push_back(iter->second);
							}
							else {
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, local->second.type_);
								arguments.push_back(local->second);
							}
						}
						else {
							arguments.push_back(std::move(state.solution[i]));
						}
					}
					state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Take the function and its arguments out of the solution stack.
					char answer = calling.func(arguments, state.solution);
					if (answer != SOLVE_OK) {
						return answer;
					}
				}
				else if (calling.type_ == Token::FUNCTION) {
					sprintf_s(state.last_called, CHAR_YIELDED "%p", (void*)calling.intu); // Save function index as "*HEX_PTR".
					const UMAP_kpCHAR(const char*, Token)::iterator& yielded = state.LOCALS.find(state.last_called);
					if (yielded != state.LOCALS.end()) {
						state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Not pass in the arguments again.
						thread.calling.push_back(std::move(*yielded->second.exe));
						const char* yielded_key = yielded->first;
						state.LOCALS.erase(yielded); // Remove locally stored Execution.
						delete[] yielded_key; // This could be stored somewhere else instead in case this same function yields.
						goto execution_end;
						break;
					}
					Function* func = calling.fx;
					if (func->argnames.size() != nArgs) {
						printError("Incorrect number of arguments. Expected '%llu' but '%d' were given.", func->argnames.size(), nArgs);
						SOLVE_FAILED;
					}
					Execution exe{ func, (int)thread.calling.size() };
					for (int i = state.solution.size() - nArgs, j = 0; i < state.solution.size(); i++, j++) {
						if (state.solution[i].type_ == Token::IDENTIFIER) { // Dereference variables.
							const Token* val = nullptr;
							const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].var);
							if (local == state.LOCALS.end()) {
								const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].var);
								if (iter == VALUE_TABLE.end()) {
									printError("Variable '%s' not initialized.", state.solution[i].var);
									SOLVE_FAILED;
								}
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, iter->second.type_);
								val = &(iter->second);
							}
							else {
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, local->second.type_);
								val = &(local->second);
							}
							const int len = func->argnames[j].size() + 1;
							char* key = new char[len];
							memset(key, '\0', len);
							memcpy(key, func->argnames[j].c_str(), len - 1);
							exe.LOCALS.insert_or_assign(key, *val);
						}
						else {
							const int len = func->argnames[j].size() + 1;
							char* key = new char[len];
							memset(key, '\0', len);
							memcpy(key, func->argnames[j].c_str(), len - 1);
							exe.LOCALS.insert_or_assign(key, std::move(state.solution[i]));
						}
					}
					state.solution.erase(state.solution.end() - 1 - nArgs, state.solution.end()); // Take the function and its arguments out of the solution stack.
					thread.calling.push_back(std::move(exe)); // Modifying the thread as the last step since it invalidates references.
					goto execution_end;
				}
				else {
					printError("Element of type '%hhd' is not callable.", calling.type_);
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
					if (result.type_ == Token::IDENTIFIER) {
						GET_VARIABLE_VALUE(result);
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
					if (result.type_ == Token::IDENTIFIER) {
						GET_VARIABLE_VALUE(result);
					}
					thread.calling[state.index - 1].solution.push_back(std::move(result));
				}
				state.solution.clear();
				//const size_t len = strlen(thread.calling[state.index - 1].last_called) + 1;
				char* yielded_key = new char[ADDR_FORMAT_LEN];
				memcpy(yielded_key, thread.calling[state.index - 1].last_called, ADDR_FORMAT_LEN);
				thread.calling[state.index - 1].LOCALS.insert({ yielded_key, Token{ Token::YIELDED, new Execution{std::move(state)} } }); // TODO: This never gets deallocated if this function is not called again.
				goto execution_terminate;
			}
			break;
			case Token::AWAIT:
				state.solution.emplace_back(Token::INT, (intt)1); // To keep the pattern.
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
					if (state.solution[i].type_ == Token::IDENTIFIER) { // Dereference variables.
						const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].var);
						if (local == state.LOCALS.end()) {
							const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].var);
							if (iter == VALUE_TABLE.end()) {
								printError("Variable '%s' not initialized.", state.solution[i].var);
								SOLVE_FAILED;
							}
							printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, iter->second.type_);
							v->a.push_back(iter->second);
						}
						else {
							printDebug("Variable '%s' found of type '%hhd'", state.solution[i].var, local->second.type_);
							v->a.push_back(local->second);
						}
					}
					else {
						v->a.push_back(std::move(state.solution[i]));
					}
				}
				state.solution.erase(state.solution.end() - nArgs, state.solution.end());
				state.solution.emplace_back(Token::ARRAY, v);
			}
			break;
			case Token::GOTO:
			{
				if (state.solution.size() != 1) {
					printError("Token::GOTO requires only '1' value on the stack but '%llu' were found.", state.solution.size());
					SOLVE_FAILED;
				}
				Token& label_name = state.solution.back();
				if (label_name.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(label_name);
				}
				if (label_name.type_ != Token::STRING) {
					printError("Token::GOTO label must be identified by a string type but found '%hhd'.", label_name.type_);
					SOLVE_FAILED;
				}
				const UMAP_kpCHAR(const char*, const size_t)::iterator& label_num = state.pFunction->labels.find(label_name.str->string);
				if (label_num == state.pFunction->labels.end()) {
					printError("No label named '%s'.", label_name.str->string);
					SOLVE_FAILED;
				}
				state.CP = label_num->second;
				state.solution.clear();
			}
			break;
			case Token::JUMP:
				state.solution.emplace_back(Token::INT, token.intu); // TODO: Remove
				state.CP = token.intu;
			case Token::JUMP_NEXT:
				// TODO: Remove.
				if (state.solution.size() == 1) {
					if (state.solution[0].type_ == Token::INT) {
						printInfo("Result is = '%d'.", state.solution[0].intu);
					}
					else if (state.solution[0].type_ == Token::FLOAT) {
						printInfo("Result is = '%f'.", state.solution[0].floatu);
					}
					else if (state.solution[0].type_ == Token::STRING) {
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
				if (condition.type_ == Token::IDENTIFIER) {
					GET_VARIABLE_VALUE(condition);
				}
				if (condition.type_ == Token::INT) {
					if ((condition.intu == LANGUAGE_FALSE) == (token.type_ != Token::JUMP_ON_TRUE)) {
						state.CP = token.intu;
					}
					state.solution.clear();
				}
				else if (condition.type_ == Token::FLOAT) {
					if ((condition.floatu == LANGUAGE_ZERO_FLOAT) == (token.type_ != Token::JUMP_ON_TRUE)) {
						state.CP = token.intu;
					}
					state.solution.clear();
				}
				else {
					printError("Can not resolve a '%hhd' type to a boolean.", condition.type_);
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
