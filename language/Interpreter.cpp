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
	num = 0;
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
		num = token.num;
		break;
	case Token::FLOAT:
		frac = token.frac;
		break;
	case Token::STRING:
		STR_OWNERS(token.str)++;
	case Token::IDENTIFIER:
		str = token.str;
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
		num = token.num;
	}
	type_ = token.type_;
}

Token::Token(tok_tag t, long int i)
{
	type_ = t;
	num = i;
}

Token::Token(tok_tag t, float f)
{
	type_ = t;
	frac = f;
}

Token::Token(tok_tag t, char* s)
{
	type_ = t;
	str = s;
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

Token::~Token()
{
	switch (type_) {
	case Token::STRING:
	{
		str_own& owners = STR_OWNERS(str);
		owners--;
		if (owners <= 0) {
			delete[] str;
		}
	}
		break;
	case Token::ARRAY:
		vec->owners--;
		if (vec->owners == 0) {
			delete vec;
		}
		break;
	default:
		break;
	}
}

Token& Token::operator=(const Token& token)
{
	if (this != &token) {
		switch (type_)
		{
		case Token::STRING:
		{
			str_own& owners = STR_OWNERS(str);
			owners--;
			if (owners <= 0) {
				delete[] str;
			}
		}
			break;
		case Token::ARRAY:
			vec->owners--;
			if (vec->owners == 0) {
				delete vec;
			}
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
			num = token.num;
			break;
		case Token::FLOAT:
			frac = token.frac;
			break;
		case Token::STRING:
			STR_OWNERS(token.str)++;
		case Token::IDENTIFIER:
			str = token.str;
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
			num = token.num;
			break;
		}
	}
	return *this;
}


#define tagCOUPLE(l, r) (((l) << 8) | (r))

#define GET_VARIABLE_VALUE(var) \
const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(var.str); \
if (local == state.LOCALS.end()) { \
	const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(var.str); \
	if (iter == VALUE_TABLE.end()) { \
		printError("Variable '%s' not initialized.", var.str); \
		SOLVE_FAILED; \
	} \
	printDebug("Variable '%s' found of type '%hhd'", var.str, iter->second.type_); \
	var = iter->second; \
} \
else { \
	printDebug("Variable '%s' found of type '%hhd'", var.str, local->second.type_); \
	var = local->second; \
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
					arg.num = ~arg.num;
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
					arg.num = !arg.num;
				}
				else if (arg.type_ == Token::FLOAT) {
					arg.frac = !arg.frac;
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
					arg.num = -arg.num;
				}
				else if (arg.type_ == Token::FLOAT) {
					arg.frac = -arg.frac;
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
					left.num += right.num;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (float)left.num + right.frac);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.frac += (float)right.num;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.frac += right.frac;
					break;
				case tagCOUPLE(Token::INT, Token::STRING):
				{
					const size_t l0 = 1 + (left.num == 0 ? 1 : (left.num > 0 ? log10(left.num) : 1 + log10(-left.num)));
					const size_t l1 = strlen(STR_OWN_STR(right.str));
					const size_t l = STR_OWN_STR(l0 + l1 + 1);
					str_tok s = new char[l];
					STR_OWNERS(s) = 1;
					_itoa_s(left.num, STR_OWN_STR(s), l0 + 1, 10);
					memcpy(STR_OWN_STR(s) + l0, STR_OWN_STR(right.str), l1 + 1);
					left = Token(Token::STRING, s);
				}
				break;
				case tagCOUPLE(Token::STRING, Token::INT):
				{
					const size_t l0 = strlen(STR_OWN_STR(left.str));
					const size_t l1 = 1 + (right.num == 0 ? 1 : (right.num > 0 ? log10(right.num) : 1 + log10(-right.num)));
					const size_t l = STR_OWN_STR(l0 + l1 + 1);
					str_tok s = new char[l];
					STR_OWNERS(s) = 1;
					memcpy(STR_OWN_STR(s), STR_OWN_STR(left.str), l0);
					_itoa_s(right.num, STR_OWN_STR(s) + l0, l1 + 1, 10);
					left = Token(Token::STRING, s); // This must trigger constructor on left so its str_tok gets decremented!
					break;
				}
				case tagCOUPLE(Token::FLOAT, Token::STRING):
				{
					const size_t l0 = snprintf(NULL, 0, "%f", left.frac);
					const size_t l1 = strlen(STR_OWN_STR(right.str));
					const size_t l = STR_OWN_STR(l0 + l1 + 1);
					str_tok s = new char[l];
					STR_OWNERS(s) = 1;
					snprintf(STR_OWN_STR(s), l0 + 1, "%f", left.frac);
					memcpy(STR_OWN_STR(s) + l0, STR_OWN_STR(right.str), l1 + 1);
					left = Token(Token::STRING, s);
				}
				break;
				case tagCOUPLE(Token::STRING, Token::FLOAT):
				{
					const size_t l0 = strlen(STR_OWN_STR(left.str));
					const size_t l1 = snprintf(NULL, 0, "%f", right.frac);
					const size_t l = STR_OWN_STR(l0 + l1 + 1);
					str_tok s = new char[l];
					STR_OWNERS(s) = 1;
					memcpy(STR_OWN_STR(s), STR_OWN_STR(left.str), l0);
					snprintf(STR_OWN_STR(s) + l0, l1 + 1, "%f", right.frac);
					left = Token(Token::STRING, s);
				}
				break;
				case tagCOUPLE(Token::STRING, Token::STRING):
				{
					const size_t l0 = strlen(STR_OWN_STR(left.str));
					const size_t l1 = strlen(STR_OWN_STR(right.str));
					const size_t l = STR_OWN_STR(l0 + l1 + 1);
					str_tok s = new char[l];
					STR_OWNERS(s) = 1;
					memcpy(STR_OWN_STR(s), STR_OWN_STR(left.str), l0);
					memcpy(STR_OWN_STR(s) + l0, STR_OWN_STR(right.str), l1 + 1);
					left = Token(Token::STRING, s);
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
					left.num -= right.num;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (float)left.num - right.frac);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.frac -= (float)right.num;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.frac -= right.frac;
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
					left.num *= right.num;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(Token::FLOAT, (float)left.num * right.frac);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.frac *= (float)right.num;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.frac *= right.frac;
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
					if (right.num == 0) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.num /= right.num;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					if (right.frac == 0.0f) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left = Token(Token::FLOAT, (float)left.num / right.frac);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					if (right.num == 0) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.frac /= (float)right.num;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					if (right.frac == 0.0f) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.frac /= right.frac;
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
					if (right.num == 0) {
						printError("Zero division.");
						SOLVE_FAILED;
					}
					left.num %= right.num;
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
					left.num <<= right.num;
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
					left.num >>= right.num;
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
					left.num = (left.num < right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num < right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac < (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac < right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					left.num = (left.num > right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num > right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac > (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac > right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					left.num = (left.num <= right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num <= right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac <= (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac <= right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					left.num = (left.num >= right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num >= right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac >= (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac >= right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					left.num = (left.num == right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num == right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac == (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac == right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					left.num = (left.num != right.num) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.num = ((float)left.num != right.frac) ? LANGUAGE_TRUE : LANGUAGE_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = (left.frac != (float)right.num) ? TOKEN_TRUE : TOKEN_FALSE;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = (left.frac != right.frac) ? TOKEN_TRUE : TOKEN_FALSE;
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
					state.LOCALS.insert_or_assign(left.str, right);
					printInfo("Registered variable '%s' of type '%hhd'.", left.str, right.type_);
					left = right;
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
				if (right.num < 0) {
					printError("%d is not a valid index.", right.num);
					SOLVE_FAILED;
				}
				// TODO: Check for assingment. array[index] = value;
				if (left.type_ == Token::STRING) {
					if (right.num < strlen(STR_OWN_STR(left.str))) {
						str_tok cc = new char[STR_OWN_STR(2)];
						STR_OWNERS(cc) = 1;
						cc[STR_OWN_BYTES] = left.str[STR_OWN_STR(right.num)];
						cc[STR_OWN_STR(2) - 1] = '\0';
						state.solution.push_back(Token(Token::STRING, cc));
					}
					else {
						printError("Index [%d] is out of bounds [%llu]'%s'.", right.num, strlen(STR_OWN_STR(left.str)), STR_OWN_STR(left.str));
						SOLVE_FAILED;
					}
				}
				else if (left.type_ == Token::ARRAY) {
					if (right.num < left.vec->a.size()) {
						state.solution.push_back(left.vec->a[right.num]);
					}
					else {
						printError("Index '%d' is out of bounds '%llu'.", right.num, left.vec->a.size());
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
				const int nArgs = token.num;
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
							const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].str);
							if (local == state.LOCALS.end()) {
								const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].str);
								if (iter == VALUE_TABLE.end()) {
									printError("Variable '%s' not initialized.", state.solution[i].str);
									SOLVE_FAILED;
								}
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, iter->second.type_);
								arguments.push_back(iter->second);
							}
							else {
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, local->second.type_);
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
					sprintf_s(state.last_called, CHAR_YIELDED "%p", (void*)calling.num); // Save function index as "*HEX_PTR".
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
							const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].str);
							if (local == state.LOCALS.end()) {
								const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].str);
								if (iter == VALUE_TABLE.end()) {
									printError("Variable '%s' not initialized.", state.solution[i].str);
									SOLVE_FAILED;
								}
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, iter->second.type_);
								val = &(iter->second);
							}
							else {
								printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, local->second.type_);
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
							exe.LOCALS.insert_or_assign(key, state.solution[i]);
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
				if (token.num > 0) {
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
				state.solution.push_back(Token(Token::INT, (long int)1)); // To keep the pattern.
				return SOLVE_AWAIT;
				break;
			case Token::ARRAY_INIT:
			{
				const int nArgs = token.num;
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
						const UMAP_kpCHAR(const char*, Token)::iterator& local = state.LOCALS.find(state.solution[i].str);
						if (local == state.LOCALS.end()) {
							const UMAP_kpCHAR(const char*, Token)::iterator& iter = VALUE_TABLE.find(state.solution[i].str);
							if (iter == VALUE_TABLE.end()) {
								printError("Variable '%s' not initialized.", state.solution[i].str);
								SOLVE_FAILED;
							}
							printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, iter->second.type_);
							v->a.push_back(iter->second);
						}
						else {
							printDebug("Variable '%s' found of type '%hhd'", state.solution[i].str, local->second.type_);
							v->a.push_back(local->second);
						}
					}
					else {
						v->a.push_back(std::move(state.solution[i]));
					}
				}
				state.solution.erase(state.solution.end() - nArgs, state.solution.end());
				state.solution.push_back(Token(Token::ARRAY, v));
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
				const UMAP_kpCHAR(const char*, const size_t)::iterator& label_num = state.pFunction->labels.find(STR_OWN_STR(label_name.str));
				if (label_num == state.pFunction->labels.end()) {
					printError("No label named '%s'.", STR_OWN_STR(label_name.str));
					SOLVE_FAILED;
				}
				state.CP = label_num->second;
				state.solution.clear();
			}
			break;
			case Token::JUMP:
				state.solution.push_back(Token{ Token::INT, (long int)token.num }); // TODO: Remove
				state.CP = token.num;
			case Token::JUMP_NEXT:
				// TODO: Remove.
				if (state.solution.size() == 1) {
					if (state.solution[0].type_ == Token::INT) {
						printInfo("Result is = '%d'.", state.solution[0].num);
					}
					else if (state.solution[0].type_ == Token::FLOAT) {
						printInfo("Result is = '%f'.", state.solution[0].frac);
					}
					else if (state.solution[0].type_ == Token::STRING) {
						printInfo("Result is = '%s'.", STR_OWN_STR(state.solution[0].str));
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
					if ((condition.num == LANGUAGE_FALSE) == (token.type_ != Token::JUMP_ON_TRUE)) {
						state.CP = token.num;
					}
					state.solution.clear();
				}
				else if (condition.type_ == Token::FLOAT) {
					if ((condition.frac == LANGUAGE_FALSE) == (token.type_ != Token::JUMP_ON_TRUE)) {
						state.CP = token.num;
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
