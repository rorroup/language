#include "Parser.h"

#define PARSE_ERROR -1

/*
Returns the number of elements in the sequence.
*/
short int Parser::parse_sequence(Function& function, const tok_tag separator_symbol = Token::COMMA)
{
	std::vector<Token>& program = function.program;
	int elements = 0;
	while (tokenIndex < tokens.size())
	{
		tok_tag parsed = parse_operation(function);
		if (parsed == PARSE_ERROR) {
			return PARSE_ERROR;
		}
		if (parsed == 0) {
			break;
		}
		if (elements >= SHRT_MAX) {
			printError("Sequences can not be grater that '%d' elements long.", SHRT_MAX);
			return PARSE_ERROR;
		}
		elements++;
		if (tokenIndex >= tokens.size()) {
			break;
		}
		if (tokens[tokenIndex].tag != separator_symbol) {
			break;
		}
		tokenIndex++;
	}
	return elements;
}

/*
Returns the id of the last parsed structure.
*/
tok_tag Parser::parse_operation(Function& function)
{
	static const Token TOKEN_POSITIVE(Token::UNARY_POSITIVE, 100);
	static const Token TOKEN_NEGATIVE(Token::UNARY_NEGATIVE, 100);

	std::vector<Token>& program = function.program;
	std::vector<Token> hold;
	bool operandLast = false; // TODO: deprecate in favor of typeLast.
	bool functionLast = false; // TODO: deprecate in favor of typeLast.
	tok_tag typeLast = 0;
	int parenthesis = 0;

	while (tokenIndex < tokens.size())
	{
		Token& token = tokens[tokenIndex];

		switch (token.tag)
		{
		case Token::NONE:
		case Token::INT:
		case Token::FLOAT:
		case Token::STRING:
		case Token::IDENTIFIER:
			if (operandLast) {
				if (functionLast) { // No operator thus assume new instruction.
					goto flush_hold;
					break;
				}
				printError("Operands may not be immediately followed by another one.");
				return PARSE_ERROR;
			}
			if (token.tag == Token::IDENTIFIER) {
				const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ token.var, NAME_TABLE.size() + 1 });
				program.emplace_back(Token::VARIABLE, insertion.first->second);
				if (insertion.second) token.var = nullptr; // Steal.
			}
			else {
				program.push_back(std::move(tokens[tokenIndex]));
			}
			tokenIndex++;
			operandLast = true;
			functionLast = false;
			typeLast = token.tag; // tag is being interrogated after Token was moved. TODO: Move this code up.
			break;

		case Token::SEMICOLON:
		case Token::COMMA:
		case Token::BRACE_OPEN:
		case Token::BRACE_CLOSE:
		case Token::BRACKET_CLOSE:
		case Token::COLON:
			goto flush_hold; //https://en.cppreference.com/w/cpp/language/goto
			break;
		case Token::BINARY_EQUAL:
			hold.push_back(std::move(tokens[tokenIndex]));
			tokenIndex++;
			operandLast = false;
			functionLast = false;
			typeLast = token.tag;
			break;
		case Token::BRACKET_OPEN:
			if (operandLast) { // Index.
				tokenIndex++;
				if (parse_operation(function) == PARSE_ERROR) {
					return PARSE_ERROR;
				}
				if (tokenIndex >= tokens.size()) {
					printError("Expected ']' to indicate array index.");
					return PARSE_ERROR;
				}
				if (tokens[tokenIndex].tag != Token::BRACKET_CLOSE) {
					printError("Expected ']' to indicate array index.");
					return PARSE_ERROR;
				}
				tokenIndex++;
				program.emplace_back(Token::INDEX, 0);
				operandLast = true;
				functionLast = false;
				typeLast = tokens[tokenIndex].tag;
			}
			else { // Array.
				tokenIndex++;
				short numer_of_elements = parse_sequence(function, Token::COMMA);
				if (numer_of_elements == PARSE_ERROR) {
					return PARSE_ERROR;
				}
				if (tokenIndex >= tokens.size()) {
					printError("Expected ']' to end the array.");
					return PARSE_ERROR;
				}
				if (tokens[tokenIndex].tag != Token::BRACKET_CLOSE) {
					printError("Expected ']' to end the array.");
					return PARSE_ERROR;
				}
				tokenIndex++;
				program.emplace_back(Token::ARRAY_INIT, numer_of_elements);
				operandLast = true;
				functionLast = false;
				typeLast = tokens[tokenIndex].tag;
			}
			break;
		case Token::PARENTHESIS_OPEN:
			if (operandLast) { // Call.
				tokenIndex++;
				short numer_of_arguments = parse_sequence(function, Token::COMMA);
				if (numer_of_arguments == PARSE_ERROR) {
					return PARSE_ERROR;
				}
				if (tokenIndex >= tokens.size()) {
					printError("Expected ')' to end the function call.");
					return PARSE_ERROR;
				}
				if (tokens[tokenIndex].tag != Token::PARENTHESIS_CLOSE) {
					printError("Expected ')' to end the function call.");
					return PARSE_ERROR;
				}
				tokenIndex++;
				program.emplace_back(Token::CALL, numer_of_arguments);
				operandLast = true;
				functionLast = false;
				typeLast = tokens[tokenIndex].tag; // TODO: Check. type is being assigned after the index has been increased.
			}
			else { // Parenthesised expression.
				hold.push_back(std::move(tokens[tokenIndex]));
				tokenIndex++;
				parenthesis++;
				operandLast = false;
				functionLast = false;
				typeLast = token.tag; // TODO: Review.
			}
			break;
		case Token::PARENTHESIS_CLOSE:
			if (parenthesis == 0) {
				goto flush_hold;
				break;
			}
			if (!operandLast) {
				printError("Parenthesis content must end in an operand.");
				return PARSE_ERROR;
				break;
			}
			parenthesis--;
			while (!hold.empty())
			{
				if (hold.back().tag == Token::PARENTHESIS_OPEN) {
					break;
				}
				program.push_back(std::move(hold.back()));
				hold.pop_back();
			}
			if (hold.empty()) {
				printError("Mismatched Parenthesis! 0");
				return PARSE_ERROR;
				break;
			}
			if (hold.back().tag != Token::PARENTHESIS_OPEN) {
				printError("Mismatched Parenthesis!");
				return PARSE_ERROR;
				break;
			}
			hold.pop_back();
			tokenIndex++;
			operandLast = true;
			functionLast = false;
			typeLast = token.tag;
			break;

		case Token::FUNCTION_DEF:
		{
			if (operandLast) {
				if (functionLast) {
					goto flush_hold;
					break;
				}
				printError("Operands may not be immediately followed by another one.");
				return PARSE_ERROR;
			}
			Function* func = new Function{};
			if (parse_function(*func) == PARSE_ERROR) {
				return PARSE_ERROR;
			}
			if (func->name != nullptr) {
				printError("Global functions can not form part of an operation.");
				return PARSE_ERROR;
			}
			program.emplace_back(func);
			loaded->push_back(func); // Register function.
			operandLast = true;
			functionLast = true;
			typeLast = token.tag;
		}
		break;

		case Token::IF:
		case Token::ELSE:
		case Token::FOR:
		case Token::WHILE:
		case Token::DO:
		case Token::BREAK:
		case Token::CONTINUE:
		case Token::LABEL:
		case Token::RETURN:
		case Token::YIELD:
		case Token::AWAIT:
		case Token::GOTO:
			goto flush_hold;
			break;

		case Token::VARIABLE:
		case Token::ARRAY:
		case Token::INDEX:
		case Token::ARRAY_INIT:
		case Token::CALL:
		case Token::BUILTIN:
		case Token::FUNCTION:
		case Token::YIELDED:

		case Token::JUMP:
		case Token::JUMP_ON_FALSE:
		case Token::JUMP_ON_TRUE:
		case Token::JUMP_NEXT:
			printError("Token type '%hhd' may never come from the tokenized stream.", token.tag);
			return PARSE_ERROR;
			break;

		default: // Operator.
			if (!operandLast) {
				if (token.tag == Token::BINARY_ADD) token = TOKEN_POSITIVE;
				if (token.tag == Token::BINARY_SUBSTRACT) token = TOKEN_NEGATIVE;
			}
			while (!hold.empty())
			{
				if (token.intu <= hold.back().intu) { // Left Parenthesis and Equal have the lowest precedence.
					program.push_back(std::move(hold.back()));
					hold.pop_back();
				}
				else {
					break;
				}
			}
			hold.push_back(std::move(tokens[tokenIndex]));
			tokenIndex++;
			operandLast = false;
			functionLast = false;
			typeLast = token.tag;
			break;
		}
	}

flush_hold:
	while (!hold.empty())
	{
		program.push_back(std::move(hold.back()));
		hold.pop_back();
	}

	if (parenthesis != 0) {
		printError("Parenthesis are mismatched.");
		return PARSE_ERROR;
	}

	if (Token::UNARY_FLIP <= typeLast && typeLast < Token::SEMICOLON) {
		printError("Non empty operations must end in an operand.");
		return PARSE_ERROR;
	}

	return typeLast;
}

char Parser::parse_if(Function& function, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = function.program;
	bool branches = false;
	int condition_index = -1;
	std::vector<size_t> block_end_index{};

	while (tokenIndex < tokens.size()) {
		int keyword_num = tokens[tokenIndex].tag;

		if (keyword_num == Token::IF) {
			if (branches) {
				break;
			}
		}
		else if (keyword_num == Token::ELSE) {
			if (!branches) { // IF
				printError("Expected 'if'.");
				return PARSE_ERROR;
			}

			tokenIndex++;
			if (tokenIndex < tokens.size()) {
				if (tokens[tokenIndex].tag == Token::IF) {
					keyword_num = Token::IF; // ELSE IF
				}
				else {
					tokenIndex--;
				}
			}
			else {
				tokenIndex--;
			}
		}
		else {
			if (!branches) { // IF
				printError("Expected 'if'.");
				return PARSE_ERROR;
			}
			break;
		}
		tokenIndex++;

		if (branches) {
			block_end_index.push_back(program.size());
			program.emplace_back(Token::JUMP, -1);

			program[condition_index].intu = program.size();
		}

		condition_index = -1;
		if (keyword_num == Token::IF) {
			if (tokenIndex >= tokens.size()) {
				printError("Expected '('.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::PARENTHESIS_OPEN) {
				printError("Expected '('.");
				return PARSE_ERROR;
			}
			tokenIndex++;

			tok_tag parsed = parse_operation(function);
			if (parsed == PARSE_ERROR) {
				return PARSE_ERROR;
			}
			if (parsed == 0) {
				printError("If condition can not be empty.");
				return PARSE_ERROR;
			}
			condition_index = program.size();
			program.emplace_back(Token::JUMP_ON_FALSE, -1);

			if (tokenIndex >= tokens.size()) {
				printError("Expected ')'.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::PARENTHESIS_CLOSE) {
				printError("Expected ')'.");
				return PARSE_ERROR;
			}
			tokenIndex++;
		}
		if (tokenIndex >= tokens.size()) {
			printError("Expected '{'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::BRACE_OPEN) {
			printError("Expected '{'.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		if (parse_instructions(function, interrupts) == PARSE_ERROR) {
			return PARSE_ERROR;
		}

		if (tokenIndex >= tokens.size()) {
			printError("Expected '}'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::BRACE_CLOSE) {
			printError("Expected '}'.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		branches = true;
		if (keyword_num == Token::ELSE) {
			break;
		}
	}

	if (condition_index >= 0) {
		program[condition_index].intu = program.size();
	}

	for (const size_t& index : block_end_index) {
		program[index].intu = program.size();
	}

	return true;
}

char Parser::parse_loop(Function& function, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = function.program;
	if (tokenIndex >= tokens.size()) {
		printError("Expected loop formulation.");
		return PARSE_ERROR;
	}
	const Token& keyword = tokens[tokenIndex];
	if (keyword.tag != Token::FOR && keyword.tag != Token::WHILE && keyword.tag != Token::DO) {
		printError("Expected loop formulation.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (keyword.tag != Token::DO) {
		if (tokenIndex >= tokens.size()) {
			printError("Expected '('.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::PARENTHESIS_OPEN) {
			printError("Expected '('.");
			return PARSE_ERROR;
		}
		tokenIndex++;
	}

	if (keyword.tag == Token::FOR) {
		tok_tag loop_init = parse_operation(function);
		if (loop_init == PARSE_ERROR) {
			return PARSE_ERROR;
		}
		if (loop_init != 0) {
			program.emplace_back(Token::JUMP_NEXT, 1);
		}

		if (tokenIndex >= tokens.size()) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::SEMICOLON) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		tokenIndex++;
	}

	Function condition_content{};
	tok_tag loop_condition = 0;
	if (keyword.tag != Token::DO) {
		loop_condition = parse_operation(condition_content);
		if (loop_condition == PARSE_ERROR) {
			return PARSE_ERROR;
		}
		if (loop_condition == 0 && keyword.tag == Token::WHILE) {
			printError("While loop condition can not be missing.");
			return PARSE_ERROR;
		}
	}

	tok_tag loop_increment = 0;
	Function increment_content{};
	if (keyword.tag == Token::FOR) {
		if (tokenIndex >= tokens.size()) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::SEMICOLON) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		loop_increment = parse_operation(increment_content);
		if (loop_increment == PARSE_ERROR) {
			return PARSE_ERROR;
		}
	}

	tok_size loop_start = program.size();

	if (keyword.tag == Token::FOR && loop_increment != 0) {
		const tok_size init_jump = program.size();
		program.emplace_back(Token::JUMP, -1);
		loop_start = program.size();
		for (Token& token : increment_content.program) {
			program.push_back(std::move(token)); // TODO: Concatenate. Iterator or something probably.
		}
		program.emplace_back(Token::JUMP_NEXT, 1);
		program[init_jump].intu = program.size();
	}

	tok_size condition_jump = -1;
	if (keyword.tag != Token::DO && loop_condition != 0) {
		for (Token& token : condition_content.program) {
			program.push_back(std::move(token)); // TODO: Concatenate.
		}
		condition_jump = program.size();
		program.emplace_back(Token::JUMP_ON_FALSE, -1);
	}

	if (keyword.tag != Token::DO) {
		if (tokenIndex >= tokens.size()) {
			printError("Expected ')'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::PARENTHESIS_CLOSE) {
			printError("Expected ')'.");
			return PARSE_ERROR;
		}
		tokenIndex++;
	}

	if (tokenIndex >= tokens.size()) {
		printError("Expected '{'.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::BRACE_OPEN) {
		printError("Expected '{'.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	std::vector<int> interruptions[2] = { std::vector<int>{}, std::vector<int>{} };
	if (parse_instructions(function, interruptions) == PARSE_ERROR) {
		return PARSE_ERROR;
	}

	if (tokenIndex >= tokens.size()) {
		printError("Expected '}'.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::BRACE_CLOSE) {
		printError("Expected '}'.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (keyword.tag == Token::DO) {
		if (tokenIndex >= tokens.size()) {
			printError("Expected 'while'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::WHILE) {
			printError("Expected 'while'.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		if (tokenIndex >= tokens.size()) {
			printError("Expected '('.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::PARENTHESIS_OPEN) {
			printError("Expected '('.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		for (const auto& index : interruptions[1]) // continue;
		{
			program[index].intu = program.size();
		}

		loop_condition = parse_operation(function);
		if (loop_condition == PARSE_ERROR) {
			return PARSE_ERROR;
		}
		if (loop_condition == 0) {
			printError("Do While loop condition can not be missing.");
			return PARSE_ERROR;
		}
		program.emplace_back(Token::JUMP_ON_TRUE, loop_start);

		if (tokenIndex >= tokens.size()) {
			printError("Expected ')'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::PARENTHESIS_CLOSE) {
			printError("Expected ')'.");
			return PARSE_ERROR;
		}
		tokenIndex++;

		if (tokenIndex >= tokens.size()) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		if (tokens[tokenIndex].tag != Token::SEMICOLON) {
			printError("Expected ';'.");
			return PARSE_ERROR;
		}
		tokenIndex++;
	}
	else {
		program.emplace_back(Token::JUMP, loop_start);

		if (loop_condition != 0) {
			program[condition_jump].intu = program.size();
		}

		for (const auto& index : interruptions[1]) // continue;
		{
			program[index].intu = loop_start;
		}
	}

	if (tokenIndex < tokens.size()) {
		if (tokens[tokenIndex].tag == Token::ELSE) { // NO BREAK.
			tokenIndex++;

			if (tokenIndex >= tokens.size()) {
				printError("Expected '{'.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::BRACE_OPEN) {
				printError("Expected '{'.");
				return PARSE_ERROR;
			}
			tokenIndex++;

			if (parse_instructions(function, interrupts) == PARSE_ERROR) {
				return PARSE_ERROR;
			}

			if (tokenIndex >= tokens.size()) {
				printError("Expected '}'.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::BRACE_CLOSE) {
				printError("Expected '}'.");
				return PARSE_ERROR;
			}
			tokenIndex++;
		}
	}

	for (const auto& index : interruptions[0]) // break;
	{
		program[index].intu = program.size();
	}

	return true;
}

char Parser::parse_function(Function& function)
{
	function.name = nullptr; // TODO: maybe check if a function with the same name was already registered.
	function.arg_id.clear();
	function.program.clear();

	if (tokenIndex >= tokens.size()) {
		printError("Expected 'function' keyword.");
		return PARSE_ERROR;
	}
	const Token keyword = tokens[tokenIndex];
	if (keyword.tag != Token::FUNCTION_DEF) {
		printError("Expected 'function' keyword.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (tokenIndex >= tokens.size()) {
		printError("Expected function definition.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag == Token::IDENTIFIER) {
		if (scopeLevel != 0) {
			printError("Global functions may not be defined inside another function.");
			return PARSE_ERROR;
		}
		size_t len = strlen(tokens[tokenIndex].var) + 1;
		function.name = new char[len];
		memcpy(function.name, tokens[tokenIndex].var, len);
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].var, NAME_TABLE.size() + 1 });
		function.variable_id = insertion.first->second;
		if (insertion.second) tokens[tokenIndex].var = nullptr; // Steal.
		tokenIndex++;
	}

	if (tokenIndex >= tokens.size()) {
		printError("Expected '('.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::PARENTHESIS_OPEN) {
		printError("Expected '('.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	while (tokenIndex < tokens.size()) {
		if (tokens[tokenIndex].tag != Token::IDENTIFIER) {
			break;
		}
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].var, NAME_TABLE.size() + 1 });
		function.arg_id.emplace_back(insertion.first->second);
		if (insertion.second) tokens[tokenIndex].var = nullptr; // Steal.
		tokenIndex++;

		if (tokenIndex >= tokens.size()) {
			break;
		}
		if (tokens[tokenIndex].tag != Token::COMMA) {
			break;
		}
		tokenIndex++;
	}

	if (tokenIndex >= tokens.size()) {
		printError("Expected ')'.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::PARENTHESIS_CLOSE) {
		printError("Expected ')'.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (tokenIndex >= tokens.size()) {
		printError("Expected '{'.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::BRACE_OPEN) {
		printError("Expected '{'.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	scopeLevel++;
	if (parse_instructions(function, nullptr) == PARSE_ERROR) {
		return PARSE_ERROR;
	}
	scopeLevel--;

	if (tokenIndex >= tokens.size()) {
		printError("Expected '}'.");
		return PARSE_ERROR;
	}
	if (tokens[tokenIndex].tag != Token::BRACE_CLOSE) {
		printError("Expected '}'.");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (function.program.size() >= TOKEN_MAX) {
		printError("Function reached the maximum amount of Tokens.");
		return PARSE_ERROR;
	}

	return true;
}

char Parser::parse_instructions(Function& function, std::vector<int> interrupts[2] = nullptr)
{
	std::vector<Token>& program = function.program;
	while (tokenIndex < tokens.size())
	{
		const Token& token = tokens[tokenIndex];

		switch (token.tag)
		{
		case Token::NONE:
		case Token::INT:
		case Token::FLOAT:
		case Token::STRING:
		case Token::IDENTIFIER:
		{
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				if (parse_result != Token::FUNCTION_DEF) {
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							program.emplace_back(Token::JUMP_NEXT, 1);
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
				program.emplace_back(Token::JUMP_NEXT, 1);
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;
		case Token::COMMA:
		case Token::COLON:
		case Token::BRACE_OPEN:
			printError("Unexpected symbol '%hhd'.", token.tag);
			return PARSE_ERROR;
			break;
		case Token::BRACE_CLOSE:
			return true;
			break;

		case Token::IF:
			if (parse_if(function, interrupts) != PARSE_ERROR) {
				break;
			}
			else {
				return PARSE_ERROR;
			}
			break;
		case Token::ELSE:
			printError("'else' keyword may not be used in this context.");
			return PARSE_ERROR;
			break;
		case Token::FOR:
		case Token::WHILE:
		case Token::DO:
			if (parse_loop(function, interrupts) == PARSE_ERROR) {
				return PARSE_ERROR;
			}
			break;
		case Token::BREAK:
			if (interrupts == nullptr) {
				printError("'break' keyword may not be used in this context.");
				return PARSE_ERROR;
			}
			interrupts[0].push_back(program.size());
			program.emplace_back(Token::JUMP, -1);
			tokenIndex++;
			break;
		case Token::CONTINUE:
			if (interrupts == nullptr) {
				printError("'continue' keyword may not be used in this context.");
				return PARSE_ERROR;
			}
			interrupts[1].push_back(program.size());
			program.emplace_back(Token::JUMP, -1);
			tokenIndex++;
			break;
		case Token::FUNCTION_DEF:
		{
			tokenIndex++;
			if (tokenIndex < tokens.size()) {
				if (tokens[tokenIndex].tag == Token::IDENTIFIER) {
					tokenIndex--;
					Function* fx = new Function{};
					char parse_result = parse_function(*fx);
					if (parse_result == PARSE_ERROR) {
						return PARSE_ERROR;
					}
					if (fx->name == nullptr) {
						printError("Global functions must have a name.");
						return PARSE_ERROR;
					}
					program.emplace_back(fx);
					loaded->push_back(fx);
					program.emplace_back(Token::JUMP_NEXT, 1); // Probably unnecesary.
					break;
				}
			}
			tokenIndex--;
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				if (parse_result != Token::FUNCTION_DEF) {
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							program.emplace_back(Token::JUMP_NEXT, 1);
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
				program.emplace_back(Token::JUMP_NEXT, 1);
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;
		case Token::RETURN:
		{
			tokenIndex++;
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				program.emplace_back(Token::RETURN, parse_result != 0);
				if (parse_result != Token::FUNCTION_DEF) {
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;
		case Token::YIELD:
		{
			tokenIndex++;
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				program.emplace_back(Token::YIELD, 1);
				if (parse_result != Token::FUNCTION_DEF) {
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;
		case Token::AWAIT:
			program.emplace_back(Token::AWAIT, 0);
			tokenIndex++;
			if (tokenIndex < tokens.size()) {
				if (tokens[tokenIndex].tag == Token::SEMICOLON) {
					tokenIndex++;
					program.emplace_back(Token::JUMP_NEXT, 1); // To properly end to clear the solution stack after yielding. TODO: Remove.
					break;
				}
			}
			printError("Expected semicolon.");
			return PARSE_ERROR;
			break;
		case Token::LABEL:
		{
			tokenIndex++;
			if (tokenIndex >= tokens.size()) {
				printError("Expected label name.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::STRING) {
				printError("Expected a \"string\" for the label name.");
				return PARSE_ERROR;
			}
			if (function.labels.find(tokens[tokenIndex].str->string) != function.labels.end()) {
				printError("Can not have more than 1 label with the same name.");
				return PARSE_ERROR;
			}
			const size_t len = strlen(tokens[tokenIndex].str->string) + 1;
			char* label_name = new char[len];
			memcpy(label_name, tokens[tokenIndex].str->string, len);
			function.labels.insert({ label_name, function.program.size() }); // Labels are not OWNED. // TODO: Deallocate label names on erase.
			tokenIndex++;
			if (tokenIndex >= tokens.size()) {
				printError("Expected ':'.");
				return PARSE_ERROR;
			}
			if (tokens[tokenIndex].tag != Token::COLON) {
				printError("Expected ':'.");
				return PARSE_ERROR;
			}
			tokenIndex++;
		}
			break;
		case Token::GOTO:
		{
			tokenIndex++;
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				program.emplace_back(Token::GOTO, 1);
				if (parse_result != Token::FUNCTION_DEF) { // Token::GOTO should never actually end in a function.
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;

		case Token::VARIABLE:
		case Token::ARRAY:
		case Token::ARRAY_INIT:
		case Token::INDEX:
		case Token::CALL:
		case Token::BUILTIN:
		case Token::FUNCTION:
		case Token::JUMP:
		case Token::JUMP_ON_FALSE:
		case Token::JUMP_ON_TRUE:
		case Token::JUMP_NEXT:

		case Token::YIELDED:
			printError("Token type '%hhd' may never come from the parsed stream.", token.tag);
			return PARSE_ERROR;
			break;

		default: // All Operators and Operation Delimiters.
		{
			tok_tag parse_result = parse_operation(function);
			if (parse_result != PARSE_ERROR) {
				if (parse_result != Token::FUNCTION_DEF) {
					if (tokenIndex < tokens.size()) {
						if (tokens[tokenIndex].tag == Token::SEMICOLON) {
							tokenIndex++;
							program.emplace_back(Token::JUMP_NEXT, 1);
							break;
						}
					}
					printError("Expected semicolon.");
					return PARSE_ERROR;
				}
				program.emplace_back(Token::JUMP_NEXT, 1);
			}
			else {
				return PARSE_ERROR;
			}
		}
			break;
		}

		if (program.size() >= TOKEN_MAX) {
			printError("Program exceeds the maximum amount of tokens.");
			return PARSE_ERROR;
		}
	}

	return true;
}

bool Parser::parse(std::vector<Function*>* file_)
{
	loaded = file_;
	loaded->push_back(new Function{});
	if (parse_instructions(*loaded->back(), nullptr) == PARSE_ERROR) {
		return false;
	}

	if (tokenIndex < tokens.size()) {
		printError("Tokens remaining unprocessed.");
		return false;
	}

	return true;
}
