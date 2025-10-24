#include "Parser.h"

#define PARSE_ERROR -1
#define OPERATION_EMPTY -10

const char* Parser::file_name()
{
	return loaded ? loaded->name.c_str() : nullptr;
}

bool Parser::tag_unary(tok_tag tag)
{
	return Token::UNARY_BEGIN <= tag && tag < Token::UNARY_END;
}

bool Parser::tag_binary(tok_tag tag)
{
	return Token::BINARY_BEGIN <= tag && tag < Token::BINARY_END;
}

typedef unsigned char ErrMesType;
enum : ErrMesType
{
	Parser = 0,
	SYNTAX_ERROR,
	OPERATOR_MISSING,
	OPERAND_MISSING,
	EXPRESSION_MISSING,
	DELIMITER_MISMATCH,
	WRONG_CONTEXT,
	NAME_DUPLICATE,
};

static const char* ERROR_MESSAGE_TYPES[]
{
	STRINGIZING(Parser),
	STRINGIZING(SYNTAX_ERROR),
	STRINGIZING(OPERATOR_MISSING),
	STRINGIZING(OPERAND_MISSING),
	STRINGIZING(EXPRESSION_MISSING),
	STRINGIZING(DELIMITER_MISMATCH),
	STRINGIZING(WRONG_CONTEXT),
	STRINGIZING(NAME_DUPLICATE),
};

static const std::pair<const ErrMesType, const char*> ERROR_MESSAGES[]
{
	{ Parser, nullptr },
	{ SYNTAX_ERROR, "Expected '%s' token." },
	{ OPERAND_MISSING, "'%s' operator requires an argument." },
	{ EXPRESSION_MISSING, "%s must contain an operation." },
	{ DELIMITER_MISMATCH, "A corresponding '%s' is missing." },
	{ WRONG_CONTEXT, "Global function definitions %s." },
	{ OPERAND_MISSING, "'%s' operator must be followed by an operand." },
	{ EXPRESSION_MISSING, "%s missing" },
	{ WRONG_CONTEXT, "'%s' token may not be used in this context." },
	{ NAME_DUPLICATE, "Label name '%s' can not be repeated." },
	{ WRONG_CONTEXT, "Unable to finish parsing because Tokens in the stream remain external to its scope." },
	{ NAME_DUPLICATE, "Function name '%s' already exists inside this file." },
};

void parserError(const char* filename, lin_num line, col_num column, std::pair<const ErrMesType, const char*> f, ...)
{
	va_list argp;
	va_start(argp, f);
	printLanguageError(ERROR_MESSAGE_TYPES[0], ERROR_MESSAGE_TYPES[f.first], filename, line, column, f.second, argp);
	va_end(argp);
}

#define REQUIRE_CURRENT_TAG_RETURN(required_tag, returned) \
if (tokenIndex >= tokens.size() || tokens[tokenIndex].tag != required_tag) { \
	parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(required_tag)); \
	return returned; \
}

#define REQUIRE_CURRENT_TAG(required_tag) REQUIRE_CURRENT_TAG_RETURN(required_tag, PARSE_ERROR)

/*
Returns the id of the last structure in the sequence.
*/
tok_tag Parser::parse_sequence(std::vector<Token>& program, const tok_tag separator_symbol = Token::COMMA)
{
	tok_tag parsed = OPERATION_EMPTY;
	while (tokenIndex < tokens.size())
	{
		std::vector<Token> element;
		parsed = parse_operation(element, PRECEDENCE_MIN);
		if (parsed == PARSE_ERROR)
			return PARSE_ERROR;
		if (parsed == OPERATION_EMPTY)
			break;
		program.insert(program.end(), std::make_move_iterator(element.begin()), std::make_move_iterator(element.end()));
		if (tokenIndex >= tokens.size())
			break;
		if (tokens[tokenIndex].tag != separator_symbol)
			break;
		tokenIndex++;
	}
	return parsed;
}

// Precedence climbing.
// https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
/*
Returns the id of the last parsed structure.
*/
tok_tag Parser::parse_operand(std::vector<Token>& program)
{
	static const RegisteredSequence* TOKEN_POSITIVE = tag_id(Token::UNARY_POSITIVE);
	static const RegisteredSequence* TOKEN_NEGATIVE = tag_id(Token::UNARY_NEGATIVE);

	int unary_begin = tokenIndex;
	int unary_end = -1;

	while (tokenIndex < tokens.size())
	{
		Token& unary = tokens[tokenIndex];

		if (tag_unary(unary.tag) || unary.tag == Token::BINARY_ADD || unary.tag == Token::BINARY_SUBSTRACT) {
			if		(unary.tag == Token::BINARY_ADD)		unary = Token(unary.line, unary.column, TOKEN_POSITIVE->tag, TOKEN_POSITIVE->value);
			else if	(unary.tag == Token::BINARY_SUBSTRACT)	unary = Token(unary.line, unary.column, TOKEN_NEGATIVE->tag, TOKEN_NEGATIVE->value);
			unary_end = tokenIndex;
		}
		else break;

		tokenIndex++;
	}

	tok_tag typeLast = OPERATION_EMPTY;

	if (tokenIndex >= tokens.size())
	{
		if (unary_end != -1) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[2], tag_name(tokens[tokenIndex].tag));
			return PARSE_ERROR;
		}

		return typeLast;
	}

	Token& token = tokens[tokenIndex];
	
	switch (token.tag)
	{
	case Token::NONE:
	case Token::INT:
	case Token::FLOAT:
	case Token::STRING:
		typeLast = token.tag;
		program.push_back(std::move(tokens[tokenIndex]));
		tokenIndex++;
		break;

	case Token::IDENTIFIER:
	{
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ token.u_identifier, NAME_TABLE.size() + 1 });
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::VARIABLE, insertion.first->second);
		if (insertion.second) token.u_identifier = nullptr; // Steal.
		typeLast = Token::VARIABLE;
		tokenIndex++;
	}
	break;

	case Token::PARENTHESIS_OPEN: // Parenthesised expression.
	{
		tokenIndex++;
		std::vector<Token> inner;
		typeLast = parse_operation(inner, PRECEDENCE_MIN);
		if (typeLast == PARSE_ERROR)
			return typeLast;
		if (typeLast == OPERATION_EMPTY) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[3], "Parenthesised expression");
			return PARSE_ERROR;
		}
		program.insert(program.end(), std::make_move_iterator(inner.begin()), std::make_move_iterator(inner.end()));
		REQUIRE_CURRENT_TAG(Token::PARENTHESIS_CLOSE);
		typeLast = tokens[tokenIndex].tag;
		tokenIndex++;
	}
	break;

	case Token::BRACKET_OPEN: // Array.
	{
		program.emplace_back(token.line, token.column, Token::SEQUENCE, -1);
		tokenIndex++;
		std::vector<Token> sequence;
		if (parse_sequence(sequence, Token::COMMA) == PARSE_ERROR)
			return PARSE_ERROR;
		program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
		REQUIRE_CURRENT_TAG(Token::BRACKET_CLOSE);
		tokenIndex++;
		program.emplace_back(token.line, token.column, Token::ARRAY_INIT, LANGUAGE_INT(1));
		typeLast = Token::ARRAY_INIT;
	}
	break;

	case Token::FUNCTION_DEF:
	//	// Anonymous function.
	//{
	//	loaded->functions.emplace_back(Function_tL{ loaded }); // Register function.
	//	Function_tL& func = loaded->functions.back();
	//	func.program = std::make_shared<Program_tL>();
	//	if (parse_function(func) == PARSE_ERROR)
	//		return PARSE_ERROR;
	//	if (func.name != nullptr) {
	//		parserError(file_name(), token.line, token.column, ERROR_MESSAGES[5], "can not form part of an operation");
	//		return PARSE_ERROR;
	//	}
	//	program.emplace_back(token.line, token.column, &func);
	//	typeLast = Token::FUNCTION;
	//}
	//break;

	/* Impossible to come from the tokenized stream.
	case Token::ARRAY:
	case Token::FUNCTION:
	case Token::BUILTIN:

	case Token::VARIABLE:
	case Token::REFERENCE:

	case Token::SEQUENCE:
	case Token::INDEX:
	case Token::ARRAY_INIT:
	case Token::CALL:

	case Token::JUMP:
	case Token::JUMP_ON_FALSE:
	case Token::JUMP_ON_NOT_FALSE:

	case Token::UNARY_FLIP:
	case Token::UNARY_NEGATION:
	case Token::UNARY_POSITIVE:
	case Token::UNARY_NEGATIVE:
		return typeLast;
	*/

	case Token::SEMICOLON:
	case Token::COMMA:
	case Token::COLON:
	case Token::PARENTHESIS_CLOSE:
	case Token::BRACKET_CLOSE:
	case Token::BRACE_OPEN:
	case Token::BRACE_CLOSE:

	case Token::IF:
	case Token::ELSE:
	case Token::FOR:
	case Token::WHILE:
	case Token::DO:
	case Token::BREAK:
	case Token::CONTINUE:
	case Token::RETURN:
	case Token::AWAIT:
	case Token::LABEL:
	case Token::GOTO:
		return typeLast;

	default: // ALL remaining BINARY_OPERATORS:
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[4], "Binary Operator");
		return PARSE_ERROR;
	}

	while (tokenIndex < tokens.size())
	{
		if (tokens[tokenIndex].tag == Token::PARENTHESIS_OPEN) { // Call.
			program.emplace_back(token.line, token.column, Token::SEQUENCE, -1);
			tokenIndex++;
			std::vector<Token> sequence;
			if (parse_sequence(sequence, Token::COMMA) == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
			REQUIRE_CURRENT_TAG(Token::PARENTHESIS_CLOSE);
			tokenIndex++;
			program.emplace_back(token.line, token.column, Token::CALL, LANGUAGE_INT(1));
			typeLast = Token::CALL;
		}
		else if (tokens[tokenIndex].tag == Token::BRACKET_OPEN) { // Index.
			tokenIndex++;
			std::vector<Token> index;
			if (parse_operation(index, PRECEDENCE_MIN) == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(index.begin()), std::make_move_iterator(index.end()));
			REQUIRE_CURRENT_TAG(Token::BRACKET_CLOSE);
			tokenIndex++;
			program.emplace_back(token.line, token.column, Token::INDEX, LANGUAGE_INT(1));
			typeLast = Token::INDEX;
		}
		else break;
	}

	if (unary_end != -1)
		program.insert(program.end(), std::make_move_iterator(tokens.rend() - unary_end - 1), std::make_move_iterator(tokens.rend() - unary_begin));

	return typeLast;
}

/*
Returns the id of the last parsed structure.
*/
tok_tag Parser::parse_operation(std::vector<Token>& program, int_tL precedence_min = PRECEDENCE_MIN)
{
	std::vector<Token>& left = program;
	tok_tag typeLast = parse_operand(left);
	if (typeLast == PARSE_ERROR || typeLast == OPERATION_EMPTY)
		return typeLast;

	if (tokenIndex < tokens.size() && tokens[tokenIndex].tag == Token::BINARY_EQUAL && left[0].tag == Token::VARIABLE) {
		left[0].tag = Token::REFERENCE;
	}

	while (tokenIndex < tokens.size() && tag_binary(tokens[tokenIndex].tag) && OP_PRECEDENCE(tokens[tokenIndex].u_int) >= precedence_min)
	{
		Token& token = tokens[tokenIndex];
		tokenIndex++;

#define PRECEDENCE_MIN_NEXT(val) (OP_PRECEDENCE(val) + OP_ASSOCIATIVITY(val))
		std::vector<Token> right;
		typeLast = parse_operation(right, PRECEDENCE_MIN_NEXT(token.u_int));
		if (typeLast == PARSE_ERROR)
			return typeLast;
		if (typeLast == OPERATION_EMPTY) {
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag));
			return PARSE_ERROR;
		}

		// Combine operands and operator in RPN.
		if (token.tag == Token::BINARY_EQUAL) { // Backwards.
			right.insert(right.end(), std::make_move_iterator(left.begin()), std::make_move_iterator(left.end()));
			std::swap(left, right);
		}
		else {
			left.insert(left.end(), std::make_move_iterator(right.begin()), std::make_move_iterator(right.end()));
		}
		left.push_back(std::move(token));
	}

	return typeLast;
}

short Parser::parse_if(Function_tL& function, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = function.program->instructions;
	short branches = 0;
	int condition_index = -1;
	std::vector<size_t> block_end_index{};

	while (tokenIndex < tokens.size()) {
		tok_tag keyword_tag = tokens[tokenIndex].tag;

		if (keyword_tag == Token::IF) {
			if (branches) {
				break;
			}
		}
		else if (keyword_tag == Token::ELSE) {
			if (!branches) { // IF
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(Token::IF));
				return PARSE_ERROR;
			}

			tokenIndex++;
			if (tokenIndex < tokens.size()) {
				if (tokens[tokenIndex].tag == Token::IF) {
					keyword_tag = Token::IF; // ELSE IF
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
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(Token::IF));
				return PARSE_ERROR;
			}
			break;
		}
		if (branches) {
			block_end_index.push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, -1);

			program[condition_index].u_int = program.size();
		}
		tokenIndex++;

		condition_index = -1;
		if (keyword_tag == Token::IF) {
			REQUIRE_CURRENT_TAG(Token::PARENTHESIS_OPEN);
			tokenIndex++;

			std::vector<Token> condition;
			tok_tag parsed = parse_operation(condition, PRECEDENCE_MIN);
			if (parsed == PARSE_ERROR)
				return PARSE_ERROR;
			if (parsed == OPERATION_EMPTY) {
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'if' condition");
				return PARSE_ERROR;
			}
			program.insert(program.end(), std::make_move_iterator(condition.begin()), std::make_move_iterator(condition.end()));
			condition_index = program.size();
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP_ON_FALSE, -1);

			REQUIRE_CURRENT_TAG(Token::PARENTHESIS_CLOSE);
			tokenIndex++;
		}
		REQUIRE_CURRENT_TAG(Token::BRACE_OPEN);
		tokenIndex++;

		if (parse_instructions(function, interrupts) == PARSE_ERROR)
			return PARSE_ERROR;

		REQUIRE_CURRENT_TAG(Token::BRACE_CLOSE);
		tokenIndex++;

		branches++;
		if (keyword_tag == Token::ELSE) {
			break;
		}
	}

	if (condition_index >= 0)
		program[condition_index].u_int = program.size();

	for (const size_t& index : block_end_index)
		program[index].u_int = program.size();

	return true;
}

char Parser::parse_loop(Function_tL& function, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = function.program->instructions;
	if (tokenIndex >= tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], "Loop declaration");
		return PARSE_ERROR;
	}
	const Token& keyword = tokens[tokenIndex];
	if (keyword.tag != Token::FOR && keyword.tag != Token::WHILE && keyword.tag != Token::DO) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], "Loop declaration");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (keyword.tag != Token::DO) {
		REQUIRE_CURRENT_TAG(Token::PARENTHESIS_OPEN);
		tokenIndex++;
	}

	if (keyword.tag == Token::FOR) {
		std::vector<Token> init;
		tok_tag loop_init = parse_operation(init, PRECEDENCE_MIN);
		if (loop_init == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_init != OPERATION_EMPTY) {
			program.insert(program.end(), std::make_move_iterator(init.begin()), std::make_move_iterator(init.end()));
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
		}

		REQUIRE_CURRENT_TAG(Token::SEMICOLON);
		tokenIndex++;
	}

	std::vector<Token> condition_content{};
	tok_tag loop_condition = OPERATION_EMPTY;
	if (keyword.tag != Token::DO) {
		loop_condition = parse_operation(condition_content, PRECEDENCE_MIN);
		if (loop_condition == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_condition == OPERATION_EMPTY && keyword.tag == Token::WHILE) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'while' condition");
			return PARSE_ERROR;
		}
	}

	tok_tag loop_increment = OPERATION_EMPTY;
	std::pair<lin_num, col_num> increment_position;
	std::vector<Token> increment_content{};
	if (keyword.tag == Token::FOR) {
		REQUIRE_CURRENT_TAG(Token::SEMICOLON);
		tokenIndex++;
		increment_position = { tokens[tokenIndex].line, tokens[tokenIndex].column };

		loop_increment = parse_operation(increment_content, PRECEDENCE_MIN);
		if (loop_increment == PARSE_ERROR)
			return PARSE_ERROR;
	}

	int loop_start = program.size();

	if (keyword.tag == Token::FOR && loop_increment != OPERATION_EMPTY) {
		const int init_jump = program.size();
		program.emplace_back(increment_position.first, increment_position.second, Token::JUMP, -1);
		loop_start = program.size();
		program.insert(program.end(), std::make_move_iterator(increment_content.begin()), std::make_move_iterator(increment_content.end())); // https://stackoverflow.com/questions/15004517/moving-elements-from-stdvector-to-another-one?rq=3
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
		program[init_jump].u_int = program.size();
	}

	int condition_jump = -1;
	if (keyword.tag != Token::DO && loop_condition != OPERATION_EMPTY) {
		program.insert(program.end(), std::make_move_iterator(condition_content.begin()), std::make_move_iterator(condition_content.end()));
		condition_jump = program.size();
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP_ON_FALSE, -1);
	}

	if (keyword.tag != Token::DO) {
		REQUIRE_CURRENT_TAG(Token::PARENTHESIS_CLOSE);
		tokenIndex++;
	}

	REQUIRE_CURRENT_TAG(Token::BRACE_OPEN);
	tokenIndex++;

	std::vector<int> interruptions[2] = { std::vector<int>{}, std::vector<int>{} };
	if (parse_instructions(function, interruptions) == PARSE_ERROR)
		return PARSE_ERROR;

	REQUIRE_CURRENT_TAG(Token::BRACE_CLOSE);
	tokenIndex++;

	if (keyword.tag == Token::DO) {
		REQUIRE_CURRENT_TAG(Token::WHILE);
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::PARENTHESIS_OPEN);
		tokenIndex++;

		for (const auto& index : interruptions[1]) // continue;
			program[index].u_int = program.size();

		std::vector<Token> condition;
		loop_condition = parse_operation(condition, PRECEDENCE_MIN);
		if (loop_condition == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_condition == OPERATION_EMPTY) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'do while' condition");
			return PARSE_ERROR;
		}
		program.insert(program.end(), std::make_move_iterator(condition.begin()), std::make_move_iterator(condition.end()));
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP_ON_NOT_FALSE, loop_start);

		REQUIRE_CURRENT_TAG(Token::PARENTHESIS_CLOSE);
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::SEMICOLON);
		tokenIndex++;
	}
	else {
		program.emplace_back(tokens[tokenIndex - 1].line, tokens[tokenIndex - 1].column, Token::JUMP, loop_start);

		if (loop_condition != OPERATION_EMPTY)
			program[condition_jump].u_int = program.size();

		for (const auto& index : interruptions[1]) // continue;
			program[index].u_int = loop_start;
	}

	if (tokenIndex < tokens.size() && tokens[tokenIndex].tag == Token::ELSE) { // NO BREAK.
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::BRACE_OPEN);
		tokenIndex++;

		if (parse_instructions(function, interrupts) == PARSE_ERROR)
			return PARSE_ERROR;

		REQUIRE_CURRENT_TAG(Token::BRACE_CLOSE);
		tokenIndex++;
	}

	for (const auto& index : interruptions[0]) // break;
		program[index].u_int = program.size();

	return true;
}

Function_tL* Parser::parse_function()
{
	REQUIRE_CURRENT_TAG_RETURN(Token::FUNCTION_DEF, nullptr);
	tokenIndex++;

	if (tokenIndex >= tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "Function definition");
		return nullptr;
	}

	if (tokens[tokenIndex].tag != Token::IDENTIFIER) { // Anonymous function.
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'Global function' name");
		return nullptr;
	}

	if (scopeLevel != 0) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[5], "can not occur inside another function");
		return nullptr;
	}

	const auto& function_insert = loaded->functions.insert({ tokens[tokenIndex].u_identifier, Function_tL{ loaded } });
	if (!function_insert.second) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[11], tokens[tokenIndex].u_identifier);
		return nullptr;
	}
	Function_tL& function = function_insert.first->second;

	const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].u_identifier, NAME_TABLE.size() + 1 });

	if (insertion.second) {
		size_t len = strlen(tokens[tokenIndex].u_identifier) + 1;
		function.name = new char[len];
		memcpy(function.name, tokens[tokenIndex].u_identifier, len);
	}
	else {
		function.name = tokens[tokenIndex].u_identifier;
	}
	tokens[tokenIndex].u_identifier = nullptr; // Steal.

	function.variable_id = insertion.first->second;
	function.program = std::make_shared<Program_tL>();
	function.program->instructions.reserve(tokens.size() - tokenIndex);

	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::PARENTHESIS_OPEN, nullptr);
	tokenIndex++;

	while (tokenIndex < tokens.size()) {
		if (tokens[tokenIndex].tag != Token::IDENTIFIER)
			break;
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].u_identifier, NAME_TABLE.size() + 1 });
		function.arg_id.emplace_back(insertion.first->second);
		if (insertion.second) tokens[tokenIndex].u_identifier = nullptr; // Steal.
		tokenIndex++;

		if (tokenIndex >= tokens.size() || tokens[tokenIndex].tag != Token::COMMA)
			break;
		tokenIndex++;
	}
	function.arg_id.shrink_to_fit();

	REQUIRE_CURRENT_TAG_RETURN(Token::PARENTHESIS_CLOSE, nullptr);
	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::BRACE_OPEN, nullptr);
	tokenIndex++;

	scopeLevel++;
	if (parse_instructions(function, nullptr) == PARSE_ERROR)
		return nullptr;
	scopeLevel--;

	REQUIRE_CURRENT_TAG_RETURN(Token::BRACE_CLOSE, nullptr);
	tokenIndex++;

	function.program->instructions.shrink_to_fit();
	return &function;
}

char Parser::parse_instructions(Function_tL& function, std::vector<int> interrupts[2] = nullptr)
{
	std::vector<Token>& program = function.program->instructions;
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
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			if (parse_result == Token::FUNCTION) {
				program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
				break;
			}
			REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
			tokenIndex++;
		}
		break;

		case Token::COMMA:
		case Token::COLON:
		case Token::BRACE_OPEN:
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
			return PARSE_ERROR;

		case Token::BRACE_CLOSE:
			return true;

		case Token::IF:
			if (parse_if(function, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		case Token::ELSE:
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
			return PARSE_ERROR;

		case Token::FOR:
		case Token::WHILE:
		case Token::DO:
			if (parse_loop(function, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		case Token::BREAK:
			if (interrupts == nullptr) {
				parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
				return PARSE_ERROR;
			}
			interrupts[0].push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, -1);
			tokenIndex++;
			break;

		case Token::CONTINUE:
			if (interrupts == nullptr) {
				parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
				return PARSE_ERROR;
			}
			interrupts[1].push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, -1);
			tokenIndex++;
			break;

		case Token::FUNCTION_DEF:
		{
			//tokenIndex++;
			//if (tokenIndex < tokens.size()) {
			//	if (tokens[tokenIndex].tag == Token::IDENTIFIER) {
			//		tokenIndex--;
					std::pair<lin_num, col_num> function_position{ tokens[tokenIndex].line, tokens[tokenIndex].column };
					Function_tL* parse_result = parse_function();
					if (parse_result == nullptr)
						return PARSE_ERROR;
					program.emplace_back(function_position.first, function_position.second, parse_result);
					//break;
			//	}
			//}
			// // Anonymous function.
			//tokenIndex--;
			//std::vector<Token> operation;
			//tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			//if (parse_result == PARSE_ERROR)
			//	return PARSE_ERROR;
			//program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			//if (parse_result == Token::FUNCTION) {
			//	program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
			//	break;
			//}
			//REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			//program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
			//tokenIndex++;
		}
		break;

		case Token::RETURN:
		{
			tokenIndex++;
			std::vector<Token> sequence;
			tok_tag parse_result = parse_sequence(sequence, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
			program.push_back(std::move(token));
			if (parse_result == Token::FUNCTION)
				break;
			REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			tokenIndex++;
		}
		break;

		case Token::AWAIT:
			program.emplace_back(std::move(tokens[tokenIndex]));
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			tokenIndex++;
			break;

		case Token::LABEL:
		{
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::STRING); // Expected a "string" for the label name.
			if (function.program->labels.find(tokens[tokenIndex].u_string->string_get()) != function.program->labels.end()) {
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[9], tokens[tokenIndex].u_string->string_get());
				return PARSE_ERROR;
			}
			const size_t len = strlen(tokens[tokenIndex].u_string->string_get()) + 1;
			char* label_name = new char[len];
			memcpy(label_name, tokens[tokenIndex].u_string->string_get(), len);
			function.program->labels.insert({ label_name, program.size() }); // Labels are not OWNED.
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::COLON);
			tokenIndex++;
		}
		break;

		case Token::GOTO:
		{
			tokenIndex++;
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			program.push_back(std::move(token));
			if (parse_result == Token::FUNCTION) {
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "Evaluing operand");
				return PARSE_ERROR;
			}
			REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			tokenIndex++;
		}
		break;

		/* Impossible to come from the tokenized stream.
		case Token::VARIABLE:
		case Token::REFERENCE:
		case Token::ARRAY:
		case Token::SEQUENCE:
		case Token::ARRAY_INIT:
		case Token::INDEX:
		case Token::CALL:
		case Token::BUILTIN:
		case Token::FUNCTION:
		case Token::JUMP:
		case Token::JUMP_ON_FALSE:
		case Token::JUMP_ON_NOT_FALSE:
			return PARSE_ERROR;
		*/

		case Token::SEMICOLON:
			tokenIndex++;
			break;

		default: // All Operators and Operation Delimiters.
		{
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			if (parse_result == Token::FUNCTION) {
				program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
				break;
			}
			REQUIRE_CURRENT_TAG(Token::SEMICOLON);
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::JUMP, program.size() + 1);
			tokenIndex++;
		}
		break;

		}
	}

	return true;
}

Function_tL* Parser::parse(SourceFile* file_, bool global_first)
{
	loaded = file_;

	const auto& file_function_insert = loaded->functions.insert({ file_->name, Function_tL{ loaded } });
	if (!file_function_insert.second) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[11], loaded->name.c_str());
		return nullptr;
	}
	Function_tL* file_function = &file_function_insert.first->second;
	file_function->global = global_first;
	file_function->program = std::make_shared<Program_tL>();
	file_function->program->instructions.reserve(tokens.size() - tokenIndex);

	if (parse_instructions(*file_function, nullptr) == PARSE_ERROR)
		return nullptr;

	if (tokenIndex < tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[10]);
		return nullptr;
	}

	file_function->program->instructions.shrink_to_fit();
	return file_function;
}
