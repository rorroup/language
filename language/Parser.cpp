#include "Parser.h"

#define PARSE_ERROR -1
#define OPERATION_EMPTY -10

const char* Parser::file_name()
{
	return loaded ? loaded->name.c_str() : nullptr;
}

bool Parser::tag_unary(tok_tag tag)
{
	return Token::TTAG_UNARY_BEGIN <= tag && tag < Token::TTAG_UNARY_END;
}

bool Parser::tag_binary(tok_tag tag)
{
	return Token::TTAG_BINARY_BEGIN <= tag && tag < Token::TTAG_BINARY_END;
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
	{ SYNTAX_ERROR, "Expected '%s' token%s." },
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
if (tokenIndex >= tokens.size()) { \
	parserError(file_name(), tokens.back().line, tokens.back().column, ERROR_MESSAGES[1], tag_name(required_tag), " but none are left"); \
	return returned; \
} \
if (tokens[tokenIndex].tag != required_tag) { \
	parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(required_tag), ""); \
	return returned; \
}

#define REQUIRE_CURRENT_TAG(required_tag) REQUIRE_CURRENT_TAG_RETURN(required_tag, PARSE_ERROR)

/*
Returns the id of the last structure in the sequence.
*/
tok_tag Parser::parse_sequence(std::vector<Token>& program, const tok_tag separator_symbol = Token::TTAG_COMMA)
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
	static const RegisteredSequence* TOKEN_POSITIVE = tag_id(Token::TTAG_UNARY_POSITIVE);
	static const RegisteredSequence* TOKEN_NEGATIVE = tag_id(Token::TTAG_UNARY_NEGATIVE);

	int unary_begin = tokenIndex;
	int unary_end = -1;

	while (tokenIndex < tokens.size())
	{
		Token& unary = tokens[tokenIndex];

		if (tag_unary(unary.tag) || unary.tag == Token::TTAG_BINARY_ADD || unary.tag == Token::TTAG_BINARY_SUBSTRACT) {
			if		(unary.tag == Token::TTAG_BINARY_ADD)		unary = Token(unary.line, unary.column, TOKEN_POSITIVE->tag, TOKEN_POSITIVE->value);
			else if	(unary.tag == Token::TTAG_BINARY_SUBSTRACT)	unary = Token(unary.line, unary.column, TOKEN_NEGATIVE->tag, TOKEN_NEGATIVE->value);
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
	case Token::TTAG_NONE:
	case Token::TTAG_INT:
	case Token::TTAG_FLOAT:
	case Token::TTAG_STRING:
		typeLast = token.tag;
		program.push_back(std::move(tokens[tokenIndex]));
		tokenIndex++;
		break;

	case Token::TTAG_IDENTIFIER:
	{
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ token.val_identifier, NAME_TABLE.size() + 1 });
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_VARIABLE, insertion.first->second);
		if (insertion.second) token.val_identifier = nullptr; // Steal.
		typeLast = Token::TTAG_VARIABLE;
		tokenIndex++;
	}
	break;

	case Token::TTAG_PARENTHESIS_OPEN: // Parenthesised expression.
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
		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);
		typeLast = tokens[tokenIndex].tag;
		tokenIndex++;
	}
	break;

	case Token::TTAG_BRACKET_OPEN: // Array.
	{
		program.emplace_back(token.line, token.column, Token::TTAG_SEQUENCE, -1);
		tokenIndex++;
		std::vector<Token> sequence;
		if (parse_sequence(sequence, Token::TTAG_COMMA) == PARSE_ERROR)
			return PARSE_ERROR;
		program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
		REQUIRE_CURRENT_TAG(Token::TTAG_BRACKET_CLOSE);
		tokenIndex++;
		program.emplace_back(token.line, token.column, Token::TTAG_ARRAY_INIT, LANGUAGE_INT(1));
		typeLast = Token::TTAG_ARRAY_INIT;
	}
	break;

	case Token::TTAG_FUNCTION_DEF:
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
	//	typeLast = Token::TTAG_FUNCTION;
	//}
	//break;

	/* Impossible to come from the tokenized stream.
	case Token::TTAG_ARRAY:
	case Token::TTAG_FUNCTION:
	case Token::TTAG_BUILTIN:

	case Token::TTAG_VARIABLE:
	case Token::TTAG_REFERENCE:

	case Token::TTAG_SEQUENCE:
	case Token::TTAG_INDEX:
	case Token::TTAG_ARRAY_INIT:
	case Token::TTAG_CALL:

	case Token::TTAG_JUMP:
	case Token::TTAG_JUMP_ON_FALSE:
	case Token::TTAG_JUMP_ON_NOT_FALSE:

	case Token::TTAG_UNARY_FLIP:
	case Token::TTAG_UNARY_NEGATION:
	case Token::TTAG_UNARY_POSITIVE:
	case Token::TTAG_UNARY_NEGATIVE:
		return typeLast;
	*/

	case Token::TTAG_SEMICOLON:
	case Token::TTAG_COMMA:
	case Token::TTAG_COLON:
	case Token::TTAG_PARENTHESIS_CLOSE:
	case Token::TTAG_BRACKET_CLOSE:
	case Token::TTAG_BRACE_OPEN:
	case Token::TTAG_BRACE_CLOSE:

	case Token::TTAG_IF:
	case Token::TTAG_ELSE:
	case Token::TTAG_FOR:
	case Token::TTAG_WHILE:
	case Token::TTAG_DO:
	case Token::TTAG_BREAK:
	case Token::TTAG_CONTINUE:
	case Token::TTAG_RETURN:
	case Token::TTAG_AWAIT:
	case Token::TTAG_LABEL:
	case Token::TTAG_GOTO:
		return typeLast;

	default: // ALL remaining BINARY_OPERATORS:
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[4], "Binary Operator");
		return PARSE_ERROR;
	}

	while (tokenIndex < tokens.size())
	{
		if (tokens[tokenIndex].tag == Token::TTAG_PARENTHESIS_OPEN) { // Call.
			program.emplace_back(token.line, token.column, Token::TTAG_SEQUENCE, -1);
			tokenIndex++;
			std::vector<Token> sequence;
			if (parse_sequence(sequence, Token::TTAG_COMMA) == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
			REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);
			tokenIndex++;
			program.emplace_back(token.line, token.column, Token::TTAG_CALL, LANGUAGE_INT(1));
			typeLast = Token::TTAG_CALL;
		}
		else if (tokens[tokenIndex].tag == Token::TTAG_BRACKET_OPEN) { // Index.
			tokenIndex++;
			std::vector<Token> index;
			if (parse_operation(index, PRECEDENCE_MIN) == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(index.begin()), std::make_move_iterator(index.end()));
			REQUIRE_CURRENT_TAG(Token::TTAG_BRACKET_CLOSE);
			tokenIndex++;
			program.emplace_back(token.line, token.column, Token::TTAG_INDEX, LANGUAGE_INT(1));
			typeLast = Token::TTAG_INDEX;
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

	if (tokenIndex < tokens.size() && tokens[tokenIndex].tag == Token::TTAG_BINARY_EQUAL && left[0].tag == Token::TTAG_VARIABLE) {
		left[0].tag = Token::TTAG_REFERENCE;
	}

	while (tokenIndex < tokens.size() && tag_binary(tokens[tokenIndex].tag) && OP_PRECEDENCE(tokens[tokenIndex].val_int) >= precedence_min)
	{
		Token& token = tokens[tokenIndex];
		tokenIndex++;

#define PRECEDENCE_MIN_NEXT(val) (OP_PRECEDENCE(val) + OP_ASSOCIATIVITY(val))
		std::vector<Token> right;
		typeLast = parse_operation(right, PRECEDENCE_MIN_NEXT(token.val_int));
		if (typeLast == PARSE_ERROR)
			return typeLast;
		if (typeLast == OPERATION_EMPTY) {
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag));
			return PARSE_ERROR;
		}

		// Combine operands and operator in RPN.
		if (token.tag == Token::TTAG_BINARY_EQUAL) { // Backwards.
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

		if (keyword_tag == Token::TTAG_IF) {
			if (branches) {
				break;
			}
		}
		else if (keyword_tag == Token::TTAG_ELSE) {
			if (!branches) { // TTAG_IF
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(Token::TTAG_IF), "");
				return PARSE_ERROR;
			}

			tokenIndex++;
			if (tokenIndex < tokens.size()) {
				if (tokens[tokenIndex].tag == Token::TTAG_IF) {
					keyword_tag = Token::TTAG_IF; // TTAG_ELSE TTAG_IF
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
			if (!branches) { // TTAG_IF
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], tag_name(Token::TTAG_IF), "");
				return PARSE_ERROR;
			}
			break;
		}
		if (branches) {
			block_end_index.push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, -1);

			program[condition_index].val_int = program.size();
		}
		tokenIndex++;

		condition_index = -1;
		if (keyword_tag == Token::TTAG_IF) {
			REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_OPEN);
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
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP_ON_FALSE, -1);

			REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);
			tokenIndex++;
		}
		REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_OPEN);
		tokenIndex++;

		if (parse_instructions(function, interrupts) == PARSE_ERROR)
			return PARSE_ERROR;

		REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_CLOSE);
		tokenIndex++;

		branches++;
		if (keyword_tag == Token::TTAG_ELSE) {
			break;
		}
	}

	if (condition_index >= 0)
		program[condition_index].val_int = program.size();

	for (const size_t& index : block_end_index)
		program[index].val_int = program.size();

	return true;
}

char Parser::parse_loop(Function_tL& function, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = function.program->instructions;
	if (tokenIndex >= tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], "Loop declaration", "");
		return PARSE_ERROR;
	}
	const Token& keyword = tokens[tokenIndex];
	if (keyword.tag != Token::TTAG_FOR && keyword.tag != Token::TTAG_WHILE && keyword.tag != Token::TTAG_DO) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[1], "Loop declaration", "");
		return PARSE_ERROR;
	}
	tokenIndex++;

	if (keyword.tag != Token::TTAG_DO) {
		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_OPEN);
		tokenIndex++;
	}

	if (keyword.tag == Token::TTAG_FOR) {
		std::vector<Token> init;
		tok_tag loop_init = parse_operation(init, PRECEDENCE_MIN);
		if (loop_init == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_init != OPERATION_EMPTY) {
			program.insert(program.end(), std::make_move_iterator(init.begin()), std::make_move_iterator(init.end()));
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
		}

		REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
		tokenIndex++;
	}

	std::vector<Token> condition_content{};
	tok_tag loop_condition = OPERATION_EMPTY;
	if (keyword.tag != Token::TTAG_DO) {
		loop_condition = parse_operation(condition_content, PRECEDENCE_MIN);
		if (loop_condition == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_condition == OPERATION_EMPTY && keyword.tag == Token::TTAG_WHILE) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'while' condition");
			return PARSE_ERROR;
		}
	}

	tok_tag loop_increment = OPERATION_EMPTY;
	std::pair<lin_num, col_num> increment_position;
	std::vector<Token> increment_content{};
	if (keyword.tag == Token::TTAG_FOR) {
		REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
		tokenIndex++;
		increment_position = { tokens[tokenIndex].line, tokens[tokenIndex].column };

		loop_increment = parse_operation(increment_content, PRECEDENCE_MIN);
		if (loop_increment == PARSE_ERROR)
			return PARSE_ERROR;
	}

	int loop_start = program.size();

	if (keyword.tag == Token::TTAG_FOR && loop_increment != OPERATION_EMPTY) {
		const int init_jump = program.size();
		program.emplace_back(increment_position.first, increment_position.second, Token::TTAG_JUMP, -1);
		loop_start = program.size();
		program.insert(program.end(), std::make_move_iterator(increment_content.begin()), std::make_move_iterator(increment_content.end())); // https://stackoverflow.com/questions/15004517/moving-elements-from-stdvector-to-another-one?rq=3
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
		program[init_jump].val_int = program.size();
	}

	int condition_jump = -1;
	if (keyword.tag != Token::TTAG_DO && loop_condition != OPERATION_EMPTY) {
		program.insert(program.end(), std::make_move_iterator(condition_content.begin()), std::make_move_iterator(condition_content.end()));
		condition_jump = program.size();
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP_ON_FALSE, -1);
	}

	if (keyword.tag != Token::TTAG_DO) {
		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);
		tokenIndex++;
	}

	REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_OPEN);
	tokenIndex++;

	std::vector<int> interruptions[2] = { std::vector<int>{}, std::vector<int>{} };
	if (parse_instructions(function, interruptions) == PARSE_ERROR)
		return PARSE_ERROR;

	REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_CLOSE);
	tokenIndex++;

	if (keyword.tag == Token::TTAG_DO) {
		REQUIRE_CURRENT_TAG(Token::TTAG_WHILE);
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_OPEN);
		tokenIndex++;

		for (const auto& index : interruptions[1]) // continue;
			program[index].val_int = program.size();

		std::vector<Token> condition;
		loop_condition = parse_operation(condition, PRECEDENCE_MIN);
		if (loop_condition == PARSE_ERROR)
			return PARSE_ERROR;
		if (loop_condition == OPERATION_EMPTY) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'do while' condition");
			return PARSE_ERROR;
		}
		program.insert(program.end(), std::make_move_iterator(condition.begin()), std::make_move_iterator(condition.end()));
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP_ON_NOT_FALSE, loop_start);

		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
		tokenIndex++;
	}
	else {
		program.emplace_back(tokens[tokenIndex - 1].line, tokens[tokenIndex - 1].column, Token::TTAG_JUMP, loop_start);

		if (loop_condition != OPERATION_EMPTY)
			program[condition_jump].val_int = program.size();

		for (const auto& index : interruptions[1]) // continue;
			program[index].val_int = loop_start;
	}

	if (tokenIndex < tokens.size() && tokens[tokenIndex].tag == Token::TTAG_ELSE) { // NO TTAG_BREAK.
		tokenIndex++;

		REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_OPEN);
		tokenIndex++;

		if (parse_instructions(function, interrupts) == PARSE_ERROR)
			return PARSE_ERROR;

		REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_CLOSE);
		tokenIndex++;
	}

	for (const auto& index : interruptions[0]) // break;
		program[index].val_int = program.size();

	return true;
}

Function_tL* Parser::parse_function()
{
	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_FUNCTION_DEF, nullptr);
	tokenIndex++;

	if (tokenIndex >= tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "Function definition");
		return nullptr;
	}

	if (tokens[tokenIndex].tag != Token::TTAG_IDENTIFIER) { // Anonymous function.
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "'Global function' name");
		return nullptr;
	}

	if (~flags & PARSE_FLAG::ALLOW_FUNCTION_DEF) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[5], "are not authorized within this file");
		return nullptr;
	}

	if (scopeLevel != 0) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[5], "can not occur inside another function");
		return nullptr;
	}

	const auto& function_insert = functions->insert({ tokens[tokenIndex].val_identifier, Function_tL{ loaded } });
	if (!function_insert.second) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[11], tokens[tokenIndex].val_identifier);
		return nullptr;
	}
	Function_tL& function = function_insert.first->second;

	const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].val_identifier, NAME_TABLE.size() + 1 });

	if (insertion.second) {
		size_t len = strlen(tokens[tokenIndex].val_identifier) + 1;
		function.name = new char[len];
		memcpy(function.name, tokens[tokenIndex].val_identifier, len);
	}
	else {
		function.name = tokens[tokenIndex].val_identifier;
	}
	tokens[tokenIndex].val_identifier = nullptr; // Steal.

	function.variable_id = insertion.first->second;
	function.program = std::make_shared<Program_tL>();
	function.program->instructions.reserve(tokens.size() - tokenIndex);
	function.global = flags & PARSE_FLAG::GLOBAL_ALL;

	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_PARENTHESIS_OPEN, nullptr);
	tokenIndex++;

	while (tokenIndex < tokens.size()) {
		if (tokens[tokenIndex].tag != Token::TTAG_IDENTIFIER)
			break;
		const std::pair<NAME_TABLE_TYPE::iterator, bool>& insertion = NAME_TABLE.insert({ tokens[tokenIndex].val_identifier, NAME_TABLE.size() + 1 });
		function.arg_id.emplace_back(insertion.first->second);
		if (insertion.second) tokens[tokenIndex].val_identifier = nullptr; // Steal.
		tokenIndex++;

		if (tokenIndex >= tokens.size() || tokens[tokenIndex].tag != Token::TTAG_COMMA)
			break;
		tokenIndex++;
	}
	function.arg_id.shrink_to_fit();

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_PARENTHESIS_CLOSE, nullptr);
	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_BRACE_OPEN, nullptr);
	tokenIndex++;

	scopeLevel++;
	if (parse_instructions(function, nullptr) == PARSE_ERROR)
		return nullptr;
	scopeLevel--;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_BRACE_CLOSE, nullptr);
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
		case Token::TTAG_NONE:
		case Token::TTAG_INT:
		case Token::TTAG_FLOAT:
		case Token::TTAG_STRING:
		case Token::TTAG_IDENTIFIER:
		{
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			if (parse_result == Token::TTAG_FUNCTION) {
				program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
				break;
			}
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
			tokenIndex++;
		}
		break;

		case Token::TTAG_COMMA:
		case Token::TTAG_COLON:
		case Token::TTAG_BRACE_OPEN:
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
			return PARSE_ERROR;

		case Token::TTAG_BRACE_CLOSE:
			return true;

		case Token::TTAG_IF:
			if (parse_if(function, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		case Token::TTAG_ELSE:
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
			return PARSE_ERROR;

		case Token::TTAG_FOR:
		case Token::TTAG_WHILE:
		case Token::TTAG_DO:
			if (parse_loop(function, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		case Token::TTAG_BREAK:
			if (interrupts == nullptr) {
				parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
				return PARSE_ERROR;
			}
			interrupts[0].push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, -1);
			tokenIndex++;
			break;

		case Token::TTAG_CONTINUE:
			if (interrupts == nullptr) {
				parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
				return PARSE_ERROR;
			}
			interrupts[1].push_back(program.size());
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, -1);
			tokenIndex++;
			break;

		case Token::TTAG_FUNCTION_DEF:
		{
			//tokenIndex++;
			//if (tokenIndex < tokens.size()) {
			//	if (tokens[tokenIndex].tag == Token::TTAG_IDENTIFIER) {
			//		tokenIndex--;
					std::pair<lin_num, col_num> function_position{ tokens[tokenIndex].line, tokens[tokenIndex].column };
					Function_tL* parse_result = parse_function();
					if (parse_result == nullptr)
						return PARSE_ERROR;
					const auto& original = loaded->functions.find(parse_result->name);
					program.emplace_back(function_position.first, function_position.second, (original == loaded->functions.end()) ? parse_result : &original->second);
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
			//if (parse_result == Token::TTAG_FUNCTION) {
			//	program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
			//	break;
			//}
			//REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			//program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
			//tokenIndex++;
		}
		break;

		case Token::TTAG_RETURN:
		{
			tokenIndex++;
			std::vector<Token> sequence;
			tok_tag parse_result = parse_sequence(sequence, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));
			program.push_back(std::move(token));
			if (parse_result == Token::TTAG_FUNCTION)
				break;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			tokenIndex++;
		}
		break;

		case Token::TTAG_AWAIT:
			program.emplace_back(std::move(tokens[tokenIndex]));
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			tokenIndex++;
			break;

		case Token::TTAG_LABEL:
		{
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_STRING); // Expected a "string" for the label name.
			if (function.program->labels.find(tokens[tokenIndex].val_string->string_get()) != function.program->labels.end()) {
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[9], tokens[tokenIndex].val_string->string_get());
				return PARSE_ERROR;
			}
			const size_t len = strlen(tokens[tokenIndex].val_string->string_get()) + 1;
			char* label_name = new char[len];
			memcpy(label_name, tokens[tokenIndex].val_string->string_get(), len);
			function.program->labels.insert({ label_name, program.size() }); // Labels are not OWNED.
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_COLON);
			tokenIndex++;
		}
		break;

		case Token::TTAG_GOTO:
		{
			tokenIndex++;
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			program.push_back(std::move(token));
			if (parse_result == Token::TTAG_FUNCTION) {
				parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[7], "Evaluing operand");
				return PARSE_ERROR;
			}
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			tokenIndex++;
		}
		break;

		/* Impossible to come from the tokenized stream.
		case Token::TTAG_VARIABLE:
		case Token::TTAG_REFERENCE:
		case Token::TTAG_ARRAY:
		case Token::TTAG_SEQUENCE:
		case Token::TTAG_ARRAY_INIT:
		case Token::TTAG_INDEX:
		case Token::TTAG_CALL:
		case Token::TTAG_BUILTIN:
		case Token::TTAG_FUNCTION:
		case Token::TTAG_JUMP:
		case Token::TTAG_JUMP_ON_FALSE:
		case Token::TTAG_JUMP_ON_NOT_FALSE:
			return PARSE_ERROR;
		*/

		case Token::TTAG_SEMICOLON:
			tokenIndex++;
			break;

		default: // All Operators and Operation Delimiters.
		{
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_MIN);
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));
			if (parse_result == Token::TTAG_FUNCTION) {
				program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
				break;
			}
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);
			tokenIndex++;
		}
		break;

		}
	}

	return true;
}

Function_tL* Parser::parse(SourceFile* file_, std::unordered_map<std::string, Function_tL>* _functions, const char* funcname, unsigned short _flags)
{
	loaded = file_;
	functions = _functions;
	flags = _flags;

	const auto& file_function_insert = functions->insert({ funcname, Function_tL{ loaded } });
	if (!file_function_insert.second) {
		parserError(file_name(), 0, 0, ERROR_MESSAGES[11], funcname);
		return nullptr;
	}
	Function_tL* file_function = &file_function_insert.first->second;
	file_function->global = flags & (PARSE_FLAG::GLOBAL_FIRST | PARSE_FLAG::GLOBAL_ALL);
	file_function->program = std::make_shared<Program_tL>();
	file_function->program->instructions.reserve(tokens.size() - tokenIndex);

	const size_t len = strlen(funcname) + 1;
	file_function->name = new char[len];
	std::memcpy(file_function->name, funcname, len);

	if (parse_instructions(*file_function, nullptr) == PARSE_ERROR)
		return nullptr;

	if (tokenIndex < tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[10]);
		return nullptr;
	}

	/*
	* TODO: At some point check
	* !tokens.empty();
	* !file_function->program->instructions.empty();
	* The source code may be all blankspaces and semicolons.
	*/

	file_function->program->instructions.shrink_to_fit();
	return file_function;
}
