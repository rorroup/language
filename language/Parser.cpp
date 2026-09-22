#include "Parser.h"

#define PARSE_ERROR -1
#define OPERATION_EMPTY -10

const char* Language::Parser::file_name()
{
	return loaded ? loaded->name.c_str() : nullptr;
}

bool Language::Parser::tag_unary(tok_tag tag)
{
	return Token::TTAG_UNARY_BEGIN_ <= tag && tag < Token::TTAG_UNARY_END_;
}

bool Language::Parser::tag_binary(tok_tag tag)
{
	return Token::TTAG_BINARY_BEGIN_ <= tag && tag < Token::TTAG_BINARY_END_;
}

namespace Language
{
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
		{ EXPRESSION_MISSING, "Label name '%s' not exists in function '%s'." },
	};

	static void parserError(const char* filename, lin_num line, col_num column, std::pair<const ErrMesType, const char*> f, ...)
	{
		va_list argp;
		va_start(argp, f);
		printLanguageError(ERROR_MESSAGE_TYPES[0], ERROR_MESSAGE_TYPES[f.first], filename, line, column, f.second, argp);
		va_end(argp);
	}
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

/* goto_label.
* Assign matching registered GOTO and LABEL token label names to their corresponding JUMP indices.
*/
bool Language::Parser::goto_label(Function_tL& _function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps)
{
	// Aliases.
	std::vector<Token>& program = *_function.program;
	std::vector<std::pair<size_t, std::string>>& gotos = _jumps.first;
	std::unordered_map<std::string, size_t>& labels = _jumps.second;

	for (const auto& _goto : gotos)
	{
		const auto& _label = labels.find(_goto.second); // Find destination LABEL matching GOTO label name.
		if (_label == labels.end()) // GOTO label name not found.
		{
			parserError(_function.source->name.c_str(), program[_goto.first].line, program[_goto.first].column, ERROR_MESSAGES[12], _goto.second.c_str(), _function.name);
			return false;
		}
		program[_goto.first].val_int = _label->second; // Assign LABEL token index as GOTO associated JUMP destination.
	}

	return true;
}

/* parse_operand.
* Parse a single operand (atom).
* Operand contains:
*	Any combination of Pre Unary operators.
*	The operand structure: a simple Value, Array, Variable, or Parenthesised operation.
*	Any combination of Post operations, such as Array Indexing (Brackets) or Function Call (Parenthesis).
* Returns the tag ID of the last parsed structure on success.
*/
Language::tok_tag Language::Parser::parse_operand(std::vector<Token>& program)
{
	static const RegisteredSequence* TOKEN_POSITIVE = tag_id(Token::TTAG_UNARY_POSITIVE);
	static const RegisteredSequence* TOKEN_NEGATIVE = tag_id(Token::TTAG_UNARY_NEGATIVE);

	const auto unary_begin = tokens.rend() - tokenIndex;	// Reverse iterator up to the first Pre Unary operator.
	auto unary_end = unary_begin;							// Reverse iterator up to the last Pre Unary operator.

	// Parse Pre Unary operators.
	while (tokenIndex < tokens.size())
	{
		Token& unary = tokens[tokenIndex];
		if		(unary.tag == Token::TTAG_BINARY_ADD)		unary = Token(unary.line, unary.column, TOKEN_POSITIVE->tag, TOKEN_POSITIVE->value); // ADD to POSITIVE.
		else if	(unary.tag == Token::TTAG_BINARY_SUBTRACT)	unary = Token(unary.line, unary.column, TOKEN_NEGATIVE->tag, TOKEN_NEGATIVE->value); // SUBTRACT to NEGATIVE.
		else if	(!tag_unary(unary.tag)) break; // Non Unary operator.
		unary_end--;
		tokenIndex++;
	}

	// No main operand structure parsed yet.
	tok_tag typeLast = OPERATION_EMPTY;

	if (tokenIndex >= tokens.size())
	{
		if (unary_end != unary_begin) { // Unary operators found.
			parserError(file_name(), unary_end->line, unary_end->column, ERROR_MESSAGES[6], tag_name(unary_end->tag));
			return PARSE_ERROR;
		}

		return OPERATION_EMPTY;
	}

	// Operand main structure.
	Token& operand = tokens[tokenIndex];
	switch (operand.tag)
	{
	// Simple Value.
	case Token::TTAG_NONE:
	case Token::TTAG_INT:
	case Token::TTAG_FLOAT:
	case Token::TTAG_STRING:
		typeLast = operand.tag; // Register tag.
		program.push_back(std::move(tokens[tokenIndex])); // Move Value.
		tokenIndex++;
		break;

	// Variable.
	case Token::TTAG_IDENTIFIER:
	{
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_VARIABLE, NAME_TABLE_get_name_id(operand.val_identifier)); // Variable name ID.
		typeLast = Token::TTAG_VARIABLE; // Register tag.
		tokenIndex++;
	}
	break;

	// Parenthesised expression.
	case Token::TTAG_PARENTHESIS_OPEN:																					// Open parenthesis.
	{
		tokenIndex++;
		std::vector<Token> inner;
		typeLast = parse_operation(inner, PRECEDENCE_ASSIGNMENT);														// Parse parenthesis inner operation.
		if (typeLast == PARSE_ERROR)
			return typeLast;
		if (typeLast == OPERATION_EMPTY) {
			parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[3], "Parenthesised expression");
			return PARSE_ERROR;
		}
		REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);																// Close parenthesis.
		program.insert(program.end(), std::make_move_iterator(inner.begin()), std::make_move_iterator(inner.end()));	// Move parenthesis content.
		typeLast = tokens[tokenIndex].tag;																				// Register closing parenthesis tag.
		tokenIndex++;
	}
	break;

	// Array.
	case Token::TTAG_BRACKET_OPEN:																							// Open bracket.
	{
		program.emplace_back(operand.line, operand.column, Token::TTAG_SEQUENCE, -1);										// Register start of the Array sequence.
		tokenIndex++;
		std::vector<Token> elements;
		if (parse_operation(elements, PRECEDENCE_SEQUENCE) == PARSE_ERROR)													// Parse Array elements sequence.
			return PARSE_ERROR;
		REQUIRE_CURRENT_TAG(Token::TTAG_BRACKET_CLOSE);																		// Close bracket.
		program.insert(program.end(), std::make_move_iterator(elements.begin()), std::make_move_iterator(elements.end()));	// Move Array content.
		program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_ARRAY_INIT, LANGUAGE_INT(1));	// Register Array Initialization Token.
		typeLast = Token::TTAG_ARRAY_INIT;																					// Register Array Initialization tag.
		tokenIndex++;
	}
	break;

	/* Remaining invalid operand tags.
	* Impossible to come from the tokenized stream (Special operations).
	* Non operation tags (Keywords and Delimiters).
	* Binary operators.
	*/
	default:
		if (unary_end != unary_begin) { // Unary operators found.
			parserError(file_name(), unary_end->line, unary_end->column, ERROR_MESSAGES[6], tag_name(unary_end->tag));
			return PARSE_ERROR;
		}

		return OPERATION_EMPTY;
	}

	// Post operators.
	while (tokenIndex < tokens.size())
	{
		// Function Call.
		if (tokens[tokenIndex].tag == Token::TTAG_PARENTHESIS_OPEN)																	// Open parenthesis.
		{
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_SEQUENCE, -1);						// Register Call sequence start.
			tokenIndex++;
			std::vector<Token> arguments;
			if (parse_operation(arguments, PRECEDENCE_SEQUENCE) == PARSE_ERROR)														// Parse Call arguments sequence.
				return PARSE_ERROR;
			REQUIRE_CURRENT_TAG(Token::TTAG_PARENTHESIS_CLOSE);																		// Close parenthesis.
			program.insert(program.end(), std::make_move_iterator(arguments.begin()), std::make_move_iterator(arguments.end()));	// Move Call arguments.
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_CALL, LANGUAGE_INT(1));			// Register Call Token.
			typeLast = Token::TTAG_CALL;																							// Register Call tag.
			tokenIndex++;
		}
		// Array Index.
		else if (tokens[tokenIndex].tag == Token::TTAG_BRACKET_OPEN)														// Open bracket.
		{
			tokenIndex++;
			std::vector<Token> index;
			if (parse_operation(index, PRECEDENCE_ASSIGNMENT) == PARSE_ERROR)												// Parse Index operation.
				return PARSE_ERROR;
			REQUIRE_CURRENT_TAG(Token::TTAG_BRACKET_CLOSE);																	// Close bracket.
			program.insert(program.end(), std::make_move_iterator(index.begin()), std::make_move_iterator(index.end()));	// Move Index operation.
			program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_INDEX, LANGUAGE_INT(1));	// Register Array Index Token.
			typeLast = Token::TTAG_INDEX;																					// Register Array Index tag.
			tokenIndex++;
		}
		else break;
	}

	program.insert(program.end(), std::make_move_iterator(unary_end), std::make_move_iterator(unary_begin)); // Move Unary operators backwards.

	return typeLast;
}

/* parse_operation.
* Build operation Token stack in Reverse Polish Notation (operator at the end) respecting the order of operations.
* Uses Precedence Climbing algorithm to build the branches following operator precedence.
* The resulting abstract syntax tree is turned into the program stack in RPN.
* https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
* Returns the id of the last parsed structure.
* 
* Assignment special handling:
*	Assignment operands are deliberately flipped because the right hand side is supposed to resolve before being assigned to the left hand variable.
*	Considering an ARRAY 'a' and an INT 'i' in the following 2 cases:
*		(Case 1) a[i] + a[i = 1];
*		(Case 2) a[i] = a[i = 1];
*	The addition operation SHOULD index the left instance of the array on the previous value of 'i',
*	whereas the assignment SHOULD index it on the right hand newly assigned value of '1'.
*	This is particularly problematic when the assigned value clears the array, thus any previously computed indices are invalidated.
*	To solve this problem the left hand side variable WILL ALWAYS be dereferenced last, just before the assignment.
*	This way the subsequent assignment only occurs on a valid variable reference.
*/
Language::tok_tag Language::Parser::parse_operation(std::vector<Token>& program, int_tL precedence_min)
{
	std::vector<Token>& left = program;
	tok_tag typeLast = parse_operand(left); // Parse left hand side operand.
	if (typeLast < Token::TTAG_BEGIN_) // No operand.
		return typeLast;

	// Variable assignment operation.
	if (tokenIndex < tokens.size() && tokens[tokenIndex].tag == Token::TTAG_BINARY_EQUAL && left[0].tag == Token::TTAG_VARIABLE) {
		left[0].tag = Token::TTAG_REFERENCE;
	}

	// Precedence analisis and branching.
#define OP_PRECEDENCE(val) ((val) & PRECEDENCE_MASK_)
#define OP_ASSOCIATIVITY(val) ((val) & ASSOCIATIVITY_MASK_)
	while (tokenIndex < tokens.size() && tag_binary(tokens[tokenIndex].tag) && OP_PRECEDENCE(tokens[tokenIndex].val_int) >= precedence_min)
	{
		Token& binary = tokens[tokenIndex]; // Binary operator.
		tokenIndex++;

		std::vector<Token> right;
		typeLast = parse_operation(right, OP_PRECEDENCE(binary.val_int) + (OP_ASSOCIATIVITY(binary.val_int) ? 1 : 0)); // Compute new min precedence to parse right hand side operation.
		if (typeLast == PARSE_ERROR)
			return typeLast;
		if (typeLast == OPERATION_EMPTY) {
			if (binary.tag == Token::TTAG_BINARY_COMMA) // Trailing comma is allowed.
				break;
			parserError(file_name(), binary.line, binary.column, ERROR_MESSAGES[6], tag_name(binary.tag));
			return PARSE_ERROR;
		}

		// Combine operands and operator in RPN.
		if (binary.tag == Token::TTAG_BINARY_EQUAL)	// Assignment operator.
			left.swap(right);						// Invert order.
		left.insert(left.end(), std::make_move_iterator(right.begin()), std::make_move_iterator(right.end()));	// Stack together left and right hand operands.
		if (binary.tag != Token::TTAG_BINARY_COMMA)																// Skip comma sequence operator.
			left.push_back(std::move(binary));																	// Move operator to the end.
	}

	return typeLast;
}

/* parse_if.
* Parse 'if' and 'else' branching inside the current function.
*/
short Language::Parser::parse_if(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = *function.program;
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
			tok_tag parsed = parse_operation(condition, PRECEDENCE_ASSIGNMENT);
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

		if (parse_instructions(function, _jumps, interrupts) == PARSE_ERROR)
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

/* parse_loop.
* Parse 'for' and 'do'/'while' loops inside the current function.
*/
char Language::Parser::parse_loop(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = *function.program;
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
		tok_tag loop_init = parse_operation(init, PRECEDENCE_ASSIGNMENT);
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
		loop_condition = parse_operation(condition_content, PRECEDENCE_ASSIGNMENT);
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

		loop_increment = parse_operation(increment_content, PRECEDENCE_ASSIGNMENT);
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
	if (parse_instructions(function, _jumps, interruptions) == PARSE_ERROR)
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
		loop_condition = parse_operation(condition, PRECEDENCE_ASSIGNMENT);
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

		if (parse_instructions(function, _jumps, interrupts) == PARSE_ERROR)
			return PARSE_ERROR;

		REQUIRE_CURRENT_TAG(Token::TTAG_BRACE_CLOSE);
		tokenIndex++;
	}

	for (const auto& index : interruptions[0]) // break;
		program[index].val_int = program.size();

	return true;
}

Language::Function_tL* Language::Parser::parse_function()
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

	function.name = tokens[tokenIndex].val_identifier; // Function name.
	tokens[tokenIndex].val_identifier = nullptr; // Steal.

	function.variable_id = NAME_TABLE_get_name_id(function.name); // Function name ID.
	function.program = std::make_shared<Program_tL>();
	function.program->reserve(tokens.size() - tokenIndex);
	function.global = flags & PARSE_FLAG::GLOBAL_ALL;

	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_PARENTHESIS_OPEN, nullptr);
	tokenIndex++;

	while (tokenIndex < tokens.size()) {
		if (tokens[tokenIndex].tag != Token::TTAG_IDENTIFIER)
			break;
		function.arg_id.emplace_back(NAME_TABLE_get_name_id(tokens[tokenIndex].val_identifier)); // Function argument name ID.
		tokenIndex++;

		if (tokenIndex >= tokens.size() || tokens[tokenIndex].tag != Token::TTAG_BINARY_COMMA)
			break;
		tokenIndex++;
	}
	function.arg_id.shrink_to_fit();

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_PARENTHESIS_CLOSE, nullptr);
	tokenIndex++;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_BRACE_OPEN, nullptr);
	tokenIndex++;

	scopeLevel++;
	std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>> jumps; // Container for current function 'goto' and 'label' declarations.
	if (parse_instructions(function, jumps, nullptr) == PARSE_ERROR)
		return nullptr;
	if (!goto_label(function, jumps)) // Resolve 'goto' and 'label' JUMP indices.
		return nullptr;
	scopeLevel--;

	REQUIRE_CURRENT_TAG_RETURN(Token::TTAG_BRACE_CLOSE, nullptr);
	tokenIndex++;

	function.program->shrink_to_fit();
	return &function;
}

/* parse_instructions.
* Parse complete source code in the current block by calling every other parser respectively.
*/
char Language::Parser::parse_instructions(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2])
{
	std::vector<Token>& program = *function.program;
	while (tokenIndex < tokens.size())
	{
		/* Parse and stack an individual structure.
		* ALWAYS assumed to be an operation unless a keyword states otherwise.
		* It calls the corresponding method, but some are processed locally.
		*/
		Token& token = tokens[tokenIndex];
		switch (token.tag)
		{
		/* End of the current structure special symbol (not consumed).
		*/
		case Token::TTAG_BRACE_CLOSE: // }
			return true;

		/* if/else structure.
		*/
		case Token::TTAG_IF: // if
			if (parse_if(function, _jumps, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		case Token::TTAG_ELSE: // else
			parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
			return PARSE_ERROR;

		/* Loop structure.
		*/
		case Token::TTAG_FOR:	// for
		case Token::TTAG_WHILE:	// while
		case Token::TTAG_DO:	// do
			if (parse_loop(function, _jumps, interrupts) == PARSE_ERROR)
				return PARSE_ERROR;
			break;

		/* Loop interruption instruction.
		* break;
		* continue;
		*/
		case Token::TTAG_BREAK:							// break
		case Token::TTAG_CONTINUE:						// continue
			if (interrupts == nullptr) { // Not inside a loop.
				parserError(file_name(), token.line, token.column, ERROR_MESSAGES[8], tag_name(token.tag));
				return PARSE_ERROR;
			}
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);	// Semicolon.
			interrupts[token.tag - Token::TTAG_BREAK].push_back(program.size());				// Register JUMP index.
			program.emplace_back(token.line, token.column, Token::TTAG_JUMP, LANGUAGE_INT(-1));	// Register JUMP Token.
			tokenIndex++;
			break;

		/* Function definition.
		*/
		case Token::TTAG_FUNCTION_DEF: // function
		{
			std::pair<lin_num, col_num> function_position{ tokens[tokenIndex].line, tokens[tokenIndex].column };
			Function_tL* parse_result = parse_function();
			if (parse_result == nullptr)
				return PARSE_ERROR;
			const auto& original = loaded->functions.find(parse_result->name);
			program.emplace_back(function_position.first, function_position.second, (original == loaded->functions.end()) ? parse_result : &original->second);
		}
		break;

		/* Return instruction.
		* return operations;
		*/
		case Token::TTAG_RETURN:													// return
		{
			tokenIndex++;
			std::vector<Token> sequence;
			tok_tag parse_result = parse_operation(sequence, PRECEDENCE_SEQUENCE);	// Operations
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);								// Semicolon.
			program.insert(program.end(), std::make_move_iterator(sequence.begin()), std::make_move_iterator(sequence.end()));	// Move operations.
			program.push_back(std::move(token));																				// Move return Token.
			tokenIndex++;
		}
		break;

		/* Await instruction.
		* await;
		*/
		case Token::TTAG_AWAIT:										// await
			program.emplace_back(std::move(token)); // Move await.
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);				// Semicolon.
			tokenIndex++;
			break;

		/* label declaration.
		* label "label name":
		* Register label position index within the current function.
		*/
		case Token::TTAG_LABEL:																									// 'label' keyword.
		{
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_STRING);																			// "String" label name.
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_COLON);																				// COLON symbol.
			const auto& insertion = _jumps.second.try_emplace(tokens[tokenIndex - 1].val_string->string_get(), program.size());	// Register label name and token index.
			if (!insertion.second) { // Label name already existed.
				parserError(file_name(), tokens[tokenIndex - 1].line, tokens[tokenIndex - 1].column, ERROR_MESSAGES[9], tokens[tokenIndex - 1].val_string->string_get());
				return PARSE_ERROR;
			}
			tokenIndex++;
		}
		break;

		/* goto instruction.
		* goto "label name";
		* Set a JUMP instruction and register its target label name.
		* Label names and positions are resolved after parsing the function.
		*/
		case Token::TTAG_GOTO:																										// 'goto' keyword.
		{
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_STRING);																				// "String" label name.
			tokenIndex++;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);																				// SEMICOLON symbol.
			_jumps.first.emplace_back(program.size(), tokens[tokenIndex - 1].val_string->string_get());								// Register JUMP token index and target label name.
			program.emplace_back(tokens[tokenIndex - 1].line, tokens[tokenIndex - 1].column, Token::TTAG_JUMP, LANGUAGE_INT(-1));	// Add the token to JUMP to the specified label position.
			tokenIndex++;
		}
		break;

		/* Operation structure.
		* operation;
		* ALL Operands.
		* ALL Symbols.
		* Impossible to come from the tokenized stream (Special operations).
		*/
		default:
		{
			std::vector<Token> operation;
			tok_tag parse_result = parse_operation(operation, PRECEDENCE_ASSIGNMENT);	// Operation.
			if (parse_result == PARSE_ERROR)
				return PARSE_ERROR;
			REQUIRE_CURRENT_TAG(Token::TTAG_SEMICOLON);									// Semicolon.
			if (parse_result != OPERATION_EMPTY) {
				program.insert(program.end(), std::make_move_iterator(operation.begin()), std::make_move_iterator(operation.end()));	// Move operation.
				program.emplace_back(tokens[tokenIndex].line, tokens[tokenIndex].column, Token::TTAG_JUMP, program.size() + 1);			// End of operation JUMP Token.
			}
			tokenIndex++;
		}
		break;

		}
	}

	return true;
}

Language::Function_tL* Language::Parser::parse(SourceFile* file_, std::unordered_map<std::string, Function_tL>* _functions, const char* funcname, unsigned short _flags)
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
	file_function->program->reserve(tokens.size() - tokenIndex);

	const size_t len = strlen(funcname) + 1;
	file_function->name = new char[len];
	std::memcpy(file_function->name, funcname, len);
	std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>> jumps; // Container for current file function 'goto' and 'label' declarations.
	if (parse_instructions(*file_function, jumps, nullptr) == PARSE_ERROR)
		return nullptr;
	if (!goto_label(*file_function, jumps)) // Resolve 'goto' and 'label' JUMP indices.
		return nullptr;

	if (tokenIndex < tokens.size()) {
		parserError(file_name(), tokens[tokenIndex].line, tokens[tokenIndex].column, ERROR_MESSAGES[10]);
		return nullptr;
	}

	/*
	* TODO: At some point check
	* !tokens.empty();
	* !file_function->program->empty();
	* The source code may be all blankspaces and semicolons.
	*/

	file_function->program->shrink_to_fit();
	return file_function;
}
