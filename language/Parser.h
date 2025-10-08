#ifndef H_PARSER
#define H_PARSER

#include <vector>
#include "common.h"
#include "Interpreter.h"

#define parserError(format, ...) printError("Parser: " format, __VA_ARGS__)

typedef short int tok_size;
#define TOKEN_MAX 12000

enum TokenType : s_lang
{
	TOKEN_NONE = LANGUAGE_TOKEN,
	TOKEN_VALUE,
	TOKEN_IDENTIFIER,
	TOKEN_OPERATOR,
	TOKEN_DELIMITER,
	TOKEN_KEYWORD,
};

enum DelimiterType : s_lang
{
	DELIMITER_NONE = LANGUAGE_DELIMITER,
	DELIMITER_SEMICOLON,
	DELIMITER_COMMA,
	//DELIMITER_COLON,
	DELIMITER_PARENTHESIS_LEFT,
	DELIMITER_PARENTHESIS_RIGHT,
	DELIMITER_BRACKET_LEFT,
	DELIMITER_BRACKET_RIGHT,
	DELIMITER_BRACE_LEFT,
	DELIMITER_BRACE_RIGHT,
};

enum Keywords : s_lang
{
	KEYWORD_NONE = LANGUAGE_KEYWORD,
	KEYWORD_IF,
	KEYWORD_ELSEIF,
	KEYWORD_ELSE,
	KEYWORD_FOR,
	KEYWORD_WHILE,
	KEYWORD_BREAK,
	KEYWORD_CONTINUE,
	//KEYWORD_SWITCH,
	//KEYWORD_CASE,
	//KEYWORD_DEFAULT,
	KEYWORD_FUNCTION,
	KEYWORD_RETURN,
};

struct Token
{
public:
	s_lang type_;
	s_lang value;
};

class Parser
{
public:
	tok_size index;
	std::vector<Token> tokens;

public:
	Parser();

public:
	bool tokenRetrieve(Token& hold, char offset);

	bool parse_operand(Expression*& root);
	bool parse_operandIndex(Expression*& root);
	bool parse_operation(Expression*& root);

	bool build_sequence(std::vector<Expression*>& sequence, s_lang delimiter, s_lang allowed);
	bool build_loop(Expression*& root);
	bool build_function(Expression*& root);
	bool parse_conditionBranch_sub(ConditionalBranch& branch);
	bool parse_conditionBranch(Expression*& root);

	bool parse_instruction(Expression*& root, s_lang terminated);

	bool build_program(std::vector<Expression*>& program);
	bool build_file(std::vector<Expression*>& program);
};

#endif // !H_PARSER
