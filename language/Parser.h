#ifndef H_PARSER
#define H_PARSER

#include <iterator>
#include <algorithm>
#include <vector>
#include <deque>
#include <string>
#include "common.h"
#include "Interpreter.h"

struct Parser
{
public:
	std::deque<Token> tokens{};
	int tokenIndex{ 0 };
	int scopeLevel{ 0 };
	SourceFile* loaded{ nullptr };

	// TODO: Deprecate function argument. Keep its labels though.
	tok_tag parse_sequence(Function_tL& function, std::vector<Token>& program, const tok_tag separator_symbol);
	tok_tag parse_operand(Function_tL& function, std::vector<Token>& program);
	tok_tag parse_operation(Function_tL& function, std::vector<Token>& program, int_tL precedence_min);
	short parse_if(Function_tL& function, std::vector<int> interrupts[2]);
	char parse_loop(Function_tL& function, std::vector<int> interrupts[2]);
	char parse_function(Function_tL& function);
	char parse_instructions(Function_tL& function, std::vector<int> interrupts[2]);

	Function_tL* parse(SourceFile* file_, bool global_first);

private:
	const char* file_name();
	static bool tag_unary(tok_tag tag);
	static bool tag_binary(tok_tag tag);
};

#endif // !H_PARSER
