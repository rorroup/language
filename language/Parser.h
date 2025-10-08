#ifndef H_PARSER
#define H_PARSER

#include <vector>
#include <deque>
#include <string>
#include "common.h"
#include "Interpreter.h"

#define parserError(format, ...) printError("Parser: " format, __VA_ARGS__)

typedef int tok_size;
#define TOKEN_MAX 12000

struct Parser
{
public:
	std::deque<Token> tokens{};
	int tokenIndex{ 0 };
	int scopeLevel{ 0 };
	std::vector<Function*>* loaded{ nullptr };

	short int parse_sequence(Function& function, const tok_tag separator_symbol);
	tok_tag parse_operation(Function& function);
	char parse_if(Function& function, std::vector<int> interrupts[2]);
	char parse_loop(Function& function, std::vector<int> interrupts[2]);
	char parse_function(Function& function);
	char parse_instructions(Function& function, std::vector<int> interrupts[2]);

	bool parse(std::vector<Function*>* file_);
};

#endif // !H_PARSER
