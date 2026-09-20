#ifndef H_LANGUAGE_PARSER
#define H_LANGUAGE_PARSER

#include <iterator>
#include <algorithm>
#include <vector>
#include <deque>
#include <unordered_map>
#include <string>
#include "common.h"
#include "Interpreter.h"

namespace Language
{
	struct Parser
	{
	public:
		std::deque<Token> tokens{};
		int tokenIndex{ 0 };
		int scopeLevel{ 0 };
		unsigned short flags{ 0 };
		SourceFile* loaded{ nullptr };
		std::unordered_map<std::string, Function_tL>* functions{ nullptr };

		tok_tag parse_operand(std::vector<Token>& program);
		tok_tag parse_operation(std::vector<Token>& program, int_tL precedence_min); // Parse operator joined operation.

		short parse_if(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2]); // Conditional branching parser.
		char parse_loop(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2]); // Loop parser.
		Function_tL* parse_function();

		char parse_instructions(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2] = nullptr); // Complete Language parser.

		Function_tL* parse(SourceFile* file_, std::unordered_map<std::string, Function_tL>* _functions, const char* funcname, unsigned short _flags);

	private:
		const char* file_name();
		static bool tag_unary(tok_tag tag);
		static bool tag_binary(tok_tag tag);

		static bool goto_label(Function_tL& _function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps); // Resolve 'goto' and 'label' JUMP indices.
	};
}

#endif // !H_LANGUAGE_PARSER
