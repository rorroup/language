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

		tok_tag parse_operand(std::vector<Token>& program); // Parse a single operand.
		tok_tag parse_operation(std::vector<Token>& program, int_tL precedence_min); // Parse operator joined operation.

		short parse_if(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2]); // Conditional branching parser.
		char parse_loop(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2]); // Loop parser.
		Function_tL* parse_function();

		char parse_instructions(Function_tL& function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps, std::vector<int> interrupts[2] = nullptr); // Complete Language parser.

		Function_tL* parse(SourceFile* file_, std::unordered_map<std::string, Function_tL>* _functions, const char* funcname, unsigned short _flags);

		static bool tag_value(const tok_tag tag);		// Whether a tag is a VALUE.
		static bool tag_unary(const tok_tag tag);		// Whether a tag is a UNARY operator.
		static bool tag_incdec(const tok_tag tag);		// Whether a tag is a UNARY INCREMENT or DECREMENT operator.
		static bool tag_binary(const tok_tag tag);		// Whether a tag is a BINARY operator.
		static bool tag_assignment(const tok_tag tag);	// Whether a tag is a BINARY ASSIGNMENT operator.
		static bool tag_ternary(const tok_tag tag);		// Whether a tag is a TERNARY operator.

	private:
		const char* file_name();

		static bool goto_label(Function_tL& _function, std::pair<std::vector<std::pair<size_t, std::string>>, std::unordered_map<std::string, size_t>>& _jumps); // Resolve 'goto' and 'label' JUMP indices.
	};
}

#endif // !H_LANGUAGE_PARSER
