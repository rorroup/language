#ifndef H_LANGUAGE_LEXER
#define H_LANGUAGE_LEXER

#include <fstream>
#include <iterator>
#include <algorithm>
#include <deque>
#include "common.h"
#include "Parser.h"
#include "Interpreter.h"

namespace Language
{
	typedef unsigned short buf_size;
	enum : buf_size
	{
		LENGTH_NUMBER = 100,
		LENGTH_STRING = 1000,
		LENGTH_NAME = 100,
		LENGTH_SYMBOL = 100,

		BUFFER_MAX = LENGTH_STRING
	};

	const char* readfile(const char* file_name);
	bool tokenize_source(const char* filename, const char* source, std::deque<Token>& tokens);
}

#endif // !H_LANGUAGE_LEXER
