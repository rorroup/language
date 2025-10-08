#ifndef H_TOKENIZER
#define H_TOKENIZER

#include <fstream>
#include <iterator>
#include <algorithm>
#include <deque>
#include "common.h"
#include "Parser.h"
#include "Interpreter.h"

#define TAB_COLUMN 4

typedef unsigned short buf_size;
#define LENGTH_NUMBER 100
#define LENGTH_STRING 1000
#define LENGTH_NAME 100
#define LENGTH_SYMBOL 100
#define BUFFER_MAX LENGTH_STRING

const char* readfile(const char* file_name);
bool tokenize_source(const char* filename, const char* source, std::deque<Token>& tokens);

#endif // !H_TOKENIZER
