#ifndef H_TOKENIZER
#define H_TOKENIZER

#include <fstream>
#include <iterator>
#include <algorithm>
#include <deque>
#include "common.h"
#include "Parser.h"
#include "Interpreter.h"

typedef short int buf_size; // TODO: Rename.
#define LENGTH_NUMBER 100
#define LENGTH_STRING 1000
#define LENGTH_NAME 100
#define LENGTH_SYMBOL 100

#define tokenizerError(format, ...) printError("Tokenizer: " format, __VA_ARGS__)

const char* readfile(const char* file_name);
bool tokenize_source(const char* source, std::deque<Token>& tokens);

#endif // !H_TOKENIZER
