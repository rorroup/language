#include "Lexer.h"

// https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring
const char* readfile(const char* file_name)
{
	std::ifstream file_(file_name, std::ifstream::in);
	if (file_.is_open()) {
		file_.seekg(0, std::ifstream::end);
		const size_t length = file_.tellg();
		file_.seekg(0, std::ifstream::beg);

		char* buffer = new char[length + 1];
		memset(buffer, '\0', length + 1);

		file_.read(buffer, length);
		file_.close();
		return buffer;
	}

	printf(fERROR);
	printf("Failed to open file '%s'.\n", file_name);
	return nullptr;
}

typedef unsigned char ErrMesType;
enum : ErrMesType
{
	Tokenizer = 0,
	MALFORMED_TOKEN,
	BUFFER_OVERFLOW,
};

static const char* ERROR_MESSAGE_TYPES[]
{
	STRINGIZING(Tokenizer),
	STRINGIZING(MALFORMED_TOKEN),
	STRINGIZING(BUFFER_OVERFLOW),
};

static const std::pair<const ErrMesType, const char*> ERROR_MESSAGES[]
{
	{ Tokenizer, nullptr },
	{ MALFORMED_TOKEN, "Number is empty." },
	{ MALFORMED_TOKEN, "Unterminated string." },
	{ MALFORMED_TOKEN, "Failed to interpret '%s' as a supported symbol." },
	{ BUFFER_OVERFLOW, "'%s' tokens can not be more than '%d' characters long." },
};

void tokenizerError(const char* filename, lin_num line, col_num column, std::pair<const ErrMesType, const char*> f, ...)
{
	va_list argp;
	va_start(argp, f);
	printLanguageError(ERROR_MESSAGE_TYPES[0], ERROR_MESSAGE_TYPES[f.first], filename, line, column, f.second, argp);
	va_end(argp);
}

bool tokenize_source(const char* filename, const char* source, std::deque<Token>& tokens)
{
	enum class TOKEN_TYPE : char
	{
		COMMENT_LINE = -1,
		COMMENT_BLOCK = -2,
		NONE = 0,
		NUMBER,
		IDENTIFIER,
		STRING,
		SYMBOL,
	} tokenizing{ TOKEN_TYPE::NONE };

	enum : char
	{
		NUMBER_FLOAT		=  0,
		NUMBER_DECIMAL		= 10,
		NUMBER_BINARY		=  2,
		NUMBER_HEXADECIMAL	= 16,
	};

	char EscDotCom = 0; // Escape, Decimal point, Comment.

	lin_num line = 1, line_start;
	col_num column = 0, tok_start;

	buf_size i;
	char buffer[BUFFER_MAX];
#define bufferReset() i = 0; memset(buffer, '\0', BUFFER_MAX);
	bufferReset();

	source--;
	bool increment = true;

	do
	{
		if (increment)
		{
			source++;
			if (*source == '\n') {
				line++;
				column = 0;
			}
			else if (*source == '\t') {
				column += TAB_COLUMN - column % TAB_COLUMN;
			}
			else if (isprint(*source)) {
				column++;
			}
		}
		increment = false;

		switch (tokenizing)
		{
		case TOKEN_TYPE::NONE:
			if (isspace(*source)) {
				increment = true;
			}
			else if (isdigit(*source)) {
				tokenizing = TOKEN_TYPE::NUMBER;
				bufferReset();
				line_start = line;
				tok_start = column;
				EscDotCom = NUMBER_DECIMAL;
			}
			else if (isalpha(*source) || *source == '_') {
				tokenizing = TOKEN_TYPE::IDENTIFIER;
				bufferReset();
				line_start = line;
				tok_start = column;
				EscDotCom = 0;
			}
			else if (*source == '"') {
				tokenizing = TOKEN_TYPE::STRING;
				bufferReset();
				line_start = line;
				tok_start = column;
				EscDotCom = 0;
				increment = true;
			}
			else {
				tokenizing = TOKEN_TYPE::SYMBOL;
				bufferReset();
				line_start = line;
				tok_start = column;
				EscDotCom = 0;
			}
			break;

		case TOKEN_TYPE::NUMBER:
			if (EscDotCom == NUMBER_DECIMAL && (*source == 'b' || *source == 'B') && i == 1 && buffer[0] == '0') {
				memset(buffer, '\0', i);
				i = 0;
				EscDotCom = NUMBER_BINARY;
			}
			else if (EscDotCom == NUMBER_DECIMAL && (*source == 'x' || *source == 'X') && i == 1 && buffer[0] == '0') {
				memset(buffer, '\0', i);
				i = 0;
				EscDotCom = NUMBER_HEXADECIMAL;
			}
			else if (EscDotCom == NUMBER_DECIMAL && *source == '.') {
				buffer[i] = '.';
				i++;
				EscDotCom = NUMBER_FLOAT;
			}
			else if (((EscDotCom == NUMBER_DECIMAL || EscDotCom == NUMBER_FLOAT) && isdigit(*source)) ||
				(EscDotCom == NUMBER_HEXADECIMAL && isxdigit(*source)) ||
				(EscDotCom == NUMBER_BINARY && (*source == '0' || *source == '1'))) {
				buffer[i] = *source;
				i++;
			}
			else {
				if (i <= 0) {
					tokenizerError(filename, line_start, tok_start, ERROR_MESSAGES[1]);
					return false;
				}

#ifdef PTR64
#define str2int_tL strtoll
#define str2float_tL strtod
#else
#define str2int_tL strtol
#define str2float_tL strtof
#endif // PTR64

				if (EscDotCom == NUMBER_FLOAT)	tokens.emplace_back(line_start, tok_start, str2float_tL(buffer, nullptr));
				else							tokens.emplace_back(line_start, tok_start, str2int_tL(buffer, nullptr, EscDotCom));

				tokenizing = TOKEN_TYPE::NONE;
				EscDotCom = 0;
				break;
			}

			if (i >= LENGTH_NUMBER) {
				tokenizerError(filename, line, column, ERROR_MESSAGES[4], "Number", LENGTH_NUMBER);
				return false;
			}
			increment = true;
			break;

		case TOKEN_TYPE::IDENTIFIER:
			if (isalnum(*source) || *source == '_')
			{
				buffer[i] = *source;
				i++;
			}
			else
			{
				const RegisteredSequence* iter = std::find_if(
					std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_KEYWORD_BEGIN,
					std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_KEYWORD_END,
					[buffer, i](const RegisteredSequence& element) { return strlen(element.sequence) == i && strcmp(element.sequence, buffer) == 0; }
				);
				if (iter == std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_KEYWORD_END) {
					char* s = new char[i + 1];
					memcpy(s, buffer, i + 1);
					tokens.emplace_back(line_start, tok_start, s);
				}
				else {
					tokens.emplace_back(line_start, tok_start, iter->tag, iter->value);
				}

				tokenizing = TOKEN_TYPE::NONE;
				EscDotCom = 0;
				break;
			}

			if (i >= LENGTH_NAME) {
				tokenizerError(filename, line, column, ERROR_MESSAGES[4], "Name", LENGTH_NAME);
				return false;
			}
			increment = true;
			break;

		case TOKEN_TYPE::STRING:
			if (*source == '\0') // Unterminated String Token.
			{
				tokenizerError(filename, line_start, tok_start, ERROR_MESSAGES[2]);
				return false;
			}
			else if (*source == '"' && EscDotCom == 0)
			{
				tokens.emplace_back(line_start, tok_start, String_tL::init(buffer, i));
				tokenizing = TOKEN_TYPE::NONE;
			}
			else if (*source == '\\' && EscDotCom == 0) {
				EscDotCom = 1;
			}
			else if (EscDotCom == 1 && *source == 'n') {
				buffer[i] = '\n';
				i++;
				EscDotCom = 0;
			}
			else if (EscDotCom == 1 && *source == 't') {
				buffer[i] = '\t';
				i++;
				EscDotCom = 0;
			}
			else
			{
				buffer[i] = *source;
				i++;
				EscDotCom = 0;
			}

			if (i >= LENGTH_STRING) {
				tokenizerError(filename, line, column, ERROR_MESSAGES[4], "String", LENGTH_STRING);
				return false;
			}
			increment = true;
			break;

		case TOKEN_TYPE::SYMBOL:
			if (ispunct(*source) && *source != '"' && *source != '_' && !(EscDotCom && (*source == '/' || *source == '*'))) {
				buffer[i] = *source;
				i++;
				EscDotCom = *source == '/';
			}
			else
			{
				tokenizing = TOKEN_TYPE::NONE;
				if (EscDotCom && (*source == '/' || *source == '*')) {
					tokenizing = *source == '/' ? TOKEN_TYPE::COMMENT_LINE : TOKEN_TYPE::COMMENT_BLOCK;
					i--;
					buffer[i] = '\0';
					increment = true;
				}

				buf_size j = 0;
				while (j < i)
				{
					const RegisteredSequence* iter = std::find_if(
						std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_SYMBOL_BEGIN,
						std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_SYMBOL_END,
						[buffer, j](const RegisteredSequence& element) { return strncmp(&buffer[j], element.sequence, strlen(element.sequence)) == 0; }
					);
					if (iter != std::begin(LANGUAGE_TOKEN_TAG) + Token::TTAG_SYMBOL_END) {
						tokens.emplace_back(line_start, tok_start, iter->tag, iter->value);
						j += strlen(iter->sequence);
						tok_start += strlen(iter->sequence);
					}
					else // Unsupported symbol.
					{
						tokenizerError(filename, line_start, tok_start, ERROR_MESSAGES[3], &buffer[j]);
						return false;
					}
				}

				EscDotCom = 0;
				break;
			}

			if (i >= LENGTH_SYMBOL) {
				tokenizerError(filename, line, column, ERROR_MESSAGES[4], "Symbol", LENGTH_SYMBOL);
				return false;
			}
			increment = true;
			break;

		case TOKEN_TYPE::COMMENT_LINE:
			if (*source == '\n')
				tokenizing = TOKEN_TYPE::NONE;
			increment = true;
			break;

		case TOKEN_TYPE::COMMENT_BLOCK:
			if (EscDotCom && *source == '/')
				tokenizing = TOKEN_TYPE::NONE;
			EscDotCom = *source == '*';
			increment = true;
			break;
		}
	} while (*source != '\0');

	return true;
}
