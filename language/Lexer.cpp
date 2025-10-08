#include "Lexer.h"

static const Token TOKEN_FALSE{ Token::INT, (long int)LANGUAGE_FALSE };
static const Token TOKEN_TRUE{ Token::INT, (long int)LANGUAGE_TRUE };

// https://stackoverflow.com/a/22676401
// https://cplusplus.com/reference/algorithm/find_if/
// https://stackoverflow.com/a/14595314
static const std::pair<const char*, const Token> LanguageKeywords[] = {
	{"true", TOKEN_TRUE},
	{"false", TOKEN_FALSE},
	{"if", Token{Token::IF, (long int)0}},
	{"else", Token{Token::ELSE, (long int)0}},
	{"for", Token{Token::FOR, (long int)0}},
	{"while", Token{Token::WHILE, (long int)0}},
	{"do", Token{Token::DO, (long int)0}},
	{"break", Token{Token::BREAK, (long int)0}},
	{"continue", Token{Token::CONTINUE, (long int)0}},
	//{"switch", Token{TOKEN_KEYWORD, Token::SWITCH}},
	//{"case", Token{TOKEN_KEYWORD, Token::CASE}},
	//{"default", Token{TOKEN_KEYWORD, Token::DEFAULT}},
	{"function", Token{Token::FUNCTION_DEF, (long int)0}},
	{"return", Token{Token::RETURN, (long int)0}},
	{"yield", Token{Token::YIELD, (long int)0}},
	{"await", Token{Token::AWAIT, (long int)0}},
	{"label", Token{Token::LABEL, (long int)0}},
	{"goto", Token{Token::GOTO, (long int)0}},
};

static const std::pair<const char*, const Token> LanguageSymbols[] = {
	{"==", Token{Token::BINARY_EQUAL_DOUBLE, (long int)1}},
	{"!=", Token{Token::BINARY_EQUAL_NOT, (long int)1}},
	{"<=", Token{Token::BINARY_LESSER_EQUAL, (long int)1}},
	{">=", Token{Token::BINARY_GREATER_EQUAL, (long int)1}},
	{"<<", Token{Token::BINARY_SHIFT_LEFT, (long int)20}},
	{">>", Token{Token::BINARY_SHIFT_RIGHT, (long int)20}},
	{"~", Token{Token::UNARY_FLIP, (long int)100}},
	{"!", Token{Token::UNARY_NEGATION, (long int)100}},
	{"+", Token{Token::BINARY_ADD, (long int)10}},
	{"-", Token{Token::BINARY_SUBSTRACT, (long int)10}},
	{"*", Token{Token::BINARY_MULTIPLY, (long int)20}},
	{"/", Token{Token::BINARY_DIVIDE, (long int)20}},
	{"%", Token{Token::BINARY_MODULUS, (long int)20}},
	{"<", Token{Token::BINARY_LESSER, (long int)1}},
	{">", Token{Token::BINARY_GREATER, (long int)1}},
	{"=", Token{Token::BINARY_EQUAL, (long int)0}},
	{";", Token{Token::SEMICOLON, (long int)-1}},
	{",", Token{Token::COMMA, (long int)-1}},
	{":", Token{Token::COLON, (long int)-20}},
	{"(", Token{Token::PARENTHESIS_OPEN, (long int)-10}},
	{")", Token{Token::PARENTHESIS_CLOSE, (long int)-10}},
	{"[", Token{Token::BRACKET_OPEN, (long int)-10}},
	{"]", Token{Token::BRACKET_CLOSE, (long int)-10}},
	{"{", Token{Token::BRACE_OPEN, (long int)-100}},
	{"}", Token{Token::BRACE_CLOSE, (long int)-100}},
};

// https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring
const char* readfile(const char* file_name)
{
	std::ifstream file_(file_name, std::ifstream::in);
	if (file_.is_open()) {
		file_.seekg(0, std::ifstream::end);
		const unsigned int length = file_.tellg();
		file_.seekg(0, std::ifstream::beg);

		char* buffer = new char[length + 1];
		memset(buffer, '\0', length + 1);

		file_.read(buffer, length);
		file_.close();
		return buffer;
	}
	printError("Failed to open file '%s'.", file_name);
	return nullptr;
}

bool tokenize_source(const char* source, std::vector<Token>& tokens)
{
	while (*source != '\0') {
		if (isspace(*source))
		{
			source++;
			continue;
		}
		if (tokens.size() >= TOKEN_MAX) { // Check container size before pushing more tokens.
			tokenizerError("Unable to contain more than %d tokens at a time.", TOKEN_MAX);
			return false;
		}
		buf_size i = 0;
		if (isdigit(*source))
		{
			enum NUMBER_TYPE : unsigned char {
				NUMBER_DECIMAL = 0,
				NUMBER_FLOAT = 1,
				NUMBER_BINARY = 2,
				NUMBER_HEXADECIMAL = 3,
			} modifier = NUMBER_DECIMAL;
			char buffer[LENGTH_NUMBER];
			memset(buffer, '\0', LENGTH_NUMBER);
			while (true)
			{
				if (isxdigit(*source))
				{
					if (*source == 'b' && i == 1 && buffer[0] == '0' && modifier == NUMBER_DECIMAL) {
						modifier = NUMBER_BINARY;
					}
					else if (((modifier == NUMBER_DECIMAL || modifier == NUMBER_FLOAT) && !isdigit(*source)) || (modifier == NUMBER_BINARY && *source != '0' && *source != '1')) {
						tokenizerError("Character '%c' is incompatible with number '%s' of type '%d'.", *source, buffer, modifier);
						return false;
					}
					buffer[i] = *source;
					i++;
				}
				else if (*source == '.')
				{
					if (modifier == NUMBER_DECIMAL) {
						buffer[i] = '.';
						i++;
						modifier = NUMBER_FLOAT;
					}
					else {
						tokenizerError("Number '%s' can not contain an additional period.", buffer);
						return false;
					}
				}
				else if (*source == 'x') {
					if (modifier == NUMBER_DECIMAL && i == 1 && buffer[0] == '0') {
						buffer[i] = 'x';
						i++;
						modifier = NUMBER_HEXADECIMAL;
					}
					else {
						tokenizerError("Number '%s' can not be treated as hexadecimal.", buffer);
						return false;
					}
				}
				else /*TODO: Support comments.*/
				{
					switch (modifier)
					{
					case NUMBER_DECIMAL:
						tokens.push_back(Token(Token::INT, strtol(buffer, NULL, 10)));
						break;
					case NUMBER_FLOAT:
						tokens.push_back((Token{ Token::FLOAT, (float)atof(buffer) }));
						break;
					case NUMBER_BINARY:
						if (i <= 2) {
							tokenizerError("Binary number '%s' is empty.", buffer);
							return false;
						}
						tokens.push_back((Token(Token::INT, strtol(&buffer[2], NULL, 2))));
						break;
					case NUMBER_HEXADECIMAL:
						if (i <= 2) {
							tokenizerError("Hexadecimal number '%s' is empty.", buffer);
							return false;
						}
						tokens.push_back((Token(Token::INT, strtol(&buffer[2], NULL, 16))));
						break;
					default:
						tokenizerError("Unknown number type '%d'.", modifier);
						return false;
					}
					break;
				}
				if (i >= LENGTH_NUMBER) {
					tokenizerError("Numbers can not be more than '%d' characters long.", LENGTH_NUMBER);
					return false;
				}
				source++;
			}
		}
		else if (isalpha(*source) || *source == '_')
		{
			char buffer[LENGTH_NAME];
			memset(buffer, '\0', LENGTH_NAME);
			while (true)
			{
				if (isalnum(*source) || *source == '_')
				{
					buffer[i] = *source;
					i++;
				}
				else /*TODO: Support comments.*/
				{
					const std::pair<const char*, const Token>* iter = std::find_if(std::begin(LanguageKeywords), std::end(LanguageKeywords), [buffer, i](std::pair<const char*, const Token> element) { return strlen(element.first) == i && strcmp(buffer, element.first) == 0; });
					if (iter == std::end(LanguageKeywords))
					{
						char* s = new char[i + 1];
						memcpy(s, buffer, i + 1);
						printDebug("'%s' is not a reserved word. Treating as identifier.", s); // TODO: Remove.
						tokens.push_back((Token{ Token::IDENTIFIER, s }));
					}
					else
					{
						printDebug("Found reserved word '%s'", buffer); // TODO: Remove.
						tokens.push_back((iter->second));
					}
					break;
				}
				if (i >= LENGTH_NAME)
				{
					tokenizerError("Names can not be more than '%d' characters long.", LENGTH_NAME);
					return false;
				}
				source++;
			}
		}
		else if (*source == '"')
		{
			char buffer[LENGTH_STRING];
			memset(buffer, '\0', LENGTH_STRING);
			char escaped = 0; // TODO: Support escaped characters.
			source++;
			while (true)
			{
				if (*source == '\0') // Unterminated String Token
				{
					tokenizerError("Unterminated string.");
					return false;
				}
				else if (*source == '"' && escaped == 0)
				{
					str_tok s = new char[STR_OWN_STR(i + 1)];
					STR_OWNERS(s) = 1;
					memcpy(STR_OWN_STR(s), buffer, i + 1);
					tokens.push_back((Token{ Token::STRING, s }));
					source++;
					break;
				}
				else
				{
					buffer[i] = *source;
					i++;
				}
				if (i >= LENGTH_STRING) {
					tokenizerError("Strings can not be more than '%d' characters long.", LENGTH_STRING);
					return false;
				}
				source++;
			}
		}
		else // TODO: Support comments.
		{
			char buffer[LENGTH_SYMBOL];
			memset(buffer, '\0', LENGTH_SYMBOL);
			while (true)
			{
				if (isalnum(*source) || *source == '_' || *source == '"' || *source == '\0' || isspace(*source))
				{
					buf_size j = 0;
					while (j < i)
					{
						const std::pair<const char*, const Token>* iter = std::find_if(std::begin(LanguageSymbols), std::end(LanguageSymbols), [buffer, j](std::pair<const char*, const Token> element) {return strncmp(&buffer[j], element.first, strlen(element.first)) == 0; });
						if (iter != std::end(LanguageSymbols)) {
							tokens.push_back((iter->second));
							j += strlen(iter->first);
						}
						else // Unsupported symbol.
						{
							tokenizerError("Unable to interpret '%s' as a known symbol.", &buffer[j]);
							return false;
						}
					}
					break;
				}
				else {
					buffer[i] = *source;
					i++;
				}
				if (i >= LENGTH_SYMBOL) {
					tokenizerError("Symbols can not be more than '%d' characters long.", LENGTH_SYMBOL);
					return false;
				}
				source++;
			}
		}
	}

	// TODO: Resize to its size?
	return true;
}
