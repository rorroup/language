#include "Lexer.h"

// https://stackoverflow.com/a/22676401
// https://cplusplus.com/reference/algorithm/find_if/
// https://stackoverflow.com/a/14595314
static const std::pair<const char*, const Token> LanguageKeywords[] = {
	{"true", Token{TOKEN_VALUE, (s_lang)(&VALUE_BOOL_TRUE)}},
	{"false", Token{TOKEN_VALUE, (s_lang)(&VALUE_BOOL_FALSE)}},
	{"if", Token{TOKEN_KEYWORD, KEYWORD_IF}},
	{"elseif", Token{TOKEN_KEYWORD, KEYWORD_ELSEIF}},
	{"else", Token{TOKEN_KEYWORD, KEYWORD_ELSE}},
	{"for", Token{TOKEN_KEYWORD, KEYWORD_FOR}},
	{"while", Token{TOKEN_KEYWORD, KEYWORD_WHILE}},
	{"break", Token{TOKEN_KEYWORD, KEYWORD_BREAK}},
	{"continue", Token{TOKEN_KEYWORD, KEYWORD_CONTINUE}},
	//{"switch", Token{TOKEN_KEYWORD, KEYWORD_SWITCH}},
	//{"case", Token{TOKEN_KEYWORD, KEYWORD_CASE}},
	//{"default", Token{TOKEN_KEYWORD, KEYWORD_DEFAULT}},
	{"function", Token{TOKEN_KEYWORD, KEYWORD_FUNCTION}},
	{"return", Token{TOKEN_KEYWORD, KEYWORD_RETURN}},
};

static const std::pair<const char*, const Token> LanguageSymbols[] = {
	{"==", Token{TOKEN_OPERATOR, OPERATOR_EQUAL_DOUBLE}},
	{"!=", Token{TOKEN_OPERATOR, OPERATOR_EQUAL_NOT}},
	{"<=", Token{TOKEN_OPERATOR, OPERATOR_LESSER_EQUAL}},
	{">=", Token{TOKEN_OPERATOR, OPERATOR_GREATER_EQUAL}},
	{"<<", Token{TOKEN_OPERATOR, OPERATOR_SHIFT_LEFT}},
	{">>", Token{TOKEN_OPERATOR, OPERATOR_SHIFT_RIGHT}},
	{"~", Token{TOKEN_OPERATOR, OPERATOR_FLIP}},
	{"!", Token{TOKEN_OPERATOR, OPERATOR_NEGATION}},
	{"+", Token{TOKEN_OPERATOR, OPERATOR_PLUS}},
	{"-", Token{TOKEN_OPERATOR, OPERATOR_MINUS}},
	{"*", Token{TOKEN_OPERATOR, OPERATOR_MULTIPLY}},
	{"/", Token{TOKEN_OPERATOR, OPERATOR_DIVIDE}},
	{"%", Token{TOKEN_OPERATOR, OPERATOR_REMAINDER}},
	{"<", Token{TOKEN_OPERATOR, OPERATOR_LESSER}},
	{">", Token{TOKEN_OPERATOR, OPERATOR_GREATER}},
	{"=", Token{TOKEN_OPERATOR, OPERATOR_EQUAL}},
	{";", Token{TOKEN_DELIMITER, DELIMITER_SEMICOLON}},
	{",", Token{TOKEN_DELIMITER, DELIMITER_COMMA}},
	//{":", Token{TOKEN_DELIMITER, DELIMITER_COLON}},
	{"(", Token{TOKEN_DELIMITER, DELIMITER_PARENTHESIS_LEFT}},
	{")", Token{TOKEN_DELIMITER, DELIMITER_PARENTHESIS_RIGHT}},
	{"[", Token{TOKEN_DELIMITER, DELIMITER_BRACKET_LEFT}},
	{"]", Token{TOKEN_DELIMITER, DELIMITER_BRACKET_RIGHT}},
	{"{", Token{TOKEN_DELIMITER, DELIMITER_BRACE_LEFT}},
	{"}", Token{TOKEN_DELIMITER, DELIMITER_BRACE_RIGHT}},
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
	tokens.clear();
	while (*source != '\0') {
		if (isspace(*source))
		{
			source++;
			continue;
		}
		if (tokens.size() == TOKEN_MAX) { // Check container size before pushing more tokens.
			tokenizerError("Unable to contain more than %d tokens at a time.", TOKEN_MAX);
			return false;
		}
		buf_size i = 0;
		if (isdigit(*source))
		{
			char buffer[LENGTH_NUMBER];
			memset(buffer, '\0', LENGTH_NUMBER);
			unsigned char modifier = NUMBER_DECIMAL;
			while (true)
			{
				if (isdigit(*source))
				{
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
				else if (*source == 'b') {
					if (modifier == NUMBER_DECIMAL && i == 1 && buffer[0] == '0') {
						buffer[i] = 'b';
						i++;
						modifier = NUMBER_BINARY;
					}
					else {
						tokenizerError("Number '%s' can not be treated as binary.", buffer);
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
						tokens.push_back(Token{ TOKEN_VALUE, (s_lang)(new Value{VALUE_INT, (t_value)strtoll(buffer, NULL, 10)}) });
						break;
					case NUMBER_FLOAT:
						{
							float f = (float)atof(buffer);
							t_value n = 0;
							memcpy(&n, &f, sizeof(float));
							tokens.push_back(Token{ TOKEN_VALUE, (s_lang)(new Value{VALUE_FLOAT, n}) });
						}
						break;
					case NUMBER_BINARY:
						if (i <= 2) {
							tokenizerError("Binary number '%s' is empty.", buffer);
							return false;
						}
						tokens.push_back(Token{ TOKEN_VALUE, (s_lang)(new Value{VALUE_INT, (t_value)strtoll(&buffer[2], NULL, 2)}) });
						break;
					case NUMBER_HEXADECIMAL:
						if (i <= 2) {
							tokenizerError("Hexadecimal number '%s' is empty.", buffer);
							return false;
						}
						tokens.push_back(Token{ TOKEN_VALUE, (s_lang)(new Value{VALUE_INT, (t_value)strtoll(&buffer[2], NULL, 16)}) });
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
					const std::pair<const char*, const Token>* iter = std::find_if(std::begin(LanguageKeywords), std::end(LanguageKeywords), [buffer](std::pair<const char*, const Token> element) {return strlen(buffer) == strlen(element.first) && strcmp(buffer, element.first) == 0; });
					if (iter == std::end(LanguageKeywords))
					{
						printDebug("'%s' is not a reserved word. Treating as identifier.", buffer); // TODO: Remove.
						char* identifier = new char[i + 1];
						memcpy(identifier, buffer, i + 1);
						tokens.push_back(Token{ TOKEN_IDENTIFIER, (s_lang)identifier });
					}
					else
					{
						printDebug("Found reserved word '%s'", buffer); // TODO: Remove.
						tokens.push_back(iter->second);
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
					char* text = new char[i + 1];
					memcpy(text, buffer, i + 1);
					tokens.push_back(Token{ TOKEN_VALUE, (s_lang)(new Value{VALUE_STRING, text}) });
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
						const std::pair<const char*, const Token>* iter = std::find_if(std::begin(LanguageSymbols), std::end(LanguageSymbols), [buffer, j](std::pair<const char*, const Token> element) {return strlen(&buffer[j]) >= strlen(element.first) && strncmp(&buffer[j], element.first, strlen(element.first)) == 0; });
						if (iter != std::end(LanguageSymbols)) {
							tokens.push_back(iter->second);
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
	return true;
}
