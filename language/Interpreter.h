#ifndef H_INTERPRETER
#define H_INTERPRETER

#include <unordered_map>
#include <vector>
#include <algorithm>
#include <string>
#include "common.h"


struct SharedArray;
struct Function;
struct Execution;

typedef char* str_tok;
typedef int str_own;
constexpr unsigned char STR_OWN_BYTES = sizeof(str_own);
#define strlen_own(s) (STR_OWN_BYTES + strlen((s) + STR_OWN_BYTES))
#define STR_OWNERS(s) (*((str_own*)(s)))
#define STR_OWN_STR(s) ((s) + STR_OWN_BYTES)

typedef char tok_tag;

struct Token
{
public:
	enum : tok_tag
	{
		NONE = 0,					// 

		INT,						// 
		FLOAT,						// 
		STRING,						// 
		ARRAY,						// 

		FUNCTION,					// 
		BUILTIN,					// 

		IDENTIFIER,					// 

		INDEX,						// 
		ARRAY_INIT,					// 
		CALL,						// 

		RETURN,						// 
		YIELD,						// Python style 'yield'.
		AWAIT,						// Lua style 'yield'.

		YIELDED,					// TODO: Maybe merge with yield.

		// PROGRAM COUNTER CONTROLLERS.
		GOTO,						// 

		JUMP,						// 
		JUMP_ON_FALSE,				// 
		JUMP_ON_TRUE,				// 
		JUMP_NEXT,					// 

		// OPERATORS.
		UNARY_FLIP,					// ~
		UNARY_NEGATION,				// !
		UNARY_POSITIVE,				// +
		UNARY_NEGATIVE,				// -

		BINARY_ADD,					// +
		BINARY_SUBSTRACT,			// -
		BINARY_MULTIPLY,			// *
		BINARY_DIVIDE,				// /
		BINARY_MODULUS,				// %

		BINARY_SHIFT_LEFT,			// <<
		BINARY_SHIFT_RIGHT,			// >>

		// TODO:
		//SYMBOL_OR,
		//SYMBOL_AND,
		// 
		//SYMBOL_XOR,
		//SYMBOL_BOR, // bitwise
		//SYMBOL_BAND, // bitwise

		BINARY_LESSER,				// <
		BINARY_GREATER,				// >
		BINARY_LESSER_EQUAL,		// <=
		BINARY_GREATER_EQUAL,		// >=
		BINARY_EQUAL_DOUBLE,		// ==
		BINARY_EQUAL_NOT,			// !=
		BINARY_EQUAL,				// =

		// DELIMITERS.
		SEMICOLON,					// ;
		COMMA,						// ,
		COLON,						// :
		PARENTHESIS_OPEN,			// (
		PARENTHESIS_CLOSE,			// )
		BRACKET_OPEN,				// [
		BRACKET_CLOSE,				// ]
		BRACE_OPEN,					// {
		BRACE_CLOSE,				// }

		// KEYWORDS.
		IF,							// 
		ELSE,						// 
		FOR,						// 
		WHILE,						// 
		DO,							// 
		BREAK,						// 
		CONTINUE,					// 
		//SWITCH,
		//CASE,
		//DEFAULT,
		FUNCTION_DEF,				// 
		LABEL,						// 
	};

public:
	union // https://en.cppreference.com/w/cpp/language/union
	{
		long int num;
		float frac;
		char* str;
		SharedArray* vec;
		Function* fx;
		char(*func)(std::vector<Token>&, std::vector<Token>&);
		Execution* exe; // Struct may be trimmed down since only the program counter and the local variables are necesary.
	};
	tok_tag type_;

	Token();
	Token(const Token& token);
	Token(tok_tag t, long int i);
	Token(tok_tag t, float f);
	Token(tok_tag t, char* s);

	Token(tok_tag t, SharedArray* v);

	Token(tok_tag t, Function* f);

	Token(tok_tag t, char(*f)(std::vector<Token>&, std::vector<Token>&));

	Token(tok_tag t, Execution* e);

	~Token();

	Token& operator=(const Token& token);
};

struct SharedArray
{
public:
	unsigned short int owners{ 0 };
	std::vector<Token> a{};

	~SharedArray() { a.~vector(); }
};

enum SOLVE_RESULT : char {
	SOLVE_AWAIT = -1,
	SOLVE_ERROR = 0,
	SOLVE_OK = 1,
};

#define SOLVE_FAILED return SOLVE_ERROR


// https://en.cppreference.com/w/cpp/container/unordered_map/unordered_map
struct kpCHAR_HASH
{
	size_t operator()(const char* k) const;
};

struct kpCHAR_EQUAL
{
	bool operator()(const char* lhs, const char* rhs) const;
};

#define UMAP_kpCHAR(K, V) std::unordered_map<K, V, kpCHAR_HASH, kpCHAR_EQUAL>

extern UMAP_kpCHAR(const char*, Token) VALUE_TABLE;


struct Function
{
	char* name{ nullptr };
	std::vector<std::string> argnames;
	std::vector<Token> program;
	UMAP_kpCHAR(const char*, const size_t) labels {};
};

#define CHAR_YIELDED "*" // Invalid symbol can never be part of a variable name.
constexpr unsigned char ADDR_FORMAT_LEN = 1 + 2 * 8 + 1;

struct Execution
{
	int CP{ 0 };
	int ES{ 0 }; // TODO: Remove.
	int index{ 0 }; // Current index inside the Thread solution.
	std::vector<Token> solution{};
	Function* pFunction;
	UMAP_kpCHAR(const char*, Token) LOCALS {};
	char last_called[ADDR_FORMAT_LEN]{ 0x00 };

	Execution(Function* p, int idx) : pFunction(p), index(idx) {} // https://stackoverflow.com/a/14789126
};

struct Thread
{
	std::vector<Execution> calling{};
};

char run(Thread& thread);

#define LANGUAGE_FALSE 0
#define LANGUAGE_TRUE 1

extern const Token TOKEN_FALSE;
extern const Token TOKEN_TRUE;

#endif // !H_INTERPRETER
