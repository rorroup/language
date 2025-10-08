#ifndef H_INTERPRETER
#define H_INTERPRETER

#include <unordered_map>
#include <vector>
#include <algorithm>
#include <string>
#include "common.h"

typedef intptr_t intt;
#ifdef PTR64
typedef double floatt;
#else
typedef float floatt;
#endif // PTR64

/*
* Variable size struct to store a determined size char array.
* https://www.geeksforgeeks.org/cpp/overloading-new-delete-operator-c/
* WARNING: ALWAYS INITIALIZE OBJECTS INDIVIDUALLY.
*/
struct StringShared
{
public:
	unsigned short owners{ 0 };
	char string[1];

	StringShared(const char* source, size_t length) { std::memcpy(string, source, length + 1); }
	StringShared(const char* left, size_t lengthL, const char* right, size_t lengthR) { snprintf(string, lengthL + lengthR + 1, "%s%s", left, right); }
	StringShared(const char* left, size_t lengthL, intt right, size_t lengthR) { snprintf(string, lengthL + lengthR + 1, "%s%lld", left, right); }
	StringShared(intt left, size_t lengthL, const char* right, size_t lengthR) { snprintf(string, lengthL + lengthR + 1, "%lld%s", left, right); }
	StringShared(const char* left, size_t lengthL, floatt right, size_t lengthR) { snprintf(string, lengthL + lengthR + 1, "%s%f", left, right); }
	StringShared(floatt left, size_t lengthL, const char* right, size_t lengthR) { snprintf(string, lengthL + lengthR + 1, "%f%s", left, right); }

	void* operator new(size_t size, size_t length) { return ::operator new(align + length + (length % align)); }
	void operator delete(void* p) { ::operator delete(p); }

	static StringShared* init(const char* source, size_t length) { return new(length + 1) StringShared(source, length); }
#define StringShared_init(argL, lenL, argR, lenR) (new((lenL) + (lenR) + 1) StringShared((argL), (lenL), (argR), (lenR)))

private:
	static const size_t align = sizeof(owners);
};

struct SharedArray;
struct Function;
struct Execution;

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
		intt intu;
		floatt floatu;
		char* var;
		StringShared* str;
		SharedArray* vec;
		Function* fx;
		char(*func)(std::vector<Token>&, std::vector<Token>&);
		Execution* exe; // Struct may be trimmed down since only the program counter and the local variables are necesary.
	};
	tok_tag tag;

	// Constructors.
	Token();
	Token(intt i);
	Token(floatt f);
	Token(char* s);
	Token(StringShared* s);
	Token(SharedArray* v);
	Token(Function* f);
	Token(char(*f)(std::vector<Token>&, std::vector<Token>&));
	Token(Execution* e);

	Token(tok_tag t, int i);

	// https://en.cppreference.com/w/cpp/language/rule_of_three.html
	Token(const Token& token); // Copy constructor.
	Token& operator=(const Token& token); // Copy Assignment.
	Token(Token&& token) noexcept; // Move constructor.
	Token& operator=(Token&& token) noexcept; // Move Assignment.

	// Destructor.
	~Token();
};

struct SharedArray
{
public:
	unsigned short int owners{ 0 };
	std::vector<Token> a{};

	~SharedArray() { a.~vector(); }
};

enum SOLVE_RESULT : char
{
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

#define LANGUAGE_FALSE ((intt)0)
constexpr intt LANGUAGE_TRUE = !LANGUAGE_FALSE;

#define LANGUAGE_ZERO_INT LANGUAGE_FALSE
constexpr floatt LANGUAGE_ZERO_FLOAT = (floatt)(LANGUAGE_ZERO_INT);

extern const Token TOKEN_FALSE;
extern const Token TOKEN_TRUE;

#define intlen(n) ((n) == 0 ? 1 : ((n) > 0 ? log10(n) + 1 : log10(-(n)) + 2))

#endif // !H_INTERPRETER
