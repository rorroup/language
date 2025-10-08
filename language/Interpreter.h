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

#define LANGUAGE_FALSE ((intt)0)
constexpr intt LANGUAGE_TRUE = !LANGUAGE_FALSE;

#define LANGUAGE_ZERO_INT LANGUAGE_FALSE
constexpr floatt LANGUAGE_ZERO_FLOAT = (floatt)(LANGUAGE_ZERO_INT);

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

enum SOLVE_RESULT : char
{
	SOLVE_AWAIT = -1,
	SOLVE_ERROR = 0,
	SOLVE_OK = 1,
};

#define SOLVE_FAILED return SOLVE_ERROR

struct Function;
struct Execution;

struct Token;

typedef SOLVE_RESULT fBuiltin(std::vector<Token>&, std::vector<Token>&);

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

		VARIABLE,					// 

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
		fBuiltin* func;
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
	Token(fBuiltin* f);
	Token(Execution* e);

	Token(tok_tag t, intt i);

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


// https://en.cppreference.com/w/cpp/container/unordered_map/unordered_map
struct s_cstring_hash
{
	// https://stackoverflow.com/questions/34597260/stdhash-value-on-char-value-and-not-on-memory-address
#if SIZE_MAX >= ULLONG_MAX
#define FNV_offset_basis UINT64_C(14695981039346656037)
#define FNV_prime UINT64_C(1099511628211)
#else
#define FNV_offset_basis UINT32_C(2166136261)
#define FNV_prime UINT32_C(16777619)
#endif // SIZE_MAX >= ULLONG_MAX
	std::size_t operator()(const char* s) const
	{
		size_t i = 0;
		size_t hash = FNV_offset_basis;
		while (s[i] != '\0') {
			hash = (hash ^ s[i]) * FNV_prime;
			i++;
		}
		return hash;
	}
};

struct s_cstring_equal
{
	bool operator()(const char* lhs, const char* rhs) const
	{
		return lhs != nullptr && rhs != nullptr && strlen(lhs) == strlen(rhs) && strcmp(lhs, rhs) == 0;
	}
};

#define umap_cstring_key(V) std::unordered_map<const char*, V, s_cstring_hash, s_cstring_equal>

typedef umap_cstring_key(const intt) NAME_TABLE_TYPE;
typedef std::unordered_map<intt, Token> VALUE_TABLE_TYPE;

extern NAME_TABLE_TYPE NAME_TABLE;
extern VALUE_TABLE_TYPE VALUE_TABLE;

typedef umap_cstring_key(const size_t) LABEL_TABLE_TYPE;

struct Function
{
	char* name{ nullptr };
	intt variable_id{ 0 };
	std::vector<intt> arg_id;
	std::vector<Token> program;
	LABEL_TABLE_TYPE labels {};
	bool loaded{ true };
	bool global{ false };
};

bool GET_VARIABLE_VALUE(Token& variable, Execution& state, bool raw);
bool GET_VARIABLE_VALUE_GLOBAL(Token& variable, Execution& state, bool raw);
VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE(Execution& state);
VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE_GLOBAL(Execution& state);

struct Execution
{
	int CP{ 0 };
	int ES{ 0 }; // TODO: Remove.
	int index{ 0 }; // Current index inside the Thread solution.
	std::vector<Token> solution{};
	Function* pFunction;
	VALUE_TABLE_TYPE LOCALS {};
	intt last_called{ LANGUAGE_ZERO_INT };
	bool (*GET_VARIABLE_VALUE_)(Token& variable, Execution& state, bool raw) { nullptr };
	VALUE_TABLE_TYPE& (*GET_ASSIGNMENT_TABLE_)(Execution& state) { nullptr };

	Execution(Function* p, int idx) : pFunction(p), index(idx) { // https://stackoverflow.com/a/14789126
		if (pFunction->global) {
			GET_VARIABLE_VALUE_ = GET_VARIABLE_VALUE_GLOBAL;
			GET_ASSIGNMENT_TABLE_ = GET_ASSIGNMENT_TABLE_GLOBAL;
		}
		else {
			GET_VARIABLE_VALUE_ = GET_VARIABLE_VALUE;
			GET_ASSIGNMENT_TABLE_ = GET_ASSIGNMENT_TABLE;
		}
	}
	Execution(const Execution& other) : CP(other.CP), ES(other.ES), index(other.index), pFunction(other.pFunction), last_called(other.last_called) { // TODO: Delete?
		solution = other.solution;
		if (!pFunction->global) {
			LOCALS = other.LOCALS;
		}
		GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
		GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
		printError("MAY Execution Copy Constructor NEVER BE CALLED!!");
	}
	Execution(Execution&& other) noexcept : CP(other.CP), ES(other.ES), index(other.index), pFunction(other.pFunction), last_called(other.last_called) {
		solution = std::move(other.solution);
		if (!pFunction->global) {
			LOCALS = std::move(other.LOCALS);
		}
		GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
		GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
	}
	Execution& operator=(const Execution& other) { // TODO: Delete?
		if (this != &other) {
			CP = other.CP;
			ES = other.ES;
			index = other.index;
			pFunction = other.pFunction;
			last_called = other.last_called;
			solution = other.solution;
			if (!pFunction->global) {
				LOCALS = other.LOCALS;
			}
			GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
			GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
		}
		printError("MAY Execution Copy Assignment NEVER BE CALLED!!");
		return *this;
	}
	Execution& operator=(Execution&& other) noexcept {
		CP = other.CP;
		ES = other.ES;
		index = other.index;
		pFunction = other.pFunction;
		last_called = other.last_called;
		solution = std::move(other.solution);
		if (!pFunction->global) {
			LOCALS = std::move(other.LOCALS);
		}
		GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
		GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
		return *this;
	}
};

struct Thread
{
	std::vector<Execution> calling{};
};

SOLVE_RESULT run(Thread& thread);

extern const Token TOKEN_FALSE;
extern const Token TOKEN_TRUE;

#define intlen(n) ((n) == 0 ? 1 : ((n) > 0 ? log10(n) + 1 : log10(-(n)) + 2))

#endif // !H_INTERPRETER
