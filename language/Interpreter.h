#ifndef H_INTERPRETER
#define H_INTERPRETER

#include <unordered_map>
#include <vector>
#include <deque>
#include <algorithm>
#include <string>
#include "common.h"

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
		size_t hash = FNV_offset_basis;
		while (*s != '\0') {
			hash = (hash ^ *s) * FNV_prime;
			s++;
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

typedef char tok_tag;
struct Token;
enum SOLVE_RESULT : char
{
	SOLVE_AWAIT = -1,
	SOLVE_ERROR = 0,
	SOLVE_OK = 1,
};

typedef intptr_t int_tL;
#ifdef PTR64
#define LANGUAGE_INT(x) INT64_C(x)
#define fINT_TL PRId64
typedef double float_tL;
#define LANGUAGE_FLOAT(x) (x)
#else
#define LANGUAGE_INT(x) INT32_C(x)
#define fINT_TL PRId32
typedef float float_tL;
#define LANGUAGE_FLOAT(x) (x ##f)
#endif // PTR64

#define LANGUAGE_ZERO_INT LANGUAGE_INT(0)
#define LANGUAGE_FALSE_INT LANGUAGE_ZERO_INT
constexpr int_tL LANGUAGE_TRUE_INT = (int_tL)(!LANGUAGE_FALSE_INT);

constexpr float_tL LANGUAGE_ZERO_FLOAT = (float_tL)(LANGUAGE_ZERO_INT);
#define LANGUAGE_FALSE_FLOAT LANGUAGE_ZERO_FLOAT
constexpr float_tL LANGUAGE_TRUE_FLOAT = (float_tL)(!LANGUAGE_FALSE_FLOAT);

typedef unsigned short owners_t;

struct String_tL
{
	/*
	* Variable size struct to store a determined size char array.
	* https://www.geeksforgeeks.org/cpp/overloading-new-delete-operator-c/
	* WARNING: ALWAYS INITIALIZE OBJECTS INDIVIDUALLY.
	*/
public:
	owners_t owned : 1;
	owners_t owners : 8 * sizeof(owners_t) - 1;
	char string[sizeof(char*)];

	char* string_get() { return owned ? string : *(char**)string; }
	
	String_tL(const char* source)													{ owned = 0; owners = 0; *(const char**)string = source; }
	String_tL(const char* source, size_t length)									{ owned = 1; owners = 0; std::memcpy(string, source, length + 1); }
	String_tL(const char* left, size_t lengthL,	const char* right, size_t lengthR)	{ owned = 1; owners = 0; snprintf(string, lengthL + lengthR + 1, "%s%s", left, right); }
	String_tL(const char* left, size_t lengthL,	int_tL right, size_t lengthR)		{ owned = 1; owners = 0; snprintf(string, lengthL + lengthR + 1, "%s%" fINT_TL, left, right); }
	String_tL(int_tL left, size_t lengthL,		const char* right, size_t lengthR)	{ owned = 1; owners = 0; snprintf(string, lengthL + lengthR + 1, "%" fINT_TL "%s", left, right); }
	String_tL(const char* left, size_t lengthL,	float_tL right, size_t lengthR)		{ owned = 1; owners = 0; snprintf(string, lengthL + lengthR + 1, "%s%f", left, right); }
	String_tL(float_tL left, size_t lengthL,	const char* right, size_t lengthR)	{ owned = 1; owners = 0; snprintf(string, lengthL + lengthR + 1, "%f%s", left, right); }

	void* operator new(size_t size, size_t length) { return ::operator new(sizeof(owners_t) + length + 1); }
	void operator delete(void* p, size_t length) { ::operator delete(p); }
	void operator delete(void* p) { ::operator delete(p); }

#define String_tL_external(source) ::new String_tL(source)
	static String_tL* init(const char* source, size_t length) { return new(length) String_tL(source, length); }
#define String_tL_init(argL, lenL, argR, lenR) (new((lenL) + (lenR)) String_tL((argL), (lenL), (argR), (lenR)))
};

struct Array_tL;
struct Function_tL;
struct Thread_tL;
typedef SOLVE_RESULT Builtin_tL(std::vector<Token>&, std::vector<Token>&, Thread_tL*);

struct Token
{
public:
	enum : tok_tag
	{
		TAG_BEGIN = 0,
		INNER_BEGIN = TAG_BEGIN,
		VALUE_BEGIN = INNER_BEGIN,

			// VALUES.
			NONE = VALUE_BEGIN,			// 
			INT,						// 
			FLOAT,						// 
			STRING,						// 
			ARRAY,						// 
			FUNCTION,					// User-defined function.
			BUILTIN,					// C++ function.

		VALUE_END,

			// VARIABLES.
			VARIABLE = VALUE_END,		// 
			IDENTIFIER,					// 
			REFERENCE,					// 

			// SPECIAL OPERATIONS.
			SEQUENCE,					// 
			INDEX,						// 
			ARRAY_INIT,					// 
			CALL,						// 

			// PROGRAM COUNTER CONTROLLERS.
			JUMP,						// 
			JUMP_ON_FALSE,				// 
			JUMP_ON_NOT_FALSE,			// 

		INNER_END,
		KEYWORD_BEGIN = INNER_END,

			// KEYWORDS.
			IF = KEYWORD_BEGIN,			// if
			ELSE,						// else
			FOR,						// for
			WHILE,						// while
			DO,							// do
			BREAK,						// break
			CONTINUE,					// continue
			//SWITCH,
			//CASE,
			//DEFAULT,
			FUNCTION_DEF,				// function
			RETURN,						// return
			AWAIT,						// await
			LABEL,						// label
			GOTO,						// goto

			FALSE,						// false
			TRUE,						// true

		KEYWORD_END,
		SYMBOL_BEGIN = KEYWORD_END,
		DELIMITER_BEGIN = SYMBOL_BEGIN,

			// DELIMITERS.
			SEMICOLON = DELIMITER_BEGIN,// ;
			COMMA,						// ,
			COLON,						// :
			PARENTHESIS_OPEN,			// (
			PARENTHESIS_CLOSE,			// )
			BRACKET_OPEN,				// [
			BRACKET_CLOSE,				// ]
			BRACE_OPEN,					// {
			BRACE_CLOSE,				// }

		DELIMITER_END,
		OPERATOR_BEGIN = DELIMITER_END,
		UNARY_BEGIN = OPERATOR_BEGIN,

			// OPERATORS.
			UNARY_FLIP = UNARY_BEGIN,	// ~
			UNARY_NEGATION,				// !
			UNARY_POSITIVE,				// +
			UNARY_NEGATIVE,				// -

		UNARY_END,
		BINARY_BEGIN = UNARY_END,

			BINARY_ADD = BINARY_BEGIN,	// +
			BINARY_SUBSTRACT,			// -
			BINARY_MULTIPLY,			// *
			BINARY_DIVIDE,				// /
			BINARY_MODULUS,				// %

			BINARY_SHIFT_LEFT,			// <<
			BINARY_SHIFT_RIGHT,			// >>

			BINARY_AND_BITWISE,			// &
			BINARY_OR_BITWISE,			// |
			BINARY_OR_EXCLUSIVE,		// ^

			BINARY_AND,					// &&
			BINARY_OR,					// ||

			BINARY_LESSER,				// <
			BINARY_GREATER,				// >
			BINARY_LESSER_EQUAL,		// <=
			BINARY_GREATER_EQUAL,		// >=
			BINARY_EQUAL_DOUBLE,		// ==
			BINARY_EQUAL_NOT,			// !=
			BINARY_EQUAL,				// =

			// TODO: Compound Assignment.

		BINARY_END,

			// TODO: TERNARY ?

		OPERATOR_END = BINARY_END,
		SYMBOL_END = OPERATOR_END,
		TAG_END = SYMBOL_END
	};

public:
	union // https://en.cppreference.com/w/cpp/language/union
	{
		int_tL			u_int;
		float_tL		u_float;
		String_tL*		u_string;
		Array_tL*		u_array;
		Function_tL*	u_function;
		Builtin_tL*		u_builtin;
		char*			u_identifier;
	};
	lin_num line;
	col_num column;
	tok_tag tag;

	// Constructors.
	Token();
	Token(lin_num l, col_num c,				int_tL			val);
	Token(lin_num l, col_num c,				float_tL		val);
	Token(lin_num l, col_num c,				String_tL*		val);
	Token(lin_num l, col_num c,				Array_tL*		val);
	Token(lin_num l, col_num c,				Function_tL*	val);
	Token(									Builtin_tL*		val);
	Token(lin_num l, col_num c,				char*			val);
	Token(lin_num l, col_num c, tok_tag t,	int_tL			val);

	// https://en.cppreference.com/w/cpp/language/rule_of_three.html
	Token(const Token& token);					// Copy constructor.
	Token(Token&& token) noexcept;				// Move constructor.
	Token& operator=(const Token& token);		// Copy Assignment.
	Token& operator=(Token&& token) noexcept;	// Move Assignment.

	// Destructor.
	~Token();

public:
	bool as_bool() const;
	void print() const;

	void info() const;
};

struct Array_tL
{
public:
	std::vector<Token> array;
	unsigned short owners{ 0 };
};

typedef umap_cstring_key(const int_tL) NAME_TABLE_TYPE;
typedef std::unordered_map<int_tL, Token> VALUE_TABLE_TYPE;

extern NAME_TABLE_TYPE NAME_TABLE;
extern VALUE_TABLE_TYPE VALUE_TABLE;

typedef umap_cstring_key(const size_t) LABEL_TABLE_TYPE;
struct SourceFile;

struct Function_tL
{
	SourceFile* source{ nullptr };
	char* name{ nullptr };
	std::vector<int_tL> arg_id;
	std::vector<Token> program;
	LABEL_TABLE_TYPE labels;
	int_tL variable_id{ 0 };
	bool loaded{ true };
	bool global{ false };

	void unload()
	{
		if (!loaded) return;
		
		loaded = false;
		if (name) delete[] name;
		arg_id.clear();
		program.clear();
		for (auto& label : labels) delete[] label.first;
		labels.clear();
	}

	~Function_tL()
	{
		unload();
		arg_id.~vector();
		program.~vector();
	}
};

struct Execution_tL;

Token* GET_VARIABLE_VALUE(Token& variable, Execution_tL& state);
Token* GET_VARIABLE_VALUE_GLOBAL(Token& variable, Execution_tL& state);
VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE(Execution_tL& state);
VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE_GLOBAL(Execution_tL& state);

struct Execution_tL
{
public:
	VALUE_TABLE_TYPE LOCALS;
	std::vector<Token> solution;
	Function_tL* pFunction;
	Token* (*GET_VARIABLE_VALUE_)(Token& variable, Execution_tL& state);
	VALUE_TABLE_TYPE& (*GET_ASSIGNMENT_TABLE_)(Execution_tL& state);
	size_t program_counter;
	size_t lastSequence;

	Execution_tL(Function_tL* _function)
	{
		pFunction = _function;
		if (pFunction->global) {
			GET_VARIABLE_VALUE_ = GET_VARIABLE_VALUE_GLOBAL;
			GET_ASSIGNMENT_TABLE_ = GET_ASSIGNMENT_TABLE_GLOBAL;
		}
		else {
			GET_VARIABLE_VALUE_ = GET_VARIABLE_VALUE;
			GET_ASSIGNMENT_TABLE_ = GET_ASSIGNMENT_TABLE;
		}
		program_counter = 0;
		lastSequence = -1;
	}
	Execution_tL(const Execution_tL& other)
	{
		*this = other;
	}
	Execution_tL(Execution_tL&& other) noexcept
	{
		*this = std::move(other);
	}
	Execution_tL& operator=(const Execution_tL& other)
	{
		if (this != &other) {
			pFunction = other.pFunction;
			if (!pFunction->global) LOCALS = other.LOCALS;
			solution = other.solution;
			GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
			GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
			program_counter = other.program_counter;
			lastSequence = other.lastSequence;
		}
		return *this;
	}
	Execution_tL& operator=(Execution_tL&& other) noexcept
	{
		pFunction = other.pFunction;
		if (!pFunction->global) LOCALS = std::move(other.LOCALS);
		solution = std::move(other.solution);
		GET_VARIABLE_VALUE_ = other.GET_VARIABLE_VALUE_;
		GET_ASSIGNMENT_TABLE_ = other.GET_ASSIGNMENT_TABLE_;
		program_counter = other.program_counter;
		lastSequence = other.lastSequence;
		return *this;
	}
};

struct Thread_tL
{
	std::vector<Execution_tL> executing;
};

struct SourceFile
{
public:
	std::string name;
	std::deque<Function_tL> functions;

	void unload()
	{
		for (auto& function : functions)
			function.unload();
	}
};

typedef std::unordered_map<std::string, SourceFile> LOADED_SOURCEFILE_TYPE;

extern LOADED_SOURCEFILE_TYPE LOADED_SOURCEFILE;

struct RegisteredSequence
{
	const char* sequence;
	const char* name;
	const tok_tag tag;
	const int_tL value;
};

// https://learn.microsoft.com/en-us/cpp/c-language/precedence-and-order-of-evaluation?view=msvc-170
enum OPERATOR_PRECEDENCE : int_tL
{
	PRECEDENCE_INVALID = -100,
	PRECEDENCE_SEQUENCE = 0,
	PRECEDENCE_ASSIGNMENT,
	PRECEDENCE_TERNARY,
	PRECEDENCE_OR,
	PRECEDENCE_AND,
	PRECEDENCE_BITWISE_OR,
	PRECEDENCE_BITWISE_XOR,
	PRECEDENCE_BITWISE_AND,
	PRECEDENCE_EQUALITY,
	PRECEDENCE_RELATIONAL,
	PRECEDENCE_SHIFT,
	PRECEDENCE_ADDITIVE,
	PRECEDENCE_MULTIPLICATIVE,
	//PRECEDENCE_TYPECAST,
	PRECEDENCE_UNARY,
	PRECEDENCE_EXPRESSION,
};

#define PRECEDENCE_MIN PRECEDENCE_ASSIGNMENT

#define ASSOCIATIVITY_SHIFT 8
#define ASSOCIATIVITY_SET(val) ((val) << ASSOCIATIVITY_SHIFT)
#define ASSOCIATIVITY_GET(val) ((val) >> ASSOCIATIVITY_SHIFT)

enum OPERATOR_ASSOCIATIVITY : int_tL
{
	ASSOCIATIVITY_RIGHT_TO_LEFT = ASSOCIATIVITY_SET(0),
	ASSOCIATIVITY_LEFT_TO_RIGHT = ASSOCIATIVITY_SET(1),
};

#define PRECEDENCE_MASK 0xFF
#define OP_PRECEDENCE(val) ((val) & PRECEDENCE_MASK)
#define OP_ASSOCIATIVITY(val) (ASSOCIATIVITY_GET(val))

extern const RegisteredSequence LANGUAGE_TOKEN_TAG[Token::TAG_END];

#define intlen(n) ((n) == 0 ? 1 : ((n) > 0 ? log10(n) + 1 : log10(-(n)) + 2))
const RegisteredSequence* tag_id(const tok_tag tag);
const char* tag_name(tok_tag tag);
const char* variable_name(int_tL id);
SOLVE_RESULT run(Thread_tL& thread);

Function_tL* file_load(const char* filename);
SOLVE_RESULT file_import(const char* filename);
void file_unload(const char* filename);
void LANGUAGE_initialize();
void LANGUAGE_terminate();
void LANGUAGE_reload();

#endif // !H_INTERPRETER
