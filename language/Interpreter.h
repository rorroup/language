#ifndef H_INTERPRETER
#define H_INTERPRETER

#include <unordered_map>
#include <vector>
#include <deque>
#include <algorithm>
#include <string>
#include "common.h"

#define FUNCTION_RETURN_SINGLE

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
SOLVE_RESULT script_run(Thread_tL& thread);
Function_tL* script_load(const char* filename);
SOLVE_RESULT script_import(const char* filename);
void script_unload(const char* filename);

int_tL LANGUAGE_initialize();
int_tL LANGUAGE_terminate();
int_tL LANGUAGE_reload();


#ifdef LANGUAGE_IMPLEMENTATION
// ================== INTERPRETER.CPP START ==================

#include "Parser.h"
#include "Lexer.h"

// https://stackoverflow.com/a/22676401
// https://cplusplus.com/reference/algorithm/find_if/
// https://stackoverflow.com/a/14595314
static const RegisteredSequence LANGUAGE_TOKEN_TAG[Token::TAG_END]
{
#define TOKEN_NAME_TAG(s) nullptr, STRINGIZING(s), Token::s, NULL

	// INNER.

	{ TOKEN_NAME_TAG(NONE) },
	{ TOKEN_NAME_TAG(INT) },
	{ TOKEN_NAME_TAG(FLOAT) },
	{ TOKEN_NAME_TAG(STRING) },
	{ TOKEN_NAME_TAG(ARRAY) },
	{ TOKEN_NAME_TAG(FUNCTION) },
	{ TOKEN_NAME_TAG(BUILTIN) },

	{ TOKEN_NAME_TAG(VARIABLE) },
	{ TOKEN_NAME_TAG(IDENTIFIER) },
	{ TOKEN_NAME_TAG(REFERENCE) },

	{ TOKEN_NAME_TAG(SEQUENCE) },
	{ TOKEN_NAME_TAG(INDEX) },
	{ TOKEN_NAME_TAG(ARRAY_INIT) },
	{ TOKEN_NAME_TAG(CALL) },

	{ TOKEN_NAME_TAG(JUMP) },
	{ TOKEN_NAME_TAG(JUMP_ON_FALSE) },
	{ TOKEN_NAME_TAG(JUMP_ON_NOT_FALSE) },

#undef TOKEN_NAME_TAG
#define TOKEN_NAME_TAG(s) STRINGIZING(s), Token::s, NULL

	// KEYWORDS.

	{ "if",			TOKEN_NAME_TAG(IF) },
	{ "else",		TOKEN_NAME_TAG(ELSE) },
	{ "for",		TOKEN_NAME_TAG(FOR) },
	{ "while",		TOKEN_NAME_TAG(WHILE) },
	{ "do",			TOKEN_NAME_TAG(DO) },
	{ "break",		TOKEN_NAME_TAG(BREAK) },
	{ "continue",	TOKEN_NAME_TAG(CONTINUE) },
	//SWITCH,
	//CASE,
	//DEFAULT,
	{ "function",	TOKEN_NAME_TAG(FUNCTION_DEF) },
	{ "return",		TOKEN_NAME_TAG(RETURN) },
	{ "await",		TOKEN_NAME_TAG(AWAIT) },
	{ "label",		TOKEN_NAME_TAG(LABEL) },
	{ "goto",		TOKEN_NAME_TAG(GOTO) },

	{ "false",		"FALSE",	Token::INT, LANGUAGE_FALSE_INT },
	{ "true",		"TRUE",		Token::INT, LANGUAGE_TRUE_INT },

#undef TOKEN_NAME_TAG
#define TOKEN_NAME_TAG(s) STRINGIZING(s), Token::s

	// SYMBOLS.

	{ "==",		TOKEN_NAME_TAG(BINARY_EQUAL_DOUBLE),	PRECEDENCE_EQUALITY | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "!=",		TOKEN_NAME_TAG(BINARY_EQUAL_NOT),		PRECEDENCE_EQUALITY | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "<=",		TOKEN_NAME_TAG(BINARY_LESSER_EQUAL),	PRECEDENCE_RELATIONAL | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ ">=",		TOKEN_NAME_TAG(BINARY_GREATER_EQUAL),	PRECEDENCE_RELATIONAL | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "<<",		TOKEN_NAME_TAG(BINARY_SHIFT_LEFT),		PRECEDENCE_SHIFT | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ ">>",		TOKEN_NAME_TAG(BINARY_SHIFT_RIGHT),		PRECEDENCE_SHIFT | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "&&",		TOKEN_NAME_TAG(BINARY_AND),				PRECEDENCE_AND | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "||",		TOKEN_NAME_TAG(BINARY_OR),				PRECEDENCE_OR | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "&",		TOKEN_NAME_TAG(BINARY_AND_BITWISE),		PRECEDENCE_BITWISE_AND | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "|",		TOKEN_NAME_TAG(BINARY_OR_BITWISE),		PRECEDENCE_BITWISE_OR | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "^",		TOKEN_NAME_TAG(BINARY_OR_EXCLUSIVE),	PRECEDENCE_BITWISE_XOR | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "+",		TOKEN_NAME_TAG(BINARY_ADD),				PRECEDENCE_ADDITIVE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "-",		TOKEN_NAME_TAG(BINARY_SUBSTRACT),		PRECEDENCE_ADDITIVE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "*",		TOKEN_NAME_TAG(BINARY_MULTIPLY),		PRECEDENCE_MULTIPLICATIVE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "/",		TOKEN_NAME_TAG(BINARY_DIVIDE),			PRECEDENCE_MULTIPLICATIVE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "%",		TOKEN_NAME_TAG(BINARY_MODULUS),			PRECEDENCE_MULTIPLICATIVE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "<",		TOKEN_NAME_TAG(BINARY_LESSER),			PRECEDENCE_RELATIONAL | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ ">",		TOKEN_NAME_TAG(BINARY_GREATER),			PRECEDENCE_RELATIONAL | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "=",		TOKEN_NAME_TAG(BINARY_EQUAL),			PRECEDENCE_ASSIGNMENT | ASSOCIATIVITY_RIGHT_TO_LEFT	},

	{ "~",		TOKEN_NAME_TAG(UNARY_FLIP),				PRECEDENCE_UNARY | ASSOCIATIVITY_RIGHT_TO_LEFT	},
	{ "!",		TOKEN_NAME_TAG(UNARY_NEGATION),			PRECEDENCE_UNARY | ASSOCIATIVITY_RIGHT_TO_LEFT	},
	{ "+",		TOKEN_NAME_TAG(UNARY_POSITIVE),			PRECEDENCE_UNARY | ASSOCIATIVITY_RIGHT_TO_LEFT	},
	{ "-",		TOKEN_NAME_TAG(UNARY_NEGATIVE),			PRECEDENCE_UNARY | ASSOCIATIVITY_RIGHT_TO_LEFT	},

	{ ";",		TOKEN_NAME_TAG(SEMICOLON),				PRECEDENCE_INVALID											},
	{ ",",		TOKEN_NAME_TAG(COMMA),					PRECEDENCE_SEQUENCE | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ ":",		TOKEN_NAME_TAG(COLON),					PRECEDENCE_TERNARY | ASSOCIATIVITY_RIGHT_TO_LEFT	},
	{ "(",		TOKEN_NAME_TAG(PARENTHESIS_OPEN),		PRECEDENCE_EXPRESSION | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ ")",		TOKEN_NAME_TAG(PARENTHESIS_CLOSE),		PRECEDENCE_EXPRESSION | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "[",		TOKEN_NAME_TAG(BRACKET_OPEN),			PRECEDENCE_EXPRESSION | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "]",		TOKEN_NAME_TAG(BRACKET_CLOSE),			PRECEDENCE_EXPRESSION | ASSOCIATIVITY_LEFT_TO_RIGHT	},
	{ "{",		TOKEN_NAME_TAG(BRACE_OPEN),				PRECEDENCE_INVALID											},
	{ "}",		TOKEN_NAME_TAG(BRACE_CLOSE),			PRECEDENCE_INVALID											},
};

LOADED_SOURCEFILE_TYPE LOADED_SOURCEFILE;
NAME_TABLE_TYPE NAME_TABLE;
VALUE_TABLE_TYPE VALUE_TABLE;

typedef unsigned char ErrMesType;
enum : ErrMesType
{
	Interpreter = 0,
	MATH_ERROR,
	VARIABLE_UNINITIALIZED,
	ARGUMENTS_MISMATCH,
	TYPE_ERROR,
	INDEX_ERROR,
	FUNCTION_ERROR,
	LABEL_ERROR,
};

static const char* ERROR_MESSAGE_TYPES[]
{
	STRINGIZING(Interpreter),
	STRINGIZING(MATH_ERROR),
	STRINGIZING(VARIABLE_UNINITIALIZED),
	STRINGIZING(ARGUMENTS_MISMATCH),
	STRINGIZING(TYPE_ERROR),
	STRINGIZING(INDEX_ERROR),
	STRINGIZING(FUNCTION_ERROR),
	STRINGIZING(LABEL_ERROR),
};

static const std::pair<const ErrMesType, const char*> ERROR_MESSAGES[]
{
	{ Interpreter, nullptr },
	{ MATH_ERROR, "Unable to divide by 'Zero'." },
	{ VARIABLE_UNINITIALIZED, "Variable '%s' not initialized." },
	{ ARGUMENTS_MISMATCH, "Operator '%s' expected '%" fINT_TL "' arguments but received '%zu' instead." },
	{ ARGUMENTS_MISMATCH, "Operator '%s' has no valid matching '%s'." },
	{ TYPE_ERROR, "Operator '%s' may not operate on '%s' token." },
	{ TYPE_ERROR, "Operator '%s' may not operate on '%s' and '%s' tokens." },
	{ TYPE_ERROR, "Operator '%s' expected '%s' token but received '%s' instead." },
	{ INDEX_ERROR, "Operator '%s' position '%" fINT_TL "' of '%s' is out of bounds." },
	{ TYPE_ERROR, "An indexed '%s' token may only be assigned a single character '%s'." },
	{ TYPE_ERROR, "'%s' token is not callable." },
	{ FUNCTION_ERROR, "Function unloaded." },
	{ LABEL_ERROR, "'%s %s' is not registered." },
	{ TYPE_ERROR, "Invalid token '%s' received." },
	{ ARGUMENTS_MISMATCH, "External '%s' is immutable." },
};

void interpreterError(const char* filename, lin_num line, col_num column, std::pair<const ErrMesType, const char*> f, ...)
{
	va_list argp;
	va_start(argp, f);
	printLanguageError(ERROR_MESSAGE_TYPES[0], ERROR_MESSAGE_TYPES[f.first], filename, line, column, f.second, argp);
	va_end(argp);
}

Token::Token() : line(0), column(0), tag(Token::NONE), u_int(0) {}
Token::Token(lin_num l, col_num c, int_tL			val) : line(l), column(c), tag(Token::INT), u_int(val) {}
Token::Token(lin_num l, col_num c, float_tL		val) : line(l), column(c), tag(Token::FLOAT), u_float(val) {}
Token::Token(lin_num l, col_num c, String_tL* val) : line(l), column(c), tag(Token::STRING), u_string(val) { u_string->owners++; }
Token::Token(lin_num l, col_num c, Array_tL* val) : line(l), column(c), tag(Token::ARRAY), u_array(val) { u_array->owners++; }
Token::Token(lin_num l, col_num c, Function_tL* val) : line(l), column(c), tag(Token::FUNCTION), u_function(val) {}
Token::Token(Builtin_tL* val) : line(0), column(0), tag(Token::BUILTIN), u_builtin(val) {}
Token::Token(lin_num l, col_num c, char* val) : line(l), column(c), tag(Token::IDENTIFIER), u_identifier(val) {}
Token::Token(lin_num l, col_num c, tok_tag t, int_tL			val) : line(l), column(c), tag(t), u_int(val) {}

Token::Token(const Token& token)
{
	tag = token.tag;
	switch (tag) {
	case Token::FLOAT:		u_float = token.u_float;																											break;
	case Token::STRING:		u_string = token.u_string;							u_string->owners++;																break;
	case Token::ARRAY:		u_array = token.u_array;							u_array->owners++;																break;
	case Token::FUNCTION:	u_function = token.u_function;																											break;
	case Token::BUILTIN:	u_builtin = token.u_builtin;																											break;
	case Token::IDENTIFIER:	u_identifier = new char[strlen(token.u_identifier) + 1];	std::memcpy(u_identifier, token.u_identifier, strlen(token.u_identifier) + 1);	break;
	default:				u_int = token.u_int;																												break;
	}
	line = token.line;
	column = token.column;
}

Token::Token(Token&& token) noexcept
{
	tag = token.tag;
	u_int = token.u_int;
	token.u_int = NULL;
	line = token.line;
	column = token.column;
}

Token& Token::operator=(const Token& token)
{
	return (this == &token) ? *this : *this = Token(token);
}

Token& Token::operator=(Token&& token) noexcept
{
	std::swap(tag, token.tag);
	std::swap(u_int, token.u_int);
	line = token.line;
	column = token.column;
	return *this;
}

Token::~Token()
{
	switch (tag) {
	case Token::STRING:		if (u_string != nullptr) { u_string->owners--;	if (u_string->owners == 0)	delete u_string; }		break;
	case Token::ARRAY:		if (u_array != nullptr) { u_array->owners--;	if (u_array->owners == 0)	delete u_array; }		break;
	case Token::IDENTIFIER:	if (u_identifier != nullptr)													delete[] u_identifier;	break;
	default:																														break;
	}
}

bool Token::as_bool() const
{
	switch (tag) {
	case Token::INT:		return u_int != LANGUAGE_ZERO_INT;
	case Token::FLOAT:		return u_float != LANGUAGE_ZERO_FLOAT;
	case Token::STRING:		return strlen(u_string->string_get());
	case Token::ARRAY:		return u_array->array.size();
	case Token::FUNCTION:	return u_function->loaded;
	case Token::BUILTIN:	return true;
	}
	return false;
}

void Token::print() const
{
	switch (tag) {
	case Token::NONE:		printf("NONE");																										break;
	case Token::INT:		printf("%" fINT_TL, u_int);																							break;
	case Token::FLOAT:		printf("%f", u_float);																								break;
	case Token::STRING:		printf("\"%s\"", u_string->string_get());																			break;
	case Token::ARRAY:		printf("[ "); for (const auto& element : u_array->array) { element.print(); printf(", "); } printf(" ]");			break;
	case Token::FUNCTION:	printf("<FUNCTION'%s'>", u_function->loaded ? (u_function->name ? u_function->name : "ANONYMOUS") : "UNLOADED");	break;
	case Token::BUILTIN:	printf("<BUILTIN'%p'>", u_builtin);																					break;
	default:				printf("<NOT_A_VALUE>");																							break;
	}
}

void Token::info() const
{
	// https://stackoverflow.com/a/63689821
	static const int tag_name_width = strlen(std::max_element(std::begin(LANGUAGE_TOKEN_TAG), std::end(LANGUAGE_TOKEN_TAG), [](const RegisteredSequence& left, const RegisteredSequence& right) { return strlen(left.name) < strlen(right.name); })->name);

	printf("%03" fLIN ", %03" fCOL " ", line, column);

	const RegisteredSequence* iter = tag_id(tag);

	if (!iter) {
		printf("INVALID_TAG(%hhd)", tag);
	}
	else if (tag < Token::INNER_END) {
		printf("[ %-*s ] ", tag_name_width, iter->name);

		if (tag < Token::VALUE_END)			print();
		else if (tag == Token::VARIABLE
			|| tag == Token::REFERENCE)		printf("'%s'", variable_name(u_int));
		else if (tag == Token::IDENTIFIER)	printf("'%s'", u_identifier);
		else								printf("%" fINT_TL, u_int);
	}
	else {
		printf("[ %-*s ] %s", tag_name_width, iter->name, iter->sequence);
	}

	printf("\n");
}

const RegisteredSequence* tag_id(const tok_tag tag)
{
	return (tag < Token::TAG_BEGIN || Token::TAG_END <= tag) ?
		nullptr :
		(
			(tag < Token::KEYWORD_END) ?
			&LANGUAGE_TOKEN_TAG[tag] :
			std::find_if(
				std::begin(LANGUAGE_TOKEN_TAG) + Token::SYMBOL_BEGIN,
				std::begin(LANGUAGE_TOKEN_TAG) + Token::SYMBOL_END,
				[tag](const RegisteredSequence& element) { return element.tag == tag; }
			)
			);
}

const char* tag_name(tok_tag tag)
{
	const auto iter = tag_id(tag);
	return iter ? iter->name : nullptr;
}

const char* variable_name(int_tL id)
{
	if (id <= 0 || NAME_TABLE.size() < id)
		return nullptr;
	return std::find_if(NAME_TABLE.begin(), NAME_TABLE.end(), [id](const std::pair<const char*, const int_tL>& element) { return element.second == id; })->first; // TODO: Probably have an adjacent std::vector<const char*> to reversely index the name from its index.
}

#ifdef file_name
#undef file_name
#endif // file_name
#define file_name() state.pFunction->source->name.c_str()

Token* GET_VARIABLE_VALUE(Token& variable, Execution_tL& state)
{
	const VALUE_TABLE_TYPE::iterator& local = state.LOCALS.find(variable.u_int);
	if (local != state.LOCALS.end()) {
		variable = local->second;
		return (variable.tag != Token::FUNCTION || variable.u_function->loaded) ? &local->second : nullptr;
	}

	const VALUE_TABLE_TYPE::iterator& global = VALUE_TABLE.find(variable.u_int);
	if (global != VALUE_TABLE.end()) {
		variable = global->second;
		return (variable.tag != Token::FUNCTION || variable.u_function->loaded) ? &global->second : nullptr;
	}

	interpreterError(file_name(), variable.line, variable.column, ERROR_MESSAGES[2], variable_name(variable.u_int));
	return nullptr;
}

Token* GET_VARIABLE_VALUE_GLOBAL(Token& variable, Execution_tL& state)
{
	// Global scope File Functions should ALWAYS operate only on the GLOBAL VALUE_TABLE.
	const VALUE_TABLE_TYPE::iterator& global = VALUE_TABLE.find(variable.u_int);
	if (global != VALUE_TABLE.end()) {
		variable = global->second;
		return (variable.tag != Token::FUNCTION || variable.u_function->loaded) ? &global->second : nullptr; // Check unloaded function.
	}

	interpreterError(file_name(), variable.line, variable.column, ERROR_MESSAGES[2], variable_name(variable.u_int));
	return nullptr;
}

VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE(Execution_tL& state)
{
	return state.LOCALS;
}

VALUE_TABLE_TYPE& GET_ASSIGNMENT_TABLE_GLOBAL(Execution_tL& state)
{
	return VALUE_TABLE;
}

#define tagCOUPLE(l, r) (((l) << 8) | (r))

SOLVE_RESULT script_run(Thread_tL& thread)
{
	while (!thread.executing.empty()) {
		Execution_tL& state = thread.executing.back();

		while (true)
		{
			if (!state.pFunction->loaded) {
				interpreterError(file_name(), 0, 0, ERROR_MESSAGES[11]);
				return SOLVE_ERROR;
			}
			if (state.pFunction->program.size() <= state.program_counter)
				break;
			Token token = state.pFunction->program[state.program_counter];
			state.program_counter++;

			switch (token.tag)
			{
			case Token::VARIABLE:
				if (!state.GET_VARIABLE_VALUE_(token, state)) return SOLVE_ERROR;
				//case Token::NONE:
			case Token::INT:
			case Token::FLOAT:
			case Token::STRING:
			case Token::ARRAY:
			case Token::REFERENCE:
				state.solution.push_back(std::move(token));
				break;

			case Token::FUNCTION:
				if (token.u_function->name != nullptr) { // Global function.
					VALUE_TABLE.insert_or_assign(token.u_function->variable_id, token);
					break;
				}
				state.solution.push_back(std::move(token));
				break;

			case Token::INDEX:
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				if (state.solution.back().tag != Token::INT) {
					interpreterError(file_name(), state.solution.back().line, state.solution.back().column, ERROR_MESSAGES[7], tag_name(Token::INDEX), tag_name(Token::INT), tag_name(state.solution.back().tag));
					return SOLVE_ERROR;
				}

				if (state.solution[state.solution.size() - 2].tag == Token::REFERENCE || state.solution[state.solution.size() - 2].tag == Token::INDEX) {
					state.solution.back().tag = Token::INDEX; // Merge the INDEX tag into the u_int index value.
				}
				else {
					Token index = std::move(state.solution.back());
					state.solution.pop_back();

					Token& arg = state.solution.back();
					if (arg.tag == Token::ARRAY) {
						if (index.u_int < 0 || arg.u_array->array.size() <= index.u_int) {
							interpreterError(file_name(), arg.line, arg.column, ERROR_MESSAGES[8], tag_name(Token::INDEX), index.u_int, tag_name(arg.tag));
							return SOLVE_ERROR;
						}
						arg = arg.u_array->array[index.u_int];
					}
					else if (arg.tag == Token::STRING) {
						if (index.u_int < 0 || strlen(arg.u_string->string_get()) <= index.u_int) {
							interpreterError(file_name(), arg.line, arg.column, ERROR_MESSAGES[8], tag_name(Token::INDEX), index.u_int, tag_name(arg.tag));
							return SOLVE_ERROR;
						}
						if (strlen(arg.u_string->string_get()) != 1) {
							char cc[2]{ arg.u_string->string_get()[index.u_int], '\0' };
							arg = Token(arg.line, arg.column, String_tL::init(cc, 1));
						}
					}
					else {
						interpreterError(file_name(), arg.line, arg.column, ERROR_MESSAGES[5], tag_name(Token::INDEX), tag_name(arg.tag));
						return SOLVE_ERROR;
					}
				}

				break;

			case Token::SEQUENCE:
				token.u_int = state.lastSequence;
				state.lastSequence = state.solution.size();
				state.solution.push_back(std::move(token));
				break;

			case Token::UNARY_FLIP:
			{
				if (state.solution.size() < 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}

				Token& arg = state.solution.back();
				if (arg.tag == Token::INT) {
					arg.u_int = ~arg.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[5], tag_name(token.tag), tag_name(arg.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::UNARY_NEGATION:
			{
				if (state.solution.size() < 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}

				Token& arg = state.solution.back();
				if (arg.tag == Token::INT) {
					arg.u_int = !arg.u_int;
				}
				else if (arg.tag == Token::FLOAT) {
					arg.u_float = !arg.u_float;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[5], tag_name(token.tag), tag_name(arg.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::UNARY_POSITIVE:
			{
				if (state.solution.size() < 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}

				const Token& arg = state.solution.back();
				if (arg.tag != Token::INT && arg.tag != Token::FLOAT) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[5], tag_name(token.tag), tag_name(arg.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::UNARY_NEGATIVE:
			{
				if (state.solution.size() < 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}

				Token& arg = state.solution.back();
				if (arg.tag == Token::INT) {
					arg.u_int = -arg.u_int;
				}
				else if (arg.tag == Token::FLOAT) {
					arg.u_float = -arg.u_float;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[5], tag_name(token.tag), tag_name(arg.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_ADD:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int += right.u_int;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(left.line, left.column, (float_tL)left.u_int + right.u_float); // Constructor + Move Assignment + Destructor. // TODO: Bypass by assigning tag and value?
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.u_float += (float_tL)right.u_int;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.u_float += right.u_float;
					break;
				case tagCOUPLE(Token::INT, Token::STRING):
				{
					const size_t l0 = intlen(left.u_int);
					const size_t l1 = strlen(right.u_string->string_get());
					left = Token(left.line, left.column, String_tL_init(left.u_int, l0, right.u_string->string_get(), l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::INT):
				{
					const size_t l0 = strlen(left.u_string->string_get());
					const size_t l1 = intlen(right.u_int);
					left = Token(left.line, left.column, String_tL_init(left.u_string->string_get(), l0, right.u_int, l1)); // This must trigger constructor on left so its str_tok gets decremented!
				}
				break;
				case tagCOUPLE(Token::FLOAT, Token::STRING):
				{
					const size_t l0 = snprintf(NULL, 0, "%f", left.u_float);
					const size_t l1 = strlen(right.u_string->string_get());
					left = Token(left.line, left.column, String_tL_init(left.u_float, l0, right.u_string->string_get(), l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::FLOAT):
				{
					const size_t l0 = strlen(left.u_string->string_get());
					const size_t l1 = snprintf(NULL, 0, "%f", right.u_float);
					left = Token(left.line, left.column, String_tL_init(left.u_string->string_get(), l0, right.u_float, l1));
				}
				break;
				case tagCOUPLE(Token::STRING, Token::STRING):
				{
					const size_t l0 = strlen(left.u_string->string_get());
					const size_t l1 = strlen(right.u_string->string_get());
					left = Token(left.line, left.column, String_tL_init(left.u_string->string_get(), l0, right.u_string->string_get(), l1));
				}
				break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_SUBSTRACT:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int -= right.u_int;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(left.line, left.column, (float_tL)left.u_int - right.u_float);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.u_float -= (float_tL)right.u_int;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.u_float -= right.u_float;
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_MULTIPLY:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int *= right.u_int;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left = Token(left.line, left.column, (float_tL)left.u_int * right.u_float);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left.u_float *= (float_tL)right.u_int;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left.u_float *= right.u_float;
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;
			case Token::BINARY_DIVIDE:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					if (right.u_int == LANGUAGE_ZERO_INT) {
						interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[1]);
						return SOLVE_ERROR;
					}
					left.u_int /= right.u_int;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					if (right.u_float == LANGUAGE_ZERO_FLOAT) {
						interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[1]);
						return SOLVE_ERROR;
					}
					left = Token(left.line, left.column, (float_tL)left.u_int / right.u_float);
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					if (right.u_int == LANGUAGE_ZERO_INT) {
						interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[1]);
						return SOLVE_ERROR;
					}
					left.u_float /= (float_tL)right.u_int;
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					if (right.u_float == LANGUAGE_ZERO_FLOAT) {
						interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[1]);
						return SOLVE_ERROR;
					}
					left.u_float /= right.u_float;
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_MODULUS:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					if (right.u_int == LANGUAGE_ZERO_INT) {
						interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[1]);
						return SOLVE_ERROR;
					}
					left.u_int %= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_SHIFT_LEFT:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.u_int <<= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_SHIFT_RIGHT:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.u_int >>= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_AND_BITWISE:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.u_int &= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_OR_BITWISE:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.u_int |= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_OR_EXCLUSIVE:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				if (left.tag == Token::INT && right.tag == Token::INT) {
					left.u_int ^= right.u_int;
				}
				else {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_AND:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();
				if (left.tag <= Token::NONE || Token::BUILTIN < left.tag ||
					right.tag <= Token::NONE || Token::BUILTIN < right.tag) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
				left = Token(token.line, token.column, left.as_bool() && right.as_bool() ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
			}
			break;

			case Token::BINARY_OR:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();
				if (left.tag <= Token::NONE || Token::BUILTIN < left.tag ||
					right.tag <= Token::NONE || Token::BUILTIN < right.tag) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
				left = Token(token.line, token.column, left.as_bool() || right.as_bool() ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
			}
			break;

			case Token::BINARY_LESSER:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int < right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int < right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float < (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float < right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_GREATER:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int > right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int > right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float > (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float > right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_LESSER_EQUAL:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int <= right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int <= right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float <= (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float <= right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_GREATER_EQUAL:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int >= right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int >= right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float >= (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float >= right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_EQUAL_DOUBLE:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int == right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int == right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float == (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float == right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_EQUAL_NOT:
			{
				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token right = std::move(state.solution.back());
				state.solution.pop_back();

				Token& left = state.solution.back();

				switch (tagCOUPLE(left.tag, right.tag))
				{
				case tagCOUPLE(Token::INT, Token::INT):
					left.u_int = (left.u_int != right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::INT, Token::FLOAT):
					left.u_int = ((float_tL)left.u_int != right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT;
					break;
				case tagCOUPLE(Token::FLOAT, Token::INT):
					left = Token(left.line, left.column, (left.u_float != (float_tL)right.u_int) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				case tagCOUPLE(Token::FLOAT, Token::FLOAT):
					left = Token(left.line, left.column, (left.u_float != right.u_float) ? LANGUAGE_TRUE_INT : LANGUAGE_FALSE_INT);
					break;
				default:
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[6], tag_name(token.tag), tag_name(left.tag), tag_name(right.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::BINARY_EQUAL:
			{
				// Resolve left-hand variable reference for assignment.
				std::vector<int_tL> indices;
				indices.reserve((!state.solution.empty() && state.solution.back().tag == Token::INDEX) ? state.solution.size() / 2 : 0);
				while (!state.solution.empty() && state.solution.back().tag == Token::INDEX) {
					Token& index = state.solution.back();
					indices.push_back(index.u_int);
					state.solution.pop_back();
				}

				if (state.solution.size() < 2) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(2), state.solution.size());
					return SOLVE_ERROR;
				}

				Token left = std::move(state.solution.back());
				state.solution.pop_back();
				if (left.tag != Token::REFERENCE) {
					interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[7], tag_name(token.tag), tag_name(Token::REFERENCE), tag_name(left.tag));
					return SOLVE_ERROR;
				}

				void* ref = indices.size() ? state.GET_VARIABLE_VALUE_(left, state) : &left;
				if (!ref) return SOLVE_ERROR;
				enum : tok_tag {
					POINTER_CHAR = -1,
					POINTER_ARRAY = Token::ARRAY,
					POINTER_STRING = Token::STRING,
					POINTER_REFERENCE = Token::REFERENCE,
				};
				tok_tag ref_type = static_cast<Token*>(ref)->tag;
				for (int i = indices.size() - 1; i >= 0; i--) {
					if (ref_type == POINTER_ARRAY) {
						if (indices[i] < 0 || static_cast<Token*>(ref)->u_array->array.size() <= indices[i]) {
							interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[8], tag_name(Token::INDEX), indices[i], tag_name(ref_type));
							return SOLVE_ERROR;
						}
						ref = &static_cast<Token*>(ref)->u_array->array[indices[i]];
						ref_type = static_cast<Token*>(ref)->tag;
					}
					else if (ref_type == POINTER_STRING) {
						if (indices[i] < 0 || strlen(static_cast<Token*>(ref)->u_string->string_get()) <= indices[i]) {
							interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[8], tag_name(Token::INDEX), indices[i], tag_name(ref_type));
							return SOLVE_ERROR;
						}
						if (!static_cast<Token*>(ref)->u_string->owned) {
							interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[14], tag_name(POINTER_STRING));
							return SOLVE_ERROR;
						}
						ref = &static_cast<Token*>(ref)->u_string->string_get()[indices[i]];
						ref_type = POINTER_CHAR;
					}
					else if (ref_type == POINTER_CHAR) {
						if (indices[i] != 0) {
							interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[8], tag_name(Token::INDEX), indices[i], tag_name(Token::STRING));
							return SOLVE_ERROR;
						}
					}
					else {
						interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[5], tag_name(Token::INDEX), tag_name(ref_type));
						return SOLVE_ERROR;
					}
				}

				Token& right = state.solution.back();

				if (ref_type == POINTER_REFERENCE) {
					state.GET_ASSIGNMENT_TABLE_(state).insert_or_assign(left.u_int, right); // There can never be a REFERENCE inside an ARRAY.
				}
				else if (ref_type == POINTER_CHAR) {
					if (right.tag != Token::STRING || strlen(right.u_string->string_get()) != 1) {
						interpreterError(file_name(), left.line, left.column, ERROR_MESSAGES[9], tag_name(Token::STRING), tag_name(Token::STRING));
						return SOLVE_ERROR;
					}
					*static_cast<char*>(ref) = right.u_string->string_get()[0];
				}
				else {
					*static_cast<Token*>(ref) = right;
				}
				//left = std::move(right);
			}
			break;

			case Token::ARRAY_INIT:
			{
				if (state.solution.size() <= state.lastSequence || state.solution[state.lastSequence].tag != Token::SEQUENCE) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[4], tag_name(token.tag), tag_name(Token::SEQUENCE));
					return SOLVE_ERROR;
				}

				Token arr = Token(token.line, token.column, new Array_tL{}); // Build Array.
				Array_tL* v = arr.u_array;
				v->array.insert(v->array.end(), std::make_move_iterator(state.solution.begin() + state.lastSequence + 1), std::make_move_iterator(state.solution.end()));

				size_t lastSequence = state.lastSequence;
				state.lastSequence = state.solution[state.lastSequence].u_int;
				state.solution.erase(state.solution.begin() + lastSequence, state.solution.end());

				state.solution.push_back(std::move(arr));
			}
			break;

			case Token::CALL:
			{
				if (state.lastSequence < 1 || state.solution.size() <= state.lastSequence || state.solution[state.lastSequence].tag != Token::SEQUENCE) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[4], tag_name(token.tag), tag_name(Token::SEQUENCE));
					return SOLVE_ERROR;
				}

				const size_t nArgs = state.solution.size() - state.lastSequence - 1;
				const size_t sArgs = state.lastSequence;

				Token calling = std::move(state.solution[state.lastSequence - 1]);

				if (calling.tag == Token::BUILTIN) {
					std::vector<Token> arguments;
					arguments.reserve(nArgs);
					arguments.insert(arguments.end(), std::make_move_iterator(state.solution.begin() + state.lastSequence + 1), std::make_move_iterator(state.solution.end()));

					Builtin_tL* builtin = calling.u_builtin;
					state.lastSequence = state.solution[state.lastSequence].u_int;
					state.solution.erase(state.solution.begin() + sArgs - 1, state.solution.end()); // Take the function, sequence and its arguments out of the solution stack.
#ifdef FUNCTION_RETURN_SINGLE
					size_t stack_size = state.solution.size();
#endif // FUNCTION_RETURN_SINGLE
					SOLVE_RESULT answer = builtin(arguments, state.solution, &thread);
#ifdef FUNCTION_RETURN_SINGLE
					if (answer == SOLVE_ERROR) return answer;
					if (state.solution.size() == stack_size) {
						state.solution.emplace_back(token.line, token.column, LANGUAGE_TRUE_INT);
					}
					else if (state.solution.size() != stack_size + 1) {
						interpreterError(file_name(), calling.line, calling.column, ERROR_MESSAGES[3], tag_name(calling.tag), LANGUAGE_INT(1), state.solution.size() - stack_size);
						return SOLVE_ERROR;
					}
#endif // FUNCTION_RETURN_SINGLE
					if (answer != SOLVE_OK) {
						return answer;
					}
				}
				else if (calling.tag == Token::FUNCTION) {
					Function_tL* func = calling.u_function;
					if (!func->loaded) {
						interpreterError(file_name(), calling.line, calling.column, ERROR_MESSAGES[11]);
						return SOLVE_ERROR;
					}
					if (func->arg_id.size() != nArgs) {
						interpreterError(file_name(), calling.line, calling.column, ERROR_MESSAGES[3], tag_name(calling.tag), (int_tL)func->arg_id.size(), (size_t)nArgs);
						return SOLVE_ERROR;
					}
					Execution_tL exe(func);
					for (size_t i = 0, j = state.lastSequence + 1; i < nArgs; i++, j++) {
						state.GET_ASSIGNMENT_TABLE_(exe).insert_or_assign(func->arg_id[i], std::move(state.solution[j]));
					}
					state.lastSequence = state.solution[state.lastSequence].u_int;
					state.solution.erase(state.solution.begin() + sArgs - 1, state.solution.end()); // Take the function, sequence and its arguments out of the solution stack.
					thread.executing.emplace_back(std::move(exe)); // Modifying the thread as the last step since it invalidates references.
					goto execution_end; // https://en.cppreference.com/w/cpp/language/goto
				}
				else {
					interpreterError(file_name(), calling.line, calling.column, ERROR_MESSAGES[10], tag_name(calling.tag));
					return SOLVE_ERROR;
				}
			}
			break;

			case Token::GOTO:
			{
				if (state.solution.size() != 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}
				Token& label_name = state.solution.back();
				if (label_name.tag != Token::STRING) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[7], tag_name(token.tag), tag_name(Token::STRING), tag_name(label_name.tag));
					return SOLVE_ERROR;
				}
				const LABEL_TABLE_TYPE::iterator& label_num = state.pFunction->labels.find(label_name.u_string->string_get());
				if (label_num == state.pFunction->labels.end()) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[12], tag_name(Token::LABEL), label_name.u_string->string_get());
					return SOLVE_ERROR;
				}
				state.program_counter = label_num->second;
				state.solution.clear();
				state.lastSequence = -1;
			}
			break;

			case Token::JUMP:
				state.program_counter = token.u_int;
				state.solution.clear();
				state.lastSequence = -1;
				break;

			case Token::JUMP_ON_FALSE:
			case Token::JUMP_ON_NOT_FALSE:
			{
				if (state.solution.size() != 1) {
					interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[3], tag_name(token.tag), LANGUAGE_INT(1), state.solution.size());
					return SOLVE_ERROR;
				}
				Token& condition = state.solution.back();
				if (condition.as_bool() == (token.tag == Token::JUMP_ON_NOT_FALSE))
					state.program_counter = token.u_int;
				state.solution.clear();
				state.lastSequence = -1;
			}
			break;

			case Token::AWAIT:
				return SOLVE_AWAIT;

			case Token::RETURN:
				goto execution_return;

			default:
				interpreterError(file_name(), token.line, token.column, ERROR_MESSAGES[13], tag_name(token.tag));
				return SOLVE_ERROR;
			}
		}
	execution_return:
		{
			std::vector<Token> solution = std::move(state.solution);
			thread.executing.pop_back();
			if (!thread.executing.empty()) {
#ifdef FUNCTION_RETURN_SINGLE
				if (solution.empty())			thread.executing.back().solution.emplace_back(0, 0, LANGUAGE_TRUE_INT);
				else if (solution.size() == 1)	thread.executing.back().solution.push_back(std::move(solution.back()));
				else							thread.executing.back().solution.emplace_back(0, 0, new Array_tL{ std::move(solution) });
#else
				thread.executing.back().solution.insert(thread.executing.back().solution.end(), std::make_move_iterator(solution.begin()), std::make_move_iterator(solution.end()));
#endif // FUNCTION_RETURN_SINGLE
			}
		}
	execution_end:
		; // Empty statement to allow compilation.
	}

	return SOLVE_OK;
}

Function_tL* script_load(const char* filename)
{
	Function_tL* function = nullptr;

	const char* source = readfile(filename);
	if (source)
	{
		const auto& loaded = LOADED_SOURCEFILE.insert({ filename, { filename } });
		loaded.first->second.unload();

		Parser parser;
		if (tokenize_source(filename, source, parser.tokens))
			function = parser.parse(&loaded.first->second, true);

		delete[] source;

		if (!function)
			loaded.first->second.unload();
	}

	return function;
}

SOLVE_RESULT script_import(const char* filename)
{
	Function_tL* loaded_file = script_load(filename);

	if (loaded_file)
	{
		Thread_tL thread{ { Execution_tL(loaded_file) } };

		return script_run(thread);
	}

	return SOLVE_ERROR;
}

void script_unload(const char* filename)
{
	const auto& loaded = LOADED_SOURCEFILE.find(filename);
	if (loaded != LOADED_SOURCEFILE.end())
		loaded->second.unload();
}

// BUILTIN START

#include <filesystem>
#include <algorithm>

void builtinErrorVA(const char* format, va_list argp)
{
	vprintf(format, argp);
}

void builtinError_(const char* filename, const char* builtin_name, const char* f, ...)
{
	printf(fERROR);
	printf("Builtin <%s> '%s'. ", builtin_name, filename);
	va_list argp;
	va_start(argp, f);
	builtinErrorVA(f, argp);
	va_end(argp);
	printf("\n");
}

#ifdef file_name
#undef file_name
#endif // file_name
#define file_name() thread->executing.back().pFunction->source->name.c_str()

// https://gcc.gnu.org/onlinedocs/cpp/Variadic-Macros.html
#define builtinError(builtin_name, format, ...) builtinError_(file_name(), builtin_name, format, __VA_ARGS__)

#define BUILTIN_DEFINE(name) SOLVE_RESULT name(std::vector<Token>& arguments, std::vector<Token>& solution, Thread_tL* thread)
#define BUILTIN_REGISTER(name) VALUE_TABLE.insert({ NAME_TABLE_id(#name), Token(name) })

BUILTIN_DEFINE(import)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::STRING)
	{
		builtinError("import", "Argument must be a single '%s'.", tag_name(Token::STRING));
		return SOLVE_ERROR;
	}

	std::filesystem::path file_path															// Resulting path.
		= (std::filesystem::path(thread->executing.back().pFunction->source->name)			// Caller file relative path.
			.remove_filename()																// Remove file to get containing folder.
			/ arguments[0].u_string->string_get())											// Concatenate requested resource relative to caller.
		.lexically_normal();																// Resolve directory parenting.

	SOLVE_RESULT solve = script_import(file_path.string().c_str());

	if (solve == SOLVE_ERROR)
		builtinError("import", "Failed to import file '%s'.", file_path.string().c_str());

	solution.emplace_back(0, 0, (int_tL)solve);

	return solve;
}

BUILTIN_DEFINE(load)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::STRING)
	{
		builtinError("load", "Argument must be a single '%s'.", tag_name(Token::STRING));
		return SOLVE_ERROR;
	}

	std::filesystem::path file_path															// Resulting path.
		= (std::filesystem::path(thread->executing.back().pFunction->source->name)			// Caller file relative path.
			.remove_filename()																// Remove file to get containing folder.
			/ arguments[0].u_string->string_get())											// Concatenate requested resource relative to caller.
		.lexically_normal();																// Resolve directory parenting.

	Function_tL* func = script_load(file_path.string().c_str());

	if (!func)
	{
		builtinError("load", "Failed to load file '%s'.", file_path.string().c_str());
		return SOLVE_ERROR;
	}

	solution.emplace_back(0, 0, func);

	return SOLVE_OK;
}

BUILTIN_DEFINE(version)
{
	if (!arguments.empty())
	{
		builtinError("version", "This function takes no arguments.");
		return SOLVE_ERROR;
	}

	solution.emplace_back(0, 0, String_tL_external(VERSION));
	return SOLVE_OK;
}

BUILTIN_DEFINE(print)
{
	if (arguments.size() != 1)
	{
		builtinError("print", "This function takes a single argument.");
		return SOLVE_ERROR;
	}

	arguments[0].print();
	printf("\n");

	return SOLVE_OK;
}

BUILTIN_DEFINE(max)
{
	if (arguments.size() != 1 || arguments[0].tag != Token::ARRAY || arguments[0].u_array->array.empty())
	{
		builtinError("max", "Argument must be a single non-empty '%s'.", tag_name(Token::ARRAY));
		return SOLVE_ERROR;
	}

	if (std::find_if(
		arguments[0].u_array->array.begin(),
		arguments[0].u_array->array.end(),
		[](const Token& element) { return element.tag != Token::INT && element.tag != Token::FLOAT; }) ==
		arguments[0].u_array->array.end())
	{
		solution.push_back(
			std::move(
				*std::max_element(
					arguments[0].u_array->array.begin(),
					arguments[0].u_array->array.end(),
					[](const Token& left, const Token& right) {
						return ((left.tag == Token::INT) ? left.u_int : left.u_float) < ((right.tag == Token::INT) ? right.u_int : right.u_float);
					}
				)
			)
		);

		return SOLVE_OK;
	}

	builtinError("max", "'%s' must contain only '%s' and '%s'.", tag_name(Token::ARRAY), tag_name(Token::INT), tag_name(Token::FLOAT));
	return SOLVE_ERROR;
}

int_tL NAME_TABLE_id(const char* name)
{
	char* key = (char*)name;
	if (!NAME_TABLE.count(name)) {
		const size_t len = strlen(name) + 1;
		key = new char[len];
		std::memcpy(key, name, len);
	}
	return NAME_TABLE.insert({ key, NAME_TABLE.size() + 1 }).first->second;
}

int register_function()
{
	BUILTIN_REGISTER(import);
	BUILTIN_REGISTER(load);
	BUILTIN_REGISTER(version);
	BUILTIN_REGISTER(print);
	BUILTIN_REGISTER(max);

	return 0;
}

// BUILTIN END

int_tL LANGUAGE_initialize()
{
	register_function();

	return 1;
}

int_tL LANGUAGE_terminate()
{
	// Terminate all Thread_tL.

	LOADED_SOURCEFILE.clear();
	for (auto& variable : NAME_TABLE) delete[] variable.first;
	NAME_TABLE.clear();
	VALUE_TABLE.clear();

	return 1;
}

int_tL LANGUAGE_reload()
{
	LANGUAGE_terminate();
	LANGUAGE_initialize();

	return 1;
}

int main()
{
	LANGUAGE_initialize();

	SOLVE_RESULT result = script_import(std::filesystem::relative(__FILE__ "/../example/import.txt", std::filesystem::current_path()).string().c_str());

	if (result == SOLVE_OK)
	{
	}

	LANGUAGE_terminate();

	return 0;
}

// =================== INTERPRETER.CPP END ===================
#endif // LANGUAGE_IMPLEMENTATION

#endif // !H_INTERPRETER
