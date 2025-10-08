#ifndef H_INTERPRETER
#define H_INTERPRETER

#include <unordered_map>
#include <vector>
#include "common.h"

enum Operator : s_lang
{
	OPERATOR_NONE = LANGUAGE_OPERATOR,
	OPERATOR_FLIP,
	OPERATOR_NEGATION,
	OPERATOR_PLUS,
	OPERATOR_MINUS,
	OPERATOR_MULTIPLY,
	OPERATOR_DIVIDE,
	OPERATOR_REMAINDER,
	OPERATOR_SHIFT_LEFT,
	OPERATOR_SHIFT_RIGHT,

	// TODO:
	//OPERATOR_OR,
	//OPERATOR_AND,
	// 
	//OPERATOR_XOR,
	//OPERATOR_BOR, // bitwise
	//OPERATOR_BAND, // bitwise

	OPERATOR_LESSER,
	OPERATOR_GREATER,
	OPERATOR_LESSER_EQUAL,
	OPERATOR_GREATER_EQUAL,
	OPERATOR_EQUAL_DOUBLE,
	OPERATOR_EQUAL_NOT,
	OPERATOR_EQUAL,
};

enum LANGUAGEERROR : s_lang
{
	ERROR_NONE = LANGUAGE_ERROR,
	ERROR_BUFFER_INVALID,
	ERROR_BUFFER_OVERFLOW,
	ERROR_TOKEN_EMPTY,
	ERROR_TOKEN_UNDERFLOW,
	ERROR_TOKEN_OVERFLOW,
	ERROR_TOKEN_END,
	ERROR_SYNTAX_ERROR,
	ERROR_MATH_ERROR,
	ERROR_PARSE_ERROR,
	ERROR_EVALUATION_ERROR,
	ERROR_VARIABLE_UNINITIALIZED,
	ERROR_VARIABLE_MISMATCH,
	ERROR_TYPE_MISMATCH,
	ERROR_TYPE_INVALID,
	ERROR_ARGUMENT_MISMATCH,
	ERROR_DELIMITER_MISMATCH,
	ERROR_INVALID_INDEX,
	ERROR_RETURN,
	ERROR_BREAK,
	ERROR_CONTINUE,
};

enum EXPRESSION_TYPE : s_lang
{
	EXPRESSION_NONE = LANGUAGE_EXPRESSION,

	EXPRESSION_VALUE = LANGUAGE_EXPRESSION | 0x00000001,
	EXPRESSION_INDEX = LANGUAGE_EXPRESSION | 0x00000002,
	EXPRESSION_VARIABLE = LANGUAGE_EXPRESSION | 0x00000004,
	EXPRESSION_FUNCTIONCALL = LANGUAGE_EXPRESSION | 0x00000008,
	EXPRESSION_UNARY = LANGUAGE_EXPRESSION | 0x00000010,
	EXPRESSION_BINARY = LANGUAGE_EXPRESSION | 0x00000020,
	EXPRESSION_TERNARY = LANGUAGE_EXPRESSION | 0x00000040,

	EXPRESSION_LOOP_FOR = LANGUAGE_EXPRESSION | 0x00000080,
	EXPRESSION_LOOP_WHILE = LANGUAGE_EXPRESSION | 0x00000100,

	EXPRESSION_IFELSE = LANGUAGE_EXPRESSION | 0x00000200,

	EXPRESSION_BREAK = LANGUAGE_EXPRESSION | 0x00000400,
	EXPRESSION_CONTINUE = LANGUAGE_EXPRESSION | 0x00000800,
	EXPRESSION_RETURN = LANGUAGE_EXPRESSION | 0x00001000,

	EXPRESSION_FUNCTION = LANGUAGE_EXPRESSION | 0x00002000,

	EXPRESSION_ARRAY = LANGUAGE_EXPRESSION | 0x00004000,

	EXPRESSION_BUILTIN = LANGUAGE_EXPRESSION | 0x00008000,

	EXPRESSION_OPERATION = EXPRESSION_UNARY | EXPRESSION_BINARY | EXPRESSION_TERNARY | EXPRESSION_VALUE | EXPRESSION_VARIABLE | EXPRESSION_ARRAY | EXPRESSION_FUNCTIONCALL | EXPRESSION_FUNCTION | EXPRESSION_INDEX,
	EXPRESSION_LOOP = EXPRESSION_LOOP_FOR | EXPRESSION_LOOP_WHILE,
	EXPRESSION_INTERRUPT = EXPRESSION_BREAK | EXPRESSION_CONTINUE | EXPRESSION_RETURN,
	EXPRESSION_CALLABLE = EXPRESSION_FUNCTION | EXPRESSION_BUILTIN,
	EXPRESSION_ASSIGNABLE = EXPRESSION_VALUE | EXPRESSION_CALLABLE,
	EXPRESSION_CONSTRUCT = EXPRESSION_LOOP | EXPRESSION_FUNCTION | EXPRESSION_IFELSE,

	EXPRESSION_ALL = LANGUAGE_EXPRESSION | LANGUAGE_MASK,
};

enum VALUE_TYPE : s_lang
{
	VALUE_NULL = LANGUAGE_VALUE,
	VALUE_BOOL = 0x01 | LANGUAGE_VALUE,
	VALUE_INT = 0x02 | LANGUAGE_VALUE,
	VALUE_FLOAT = 0x04 | LANGUAGE_VALUE,
	VALUE_STRING = 0x08 | LANGUAGE_VALUE,

	VALUE_NUMBER = VALUE_INT | VALUE_FLOAT,
};

typedef char* t_value;

#define VALUE_ZERO nullptr

#define LANGUAGE_TRUE 1
#define LANGUAGE_FALSE 0
#define LANGUAGE_NULL 0

#define lang2Int(val) ((intptr_t)val)
#define lang2Bool(val) (val != VALUE_ZERO ? true : false)
float lang2Float(t_value val);
#define lang2char(val) ((const char*)val)

#define int2lang(val) (static_cast<t_value>(val))
#define bool2lang(val) (static_cast<t_value>(val ? LANGUAGE_TRUE : LANGUAGE_FALSE))
t_value float2lang(float val);
#define char2lang(val) (static_cast<t_value>(val))

enum RUNTIME_RESULT : s_lang
{
	RUNTIME_ERROR = ERROR_EVALUATION_ERROR,
	RUNTIME_SUCCESS = ERROR_NONE,
	RUNTIME_RETURN = ERROR_RETURN,
	RUNTIME_BREAK = ERROR_BREAK,
	RUNTIME_CONTINUE = ERROR_CONTINUE,
};

enum RESULT_TYPE : s_lang
{
	RESULT_NONE = VALUE_NULL,
	RESULT_BOOL = VALUE_BOOL,
	RESULT_INT = VALUE_INT,
	RESULT_FLOAT = VALUE_FLOAT,
	RESULT_STRING = VALUE_STRING,
	RESULT_ARRAY = EXPRESSION_ARRAY,
	RESULT_FUNCTION = EXPRESSION_FUNCTION,
	RESULT_ERROR = LANGUAGE_ERROR,
};

struct Result
{
public:
	s_lang type_;
	t_value value;
};

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

extern UMAP_kpCHAR(const char*, Result) VALUE_TABLE;


class Expression
{
public:
	s_lang exp;

public:
	Expression();
	Expression(s_lang type_);

public:
	virtual s_lang evaluate(Result& result) = 0;
	//virtual Expression* reduce() = 0;
};

class Value : public Expression
{
public:
	s_lang type_;
	t_value value;

public:
	Value();
	Value(s_lang aType, t_value aValue);
	~Value();

public:
	s_lang evaluate(Result& result) override;

public:
	intptr_t asBool();
	intptr_t asInt();
	float asFloat();
	const char* asStr();
};

extern const Value VALUE_BOOL_TRUE;
extern const Value VALUE_BOOL_FALSE;
extern const Value VALUE_NULL_NULL;

class Index : public Expression
{
public:
	Expression* expr;
	Expression* index;

public:
	Index();
	Index(Expression* e, Expression* i);

public:
	s_lang evaluate(Result& result) override;
	s_lang indexParse(int& n);
};

class Variable : public Expression
{
public:
	char* name;

public:
	Variable(char* n);

public:
	s_lang evaluate(Result& result) override;
};

class Array : public Expression
{
public:
	std::vector<Expression*> el;

public:
	Array();

public:
	s_lang evaluate(Result& result) override;
};

struct ConditionalBranch
{
public:
	Expression* condition;
	std::vector<Expression*> instructions;

public:
	ConditionalBranch();
};

class Conditional : public Expression
{
public:
	std::vector<ConditionalBranch> branches;

public:
	Conditional();

public:
	s_lang evaluate(Result& result) override;
};

#define MAX_ITER 30

class WhileLoop : public Expression
{
public:
	Expression* condition;
	std::vector<Expression*> instructions;

public:
	WhileLoop();

public:
	s_lang evaluate(Result& result);
};

class ForLoop : public Expression
{
public:
	Expression* guard[3];
	std::vector<Expression*> instructions;

public:
	ForLoop();

public:
	s_lang evaluate(Result& result);
};

class FunctionCall : public Expression
{
public:
	char* name;
	std::vector<Expression*> args;

public:
	FunctionCall(char* n);

public:
	s_lang evaluate(Result& result) override;
};

class Callable : public Expression
{
public:
	Callable();
	Callable(s_lang type_);

public:
	virtual s_lang evaluate(Result& result) = 0;

public:
	virtual s_lang call(Result& result, std::vector<Expression*>& args) = 0;
};

class BuiltIn : public Callable
{
public:
	const char* name;
	s_lang(*func)(Result&, std::vector<Result>&);

public:
	BuiltIn(const char* n, s_lang(*f)(Result&, std::vector<Result>&));

public:
	s_lang evaluate(Result& result) override;

public:
	s_lang call(Result& result, std::vector<Expression*>& args) override;
};

class Function : public Callable
{
public:
	char* name;
	std::vector<char*> argnames;
	std::vector<Expression*> instructions;

public:
	Function(char* n);

public:
	s_lang evaluate(Result& result) override;

public:
	s_lang call(Result& result, std::vector<Expression*>& args) override;
};

class Return : public Expression
{
public:
	Expression* val;

public:
	Return(Expression* v);

public:
	s_lang evaluate(Result& result) override;
};

class Break : public Expression
{
public:
	s_lang evaluate(Result& result) override;
};

class Continue : public Expression
{
public:
	s_lang evaluate(Result& result) override;
};

class Unary : public Expression
{
public:
	s_lang op;
	Expression* child;

public:
	Unary(s_lang o, Expression* p);
	~Unary();

public:
	s_lang evaluate(Result& result) override;
};

class Binary : public Expression
{
public:
	s_lang op;
	Expression* left;
	Expression* right;

public:
	Binary(s_lang o, Expression* l, Expression* r);
	~Binary();

public:
	s_lang evaluate(Result& result) override;
};

#endif // !H_INTERPRETER
