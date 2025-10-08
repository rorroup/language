#include "Interpreter.h"

static const Value VALUE_BOOL_TRUE(VALUE_BOOL, (t_value)(LANGUAGE_TRUE));
static const Value VALUE_BOOL_FALSE(VALUE_BOOL, (t_value)(LANGUAGE_FALSE));
static const Value VALUE_NULL_NULL(VALUE_NULL, (t_value)(LANGUAGE_NULL));

size_t kpCHAR_HASH::operator()(const char* k) const
{
	size_t hash = -1;
	static const int s = (sizeof(size_t) - sizeof(char)) * 8; // TODO: + 1?
	int i = 0;
	while (k[i] != '\0') {
		hash = (size_t)k[i] ^ (hash << (i % s));
		i++;
	}
	return hash;
}

bool kpCHAR_EQUAL::operator()(const char* lhs, const char* rhs) const
{
	if (lhs == nullptr || rhs == nullptr) {
		return false;
	}
	return strlen(lhs) == strlen(rhs) && strcmp(lhs, rhs) == 0;
}

UMAP_kpCHAR(const char*, Result) VALUE_TABLE;

float lang2Float(t_value val)
{
	float f;
	memcpy(&f, &val, sizeof(float));
	return f;
}

t_value float2lang(float val)
{
	t_value f;
	memset(&f, 0, sizeof(t_value));
	memcpy(&f, &val, sizeof(float));
	return f;
}

Expression::Expression()
{
	exp = EXPRESSION_NONE;
}

Expression::Expression(s_lang e)
{
	exp = e;
}

Value::Value() : Expression(EXPRESSION_VALUE)
{
	type_ = VALUE_NULL;
	value = LANGUAGE_NULL;
}

Value::Value(s_lang aType, t_value aValue) : Expression(EXPRESSION_VALUE)
{
	type_ = aType;
	value = aValue;
}

Value::~Value()
{
	if (type_ == VALUE_STRING && value != nullptr)
	{
		//delete[] value;
	}
}

s_lang Value::evaluate(Result& result)
{
	result = { type_, value };
	return ERROR_NONE;
}

intptr_t Value::asBool()
{
	switch (type_)
	{
	case VALUE_NULL:
		break;
	case VALUE_BOOL:
		return value == VALUE_ZERO ? (intptr_t)0 : (intptr_t)1;
		break;
	case VALUE_INT:
		return value == VALUE_ZERO ? (intptr_t)0 : (intptr_t)1;
		break;
	case VALUE_FLOAT:
		return value == VALUE_ZERO ? (intptr_t)0 : (intptr_t)1;
		break;
	case VALUE_STRING:
		return strlen((char*)value) == 0 ? (intptr_t)0 : (intptr_t)1;
		break;
		/*case VALUE_ARRAY:
		{
			ValueVectorSub* v = (ValueVectorSub*)(value);
			return (intptr_t)(v->el.size() > 0 ? 1 : 0);
		}
		break;*/
	default:
		break;
	}
	return (intptr_t)0;
}

intptr_t Value::asInt()
{
	switch (type_)
	{
	case VALUE_NULL:
		break;
	case VALUE_BOOL:
		return value == VALUE_ZERO ? (intptr_t)0 : (intptr_t)1;
		break;
	case VALUE_INT:
		return (intptr_t)value;
		break;
	case VALUE_FLOAT:
	{
		float f;
		memcpy(&f, &value, sizeof(float));
		return (intptr_t)f;
	}
	break;
	case VALUE_STRING:
		return (intptr_t)strlen(value);
		break;
		/*case VALUE_ARRAY:
		{
			ValueVectorSub* v = (ValueVectorSub*)(value);
			return (intptr_t)(v->el.size());
		}
			break;*/
	default:
		break;
	}
	return (intptr_t)0;
}

float Value::asFloat()
{
	switch (type_)
	{
	case VALUE_NULL:
		break;
	case VALUE_BOOL:
		return value == VALUE_ZERO ? 0.0f : 1.0f;
		break;
	case VALUE_INT:
		return (float)((intptr_t)value);
		break;
	case VALUE_FLOAT:
	{
		float f;
		memcpy(&f, &value, sizeof(float));
		return f;
	}
	break;
	case VALUE_STRING:
		return (float)strlen((char*)value);
		break;
		/*case VALUE_ARRAY:
		{
			ValueVectorSub* v = (ValueVectorSub*)(value);
			return (float)(v->el.size());
		}
		break;*/
	default:
		break;
	}
	return (intptr_t)0;
}

const char* Value::asStr()
{
	switch (type_)
	{
	case VALUE_NULL:
		break;
	case VALUE_BOOL:
		return value == VALUE_ZERO ? "false" : "true";
		break;
	case VALUE_INT:
	{
		intptr_t n = (intptr_t)value;
		const unsigned char len = 1 + (n == 0 ? 1 : (n > 0 ? log10(n) : 1 + log10(-n))) + 1;
		char* s = new char[len];
		_itoa_s(n, s, len, 10);
		return s;
	}
	break;
	case VALUE_FLOAT:
	{
		float f;
		memcpy(&f, &value, sizeof(float));
		const unsigned char len = snprintf(NULL, 0, "%f", f) + 1;
		char* s = new char[len];
		snprintf(s, len, "%f", f);
		return s;
	}
	break;
	case VALUE_STRING:
	{
		const unsigned char len = strlen(value) + 1;
		char* s = new char[len];
		memset(s, '\0', len);
		strcpy_s(s, len, value);
		return s;
	}
	break;
	//case VALUE_ARRAY:
	//{
	//	ValueVectorSub* v = (ValueVectorSub*)(value);
	//	//char* buffer = new char[50];
	//	//memset(buffer, '\0', 50);
	//	//sprintf_s(buffer, "<Array size: %d>", v->el.size());
	//	//return buffer;
	//	char* buffer = new char[9] {"<Array>"};
	//	std::cout << "I am printing an array of length " << v->el.size() << "." << std::endl;
	//	return buffer;
	//}
	//break;
	default:
		break;
	}
	return nullptr;
}

Index::Index() : Expression(EXPRESSION_INDEX)
{
	expr = nullptr;
	index = nullptr;
}

Index::Index(Expression* e, Expression* i) : Expression(EXPRESSION_INDEX)
{
	expr = e;
	index = i;
}

s_lang Index::evaluate(Result& result)
{
	s_lang error_ = expr->evaluate(result);
	if (error_ != ERROR_NONE) {
		return error_;
	}
	int n;
	error_ = indexParse(n);
	if (error_ != ERROR_NONE)
	{
		return error_;
	}
	if (result.type_ == RESULT_STRING) {
		char* s = result.value;
		if (n < strlen(s)) {
			result.type_ = RESULT_STRING;
			result.value = new char[2] {s[n], '\0'};
			return ERROR_NONE;
		}
		return ERROR_INVALID_INDEX;
	}
	else if (result.type_ == RESULT_ARRAY)
	{
		std::vector<Result>& a = *(std::vector<Result>*)(result.value);
		if (0 <= n && n < a.size())
		{
			result = a[n];
			if (result.type_ == VALUE_STRING)
			{
				const int len = strlen((char*)result.value) + 1;
				t_value s = new char[len];
				memcpy(s, result.value, len);
				result.value = s;
			}
			return ERROR_NONE;
		}
		return ERROR_INVALID_INDEX;
	}
	return ERROR_VARIABLE_MISMATCH;
}

s_lang Index::indexParse(int& n)
{
	n = -1;
	if (index == nullptr)
	{
		return ERROR_INVALID_INDEX;
	}
	Result ev;
	s_lang error_ = index->evaluate(ev);
	if (error_ != ERROR_NONE)
	{
		return error_;
	}
	if (!(ev.type_ & LANGUAGE_VALUE)) {
		return ERROR_VARIABLE_MISMATCH;
	}
	Value result;
	result.type_ = ev.type_;
	result.value = (t_value)ev.value;
	if (result.type_ & ((VALUE_INT | VALUE_BOOL | VALUE_FLOAT) & LANGUAGE_MASK))
	{
		n = result.asInt();
		return ERROR_NONE;
	}
	return ERROR_EVALUATION_ERROR;
}

Variable::Variable(char* n) : Expression(EXPRESSION_VARIABLE)
{
	name = n;
}

s_lang Variable::evaluate(Result& result)
{
	UMAP_kpCHAR(const char*, Result)::iterator iter = VALUE_TABLE.find(name);
	if (iter == VALUE_TABLE.end()) {
		printError("Variable %s not initialized.", name);
		result.type_ = RESULT_ERROR;
		result.value = (t_value)ERROR_VARIABLE_UNINITIALIZED;
		return ERROR_VARIABLE_UNINITIALIZED;
	}
	printDebug("Variable %s found of type %llu", name, iter->second.type_);
	if (iter->second.type_ == RESULT_NONE)
	{
		return ERROR_VARIABLE_UNINITIALIZED;
	}
	result = iter->second;
	if (result.type_ == VALUE_STRING)
	{
		const int len = strlen((char*)result.value) + 1;
		t_value s = new char[len];
		memcpy(s, result.value, len);
		result.value = s;
	}
	return ERROR_NONE;
}

Array::Array() : Expression(EXPRESSION_ARRAY)
{
	el.clear();
}

s_lang Array::evaluate(Result& result)
{
	std::vector<Result>* a = new std::vector<Result>;
	a->clear();
	for (int i = 0; i < el.size(); i++)
	{
		Result v;
		s_lang error_ = el[i]->evaluate(v);
		if (error_ != ERROR_NONE)
		{
			a->clear();
			return error_;
		}
		if (v.type_ == VALUE_NULL)
		{
			a->clear();
			return ERROR_EVALUATION_ERROR;
		}
		a->push_back(v);
	}
	result.type_ = exp;
	result.value = (t_value)(a);
	return ERROR_NONE;
}

ConditionalBranch::ConditionalBranch()
{
	condition = nullptr;
	instructions.clear();
}

Conditional::Conditional() : Expression(EXPRESSION_IFELSE)
{
	branches.clear();
}

s_lang Conditional::evaluate(Result& result)
{
	if (branches.size() <= 0)
	{
		return ERROR_EVALUATION_ERROR;
	}
	for (int i = 0; i < branches.size(); i++)
	{
		if (branches[i].condition != nullptr)
		{
			Result ev;
			s_lang error_ = branches[i].condition->evaluate(ev);
			if (error_ != ERROR_NONE)
			{
				return error_;
			}
			if (!(ev.type_ & LANGUAGE_VALUE)) {
				return ERROR_VARIABLE_MISMATCH;
			}
			Value v;
			v.type_ = ev.type_;
			v.value = (t_value)ev.value;
			if (v.asBool() == LANGUAGE_TRUE)
			{
				for (int j = 0; j < branches[i].instructions.size(); j++)
				{
					s_lang eerror_ = branches[i].instructions[j]->evaluate(result);
					if (eerror_ != ERROR_NONE)
					{
						return eerror_;
					}
				}
				break;
			}
		}
		else if (i == branches.size() - 1 && i != 0)
		{
			for (int j = 0; j < branches[i].instructions.size(); j++)
			{
				s_lang eerror_ = branches[i].instructions[j]->evaluate(result);
				if (eerror_ != ERROR_NONE)
				{
					return eerror_;
				}
			}
		}
		else
		{
			return ERROR_EVALUATION_ERROR;
		}
	}
	return ERROR_NONE;
}

WhileLoop::WhileLoop() : Expression(EXPRESSION_LOOP_WHILE)
{
	condition = nullptr;
	instructions.clear();
}

s_lang WhileLoop::evaluate(Result& result)
{
	if (condition == nullptr)
	{
		return ERROR_EVALUATION_ERROR;
	}
	int iter = 0;
	while (true)
	{
		Result ev;
		s_lang error_ = condition->evaluate(ev);
		if (error_ != ERROR_NONE)
		{
			return error_;
		}
		if (!(ev.type_ & LANGUAGE_VALUE)) {
			return ERROR_VARIABLE_MISMATCH;
		}
		Value v;
		v.type_ = ev.type_;
		v.value = (t_value)ev.value;
		if (v.asBool() != LANGUAGE_TRUE)
		{
			break;
		}
		for (int i = 0; i < instructions.size(); i++)
		{
			error_ = instructions[i]->evaluate(result);
			if (error_ == ERROR_BREAK)
			{
				return ERROR_NONE;
			}
			else if (error_ == ERROR_CONTINUE) {
				break;
			}
			else if (error_ == ERROR_RETURN) {
				return ERROR_RETURN;
			}
			else if (error_ != ERROR_NONE)
			{
				return error_;
			}
		}
		iter++;
		if (iter >= MAX_ITER)
		{
			printWarning("While loop forcefully ended on iter = %d.", iter);
			break;
		}
	}
	return ERROR_NONE;
}

ForLoop::ForLoop() : Expression(EXPRESSION_LOOP_FOR)
{
	memset(guard, NULL, 3 * (sizeof(Expression*)));
	instructions.clear();
}

s_lang ForLoop::evaluate(Result& result)
{
	if (guard[1] == nullptr)
	{
		printWarning("infinite 'for loop' warning...");
	}
	Result ev;
	s_lang error_;
	if (guard[0] != nullptr)
	{
		error_ = guard[0]->evaluate(ev);
		if (error_ != ERROR_NONE)
		{
			return error_;
		}
	}
	int iter = 0;
	while (true)
	{
		if (guard[1] != nullptr) {
			error_ = guard[1]->evaluate(ev);
			if (error_ != ERROR_NONE)
			{
				return error_;
			}
			if (!(ev.type_ & LANGUAGE_VALUE)) {
				return ERROR_VARIABLE_MISMATCH;
			}
			Value v;
			v.type_ = ev.type_;
			v.value = (t_value)ev.value;
			if (v.asBool() != LANGUAGE_TRUE)
			{
				break;
			}
		}
		for (int i = 0; i < instructions.size(); i++)
		{
			error_ = instructions[i]->evaluate(result);
			if (error_ == ERROR_BREAK)
			{
				return ERROR_NONE;
			}
			else if (error_ == ERROR_CONTINUE) {
				break;
			}
			else if (error_ == ERROR_RETURN) {
				return ERROR_RETURN;
			}
			else if (error_ != ERROR_NONE)
			{
				return error_;
			}
		}
		if (guard[2] != nullptr)
		{
			error_ = guard[2]->evaluate(ev);
			if (error_ != ERROR_NONE)
			{
				return error_;
			}
			if (!(ev.type_ & LANGUAGE_VALUE)) {
				return ERROR_VARIABLE_MISMATCH;
			}
		}
		iter++;
		if (iter >= MAX_ITER)
		{
			printWarning("For loop forcefully ended on iter = %d.", iter);
			break;
		}
	}
	return ERROR_NONE;
}

FunctionCall::FunctionCall(char* n) : Expression(EXPRESSION_FUNCTIONCALL)
{
	name = n;
	args.clear();
}

s_lang FunctionCall::evaluate(Result& result)
{
	UMAP_kpCHAR(const char*, Result)::iterator iter = VALUE_TABLE.find(name);
	if (iter == VALUE_TABLE.end()) {
		printError("Function '%s' is not initialized.", name);
		return ERROR_VARIABLE_UNINITIALIZED;
	}
	if (iter->second.type_ == EXPRESSION_BUILTIN)
	{
		BuiltIn* func = (BuiltIn*)(iter->second.value);
		return func->call(result, args);
	}
	else if (iter->second.type_ == EXPRESSION_FUNCTION)
	{
		Function* func = (Function*)(iter->second.value);
		return func->call(result, args);
	}
	printError("Variable '%s' with type %llu is not Callable.", name, iter->second.type_);
	return ERROR_VARIABLE_MISMATCH;
}

Callable::Callable() : Expression(EXPRESSION_CALLABLE)
{
	//exp = e;
}

Callable::Callable(s_lang e) : Expression(e)
{
	//exp = e;
}

BuiltIn::BuiltIn(const char* n, s_lang(*f)(Result&, std::vector<Result>&)) : Callable(EXPRESSION_BUILTIN)
{
	name = n;
	func = f;
}

s_lang BuiltIn::evaluate(Result& result)
{
	result.type_ = exp;
	result.value = (t_value)this;
	return ERROR_NONE;
}

s_lang BuiltIn::call(Result& result, std::vector<Expression*>& args)
{
	if (func == nullptr) {
		return ERROR_EVALUATION_ERROR;
	}
	std::vector<Result> expval;
	expval.clear();
	for (int i = 0; i < args.size(); i++) {
		Result ev;
		s_lang error_ = args[i]->evaluate(ev);
		if (error_ != ERROR_NONE) {
			return error_;
		}
		expval.push_back(ev);
	}
	return func(result, expval);
}

Function::Function(char* n) : Callable(EXPRESSION_FUNCTION)
{
	name = n;
	argnames.clear();
	instructions.clear();
}

s_lang Function::call(Result& result, std::vector<Expression*>& args)
{
	printDebug("Function being called with %llu/%llu arguments.", args.size(), argnames.size());
	if (args.size() != argnames.size())
	{
		return ERROR_ARGUMENT_MISMATCH;
	}
	for (int i = 0; i < args.size(); i++)
	{
		Result ev;
		s_lang error_ = args[i]->evaluate(ev);
		if (error_ == ERROR_NONE) {
			VALUE_TABLE.insert_or_assign(argnames[i], ev);
			//std::cout << "Registered arg: '" << argnames[i] << "' with value '" << v.asStr() << "'." << std::endl;
		}
		else {
			return error_;
		}
	}
	for (int i = 0; i < instructions.size(); i++)
	{
		s_lang error_ = instructions[i]->evaluate(result);
		if (error_ == ERROR_RETURN) {
			return ERROR_NONE;
		}
		else if (error_ == ERROR_BREAK) {
			return ERROR_EVALUATION_ERROR;
		}
		else if (error_ == ERROR_CONTINUE) {
			return ERROR_EVALUATION_ERROR;
		}
		else if (error_ != ERROR_NONE) {
			return error_;
		}
	}
	return ERROR_NONE;
}

s_lang Function::evaluate(Result& result)
{
	result.type_ = exp;
	result.value = (t_value)this;
	return ERROR_NONE;
}

Return::Return(Expression* v)
{
	val = v;
}

s_lang Return::evaluate(Result& result)
{
	s_lang error_ = val->evaluate(result);
	if (error_ != ERROR_NONE) {
		return error_;
	}
	return ERROR_RETURN;
}

s_lang Break::evaluate(Result& result)
{
	return ERROR_BREAK;
}

s_lang Continue::evaluate(Result& result)
{
	return ERROR_CONTINUE;
}

Unary::Unary(s_lang o, Expression* p) : Expression(EXPRESSION_UNARY)
{
	op = o;
	child = p;
}

Unary::~Unary()
{
	if (child != nullptr)
	{
		//delete child;
	}
}

s_lang Unary::evaluate(Result& result)
{
	if (child == nullptr || op == OPERATOR_NONE)
	{
		result = { RESULT_ERROR, (t_value)ERROR_EVALUATION_ERROR };
		return ERROR_EVALUATION_ERROR;
	}
	s_lang error_ = child->evaluate(result);
	if (error_ != ERROR_NONE) {
		return error_;
	}
	if (!(result.type_ & LANGUAGE_VALUE)) {
		return ERROR_TYPE_MISMATCH;
	}
	switch (op)
	{
	case OPERATOR_NONE:
		return ERROR_PARSE_ERROR;
		break;
	case OPERATOR_FLIP:
		switch (result.type_)
		{
		case VALUE_NULL:
			return ERROR_PARSE_ERROR;
			break;
		case VALUE_BOOL:
		case VALUE_INT:
			result.value = (t_value)(~((intptr_t)result.value));
			return ERROR_NONE;
			break;
		default:
			return ERROR_EVALUATION_ERROR;
			break;
		}
		break;
	case OPERATOR_NEGATION:
		switch (result.type_)
		{
		case VALUE_NULL:
			return ERROR_PARSE_ERROR;
			break;
		case VALUE_BOOL:
		case VALUE_INT:
			result.value = (t_value)(!((intptr_t)result.value != 0));
			return ERROR_NONE;
			break;
		default:
			return ERROR_EVALUATION_ERROR;
			break;
		}
		break;
	case OPERATOR_PLUS:
		switch (result.type_)
		{
		case VALUE_NULL:
			return ERROR_PARSE_ERROR;
			break;
		case VALUE_BOOL:
		case VALUE_INT:
		case VALUE_FLOAT:
			return ERROR_NONE;
			break;
		default:
			return ERROR_EVALUATION_ERROR;
			break;
		}
		break;
	case OPERATOR_MINUS:
		switch (result.type_)
		{
		case VALUE_NULL:
			return ERROR_PARSE_ERROR;
			break;
		case VALUE_BOOL:
		case VALUE_INT:
			result.value = (t_value)(-((intptr_t)result.value));
			return ERROR_NONE;
		case VALUE_FLOAT:
		{
			// This may be possible by identifying the bit position of the mantisa sign to flip it.
			float f;
			memcpy(&f, &result.value, sizeof(float));
			f = -f;
			memset(&result.value, 0, sizeof(void*));
			memcpy(&result.value, &f, sizeof(float));
			return ERROR_NONE;
		}
		break;
		default:
			return ERROR_EVALUATION_ERROR;
			break;
		}
		break;
	default:
		break;
	}
	return ERROR_EVALUATION_ERROR;
}

Binary::Binary(s_lang o, Expression* l, Expression* r) : Expression(EXPRESSION_BINARY)
{
	op = o;
	left = l;
	right = r;
}

Binary::~Binary()
{
	if (left != nullptr)
	{
		delete left;
	}
	if (right != nullptr)
	{
		delete right;
	}
}

s_lang Binary::evaluate(Result& result)
{
	if (left == nullptr || right == nullptr || op == OPERATOR_NONE)
	{
		printError("Invalid Binary Operation. One or more elements are missing.");
		result = { RESULT_ERROR, (char*)ERROR_EVALUATION_ERROR };
		return ERROR_EVALUATION_ERROR;
	}

	if (op == OPERATOR_EQUAL) {
		if (left->exp == EXPRESSION_VARIABLE)
		{
			s_lang error_ = right->evaluate(result);
			if (error_ != ERROR_NONE)
			{
				return error_;
			}
			printDebug("I got a Value<%llu> to be assigned.", result.type_);
			Variable* l = static_cast<Variable*>(left);
			VALUE_TABLE.insert_or_assign(l->name, result);
			printDebug("I have stored such value inside Variable '%s'", l->name);
			return ERROR_NONE;
		}
		else if (left->exp == EXPRESSION_INDEX) {
			s_lang error_ = right->evaluate(result);
			if (error_ != ERROR_NONE)
			{
				return error_;
			}
			printDebug("I got a Value<%llu> to be assigned.", result.type_);
			Index* l = static_cast<Index*>(left);
			if(l->expr->exp == EXPRESSION_VARIABLE) {
				Variable* ll = static_cast<Variable*>(l->expr);
				UMAP_kpCHAR(const char*, Result)::iterator iter = VALUE_TABLE.find(ll->name);
				if (iter == VALUE_TABLE.end()) {
					printError("Variable '%s' not initialized.", ll->name);
					return ERROR_VARIABLE_UNINITIALIZED;
				}
				int n;
				s_lang error_ = l->indexParse(n);
				if (error_ != ERROR_NONE)
				{
					return error_;
				}
				if (iter->second.type_ == RESULT_ARRAY)
				{
					std::vector<Result>& a = *(std::vector<Result>*)(iter->second.value);
					if (n < 0 || a.size() < n)
					{
						return ERROR_INVALID_INDEX;
					}
					if (n == a.size())
					{
						a.push_back(Result{});
					}
					a[n] = result;
					return ERROR_NONE;
				}
				else if (iter->second.type_ == RESULT_STRING) {
					if (result.type_ == RESULT_STRING) {
						if (strlen(result.value) == 1) {
							if (0 <= n && n < strlen(iter->second.value)) {
								iter->second.value[n] = result.value[0];
								return ERROR_NONE;
							}
						}
					}
				}
				return ERROR_TYPE_MISMATCH;
			}
			else {
				return ERROR_EVALUATION_ERROR;
			}
		}
		else
		{
			return ERROR_EVALUATION_ERROR;
		}
	}
	else
	{
		Result lx;
		s_lang lError = left->evaluate(lx);
		if (lError != ERROR_NONE) {
			return lError;
		}
		if (!(lx.type_ & LANGUAGE_VALUE))
		{
			return ERROR_EVALUATION_ERROR;
		}
		Result rx;
		s_lang rError = right->evaluate(rx);
		if (rError != ERROR_NONE) {
			return rError;
		}
		if (!(rx.type_ & LANGUAGE_VALUE))
		{
			return ERROR_EVALUATION_ERROR;
		}
		Value l = { lx.type_, (t_value)lx.value };
		Value r = { rx.type_ ,(t_value)rx.value };
		switch (op)
		{
		case OPERATOR_NONE:
			return ERROR_PARSE_ERROR;
			break;
		case OPERATOR_PLUS:
			if (l.type_ == VALUE_STRING || r.type_ == VALUE_STRING)
			{
				const char* ls = l.asStr();
				const char* rs = r.asStr();
				const unsigned int len = strlen(ls) + strlen(rs) + 1;
				char* s = new char[len];
				memset(s, '\0', len);
				strcpy_s(s, len, ls);
				strcat_s(s, len, rs);
				result.type_ = VALUE_STRING;
				result.value = s;
				delete[] ls;
				delete[] rs;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT || r.type_ == VALUE_FLOAT)
			{
				float f = l.asFloat() + r.asFloat();
				memset(&result.value, 0, sizeof(void*));
				memcpy(&result.value, &f, sizeof(float));
				result.type_ = VALUE_FLOAT;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() + r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_MINUS:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				float f = l.asFloat() - r.asFloat();
				memset(&result.value, 0, sizeof(void*));
				memcpy(&result.value, &f, sizeof(float));
				result.type_ = VALUE_FLOAT;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() - r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_MULTIPLY:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				float f = l.asFloat() * r.asFloat();
				memset(&result.value, 0, sizeof(void*));
				memcpy(&result.value, &f, sizeof(float));
				result.type_ = VALUE_FLOAT;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() * r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_DIVIDE:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if (r.value == VALUE_ZERO)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				float f = l.asFloat() / r.asFloat();
				memset(&result.value, 0, sizeof(void*));
				memcpy(&result.value, &f, sizeof(float));
				result.type_ = VALUE_FLOAT;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() / r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_REMAINDER:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else
			{
				result.value = (t_value)(l.asInt() % r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_SHIFT_LEFT:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else
			{
				result.value = (t_value)(l.asInt() << r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_SHIFT_RIGHT:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ | r.type_) & VALUE_FLOAT & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else
			{
				result.value = (t_value)(l.asInt() >> r.asInt());
				result.type_ = VALUE_INT;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_LESSER:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ == VALUE_FLOAT) && (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() < r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asFloat() < r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (r.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asInt() < r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() < r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_GREATER:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ == VALUE_FLOAT) && (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() > r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asFloat() > r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (r.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asInt() > r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() > r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_LESSER_EQUAL:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ == VALUE_FLOAT) && (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() <= r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asFloat() <= r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (r.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asInt() <= r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() <= r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_GREATER_EQUAL:
			if ((l.type_ | r.type_) & VALUE_STRING & LANGUAGE_MASK)
			{
				return ERROR_EVALUATION_ERROR;
			}
			else if ((l.type_ == VALUE_FLOAT) && (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() >= r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asFloat() >= r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (r.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asInt() >= r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() >= r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_EQUAL_DOUBLE:
			if ((l.type_ == VALUE_STRING) && (r.type_ == VALUE_STRING))
			{
				result.value = (t_value)(strcmp(l.value, r.value) == 0 ? (intptr_t)1 : (intptr_t)0); // TODO: compare lengths as well??
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if ((l.type_ == VALUE_FLOAT) && (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() == r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asFloat() == r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (r.type_ == VALUE_FLOAT)
			{
				result.value = (t_value)(l.asInt() == r.asFloat() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() == r.asInt() ? (intptr_t)1 : (intptr_t)0);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		case OPERATOR_EQUAL_NOT:
			if ((l.type_ == VALUE_STRING) && (r.type_ == VALUE_STRING))
			{
				result.value = (t_value)(strcmp(l.value, r.value) == 0 ? (intptr_t)0 : (intptr_t)1);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if (l.type_ == r.type_)
			{
				result.value = (t_value)(l.value == r.value ? (intptr_t)0 : (intptr_t)1);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else if ((l.type_ == VALUE_FLOAT) || (r.type_ == VALUE_FLOAT))
			{
				result.value = (t_value)(l.asFloat() == r.asFloat() ? (intptr_t)0 : (intptr_t)1);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			else
			{
				result.value = (t_value)(l.asInt() == r.asInt() ? (intptr_t)0 : (intptr_t)1);
				result.type_ = VALUE_BOOL;
				return ERROR_NONE;
			}
			break;
		default:
			return ERROR_EVALUATION_ERROR;
			break;
		}
	}
	return ERROR_NONE;
}
