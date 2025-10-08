#include "Parser.h"

Parser::Parser()
{
	index = 0;
	tokens.clear();
}

bool Parser::tokenRetrieve(Token& hold, char offset = 0)
{
	if (tokens.size() == 0)
	{
		printWarning("tokenRetrieve: No tokens.");
		return false;
	}
	if ((index + offset) < 0 || tokens.size() <= (index + offset))
	{
		printWarning("index %hd out of bounds.", index + offset);
		return false;
	}
	hold = tokens[index + offset];
	return true;
}

bool Parser::parse_operand(Expression*& root)
{
	root = nullptr;
	Expression** leaf = &root;
	Token current;
	while (tokenRetrieve(current))
	{
		switch (current.type_)
		{
		case TOKEN_VALUE:
			*leaf = (Value*)(current.value);
			index++;
			return true;
		case TOKEN_IDENTIFIER:
			{
				index++;
				Token following;
				if (tokenRetrieve(following)) // Discern between variable and function call.
				{
					if (following.type_ == TOKEN_DELIMITER && following.value == DELIMITER_PARENTHESIS_LEFT)
					{
						index++;
						FunctionCall* x = new FunctionCall{ (char*)(current.value) };
						build_sequence(x->args, DELIMITER_COMMA, EXPRESSION_OPERATION);
						Token closing;
						if (tokenRetrieve(closing))
						{
							if (closing.type_ == TOKEN_DELIMITER && closing.value == DELIMITER_PARENTHESIS_RIGHT)
							{
								*leaf = x;
								index++;
								return true;
							}
						}
						parserError("Missing closing parenthesis in function call.");
						return false;
					}
				}
				Variable* v = new Variable{ (char*)(current.value) };
				*leaf = v;
				return true;
			}
		case TOKEN_OPERATOR:
			switch (current.value)
			{
			case OPERATOR_FLIP:
			case OPERATOR_NEGATION:
			case OPERATOR_PLUS:
			case OPERATOR_MINUS:
				{
					Unary* u = new Unary{ current.value, nullptr };
					*leaf = u;
					leaf = &u->child;
					// TODO: leaf = new Unary{current.value, leaf};
				}
				break;
			default:
				parserError("'%llu' is an invalid unary operator.", current.value); // TODO: revert from operator enum to human readable symbol.
				return false;
			}
			break;
		case TOKEN_DELIMITER:
			switch (current.value)
			{
			case DELIMITER_PARENTHESIS_LEFT:
				{
					index++;
					Expression* inner;
					if (!parse_operation(inner)) {
						return false;
					}
					if (inner == nullptr) {
						parserError("Parenthesis is empty or not properly closed.");
						return false;
					}
					Token closing;
					if (tokenRetrieve(closing))
					{
						if (closing.type_ == TOKEN_DELIMITER && closing.value == DELIMITER_PARENTHESIS_RIGHT)
						{
							*leaf = inner;
							index++;
							return true;
						}
					}
					parserError("Mismatched Parenthesis.");
					return false;
				}
			case DELIMITER_BRACKET_LEFT:
				{
					index++;
					Array* v = new Array{};
					if (!build_sequence(v->el, DELIMITER_COMMA, EXPRESSION_OPERATION))
					{
						return false;
					}
					Token closing;
					if (tokenRetrieve(closing))
					{
						if (closing.type_ == TOKEN_DELIMITER && closing.value == DELIMITER_BRACKET_RIGHT)
						{
							*leaf = v;
							index++;
							return true;
						}
					}
					parserError("Mismatched Brackets.");
					return false;
				}
			case DELIMITER_PARENTHESIS_RIGHT:
			case DELIMITER_BRACKET_RIGHT:
			case DELIMITER_BRACE_RIGHT:
			case DELIMITER_COMMA:
			case DELIMITER_SEMICOLON:
				if (root == nullptr) {
					return true;
				}
				else {
					parserError("Unexpected delimiter %lld.", current.value); // TODO: revert to symbol.
					return false;
				}
			case DELIMITER_BRACE_LEFT:
			default:
				parserError("Unexpected delimiter %lld.", current.value); // TODO: revert to symbol.
				return false;
			}
		case TOKEN_KEYWORD:
			if (current.value == KEYWORD_FUNCTION) {
				if (!build_function(*leaf))
				{
					return false;
				}
				if (*leaf == nullptr)
				{
					parserError("Function Expression returned 'NULL' which is impossible.(2)"); // TODO: Remove.
					return false;
				}
				if ((*leaf)->exp != EXPRESSION_FUNCTION)
				{
					parserError("Returned Expression is not a Function which is impossible.(3)"); // TODO: Remove.
					return false;
				}
				return true;
			}
			parserError("Keyword '%lld' can not form an operand."); // TODO: revert enum.
			return false;
		default:
			parserError("Invalid Token type '%lld'.", current.type_);
			return false;
		}
		index++;
	}
	parserError("Failed to form a valid operand.");
	return false;
}

bool Parser::parse_operandIndex(Expression*& root)
{
	if (!parse_operand(root)) {
		return false;
	}
	Token current;
	if (tokenRetrieve(current)) {
		if (current.type_ == TOKEN_DELIMITER && current.value == DELIMITER_BRACKET_LEFT) {
			index++;
			Expression* e = nullptr;
			if (parse_operation(e)) {
				if (e != nullptr) {
					root = new Index{ root, e };
					Token closing;
					if (tokenRetrieve(closing)) {
						if (closing.type_ == TOKEN_DELIMITER && closing.value == DELIMITER_BRACKET_RIGHT) {
							index++;
							return true;
						}
					}
					parserError("Missing Right Bracket for Array indexing.");
				}
				else {
					parserError("Brackets are empty or not properly closed.");
				}
			}
			return false;
		}
	}
	return true;
}

bool Parser::parse_operation(Expression*& root)
{
	if (!parse_operandIndex(root)) {
		return false;
	}
	if (root == nullptr) {
		return true;
	}
	Expression** leaves[3] = { &root, &root, &root }; // 0 is the leaf-most.
	Token current;
	while (tokenRetrieve(current))
	{
		if (current.type_ == TOKEN_OPERATOR)
		{
			switch (current.value)
			{
			case OPERATOR_MULTIPLY:
			case OPERATOR_DIVIDE:
			case OPERATOR_REMAINDER:
			case OPERATOR_SHIFT_LEFT:
			case OPERATOR_SHIFT_RIGHT:
				{
					index++;
					Expression* operand;
					if (!parse_operandIndex(operand)) {
						return false;
					}
					Binary* bin = new Binary{ current.value, *leaves[1], operand };
					*leaves[1] = bin;
					leaves[0] = &bin->right;
				}
				break;
			case OPERATOR_PLUS:
			case OPERATOR_MINUS:
			case OPERATOR_LESSER:
			case OPERATOR_GREATER:
			case OPERATOR_LESSER_EQUAL:
			case OPERATOR_GREATER_EQUAL:
			case OPERATOR_EQUAL_DOUBLE:
				{
					index++;
					Expression* operand;
					if (!parse_operandIndex(operand)) {
						return false;
					}
					Binary* bin = new Binary{ current.value, *leaves[2], operand };
					*leaves[2] = bin;
					leaves[1] = &bin->right;
					leaves[0] = &bin->right;
				}
				break;
			case OPERATOR_EQUAL:
				{
					index++;
					Expression* operand;
					if (!parse_operandIndex(operand)) {
						return false;
					}
					Binary* bin = new Binary{ current.value, *leaves[0], operand };
					*leaves[0] = bin;
					leaves[0] = &bin->right;
					leaves[1] = &bin->right;
					leaves[2] = &bin->right;
				}
				break;
			default:
				parserError("Unknown Operator '%lld'.", current.value);
				return false;
			}
		}
		else {
			break;
		}
	}
	return true;
}

bool Parser::build_sequence(std::vector<Expression*>& sequence, s_lang delimiter = DELIMITER_COMMA, s_lang allowed = EXPRESSION_OPERATION)
{
	sequence.clear();
	while (true)
	{
		Expression* element = nullptr;
		if (!parse_operation(element))
		{
			return false;
		}
		if (element == nullptr) {
			return true;
		}

		if (element->exp & allowed & LANGUAGE_MASK)
		{
			sequence.push_back(element);
			Token separator;
			if (!tokenRetrieve(separator)) {
				break;
			}
			if (separator.type_ == TOKEN_DELIMITER && separator.value == delimiter) {
				index++;
			}
			else {
				return true;
			}
		}
		else {
			parserError("Sequence can not contain Expression of type '%lld'.", element->exp);
			return false;
		}
	}
	return ERROR_NONE;
}

bool Parser::build_loop(Expression*& root)
{
	Token loop_type;
	if (!tokenRetrieve(loop_type)) {
		return true;
	}
	if (loop_type.type_ != TOKEN_KEYWORD || (loop_type.value != KEYWORD_FOR && loop_type.value != KEYWORD_WHILE)) {
		parserError("Invalid loop type.");
		return false;
	}

	index++;
	Token current;
	if (!tokenRetrieve(current)) {
		parserError("Unfinished loop definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_LEFT) {
		parserError("Loop definition expected left Parenthesis.");
		return false;
	}

	index++;
	std::vector<Expression*> loop_guard;
	loop_guard.clear();
	if (!build_sequence(loop_guard, DELIMITER_SEMICOLON)) {
		return false;
	}
	std::vector<Expression*>* instructions;
	if (loop_type.value == KEYWORD_WHILE && loop_guard.size() == 1)
	{
		WhileLoop* loop = new WhileLoop{};
		loop->condition = loop_guard[0];
		instructions = &loop->instructions;
		root = loop;
	}
	else if (loop_type.value == KEYWORD_FOR && loop_guard.size() == 3)
	{
		ForLoop* loop = new ForLoop{};
		memcpy(loop->guard, &loop_guard[0], 3 * (sizeof(Expression*)));
		instructions = &loop->instructions;
		root = loop;
	}
	else {
		parserError("Mismatched loop guard size %d.", loop_guard.size());
		return false;
	}

	if (!tokenRetrieve(current)) {
		parserError("Unterminated loop definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_RIGHT) {
		parserError("Missing closing Parenthesis.");
		return false;
	}

	index++;
	if (!tokenRetrieve(current)) {
		parserError("Unterminated loop definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_LEFT) {
		parserError("Missing opening Brace.");
		return false;
	}

	index++;
	if (!build_program(*instructions)) {
		return false;
	}

	if (!tokenRetrieve(current)) {
		parserError("Unterminated loop definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_RIGHT) {
		parserError("Missing closing Brace.");
		return false;
	}

	index++;
	return true;
}

bool Parser::build_function(Expression*& root)
{
	Token current;
	if (!tokenRetrieve(current)) {
		parserError("Missing function declaration.");
		return false;
	}
	if (current.type_ != TOKEN_KEYWORD || current.value != KEYWORD_FUNCTION) {
		parserError("Missing function declaration.");
		return false;
	}

	index++;
	Function* func = new Function{ (t_value)"funcName" };
	if (!tokenRetrieve(current)) {
		parserError("Unterminated function definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_LEFT) {
		parserError("Malformed function definition missing opening Parenthesis.");
		return false;
	}

	do {
		index++;
		if (!tokenRetrieve(current)) {
			parserError("Unterminated function definition.");
			return false;
		}
		if (current.type_ == TOKEN_IDENTIFIER)
		{
			func->argnames.push_back((char*)current.value);
			index++;
			if (!tokenRetrieve(current)) {
				parserError("Unterminated function arguments.");
				return false;
			}
		}
		else {
			break;
		}
	} while (current.type_ == TOKEN_DELIMITER && current.value == DELIMITER_COMMA);

	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_RIGHT)
	{
		parserError("Missing closing Parenthesis.");
		return false;
	}

	index++;
	if (!tokenRetrieve(current)) {
		parserError("Unterminated function definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_LEFT) {
		parserError("Missing opening Brace.");
		return false;
	}

	index++;
	if (!build_program(func->instructions)) {
		return false;
	}

	if (!tokenRetrieve(current)) {
		parserError("Unterminated function definition.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_RIGHT) {
		parserError("Missing closing Brace.");
		return false;
	}

	index++;
	root = func;
	return true;
}

bool Parser::parse_conditionBranch_sub(ConditionalBranch& branch)
{
	Token current;
	if (!tokenRetrieve(current)) {
		parserError("Unfinished If Else block.");
		return false;
	}
	if (current.type_ != TOKEN_KEYWORD || ((current.value != KEYWORD_IF && current.value != KEYWORD_ELSEIF && current.value != KEYWORD_ELSE))) {
		parserError("Expected If Else statement.");
		return false;
	}

	index++;
	if (current.value == KEYWORD_IF || current.value == KEYWORD_ELSEIF)
	{
		if (!tokenRetrieve(current)) {
			parserError("Unfinished If Else block.");
			return false;
		}
		if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_LEFT) {
			parserError("Expected opening Parenthesis.");
			return false;
		}

		index++;
		if (!parse_operation(branch.condition)) {
			return false;
		}
		if (branch.condition == nullptr) {
			parserError("Condition is empty.");
			return false;
		}

		if (!tokenRetrieve(current)) {
			parserError("Unfinished If Else block.");
			return false;
		}
		if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_PARENTHESIS_RIGHT) {
			parserError("Expected closing Parenthesis.");
			return false;
		}

		index++;
	}
	if (!tokenRetrieve(current)) {
		parserError("Unfinished If Else block.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_LEFT) {
		parserError("Expected opening Brace.");
		return false;
	}

	index++;
	if (!build_program(branch.instructions)) {
		return false;
	}

	if (!tokenRetrieve(current)) {
		parserError("Unfinished If Else block.");
		return false;
	}
	if (current.type_ != TOKEN_DELIMITER || current.value != DELIMITER_BRACE_RIGHT) {
		parserError("Expected closing Brace.");
		return false;
	}

	index++;
	return true;
}

bool Parser::parse_conditionBranch(Expression*& root)
{
	root = nullptr;
	Token current;
	if (!tokenRetrieve(current)) {
		parserError("Unfinished If Else block.");
		return false;
	}
	if (current.type_ != TOKEN_KEYWORD || current.value != KEYWORD_IF) {
		parserError("Expected 'If' keyword.");
		return false;
	}
	ConditionalBranch branchIf = ConditionalBranch{};
	if (!parse_conditionBranch_sub(branchIf)) {
		return false;
	}

	Conditional* conditional = new Conditional();
	conditional->branches.push_back(branchIf);
	while (tokenRetrieve(current))
	{
		if (current.type_ != TOKEN_KEYWORD || current.value != KEYWORD_ELSEIF) {
			break;
		}
		ConditionalBranch branchElseif = ConditionalBranch{};
		if (!parse_conditionBranch_sub(branchElseif)) {
			return false;
		}
		conditional->branches.push_back(branchElseif);
	}

	if (!tokenRetrieve(current)) {
		return true;
	}
	if (current.type_ == TOKEN_KEYWORD && current.value == KEYWORD_ELSE) {
		ConditionalBranch branchElse = ConditionalBranch{};
		if (!parse_conditionBranch_sub(branchElse)) {
			return false;
		}
		conditional->branches.push_back(branchElse);
	}
	root = conditional;
	return true;
}

bool Parser::parse_instruction(Expression*& root, s_lang terminated = EXPRESSION_OPERATION | EXPRESSION_INTERRUPT)
{
	root = nullptr;
	Token start;
	if (!tokenRetrieve(start)) {
		return true;
	}
	bool validate = true;
	switch (start.type_)
	{
	case TOKEN_VALUE:
	case TOKEN_IDENTIFIER:
	case TOKEN_OPERATOR:
		validate = parse_operation(root);
		break;
	case TOKEN_DELIMITER:
		switch (start.value)
		{
		
		case DELIMITER_PARENTHESIS_LEFT:
		case DELIMITER_BRACKET_LEFT:
			validate = parse_operation(root);
			break;
		case DELIMITER_SEMICOLON:
			index++;
		case DELIMITER_PARENTHESIS_RIGHT:
		case DELIMITER_BRACKET_RIGHT:
		case DELIMITER_BRACE_RIGHT:
			return true;
		default:
			parserError("Instruction can not begin with delimiter '%lld'.", start.value); // TODO: Revert.
			return false;
		}
		break;
	case TOKEN_KEYWORD:
		switch (start.value)
		{
		case KEYWORD_IF:
			validate = parse_conditionBranch(root);
			break;
		case KEYWORD_FOR:
		case KEYWORD_WHILE:
			validate = build_loop(root);
			break;
			//case KEYWORD_SWITCH:
			//	return ERROR_SYNTAX_ERROR;
			//	break;
		case KEYWORD_BREAK:
			index++;
			root = new Break{};
			break;
		case KEYWORD_CONTINUE:
			index++;
			root = new Continue{};
			break;
		case KEYWORD_FUNCTION:
			validate = parse_operation(root);
			break;
		case KEYWORD_RETURN:
			{
				index++;
				Expression* returned = nullptr;
				validate = parse_operation(returned);
				if (returned == nullptr) {
					returned = (Expression*)(&VALUE_NULL_NULL);
				}
				root = new Return{ returned };
			}
			break;
		default:
			parserError("Instruction can not begin with keyword '%lld'.", start.value);
			return false;
		}
		break;
	default:
		parserError("Instruction can not begin with token of type '%lld'.", start.type_);
		return false;
	}

	if (!validate) {
		return false;
	}
	else if (root == nullptr) {
		parserError("Intruction can not be NULL.");
		return false;
	}
	else if (root->exp & terminated & LANGUAGE_MASK)
	{
		// Walk to the right-most leaf and check if it is not a function expression.
		Expression* leaf = root;
		while (leaf != nullptr) {
			if (leaf->exp == EXPRESSION_UNARY) {
				Unary* e = static_cast<Unary*>(leaf);
				leaf = e->child;
			}
			else if (leaf->exp == EXPRESSION_BINARY) {
				Binary* e = static_cast<Binary*>(leaf);
				leaf = e->right;
			}
			//else if (leaf->exp == EXPRESSION_TERNARY) {
			//	Ternary* e = static_cast<Ternary*>(leaf);
			//	leaf = e->no;
			//}
			else if (leaf->exp == EXPRESSION_RETURN) {
				Return* e = static_cast<Return*>(leaf);
				leaf = e->val;
			}
			else {
				break;
			}
		}
		if (leaf->exp != EXPRESSION_FUNCTION) {
			Token end;
			if (!tokenRetrieve(end)) {
				parserError("End of file reached before instruction was terminated.");
				return false;
			}
			if (end.type_ != TOKEN_DELIMITER || end.value != DELIMITER_SEMICOLON) {
				parserError("Instruction must terminate with a semicolon.");
				return false;
			}
			index++;
		}
	}
	return true;
}

bool Parser::build_program(std::vector<Expression*>& program)
{
	program.clear();
	Token start;
	while (tokenRetrieve(start))
	{
		if (start.type_ == TOKEN_DELIMITER && (start.value & (DELIMITER_PARENTHESIS_RIGHT | DELIMITER_BRACKET_RIGHT | DELIMITER_BRACE_RIGHT) & LANGUAGE_MASK)) {
			break;
		}

		Expression* root = nullptr;
		if (!parse_instruction(root)) {
			return false;
		}
		if (root != nullptr) {
			program.push_back(root);
		}
	}
	return true;
}

bool Parser::build_file(std::vector<Expression*>& program)
{
	program.clear();
	if (!build_program(program))
	{
		parserError("Parse FAILED.");
		return false;
	}

	Token end;
	if (tokenRetrieve(end)) {
		parserError("build_file: tokens remaining after program finished building.");
		return false;
	}
	if (program.size() <= 0) {
		printWarning("build_file: Program is empty.");
	}
	return true;
}
