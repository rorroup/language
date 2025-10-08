#include "common.h"
#include "Lexer.h"
#include "Parser.h"
#include "Interpreter.h"
#include "Builtin.h"

int main()
{
    BUILD_BUG_ON(sizeof(void*) < 4);
    //BUILD_BUG_ON(sizeof(void*) < sizeof(float));
    //BUILD_BUG_ON(sizeof(intptr_t) < sizeof(float));

    const char filename[] = "test.txt";
    const char* source = readfile(filename);
    if (source == nullptr)
    {
        printError("Failed to read file : '%s'.", filename);
        return 0;
    }
    printInfo("Loaded source File: '%s'.", filename);
    printDebug("Contents: (length: %llu)\n%s", strlen(source), source);

    Parser myParser{};
    if (!tokenize_source(source, myParser.tokens)) {
        myParser.tokens.clear(); // TODO: delete dynamically allocated memory.
        return 0;
    }
    printInfo("Tokens: %llu.", myParser.tokens.size());

    for (int i = 0; i < myParser.tokens.size(); i++) {
        switch (myParser.tokens[i].type_)
        {
        case TOKEN_VALUE:
        {
            Value* v = (Value*)(myParser.tokens[i].value);
            switch (v->type_)
            {
            case VALUE_BOOL:
                printf("BOOL(%s)\n", (bool)v->value ? "true" : "false");
                break;
            case VALUE_INT:
                printf("INT(%ld)\n", (int)v->value);
                break;
            case VALUE_FLOAT:
            {
                float f;
                memcpy(&f, &v->value, sizeof(float));
                printf("FLOAT(%f)\n", f);
            }
                break;
            case VALUE_STRING:
                printf("STR'%s'\n", (char*)v->value);
                break;
            default:
                printError("Unknown Value.");
                break;
            }
        }
        break;
        case TOKEN_IDENTIFIER:
            printf("VAR<%s>\n", (char*)(myParser.tokens[i].value));
            break;
        case TOKEN_OPERATOR:
            printf("OP<%llu>\n", myParser.tokens[i].value);
            break;
        case TOKEN_DELIMITER:
            printf("DEL<%llu>\n", myParser.tokens[i].value);
            break;
        case TOKEN_KEYWORD:
            printf("KW<%llu>\n", myParser.tokens[i].value);
            break;
        default:
            printError("Unknown Token.");
            break;
        }
    }

    printInfo("Parsing:");
    std::vector<Expression*> program;
    if (myParser.build_file(program)) {
        register_function();

        printInfo("Evaluating: %llu instructions.", program.size());
        for (int i = 0; i < program.size(); i++) {
            if (program[i] != nullptr)
            {
                Result ans = Result{};
                s_lang error_ = program[i]->evaluate(ans);
                if (error_ == ERROR_NONE)
                {
                    if ((ans.type_ & LANGUAGE_VALUE) && ans.type_ != VALUE_NULL) // TODO: change for Result.
                    {
                        Value val;
                        val.type_ = ans.type_;
                        val.value = (t_value)ans.value;
                        printInfo("%s", val.asStr());
                    }
                }
                else
                {
                    printError("Evaluation error: %llu on instruction %d.", error_, i);
                    break;
                }
            }
        }
    }

    return 0;
}
