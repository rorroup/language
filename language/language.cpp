#include "common.h"
#include "Lexer.h"
#include "Parser.h"
#include "Builtin.h"

int main()
{
    //BUILD_BUG_ON(sizeof(void*) < 4);
    //BUILD_BUG_ON(sizeof(void*) < sizeof(float));
    //BUILD_BUG_ON(sizeof(intptr_t) < sizeof(float));

    register_function();

    const char filename[] = "test.txt";
    const char* source = readfile(filename);
    if (source == nullptr)
    {
        printError("Failed to read file : '%s'.", filename);
        return 0;
    }
    printInfo("Loaded source File: '%s'.", filename);
    printDebug("Contents: (length: %llu)\n%s", strlen(source), source);


    Parser parser{};
    parser.tokens.clear();
    if (!tokenize_source(source, parser.tokens)) {
        printError("Failed to tokenize or source had no tokens.");
        return 0;
    }

    printDebug("I found '%lld' tokens.", parser.tokens.size());
    for (const Token& token: parser.tokens) {
        printDebug("%hhd -> %d", token.type_, token.intu);
    }

    parser.tokenIndex = 0;
    std::vector<Function*> loaded{};
    if (!parser.parse(&loaded)) {
        printError("Parse Failed!");
        return 0;
    }

    printDebug("PROGRAM has '%llu' TOKENS.", loaded[0]->program.size());

    Thread exec{};
    exec.calling.push_back(Execution{ loaded[0], 0}); // Parsed File as Outermost function.
    if (run(exec) == SOLVE_OK) { // This is the same as thread.calling.empty();
        return 0;
    }
    printInfo(" ========== SECOND CALL ==========");
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);
    run(exec);

    return 0;
}
