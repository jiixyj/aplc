#include <iostream>
#include "node.h"
#include "parser.cpp.h"

extern ExpressionList* file;
extern int yyparse();
extern int yylex();

int main(int argc, char **argv)
{
    bool test_lexer = (argc == 2) ? std::string("true") == argv[1] : false;
    if (test_lexer) {
        int next_token;
        while ((next_token = yylex())) {
            switch (next_token) {
              case TINTEGER:
                std::cout << "Integer " << *yylval.string << std::endl; break;
              case TSTRING:
                std::cout << "String " << *yylval.string << std::endl; break;
              case TIDENTIFIER:
                std::cout << "Identifier " << *yylval.string << std::endl; break;
              default:
                std::cout << "Token " << next_token << std::endl; break;
            }
        }
    } else {
        yyparse();
        if (file) {
            std::stringstream ss;
            for (size_t i = 0; i < file->size(); ++i) {
                (*file)[i]->print(ss);
            }
            std::cout << ss.str();
        }
    }
    return 0;
}
