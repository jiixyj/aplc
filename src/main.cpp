#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cerrno>

#include <unistd.h>

#include "node.h"
#include "parser.h"

extern ExpressionList* file;
extern int yyparse();
extern int yylex();
extern FILE *yyin;

void usage(char *arg)
{
    std::cerr << "Usage: " << arg << " [-t] <file>" << std::endl;
    std::exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
    int opt;
    bool output_tokens = false;

    while ((opt = getopt(argc, argv, "t")) != -1) {
        switch (opt) {
        case 't':
            output_tokens = true;
            break;
        default:
            usage(argv[0]);
        }
    }
    if (argc - optind != 1) { usage(argv[0]); }

    if (std::strcmp(argv[optind], "-")) {
        if (errno = 0, !(yyin = std::fopen(argv[optind], "r"))) {
            perror(argv[optind]);
            std::exit(EXIT_FAILURE);
        }
    }

    if (output_tokens) {
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

    if (errno = 0, std::fclose(yyin)) {
        perror(argv[optind]);
    }

    return 0;
}
