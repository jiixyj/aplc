#include "unicode/utf8.h"
#include "unicode/uchar.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <cstdlib>

// input file
static const uint8_t *s;

enum Token {
    tok_eof = -1,
    tok_integer = -2,
    tok_string = -3,
    tok_identifier = -4
};

static long integer_value;
static std::string string_value;
static std::string identifier_value;

static bool is_special(UChar32 c)
{
    return c == '?' || c == ':' || c == '=' || c == '\u2192' ||
           c == '|' || c == '^' || c == '&' || c == '!' ||
           c == '<' || c == '>' || c == '+' || c == '-' ||
           c == '*' || c == '/' || c == '%' || c == '_' ||
           c == ' ' || c == ',' || c == '(' || c == ')' ||
           c == '[' || c == ']' || c == '"';
}

#define NEXT_CHAR(c) do { \
    ip = i; \
    U8_NEXT(s, i, length, c); \
    if (c < 0) std::cerr << "File encoding error" << std::endl; \
} while (0)

static int32_t gettok(const std::string &file)
{
    const char *s = file.c_str();
    static int32_t i, ip;
    size_t length = file.size();
    static UChar32 c;

    // read first character
    if (!i && length) NEXT_CHAR(c);

    // skip line breaks
    while (c == '\n') {
        NEXT_CHAR(c);
    }

    if (i >= length) {
        return tok_eof;
    } else if (c <= 0x7f && u_isdigit(c)) {
        std::stringstream ss;
        do {
            ss.write(&s[ip], i - ip);
            NEXT_CHAR(c);
        } while (c <= 0x7f && u_isdigit(c));
        ss >> integer_value;
        return tok_integer;
    } else if (c == '\"') {
        std::stringstream ss;
        NEXT_CHAR(c);
        while (c != '\"') {
            if (c == '\\') {
                UChar32 new_char;
                NEXT_CHAR(new_char);
                switch (new_char) {
                  case '\\':
                    ss << '\\'; break;
                  case '"':
                    ss << '\"'; break;
                  case 'n':
                    ss << '\n'; break;
                  case 't':
                    ss << '\t'; break;
                  default:
                    std::cerr << "error parsing string literal" << std::endl;
                    ::exit(EXIT_FAILURE);
                }
            } else {
                ss.write(&s[ip], i - ip);
            }
            NEXT_CHAR(c);
        }
        // read closing "
        NEXT_CHAR(c);
        string_value = ss.str();
        return tok_string;
    } else if (is_special(c)) {
        int this_char = c;
        NEXT_CHAR(c);
        return this_char;
    } else {
        std::stringstream ss;
        ss.write(&s[ip], i - ip);
        NEXT_CHAR(c);
        while (!is_special(c)) {
            ss.write(&s[ip], i - ip);
            NEXT_CHAR(c);
        }
        identifier_value = ss.str();
        return tok_identifier;
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        std::cerr << "No input file given" << std::endl;
        return EXIT_FAILURE;
    }
    std::ifstream in(argv[1]);
    std::string file = static_cast<std::stringstream const&>(std::stringstream() << in.rdbuf()).str();

    int token;
    while ((token = gettok(file)) != tok_eof) {
        if (token == tok_integer)
            std::cout << "Integer: " << integer_value << std::endl;
        else if (token == tok_string)
            std::cout << "String: " << string_value << std::endl;
        else if (token == tok_identifier)
            std::cout << "Identifier: " << identifier_value << std::endl;
        else
            std::cout << "Char: " << token << "/" << char(token) << std::endl;
    }
    return 0;
}
