#include "tokenizer.h"

#include <iostream>
#include <sstream>

Token::Token() {}
Token::Token(TokenType type_) : type(type_) {}

uint32_t Token::c() {
    TokenVal<uint32_t> *tok = dynamic_cast<TokenVal<uint32_t> *>(this);
    if (tok) return tok->t;
    else return uint32_t(-1);
}

Token::~Token() {}

template<typename T>
TokenVal<T>::TokenVal(TokenType type, T _t) : Token(type), t(_t) {}

TokenEOF::TokenEOF() : Token(TOK_EOF) {}

Tokenizer::Tokenizer()
        : cur_tok(NULL), f(), in(std::cin),
          it(in >> std::noskipws), eos() {
    if (ok()) c = next();
}
Tokenizer::Tokenizer(const char *fn)
        : cur_tok(NULL), f(fn), in(f),
          it(in >> std::noskipws), eos() {
    if (ok()) c = next();
}

Tokenizer::~Tokenizer() {
    delete cur_tok;
}

bool Tokenizer::ok() const {
    return bool(in);
}

uint32_t Tokenizer::next() {
    try {
        return utf8::next(it, eos);
    } catch (...) {
        std::cerr << "Invalid input encoding" << std::endl;
        exit(EXIT_FAILURE);
        // ++it;
    }
    // return err_val;
}

void Tokenizer::get_next_token() {
    if (!ok()) {
        delete cur_tok; cur_tok = new TokenEOF();
    } else if (c <= 0x7f && ::isdigit(int(c))) {
        int_t result;
        std::stringstream ss;
        do {
            ss << char(c);
            c = next();
        } while (c <= 0x7f && ::isdigit(int(c)));
        ss >> result;
        delete cur_tok; cur_tok = new TokenVal<int_t>(TOK_INTEGER, result);
    } else if (c == '\"') {
        std::stringstream ss;
        c = next();
        while (c != '\"') {
            if (c == '\\') {
                uint32_t new_char;
                new_char = next();
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
                std::string s;
                utf8::utf32to8(&c, &c + 1, std::back_inserter(s));
                ss << s;
            }
            if (!ok()) {
                std::cerr << "End of file reached while parsing string literal" << std::endl;
                ::exit(EXIT_FAILURE);
            }
            c = next();
        }
        // eat closing "
        c = next();
        delete cur_tok; cur_tok = new TokenVal<std::string>(TOK_STRING, ss.str());
    } else if (is_special(c)) {
        uint32_t this_char = c;
        c = next();
        delete cur_tok; cur_tok = new TokenVal<uint32_t>(TOK_CHAR, this_char);
    } else {
        std::stringstream ss;
        do {
            std::string s;
            utf8::utf32to8(&c, &c + 1, std::back_inserter(s));
            ss << s;
            c = next();
        } while (!is_special(c) && ok());
        delete cur_tok; cur_tok = new TokenVal<std::string>(TOK_IDENTIFIER, ss.str());
    }
}

bool Tokenizer::is_special(uint32_t cp) {
     return (cp <= 0x7f && ::isspace(int(cp))) ||
            cp == '?' || cp == ':' || cp == '=' || cp == 0x2192 ||
            cp == '|' || cp == '^' || cp == '&' || cp == '!' ||
            cp == '<' || cp == '>' || cp == '+' || cp == '-' ||
            cp == '*' || cp == '/' || cp == '%' || cp == '_' ||
            cp == ',' || cp == '(' || cp == ')' || cp == '[' ||
            cp == ']' || cp == '"';
}
