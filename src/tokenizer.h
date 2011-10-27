#ifndef TOKENIZER_H
#define TOKENIZER_H

#include <iostream>
#include <fstream>

#include "utf8.h"

typedef uint64_t int_t;

enum TokenType {
    TOK_CHAR,
    TOK_EOF,
    TOK_INTEGER,
    TOK_STRING,
    TOK_IDENTIFIER
};

template<typename T>
struct TokenVal;

struct Token {
    Token();
    Token(TokenType type_);
    virtual ~Token();
    TokenType type;
    template<typename T> T val() {
        TokenVal<T> *tok = dynamic_cast<TokenVal<T> *>(this);
        if (!tok) std::cerr << "dynamic cast error" << std::endl;
        return tok->t;
    }
    uint32_t c();
};

template<typename T>
struct TokenVal : public Token {
    TokenVal(TokenType type, T t);
    T t;
};

struct TokenEOF : public Token {
    TokenEOF();
};

class Tokenizer {
  public:
    Tokenizer();
    Tokenizer(const char *fn);
    ~Tokenizer();

    bool ok() const;
    uint32_t next();
    void get_next_token();
    bool is_special(uint32_t c);

    Token *cur_tok;
  private:
    uint32_t c;
    std::ifstream f;
    std::istream &in;
    std::istream_iterator<char> it, eos;
};

#endif /* end of include guard: TOKENIZER_H */
