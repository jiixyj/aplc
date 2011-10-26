#include "unicode/utf8.h"
#include "unicode/uchar.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstdio>
#include <cstdlib>

// input file
static std::string file;
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

static int32_t gettok()
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

class ExprAST {
public:
    virtual ~ExprAST() {}
};

// Atoms
class AtomExprAST : public ExprAST {};

class IntegerExprAST : public AtomExprAST {
    long integer_;
public:
    IntegerExprAST(long integer) : integer_(integer) {}
};

class StringExprAST : public AtomExprAST {
    std::string string_;
public:
    StringExprAST(const std::string &string) : string_(string) {}
};

class IdentifierExprAST : public AtomExprAST {
    std::string string_;
public:
    IdentifierExprAST(const std::string &string) : string_(string) {}
};

// Functions
class FuncTypeExprAST : public ExprAST {
    ExprAST *atom_;
    ExprAST *func_type_;  // optional
public:
    FuncTypeExprAST(ExprAST *atom, ExprAST *func_type)
        : atom_(atom), func_type_(func_type) {}
};

class ApplyExprAST : public ExprAST {
    ExprAST *func_type_;  // optional
    ExprAST *apply_;
public:
    ApplyExprAST(ExprAST *func_type, ExprAST *apply)
        : func_type_(func_type), apply_(apply) {}
};

class UnaryExprAST : public ExprAST {
    bool underscore_;
    ExprAST *apply_;
public:
    UnaryExprAST(bool underscore, ExprAST *apply)
        : underscore_(underscore), apply_(apply) {}
};

class MulExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    MulExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
};

class AddExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    AddExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
};

class BitExprAST : public ExprAST {
    int opcode_;
    bool signshift_;
    ExprAST *lhs_, *rhs_;
public:
    BitExprAST(int opcode, bool signshift, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), signshift_(signshift), lhs_(lhs), rhs_(rhs) {}
};

class CompLTExprAST : public ExprAST {
    int opcode_;
    bool equal_;
    ExprAST *lhs_, *rhs_;
public:
    CompLTExprAST(int opcode, bool equal, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), equal_(equal), lhs_(lhs), rhs_(rhs) {}
};

class CompEQExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    CompEQExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
};

class LambdaExprAST : public ExprAST {
    ExprAST *tuple_, *func_type_, *logical_or_;
public:
    LambdaExprAST(ExprAST *tuple, ExprAST *func_type, ExprAST *logical_or)
        : tuple_(tuple), func_type_(func_type), logical_or_(logical_or) {}
};

class AssignExprAST : public ExprAST {
    ExprAST *lambda_, *lambda2_;
public:
    AssignExprAST(ExprAST *lambda, ExprAST *lambda2)
        : lambda_(lambda), lambda2_(lambda2) {}
};

class ControlExprAST : public ExprAST {
    bool terniary_;
    ExprAST *expr1_, *expr2_, *expr3_;
public:
    ControlExprAST(bool terniary, ExprAST *expr1, ExprAST *expr2, ExprAST *expr3)
        : terniary_(terniary), expr1_(expr1), expr2_(expr2), expr3_(expr3) {}
};

class ListExprAST : public ExprAST {
    std::vector<ExprAST *> exprs_;
public:
    ListExprAST(const std::vector<ExprAST *> &exprs)
        : exprs_(exprs) {}
};

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int cur_tok;
static int get_next_token()
{
    return cur_tok = gettok();
}
/// Error* - These are little helper functions for error handling.
static ExprAST *error(const char *str)
{
    fprintf(stderr, "Error: %s\n", str);
    return 0;
}

static ExprAST *parse_expr();
static ExprAST *parse_tuple();
static ExprAST *parse_func_type();
static ExprAST *parse_comp_eq();

/// expr_atom ::= INTEGER
static ExprAST *parse_atom() {
    ExprAST *result = NULL;
    switch (cur_tok) {
      case tok_integer:
        result = new IntegerExprAST(integer_value); break;
      case tok_string:
        result = new StringExprAST(string_value); break;
      case tok_identifier:
        result = new IdentifierExprAST(identifier_value); break;
      default:
        fprintf(stderr, "tok: %d\n", cur_tok);
        return error("Error parsing atom");
    }
    get_next_token();
    return result;
}

static ExprAST *parse_lambda() {
    ExprAST *tuple = NULL, *func_type = NULL, *logical_or = NULL;
    if (cur_tok == '(') {
        tuple = parse_tuple();
        if (!tuple) return NULL;
        if (cur_tok != '\u2192')
            return error("arrow expected");
        get_next_token();
        func_type = parse_func_type();
        if (!func_type) return NULL;
    } else {
        /* FIXME */
        logical_or = parse_comp_eq();
        if (!logical_or) return NULL;
    }
    return new LambdaExprAST(tuple, func_type, logical_or);
}


static ExprAST *parse_assign() {
    ExprAST *lambda = parse_lambda(), *lambda2 = NULL;
    if (!lambda) return NULL;
    if (cur_tok == '=') {
        get_next_token();
        lambda2 = parse_lambda();
        if (!lambda2) return NULL;
    }
    return new AssignExprAST(lambda, lambda2);
}

static ExprAST *parse_control() {
    ExprAST *expr1 = NULL, *expr2 = NULL, *expr3 = NULL;
    bool terniary = false;
    if (cur_tok == '(' || cur_tok == tok_integer
                       || cur_tok == tok_string
                       || cur_tok == tok_identifier) {
        expr1 = parse_assign();
        if (!expr1) return NULL;
        if (cur_tok == '?')
            terniary = true;
        get_next_token();
    }
    if (expr1 && !terniary) {
        return new ControlExprAST(false, expr1, expr2, expr3);
    }
    if (!expr1) {
        expr1 = parse_expr();
        if (!expr1) return NULL;
        if (cur_tok != '?')
            return error("expected ternary operator");
        get_next_token();
    }
    expr2 = parse_expr();
    if (!expr2) return NULL;
    if (cur_tok != ':')
        return error("expected : from ternary operator");
    get_next_token();
    expr3 = parse_expr();
    if (!expr3) return NULL;

    return new ControlExprAST(true, expr1, expr2, expr3);
}

static ExprAST *parse_expr() {
    return parse_control();
}

static ExprAST *parse_list() {
    std::vector<ExprAST *> exprs;
    ExprAST *expr = parse_expr();
    if (!expr) return NULL;
    exprs.push_back(expr);

    while (cur_tok == ',') {
        get_next_token();
        expr = parse_expr();
        if (!expr) return NULL;
        exprs.push_back(expr);
    }
    return new ListExprAST(exprs);
}

static ExprAST *parse_array() {
    get_next_token();  // eat [
    ExprAST *list = parse_list();
    if (!list) return NULL;
    if (cur_tok != ']') {
        return error("no terminating ]");
    }
    get_next_token();
    return list;
}

static ExprAST *parse_tuple() {
    get_next_token();  // eat [
    ExprAST *list = parse_list();
    if (!list) return NULL;
    if (cur_tok != ')') {
        return error("no terminating )");
    }
    get_next_token();
    return list;
}


static ExprAST *parse_func_type() {
    ExprAST *atom = parse_atom(), *func_type = NULL;
    if (!atom) return NULL;
    if (cur_tok == '\u2192') {
        get_next_token();
        func_type = parse_func_type();
    }
    return new FuncTypeExprAST(atom, func_type);
}

static ExprAST *parse_apply() {
    ExprAST *func_type = parse_func_type(), *apply = NULL;
    if (!func_type) return NULL;
    if (cur_tok == ' ') {
        get_next_token();
        func_type = parse_apply();
    }
    return new ApplyExprAST(func_type, apply);
}

static ExprAST *parse_unary() {
    bool underscore = false;

    if (cur_tok == '_') {
        underscore = true;
        get_next_token();
    }
    ExprAST *apply = parse_apply();
    if (!apply) return NULL;
    return new UnaryExprAST(underscore, apply);
}

static ExprAST *parse_mul() {
    int opcode = 0;
    ExprAST *lhs = parse_unary(), *rhs = NULL;
    if (!lhs) return NULL;

    if (cur_tok == '*' || cur_tok == '/' || cur_tok == '%') {
        opcode = cur_tok;
        get_next_token();
        rhs = parse_unary();
        if (!rhs) return NULL;
    }
    return new MulExprAST(opcode, lhs, rhs);
}

static ExprAST *parse_add() {
    int opcode = 0;
    ExprAST *lhs = parse_mul(), *rhs = NULL;
    if (!lhs) return NULL;

    if (cur_tok == '+' || cur_tok == '-') {
        opcode = cur_tok;
        get_next_token();
        rhs = parse_mul();
        if (!rhs) return NULL;
    }
    return new AddExprAST(opcode, lhs, rhs);
}

static ExprAST *parse_bit() {
    int opcode = 0;
    bool signshift = false;
    ExprAST *lhs = parse_add(), *rhs = NULL;
    if (!lhs) return NULL;

    if (cur_tok == '|' || cur_tok == '^' || cur_tok == '&') {
        opcode = cur_tok;
        get_next_token();
        rhs = parse_add();
        if (!rhs) return NULL;
    } else if (cur_tok == '<' || cur_tok == '>') {
        opcode = cur_tok;
        get_next_token();
        if (cur_tok != opcode)
            return error("invalid operator");
        get_next_token();
        if (opcode == '>' && cur_tok == '>') {
            signshift = true;
            get_next_token();
        }
        rhs = parse_add();
        if (!rhs) return NULL;
    }

    return new BitExprAST(opcode, signshift, lhs, rhs);
}

static ExprAST *parse_comp_lt() {
    int opcode = 0;
    bool equal = false;
    ExprAST *lhs = parse_bit(), *rhs = NULL;
    if (!lhs) return NULL;

    if (cur_tok == '<' || cur_tok == '>') {
        opcode = cur_tok;
        get_next_token();
        if (cur_tok == '=') {
            equal = true;
            get_next_token();
        }
        rhs = parse_bit();
        if (!rhs) return NULL;
    }

    return new CompLTExprAST(opcode, equal, lhs, rhs);
}

static ExprAST *parse_comp_eq() {
    int opcode = 0;
    ExprAST *lhs = parse_comp_lt(), *rhs = NULL;
    if (!lhs) return NULL;

    if (cur_tok == '=' || cur_tok == '!') {
        opcode = cur_tok;
        get_next_token();
        if (cur_tok != '=') {
            return NULL;
        }
        get_next_token();
        rhs = parse_bit();
        if (!rhs) return NULL;
    }

    return new CompEQExprAST(opcode, lhs, rhs);
}

static void driver() {
    while (1) {
        fprintf(stderr, "ready> ");
        switch (cur_tok) {
          case tok_eof:    return;
          case ';':        get_next_token(); break;  // ignore top-level semicolons.
          default:         parse_expr(); break;
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        std::cerr << "No input file given" << std::endl;
        return EXIT_FAILURE;
    }
    std::ifstream in(argv[1]);
    file = static_cast<const std::stringstream &>(std::stringstream() << in.rdbuf()).str();

    get_next_token();
    // driver();
    parse_expr();

    // lexer test
    // int token;
    // while ((token = gettok()) != tok_eof) {
    //     if (token == tok_integer)
    //         std::cout << "Integer: " << integer_value << std::endl;
    //     else if (token == tok_string)
    //         std::cout << "String: " << string_value << std::endl;
    //     else if (token == tok_identifier)
    //         std::cout << "Identifier: " << identifier_value << std::endl;
    //     else
    //         std::cout << "Char: " << token << "/" << char(token) << std::endl;
    // }
    return 0;
}
