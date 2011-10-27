#include <unicode/utf8.h>
#include <unicode/uchar.h>

#include <llvm/Value.h>
#include <llvm/Module.h>
#include <llvm/Support/IRBuilder.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <cstdio>
#include <cstdlib>

static int indent_step = 2;
static int indent;

// input file
static std::string file;

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
    return c == '?' || c == ':' || c == '=' || c == U'\u2192' ||
           c == '|' || c == '^' || c == '&' || c == '!' ||
           c == '<' || c == '>' || c == '+' || c == '-' ||
           c == '*' || c == '/' || c == '%' || c == '_' ||
           c == ' ' || c == ',' || c == '(' || c == ')' ||
           c == '[' || c == ']' || c == '"';
}

#define NEXT_CHAR(c) do { \
    ip = i; \
    if (i >= length) { \
        std::cerr << "End of file reached in line " << __LINE__ << std::endl; \
        exit(EXIT_FAILURE); \
    } \
    U8_NEXT(s, i, length, c); \
    if (c < 0) { \
        std::cerr << "File encoding error" << std::endl; \
        exit(EXIT_FAILURE); \
    } \
} while (0)

static int32_t gettok()
{
    const char *s = file.c_str();
    static int32_t i, ip;
    int32_t length = int32_t(file.size());
    static UChar32 c;

    // read first character
    if (!i && length) NEXT_CHAR(c);

    // skip line breaks
    while (i < length && c == '\n') {
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
            if (i >= length) {
                std::cerr << "End of file reached while parsing string literal" << std::endl;
                ::exit(EXIT_FAILURE);
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
        do {
            ss.write(&s[ip], i - ip);
            NEXT_CHAR(c);
        } while (!is_special(c) && i < length);
        identifier_value = ss.str();
        return tok_identifier;
    }
}


static void print_space(int num)
{
    for (int i = 0; i < num; ++i) {
        std::cout << " ";
    }
}

class ExprAST {
public:
    virtual ~ExprAST();
    virtual llvm::Value *Codegen() = 0;
    virtual void print_node() = 0;
};
ExprAST::~ExprAST() {}

// Atoms
class IntegerExprAST : public ExprAST {
    long integer_;
public:
    IntegerExprAST(long integer) : integer_(integer) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "IntegerExprAST: " << integer_ << std::endl;
    }
};

class StringExprAST : public ExprAST {
    std::string string_;
public:
    StringExprAST(const std::string &string) : string_(string) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "StringExprAST: " << string_ << std::endl;
    }
};

class IdentifierExprAST : public ExprAST {
    std::string string_;
public:
    IdentifierExprAST(const std::string &string) : string_(string) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "IdentifierExprAST: " << string_ << std::endl;
    }
};

// Functions
class FuncTypeExprAST : public ExprAST {
    ExprAST *atom_;
    ExprAST *func_type_;  // optional
public:
    FuncTypeExprAST(ExprAST *atom, ExprAST *func_type)
        : atom_(atom), func_type_(func_type) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "FuncTypeExprAST:" << std::endl;
        indent += indent_step;
        atom_->print_node();
        if (func_type_) func_type_->print_node();
        indent -= indent_step;
    }
};

class ApplyExprAST : public ExprAST {
    ExprAST *func_type_;  // optional
    ExprAST *apply_;
public:
    ApplyExprAST(ExprAST *func_type, ExprAST *apply)
        : func_type_(func_type), apply_(apply) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "ApplyExprAST:" << std::endl;
        indent += indent_step;
        func_type_->print_node();
        if (apply_) apply_->print_node();
        indent -= indent_step;
    }
};

class UnaryExprAST : public ExprAST {
    bool underscore_;
    ExprAST *apply_;
public:
    UnaryExprAST(bool underscore, ExprAST *apply)
        : underscore_(underscore), apply_(apply) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "UnaryExprAST: " << (underscore_ ? "_" : "no_") << std::endl;
        indent += indent_step;
        if (apply_) apply_->print_node();
        indent -= indent_step;
    }
};

class MulExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    MulExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "MulExprAST: " << char(opcode_) << std::endl;
        indent += indent_step;
        if (lhs_) lhs_->print_node();
        if (rhs_) rhs_->print_node();
        indent -= indent_step;
    }
};

class AddExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    AddExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "AddExprAST: " << char(opcode_) << std::endl;
        indent += indent_step;
        if (lhs_) lhs_->print_node();
        if (rhs_) rhs_->print_node();
        indent -= indent_step;
    }
};

class BitExprAST : public ExprAST {
    int opcode_;
    bool signshift_;
    ExprAST *lhs_, *rhs_;
public:
    BitExprAST(int opcode, bool signshift, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), signshift_(signshift), lhs_(lhs), rhs_(rhs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "BitExprAST: " << char(opcode_) << (signshift_ ? ">" : "") << std::endl;
        indent += indent_step;
        if (lhs_) lhs_->print_node();
        if (rhs_) rhs_->print_node();
        indent -= indent_step;
    }
};

class CompLTExprAST : public ExprAST {
    int opcode_;
    bool equal_;
    ExprAST *lhs_, *rhs_;
public:
    CompLTExprAST(int opcode, bool equal, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), equal_(equal), lhs_(lhs), rhs_(rhs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "CompLTExprAST: " << char(opcode_) << (equal_ ? "=" : "") << std::endl;
        indent += indent_step;
        if (lhs_) lhs_->print_node();
        if (rhs_) rhs_->print_node();
        indent -= indent_step;
    }
};

class CompEQExprAST : public ExprAST {
    int opcode_;
    ExprAST *lhs_, *rhs_;
public:
    CompEQExprAST(int opcode, ExprAST *lhs, ExprAST *rhs)
        : opcode_(opcode), lhs_(lhs), rhs_(rhs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "CompEQExprAST: " << char(opcode_) << (opcode_ ? "=" : "") << std::endl;
        indent += indent_step;
        if (lhs_) lhs_->print_node();
        if (rhs_) rhs_->print_node();
        indent -= indent_step;
    }
};

class LambdaExprAST : public ExprAST {
    ExprAST *tuple_, *func_type_, *logical_or_;
public:
    LambdaExprAST(ExprAST *tuple, ExprAST *func_type, ExprAST *logical_or)
        : tuple_(tuple), func_type_(func_type), logical_or_(logical_or) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "LambdaExprAST:" << std::endl;
        indent += indent_step;
        if (tuple_) tuple_->print_node();
        if (func_type_) func_type_->print_node();
        if (logical_or_) logical_or_->print_node();
        indent -= indent_step;
    }
};

class AssignExprAST : public ExprAST {
    ExprAST *lambda_, *lambda2_;
public:
    AssignExprAST(ExprAST *lambda, ExprAST *lambda2)
        : lambda_(lambda), lambda2_(lambda2) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "AssignExprAST:" << std::endl;
        indent += indent_step;
        if (lambda_) lambda_->print_node();
        if (lambda2_) lambda2_->print_node();
        indent -= indent_step;
    }
};

class ControlExprAST : public ExprAST {
    bool terniary_;
    ExprAST *expr1_, *expr2_, *expr3_;
public:
    ControlExprAST(bool terniary, ExprAST *expr1, ExprAST *expr2, ExprAST *expr3)
        : terniary_(terniary), expr1_(expr1), expr2_(expr2), expr3_(expr3) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "ControlExprAST: " << "tern " << (terniary_ ? "true" : "false") << std::endl;
        indent += indent_step;
        if (expr1_) expr1_->print_node();
        if (expr2_) expr2_->print_node();
        if (expr3_) expr3_->print_node();
        indent -= indent_step;
    }
};

class ListExprAST : public ExprAST {
    std::vector<ExprAST *> exprs_;
public:
    ListExprAST(const std::vector<ExprAST *> &exprs)
        : exprs_(exprs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "ListExprAST:" << std::endl;
        indent += indent_step;
        for (size_t i = 0; i < exprs_.size(); ++i) {
            if (exprs_[i]) exprs_[i]->print_node();
        }
        indent -= indent_step;
    }
};

class FileExprAST : public ExprAST {
    std::vector<ExprAST *> exprs_;
public:
    FileExprAST(const std::vector<ExprAST *> &exprs)
        : exprs_(exprs) {}
    virtual llvm::Value *Codegen();
    void print_node() {
        print_space(indent);
        std::cout << "FileExprAST: size " << exprs_.size() << std::endl;
        indent += indent_step;
        for (size_t i = 0; i < exprs_.size(); ++i) {
            if (exprs_[i]) exprs_[i]->print_node();
        }
        indent -= indent_step;
    }
};

static ExprAST *error(const char *str);
static llvm::Value *errorv(const char *str) { error(str); return 0; }

static llvm::Module *TheModule;
static llvm::IRBuilder<> Builder(llvm::getGlobalContext());
static std::map<std::string, llvm::Value *> NamedValues;

llvm::Value *IntegerExprAST::Codegen() {
  return llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, uint64_t(integer_), true));
}

llvm::Value *IdentifierExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *FuncTypeExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *StringExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *ApplyExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *UnaryExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *MulExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *AddExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *BitExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *CompLTExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *CompEQExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *LambdaExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *ListExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *AssignExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *ControlExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

llvm::Value *FileExprAST::Codegen() {
    /* FIXME */
  return NULL;
}

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
        if (cur_tok != U'\u2192')
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
        if (cur_tok == '?') {
            terniary = true;
            get_next_token();
        }
    }
    if (expr1 && !terniary) {
        return new ControlExprAST(false, expr1, expr2, expr3);
    }
    if (!expr1) {
        expr1 = parse_assign();
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

static ExprAST *parse_file() {
    std::vector<ExprAST *> exprs;
    ExprAST *expr = NULL;
    while (cur_tok != tok_eof && (expr = parse_expr())) {
        exprs.push_back(expr);
    }
    if (exprs.size()) {
        return new FileExprAST(exprs);
    } else {
        return error("Parse error");
    }
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
    if (cur_tok == U'\u2192') {
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
        apply = parse_apply();
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

    bool test_lexer = false;
    if (!test_lexer) {
        get_next_token();
        // driver();
        ExprAST *ast = parse_file();
        if (ast) ast->print_node();
    } else {
        int token;
        while ((token = gettok()) != tok_eof) {
            if (token == tok_integer)
                std::cout << "Integer: " << integer_value << std::endl;
            else if (token == tok_string)
                std::cout << "String: " << string_value << std::endl;
            else if (token == tok_identifier)
                std::cout << "Identifier: " << identifier_value << std::endl;
            else
                std::cout << "Char: " << token << "/" << char(token) << std::endl;
        }
    }
    return 0;
}
