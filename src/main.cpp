#include <llvm/Value.h>
#include <llvm/Module.h>
#include <llvm/Support/IRBuilder.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>

#include "tokenizer.h"

static int indent_step = 2;
static int indent;
static Tokenizer *t;

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
    int_t integer_;
public:
    IntegerExprAST(int_t integer) : integer_(integer) {}
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
    switch (t->cur_tok->type) {
      case TOK_INTEGER:
        result = new IntegerExprAST(t->cur_tok->val<int_t>()); break;
      case TOK_STRING:
        result = new StringExprAST(t->cur_tok->val<std::string>()); break;
      case TOK_IDENTIFIER:
        result = new IdentifierExprAST(t->cur_tok->val<std::string>()); break;
      default:
        std::cerr << "tok: " << t->cur_tok->c() << std::endl;
        return error("Error parsing atom");
    }
    t->get_next_token();
    return result;
}

static ExprAST *parse_lambda() {
    ExprAST *tuple = NULL, *func_type = NULL, *logical_or = NULL;
    if (t->cur_tok->c() == '(') {
        tuple = parse_tuple();
        if (!tuple) return NULL;
        if (t->cur_tok->c() != 0x2192)
            return error("arrow expected");
        t->get_next_token();
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
    if (t->cur_tok->c() == '=') {
        t->get_next_token();
        lambda2 = parse_lambda();
        if (!lambda2) return NULL;
    }
    return new AssignExprAST(lambda, lambda2);
}

static ExprAST *parse_control() {
    ExprAST *expr1 = NULL, *expr2 = NULL, *expr3 = NULL;
    bool terniary = false;
    if (t->cur_tok->c() == '(' || t->cur_tok->type == TOK_INTEGER
                             || t->cur_tok->type == TOK_STRING
                             || t->cur_tok->type == TOK_IDENTIFIER) {
        expr1 = parse_assign();
        if (!expr1) return NULL;
        if (t->cur_tok->c() == '?') {
            terniary = true;
            t->get_next_token();
        }
    }
    if (expr1 && !terniary) {
        return new ControlExprAST(false, expr1, expr2, expr3);
    }
    if (!expr1) {
        expr1 = parse_assign();
        if (!expr1) return NULL;
        if (t->cur_tok->c() != '?')
            return error("expected ternary operator");
        t->get_next_token();
    }
    expr2 = parse_expr();
    if (!expr2) return NULL;
    if (t->cur_tok->c() != ':')
        return error("expected : from ternary operator");
    t->get_next_token();
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
    while (t->cur_tok->type != TOK_EOF && (expr = parse_expr())) {
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

    while (t->cur_tok->c() == ',') {
        t->get_next_token();
        expr = parse_expr();
        if (!expr) return NULL;
        exprs.push_back(expr);
    }
    return new ListExprAST(exprs);
}

static ExprAST *parse_array() {
    t->get_next_token();  // eat [
    ExprAST *list = parse_list();
    if (!list) return NULL;
    if (t->cur_tok->c() != ']') {
        return error("no terminating ]");
    }
    t->get_next_token();
    return list;
}

static ExprAST *parse_tuple() {
    t->get_next_token();  // eat [
    ExprAST *list = parse_list();
    if (!list) return NULL;
    if (t->cur_tok->c() != ')') {
        return error("no terminating )");
    }
    t->get_next_token();
    return list;
}


static ExprAST *parse_func_type() {
    ExprAST *atom = parse_atom(), *func_type = NULL;
    if (!atom) return NULL;
    if (t->cur_tok->c() == 0x2192) {
        t->get_next_token();
        func_type = parse_func_type();
    }
    return new FuncTypeExprAST(atom, func_type);
}

static ExprAST *parse_apply() {
    ExprAST *func_type = parse_func_type(), *apply = NULL;
    if (!func_type) return NULL;
    if (t->cur_tok->c() == ' ') {
        t->get_next_token();
        apply = parse_apply();
    }
    return new ApplyExprAST(func_type, apply);
}

static ExprAST *parse_unary() {
    bool underscore = false;

    if (t->cur_tok->c() == '_') {
        underscore = true;
        t->get_next_token();
    }
    ExprAST *apply = parse_apply();
    if (!apply) return NULL;
    return new UnaryExprAST(underscore, apply);
}

static ExprAST *parse_mul() {
    int opcode = 0;
    ExprAST *lhs = parse_unary(), *rhs = NULL;
    if (!lhs) return NULL;

    if (t->cur_tok->c() == '*' || t->cur_tok->c() == '/' || t->cur_tok->c() == '%') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
        rhs = parse_unary();
        if (!rhs) return NULL;
    }
    return new MulExprAST(opcode, lhs, rhs);
}

static ExprAST *parse_add() {
    int opcode = 0;
    ExprAST *lhs = parse_mul(), *rhs = NULL;
    if (!lhs) return NULL;

    if (t->cur_tok->c() == '+' || t->cur_tok->c() == '-') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
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

    if (t->cur_tok->c() == '|' || t->cur_tok->c() == '^' || t->cur_tok->c() == '&') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
        rhs = parse_add();
        if (!rhs) return NULL;
    } else if (t->cur_tok->c() == '<' || t->cur_tok->c() == '>') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
        if (int(t->cur_tok->c()) != opcode)
            return error("invalid operator");
        t->get_next_token();
        if (opcode == '>' && t->cur_tok->c() == '>') {
            signshift = true;
            t->get_next_token();
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

    if (t->cur_tok->c() == '<' || t->cur_tok->c() == '>') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
        if (t->cur_tok->c() == '=') {
            equal = true;
            t->get_next_token();
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

    if (t->cur_tok->c() == '=' || t->cur_tok->c() == '!') {
        opcode = int(t->cur_tok->c());
        t->get_next_token();
        if (t->cur_tok->c() != '=') {
            return NULL;
        }
        t->get_next_token();
        rhs = parse_bit();
        if (!rhs) return NULL;
    }

    return new CompEQExprAST(opcode, lhs, rhs);
}

int main(int argc, char *argv[])
{
    t = (argc == 1) ? new Tokenizer() : new Tokenizer(argv[1]);
    if (!t->ok()) {
        std::cerr << "File could not be opened" << std::endl;
        exit(EXIT_FAILURE);
    }

    bool test_lexer = true;
    if (!test_lexer) {
        t->get_next_token();
        // driver();
        ExprAST *ast = parse_file();
        if (ast) ast->print_node();
    } else {
        t->get_next_token();
        while (t->cur_tok->type != TOK_EOF) {
            if (t->cur_tok->type == TOK_INTEGER)
                std::cout << "Integer: " << t->cur_tok->val<int_t>() << std::endl;
            else if (t->cur_tok->type == TOK_STRING)
                std::cout << "String: " << t->cur_tok->val<std::string>() << std::endl;
            else if (t->cur_tok->type == TOK_IDENTIFIER)
                std::cout << "Identifier: " << t->cur_tok->val<std::string>() << std::endl;
            else
                std::cout << "Char: " << t->cur_tok->c() << "/" << char(t->cur_tok->c()) << std::endl;
            t->get_next_token();
        }
    }
    delete t;
    return 0;
}
