#include <iostream>
#include <vector>
#include <sstream>
//#include <llvm/Value.h>

static int indent_step = 2;
static int indent;

class CodeGenContext;
class NExpression;

typedef std::vector<NExpression *> ExpressionList;

class NExpression {
public:
    virtual ~NExpression() {}
//    virtual llvm::Value* codeGen(CodeGenContext& context) { }
    virtual void print(std::stringstream &ss) = 0;
};

class NInteger : public NExpression {
public:
    long long value;
    NInteger(long long value) : value(value) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Integer " << value << std::endl;
        indent -= indent_step;
    }
};

class NIdentifier : public NExpression {
public:
    std::string name;
    NIdentifier(const std::string& name) : name(name) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Identifier " << name << std::endl;
        indent -= indent_step;
    }
};

class NString : public NExpression {
public:
    std::string name;
    NString(const std::string& name) : name(name) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "String " << name << std::endl;
        indent -= indent_step;
    }
};

class NControl : public NExpression {
public:
    NExpression& l;
    NExpression& m;
    NExpression& r;
    NControl(NExpression& l, NExpression& m, NExpression& r) : l(l), m(m), r(r) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Control" << std::endl;
        l.print(ss);
        m.print(ss);
        r.print(ss);
        indent -= indent_step;
    }
};

class NAssign : public NExpression {
public:
    NExpression& l;
    NExpression& r;
    NAssign(NExpression& l, NExpression& r) : l(l), r(r) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Assign" << std::endl;
        l.print(ss);
        r.print(ss);
        indent -= indent_step;
    }
};

class NLambda : public NExpression {
public:
    NExpression& l;
    NExpression& m;
    NExpression& r;
    NLambda(NExpression& l, NExpression& m, NExpression& r) : l(l), m(m), r(r) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Lambda" << std::endl;
        l.print(ss);
        m.print(ss);
        r.print(ss);
        indent -= indent_step;
    }
};

class NComparisonOperator : public NExpression {
public:
    int op;
    ExpressionList l;
    NComparisonOperator(int op) : op(op) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Comparison " << op << " size: " << l.size() << std::endl;
        for (size_t i = 0; i < l.size(); ++i) {
            l[i]->print(ss);
        }
        indent -= indent_step;
    }
};

class NBinaryOperator : public NExpression {
public:
    int op;
    NExpression& lhs;
    NExpression& rhs;
    NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
        lhs(lhs), rhs(rhs), op(op) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Binary " << op << std::endl;
        lhs.print(ss);
        rhs.print(ss);
        indent -= indent_step;
    }
};

class NUnaryOperator : public NExpression {
public:
    NExpression& u;
    NUnaryOperator(NExpression& u) : u(u) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Unary" << std::endl;
        u.print(ss);
        indent -= indent_step;
    }
};

class NApply : public NExpression {
public:
    NExpression& lhs;
    NExpression& rhs;
    NApply(NExpression& lhs, NExpression& rhs) :
        lhs(lhs), rhs(rhs) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Apply" << std::endl;
        lhs.print(ss);
        rhs.print(ss);
        indent -= indent_step;
    }
};

class NFuncType : public NExpression {
public:
    NExpression& lhs;
    NExpression& rhs;
    NFuncType(NExpression& lhs, NExpression& rhs) :
        lhs(lhs), rhs(rhs) { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "FuncType" << std::endl;
        lhs.print(ss);
        rhs.print(ss);
        indent -= indent_step;
    }
};

class NTuple : public NExpression {
public:
    ExpressionList l;
    NTuple() { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Tuple; size: " << l.size() << std::endl;
        for (size_t i = 0; i < l.size(); ++i) {
            l[i]->print(ss);
        }
        indent -= indent_step;
    }
};

class NArray : public NExpression {
public:
    ExpressionList l;
    NArray() { }
//    virtual llvm::Value* codeGen(CodeGenContext& context);
    void print(std::stringstream &ss) {
        indent += indent_step;
        ss << std::string(indent, ' ') << "Array; size: " << l.size() << std::endl;
        for (size_t i = 0; i < l.size(); ++i) {
            l[i]->print(ss);
        }
        indent -= indent_step;
    }
};

