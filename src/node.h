#ifndef NODE_H
#define NODE_H

#include <iostream>
#include <vector>
#include <sstream>
#include <llvm/Value.h>

class CodeGenContext;
class NExpression;

typedef std::vector<NExpression *> ExpressionList;

enum NodeId {
    NIntegerId,
    NIdentifierId,
    NStringId,
    NControlId,
    NAssignId,
    NLambdaId,
    NComparisonOperatorId,
    NBinaryOperatorId,
    NUnaryOperatorId,
    NApplyId,
    NFuncTypeId,
    NTupleId,
    NArrayId
};

class NExpression {
public:
    virtual ~NExpression() {}
    virtual llvm::Value* codeGen() { return NULL; }
    virtual void print(std::stringstream &ss) = 0;
    virtual NodeId getValueID() const = 0;
};

class NInteger : public NExpression {
public:
    uint64_t value;
    NInteger(uint64_t value) : value(value) {}
    virtual llvm::Value* codeGen();
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NIntegerId; }
};

class NIdentifier : public NExpression {
public:
    std::string name;
    NIdentifier(const std::string& name) : name(name) {}
    virtual llvm::Value* codeGen();
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NIdentifierId; }
};

class NString : public NExpression {
public:
    std::string name;
    NString(const std::string& name) : name(name) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NStringId; }
};

class NControl : public NExpression {
public:
    NExpression& l;
    NExpression& m;
    NExpression& r;
    NControl(NExpression& l, NExpression& m, NExpression& r) : l(l), m(m), r(r) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NControlId; }
};

class NAssign : public NExpression {
public:
    NExpression& l;
    NExpression& r;
    NAssign(NExpression& l, NExpression& r) : l(l), r(r) {}
    virtual llvm::Value* codeGen();
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NAssignId; }
};

class NLambda : public NExpression {
public:
    NExpression& l;
    NExpression& m;
    NExpression& r;
    NLambda(NExpression& l, NExpression& m, NExpression& r) : l(l), m(m), r(r) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NLambdaId; }
};

class NComparisonOperator : public NExpression {
public:
    int op;
    ExpressionList l;
    NComparisonOperator(int op) : op(op), l() {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NComparisonOperatorId; }
    static inline bool classof(const NExpression *b) {
        return b->getValueID() == NComparisonOperatorId;
    }
};

class NBinaryOperator : public NExpression {
public:
    NExpression& lhs;
    int op;
    NExpression& rhs;
    NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
        lhs(lhs), op(op), rhs(rhs) {}
    virtual llvm::Value* codeGen();
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NBinaryOperatorId; }
};

class NUnaryOperator : public NExpression {
public:
    NExpression& u;
    NUnaryOperator(NExpression& u) : u(u) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NUnaryOperatorId; }
};

class NApply : public NExpression {
public:
    NExpression& lhs;
    NExpression& rhs;
    NApply(NExpression& lhs, NExpression& rhs) :
        lhs(lhs), rhs(rhs) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NApplyId; }
};

class NFuncType : public NExpression {
public:
    NExpression& lhs;
    NExpression& rhs;
    NFuncType(NExpression& lhs, NExpression& rhs) :
        lhs(lhs), rhs(rhs) {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NFuncTypeId; }
};

class NTuple : public NExpression {
public:
    ExpressionList l;
    NTuple() {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NTupleId; }
    static inline bool classof(const NExpression *b) {
        return b->getValueID() == NTupleId;
    }
};

class NArray : public NExpression {
public:
    ExpressionList l;
    NArray() {}
    virtual llvm::Value* codeGen() {}
    void print(std::stringstream &ss);
    virtual NodeId getValueID() const { return NArrayId; }
    static inline bool classof(const NExpression *b) {
        return b->getValueID() == NArrayId;
    }
};

#endif /* end of include guard: NODE_H */
