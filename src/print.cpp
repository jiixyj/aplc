#include "node.h"

static int indent_step = 2;
static int indent;

void NInteger::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Integer " << value << std::endl;
    indent -= indent_step;
}

void NIdentifier::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Identifier " << name << std::endl;
    indent -= indent_step;
}

void NControl::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Control" << std::endl;
    l.print(ss);
    m.print(ss);
    r.print(ss);
    indent -= indent_step;
}

void NAssign::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Assign" << std::endl;
    l.print(ss);
    r.print(ss);
    indent -= indent_step;
}

void NLambda::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Lambda" << std::endl;
    l.print(ss);
    m.print(ss);
    r.print(ss);
    indent -= indent_step;
}

void NComparisonOperator::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Comparison " << op << " size: " << l.size() << std::endl;
    for (size_t i = 0; i < l.size(); ++i) {
        l[i]->print(ss);
    }
    indent -= indent_step;
}

void NBinaryOperator::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Binary " << op << std::endl;
    lhs.print(ss);
    rhs.print(ss);
    indent -= indent_step;
}

void NUnaryOperator::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Unary" << std::endl;
    u.print(ss);
    indent -= indent_step;
}

void NApply::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Apply" << std::endl;
    lhs.print(ss);
    rhs.print(ss);
    indent -= indent_step;
}

void NFuncType::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "FuncType" << std::endl;
    lhs.print(ss);
    rhs.print(ss);
    indent -= indent_step;
}

void NTuple::print(std::stringstream &ss) {
    indent += indent_step;
    ss << std::string(indent, ' ') << "Tuple; size: " << l.size() << std::endl;
    for (size_t i = 0; i < l.size(); ++i) {
        l[i]->print(ss);
    }
    indent -= indent_step;
}

void NArray::print(std::stringstream &ss) {
    indent += indent_step;
    if (isString) {
        ss << std::string(indent, ' ') << "String " << name << std::endl;
    } else {
        ss << std::string(indent, ' ') << "Array; size: " << l.size() << std::endl;
        for (size_t i = 0; i < l.size(); ++i) {
            l[i]->print(ss);
        }
    }
    indent -= indent_step;
}
