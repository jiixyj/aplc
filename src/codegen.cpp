#include "codegen.h"

#include <map>

#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/PassManager.h>
#include <llvm/LinkAllPasses.h>

#include "node.h"
#include "parser.h"

llvm::Value *ErrorV(const char *str)
{
    std::cerr << "Error: " << str << std::endl;
    return 0;
}

static llvm::Module *the_module;
static llvm::LLVMContext &context = llvm::getGlobalContext();
static llvm::IRBuilder<> builder(context);
static std::map<std::string, llvm::Value *> named_values;

llvm::Value *NAssign::codeGen() {
    llvm::Value *R = r.codeGen();
    if (R == 0) return NULL;

    named_values[llvm::cast<NIdentifier>(l).name] = R;
    return R;
}

llvm::Value *NInteger::codeGen() {
    return builder.getInt64(value);
}

llvm::Value *NIdentifier::codeGen() {
    return named_values[name];
}

llvm::Value *NBinaryOperator::codeGen() {
    llvm::Value *L = lhs.codeGen();
    llvm::Value *R = rhs.codeGen();
    if (L == 0 || R == 0) return NULL;

    switch (op) {
    case TPLUS:
        std::cerr << "TPLUS" << std::endl;
        return builder.CreateAdd(L, R, "addtmp");
    case TMINUS:
        std::cerr << "TMINUS" << std::endl;
        return builder.CreateSub(L, R, "subtmp");
    case TMUL:
        std::cerr << "TMUL" << std::endl;
        return builder.CreateMul(L, R, "multmp");
    default:
        return ErrorV("invalid binary operator");
    }

    return NULL;
}

void generate_code(ExpressionList *exprs)
{
    the_module = new llvm::Module("main", context);

    std::vector<const llvm::Type *> no_arguments;
    llvm::FunctionType *ftype = llvm::FunctionType::get(llvm::Type::getInt64Ty(context), no_arguments, false);
    llvm::Function *mainFunction = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage, "main", the_module);
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context, "entry", mainFunction, 0);

    builder.SetInsertPoint(bblock);
    llvm::Value *last = NULL;
    for (size_t i = 0; i < exprs->size(); ++i) {
        last = (*exprs)[i]->codeGen();
    }
    builder.CreateRet(last);

    std::cerr << "Code is generated." << std::endl;
    llvm::PassManager pm;
    pm.add(llvm::createPrintModulePass(&llvm::outs()));
    pm.run(*the_module);
}
