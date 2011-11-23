#include "codegen.h"

#include <map>

#include <llvm/DerivedTypes.h>
#include <llvm/Function.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/ADT/ValueMap.h>
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

static llvm::Module *mod;
static llvm::IRBuilder<> builder(llvm::getGlobalContext());
static std::map<std::string, llvm::Value *> named_values;

namespace llvm { struct UnresolvedType; }
static std::map<int, llvm::UnresolvedType *> unresolved_type_map;

namespace llvm {
struct UnresolvedType : public Type {
    int x;
    UnresolvedType(int x) : Type(mod->getContext(), NumTypeIDs), x(x) {}
    static const UnresolvedType *get(int x) {
        UnresolvedType *ret = unresolved_type_map[x];
        if (!ret) {
            ret = unresolved_type_map[x] = new UnresolvedType(x);
        }
        return ret;
    }
    virtual TypeID getTypeID() const { return NumTypeIDs; }
    static inline bool classof(const Type *b) {
        return b->getTypeID() == NumTypeIDs;
    }
};
}

llvm::Function *generate_identity()
{
    using namespace llvm;

    std::vector<const Type *> func_ident_args;
    func_ident_args.push_back(UnresolvedType::get(0));
    FunctionType *func_ident_type = FunctionType::get(UnresolvedType::get(0), func_ident_args, false);
    Function *func_ident = Function::Create(func_ident_type, GlobalValue::InternalLinkage, "ι");

    BasicBlock *block_ident = BasicBlock::Create(mod->getContext(), "entry", func_ident, 0);
    llvm::IRBuilder<> builder_ident(llvm::getGlobalContext());
    builder_ident.SetInsertPoint(block_ident);
    builder_ident.CreateRet(func_ident->arg_begin());

    return func_ident;
}

llvm::Value *NAssign::codeGen() {
    NApply *decl = llvm::dyn_cast<NApply>(&l);
    if (!decl) {
        return ErrorV("Expected NApply on the left");
    }
    NIdentifier *ident = llvm::dyn_cast<NIdentifier>(&decl->rhs);
    if (!ident) {
        return ErrorV("Identifier expected");
    }
    llvm::Value *decl_type = decl->lhs.codeGen();
    llvm::Value *ident_val = ident->codeGen();
    if (!decl_type || ident_val ||
        !llvm::isa<llvm::UndefValue>(decl_type)) {
        return ErrorV("Invalid declaration");
    }
    llvm::Value *R = r.codeGen();
    if (R == 0) return NULL;

    if (decl_type->getType() != R->getType()) {
        return ErrorV("Types don't match");
    }

    named_values[ident->name] = R;
    return R;
}

static bool is_resolved(llvm::Function *F)
{
    std::vector<const llvm::Type*> ArgTypes;
    for (llvm::Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
        I != E; ++I) {
        if (llvm::isa<llvm::UnresolvedType>(I->getType())) {
            return false;
        }
    }
    const llvm::Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<llvm::UnresolvedType>(ret_type)) {
        return false;
    }

    return true;
}

static llvm::Function *replace_unresolved(llvm::Function *F, int index, const llvm::Type *type, bool put_in_module)
{
    using namespace llvm;

    std::vector<const Type*> ArgTypes;
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
        I != E; ++I) {
        const UnresolvedType *t;
        if ((t = llvm::dyn_cast<UnresolvedType>(I->getType())) && t->x == index) {
            ArgTypes.push_back(type);
        } else {
            ArgTypes.push_back(I->getType());
        }
    }

    // Create a new function type...
    const Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<UnresolvedType>(ret_type) && llvm::cast<UnresolvedType>(ret_type)->x == index) {
        ret_type = type;
    }
    FunctionType *FTy = FunctionType::get(ret_type, ArgTypes, F->getFunctionType()->isVarArg());

    // Create the new function...
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), put_in_module ? mod : NULL);

    ValueMap<const Value *, Value *> vmap;
    Function::arg_iterator DestI = NewF->arg_begin();
    for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        DestI->setName(I->getName());
        vmap[I] = DestI++;
    }
    SmallVectorImpl<ReturnInst *> ri(0);
    CloneFunctionInto(NewF, F, vmap, false, ri);

    return NewF;
}


llvm::Value *NApply::codeGen() {
    using namespace llvm;

    llvm::Value *func = lhs.codeGen();
    llvm::Value *apply = rhs.codeGen();
    if (!func) {
        return ErrorV("Error in NApply");
    }
    if (llvm::dyn_cast<llvm::Function>(apply)) {
        return ErrorV("Non-value arguments to NApply not supported yet");
    }
    llvm::Function *func_func;
    if (!(func_func = llvm::dyn_cast<llvm::Function>(func))) {
        return ErrorV("Non-function left arguments to NApply not supported yet");
    }

    if (!is_resolved(func_func)) {
        func_func = replace_unresolved(func_func, 0, apply->getType(), true);
    }

    return builder.CreateCall(func_func, apply);
}

llvm::Value *NInteger::codeGen() {
    return builder.getInt64(value);
}

llvm::Value *NIdentifier::codeGen() {
    if (name == "int") {
        return llvm::UndefValue::get(builder.getInt64Ty());
    } else {
        return named_values[name];
    }
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
    using namespace llvm;

    mod = new Module("main", getGlobalContext());


    // char *
    PointerType* char_ptr = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    // char **
    PointerType* char_ptr_ptr = PointerType::get(char_ptr, 0);

    // main
    std::vector<const Type *> func_main_args;
    func_main_args.push_back(IntegerType::get(mod->getContext(), 32));
    func_main_args.push_back(char_ptr_ptr);
    FunctionType* func_main_type = FunctionType::get(IntegerType::get(mod->getContext(), 32), func_main_args, false);
    Function *func_main = Function::Create(func_main_type, GlobalValue::ExternalLinkage, "main", mod);

    // printf
    std::vector<const Type *> func_printf_args;
    func_printf_args.push_back(char_ptr);
    FunctionType *func_printf_type = FunctionType::get(IntegerType::get(mod->getContext(), 32), func_printf_args, true);
    Function *func_printf = Function::Create(func_printf_type, GlobalValue::ExternalLinkage, "printf", mod);

    // main block
    BasicBlock *bblock = BasicBlock::Create(mod->getContext(), "entry", func_main, 0);
    builder.SetInsertPoint(bblock);

    // globals
    Value *g_format_int = builder.CreateGlobalStringPtr("%d\n");
    named_values["ι"] = generate_identity();


    Value *last = NULL;
    for (size_t i = 0; i < exprs->size(); ++i) {
        last = (*exprs)[i]->codeGen();
        if (!isa<NAssign>((*exprs)[i]) && last && last->getType()->isIntegerTy()) {
            builder.CreateCall2(func_printf, g_format_int, last);
        }
    }
    builder.CreateRet(builder.getInt32(0));

    std::cerr << "Code is generated." << std::endl;
    PassManager pm;
    pm.add(createPrintModulePass(&outs()));
    pm.run(*mod);
}
