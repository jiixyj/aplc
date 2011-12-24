#include "codegen.h"

#include <map>

#include <llvm/Constants.h>
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

using namespace llvm;

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
    static UnresolvedType *get(int x) {
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

#ifndef LLVM_TRANSFORMS_UTILS_VALUEMAPPER_H
#define Type const Type
#endif

llvm::Function *generate_identity()
{
    std::vector<Type *> func_ident_args;
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
    std::vector<Type *> ArgTypes;
    for (llvm::Function::arg_iterator I = F->arg_begin(), E = F->arg_end();
        I != E; ++I) {
        if (llvm::isa<llvm::UnresolvedType>(I->getType())) {
            return false;
        }
    }
    Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<llvm::UnresolvedType>(ret_type)) {
        return false;
    }

    return true;
}

static llvm::Function *replace_unresolved(llvm::Function *F, int index, Type *type, bool put_in_module)
{

    std::vector<Type*> ArgTypes;
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
    Type *ret_type = F->getFunctionType()->getReturnType();
    if (llvm::isa<UnresolvedType>(ret_type) && llvm::cast<UnresolvedType>(ret_type)->x == index) {
        ret_type = type;
    }
    FunctionType *FTy = FunctionType::get(ret_type, ArgTypes, F->getFunctionType()->isVarArg());

    // Create the new function...
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), put_in_module ? mod : NULL);

#ifndef LLVM_TRANSFORMS_UTILS_VALUEMAPPER_H
    typedef llvm::DenseMap<const llvm::Value*, llvm::Value*, llvm::DenseMapInfo<const llvm::Value*> > ValueToValueMapTy;
#endif
    ValueToValueMapTy vmap;
    Function::arg_iterator DestI = NewF->arg_begin();
    for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        DestI->setName(I->getName());
        vmap[I] = DestI++;
    }
    SmallVectorImpl<ReturnInst *> ri(0);
    CloneFunctionInto(NewF, F, vmap,
#ifdef LLVM_TRANSFORMS_UTILS_VALUEMAPPER_H
            false,
#endif
    ri);

    return NewF;
}


llvm::Value *NApply::codeGen() {

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
    return ConstantInt::get(builder.getInt64Ty(), value);
}

llvm::Value *NIdentifier::codeGen() {
    if (name == "int") {
        return llvm::UndefValue::get(builder.getInt64Ty());
    } else {
        return named_values[name];
    }
}

llvm::Value *NTuple::codeGen() {
    std::vector<Constant *> values;
    std::vector<Type *> types;
    for (size_t i = 0; i < l.size(); ++i) {
        llvm::Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!llvm::isa<llvm::Constant>(val)) { return ErrorV("Tuple elements must be constants"); }
        values.push_back(llvm::cast<llvm::Constant>(val));
        types.push_back(val->getType());
    }
    Constant *tuple = llvm::ConstantStruct::get(llvm::StructType::get(mod->getContext(), types, false), values);
    GlobalVariable* gvar = new GlobalVariable(*mod, tuple->getType(), true, GlobalValue::PrivateLinkage, 0, "struct");
    gvar->setInitializer(tuple);
    return gvar;
}

llvm::Value *NArray::codeGen() {
    std::vector<Constant *> values;
    Type *type = NULL;
    for (size_t i = 0; i < l.size(); ++i) {
        Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!isa<Constant>(val)) return ErrorV("Tuple elements must be constants");
        values.push_back(cast<Constant>(val));
        if (i == 0) type = val->getType();
        else if (val->getType() != type) return ErrorV("Types of array elements must be the same");
    }
    std::vector<Type *> types;
    Type *size = builder.getInt64Ty();
    ArrayType *array = ArrayType::get(type, values.size());
    types.push_back(size);
    types.push_back(array);
    Type *array_real = StructType::get(mod->getContext(), types, false);
    Constant *alloca_size = ConstantExpr::getSizeOf(array_real);

    types[1] = ArrayType::get(type, 0);
    Type *array_type = StructType::get(mod->getContext(), types, false);

    Value *mem = builder.CreateAlloca(builder.getInt8Ty(), alloca_size);
    Value *ret = builder.CreateBitCast(mem, PointerType::getUnqual(array_type));

    builder.CreateStore(ConstantInt::get(builder.getInt64Ty(), values.size()), builder.CreateStructGEP(ret, 0));
    builder.CreateStore(ConstantArray::get(array, values), builder.CreateBitCast(builder.CreateStructGEP(ret, 1), PointerType::getUnqual(array)));

    return ret;
}

llvm::Value *NBinaryOperator::codeGen() {
    Value *L = lhs.codeGen();
    Value *R = rhs.codeGen();
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

static void print_value(llvm::Function *printf, llvm::Value *val) {

    static Value *format_int = builder.CreateGlobalStringPtr("%d");
    static Value *format_str = builder.CreateGlobalStringPtr("%s");
    static Value *format_newline = builder.CreateGlobalStringPtr("\n");
    static Value *struct_beg = builder.CreateGlobalStringPtr("(");
    static Value *struct_del = builder.CreateGlobalStringPtr(", ");
    static Value *struct_end = builder.CreateGlobalStringPtr(")");
    static Value *array_beg = builder.CreateGlobalStringPtr("[");
    static Value *array_end = builder.CreateGlobalStringPtr("]");
    if (val->getType()->isIntegerTy()) {
        builder.CreateCall2(printf, format_int, val);
    } else if (val->getType()->isPointerTy()) {
        if (builder.CreateLoad(val)->getType()->isStructTy() &&
                cast<StructType>(builder.CreateLoad(val)->getType())->getNumElements() == 2 &&
                 isa<ArrayType>(builder.CreateLoad(builder.CreateStructGEP(val, 1))->getType()) &&
                cast<ArrayType>(builder.CreateLoad(builder.CreateStructGEP(val, 1))->getType())->getNumElements() == 0) {
            Value *array_size = builder.CreateLoad(builder.CreateStructGEP(val, 0));
            Value *array_data = builder.CreateStructGEP(val, 1);

            builder.CreateCall(printf, array_beg);
            for (size_t i = 0; i < 2; ++i) {
                builder.CreateCall2(printf, format_int, builder.CreateLoad(builder.CreateStructGEP(array_data, i)));
                if (i != 2 - 1) builder.CreateCall(printf, struct_del);
            }
            builder.CreateCall(printf, array_end);
        } else if (builder.CreateLoad(val)->getType()->isStructTy()) {
            size_t size = cast<StructType>(builder.CreateLoad(val)->getType())->getNumElements();
            builder.CreateCall(printf, struct_beg);
            for (size_t i = 0; i < size; ++i) {
                builder.CreateCall2(printf, format_int, builder.CreateLoad(builder.CreateStructGEP(val, i)));
                if (i != size - 1) builder.CreateCall(printf, struct_del);
            }
            builder.CreateCall(printf, struct_end);
        }
    } else {
        return;
    }
    builder.CreateCall(printf, format_newline);
}

void generate_code(ExpressionList *exprs)
{

    mod = new Module("main", getGlobalContext());


    // char *
    PointerType* char_ptr = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    // char **
    PointerType* char_ptr_ptr = PointerType::get(char_ptr, 0);

    // main
    std::vector<Type *> func_main_args;
    func_main_args.push_back(IntegerType::get(mod->getContext(), 32));
    func_main_args.push_back(char_ptr_ptr);
    FunctionType* func_main_type = FunctionType::get(IntegerType::get(mod->getContext(), 32), func_main_args, false);
    Function *func_main = Function::Create(func_main_type, GlobalValue::ExternalLinkage, "main", mod);

    // printf
    std::vector<Type *> func_printf_args;
    func_printf_args.push_back(char_ptr);
    FunctionType *func_printf_type = FunctionType::get(IntegerType::get(mod->getContext(), 32), func_printf_args, true);
    Function *func_printf = Function::Create(func_printf_type, GlobalValue::ExternalLinkage, "printf", mod);

    // main block
    BasicBlock *bblock = BasicBlock::Create(mod->getContext(), "entry", func_main, 0);
    builder.SetInsertPoint(bblock);

    // globals
    named_values["ι"] = generate_identity();


    Value *last = NULL;
    for (size_t i = 0; i < exprs->size(); ++i) {
        last = (*exprs)[i]->codeGen();
        if (!isa<NAssign>((*exprs)[i]) && last) {
            print_value(func_printf, last);
        }
    }
    builder.CreateRet(ConstantInt::get(builder.getInt32Ty(), 0));

    std::cerr << "Code is generated." << std::endl;
    PassManager pm;
    pm.add(createPrintModulePass(&outs()));
    pm.run(*mod);
}
