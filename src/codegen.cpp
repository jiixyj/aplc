#include "codegen.h"

#include <cstdio>
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

typedef std::map<std::string, llvm::Value *> symbolTable;
static symbolTable named_values;

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

llvm::Function *generate_identity()
{
    std::vector<Type *> func_args;
    func_args.push_back(builder.getInt64Ty());
    func_args.push_back(builder.getInt64Ty());
    FunctionType *func_type = FunctionType::get(StructType::create(func_args), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "ι");

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    builder.SetInsertPoint(block);

    std::vector<Value *> retvals;
    for (Function::arg_iterator I = func->arg_begin(), E = func->arg_end(); I != E; ++I) {
        retvals.push_back(I);
    }
    builder.CreateAggregateRet(&retvals[0], retvals.size());

    return func;
}

llvm::Function *generate_alpha(Type *type_ptr)
{
    PointerType *type = cast<PointerType>(type_ptr);
    std::vector<Type *> func_args;
    func_args.push_back(type);
    FunctionType *func_type = FunctionType::get(cast<StructType>(type->getElementType())->getContainedType(0), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "α", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    builder.SetInsertPoint(block);
    Value *el = builder.CreateStructGEP(func->arg_begin(), 0);
    builder.CreateRet(builder.CreateLoad(el));

    return func;
}

llvm::Function *generate_beta(Type *type_ptr)
{
    PointerType *type = cast<PointerType>(type_ptr);
    std::vector<Type *> func_args;
    func_args.push_back(type);
    FunctionType *func_type = FunctionType::get(cast<StructType>(type->getElementType())->getContainedType(1), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "β", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    builder.SetInsertPoint(block);
    Value *el = builder.CreateStructGEP(func->arg_begin(), 1);
    builder.CreateRet(builder.CreateLoad(el));

    return func;
}

llvm::Function *generate_putchar()
{
    std::vector<Type *> func_args;
    func_args.push_back(builder.getInt64Ty());
    FunctionType *func_type = FunctionType::get(builder.getVoidTy(), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "putchar", mod);

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    builder.SetInsertPoint(block);

    static Value* const_ptr_12 = builder.CreateGlobalStringPtr("%lc");
    builder.CreateCall2(named_values["__printf"], const_ptr_12, func->arg_begin());
    builder.CreateRetVoid();

    return func;
}

llvm::Value *NControl::codeGen() {
    return ErrorV("NControl codegen not implemented yet");
}

static bool is_aplc_array(Value *val)
{
    PointerType *tmp1;
    ArrayType *tmp;
    return (tmp1 = dyn_cast<PointerType>(val->getType())) &&
           tmp1->getElementType()->isStructTy() &&
           tmp1->getElementType()->getNumContainedTypes() == 2 &&
           (tmp = dyn_cast<ArrayType>(tmp1->getElementType()->getContainedType(1))) &&
           tmp->getNumElements() == 0;
}

llvm::Value *NAssign::codeGen() {
    Value *id = l.codeGen();
    if (isa<Argument>(id)) {
        named_values[id->getName()] = id;
    }
    std::string ident = id->getName();
    std::cerr << ident << std::endl;
    llvm::Value *R = r.codeGen();
    if (!R) return NULL;

    // TODO special case aplc array
    std::cerr << "NAssign" << ident << std::endl;
    if (named_values[ident]->getType()->isArrayTy() && is_aplc_array(R)) {
    } else if (named_values[ident]->getType()->isArrayTy() && R->getType()->isArrayTy()) {
      /* skip */
    } else if (named_values[ident]->getType() != R->getType()) {
        raw_fd_ostream o(fileno(stderr), false);
        named_values[ident]->getType()->print(o);
        std::cerr << std::endl;
        R->getType()->print(o);
        std::cerr << std::endl;
        return ErrorV("Types don't match in assignment");
    }

    named_values[ident] = R;
    return R;
}

llvm::Value *NLambda::codeGen() {
    Value* lhs = l.codeGen();
    Value* mhs = m.codeGen();

    Function *F = dyn_cast<Function>(lhs);
    std::vector<Type*> ArgTypes;
    std::map<std::string, Argument *> func_args;
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        func_args[I->getName()] = I;
        ArgTypes.push_back(I->getType());
    }

    FunctionType *FTy = FunctionType::get(mhs->getType(), ArgTypes, F->getFunctionType()->isVarArg());
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), mod);
    for (Function::arg_iterator I = F->arg_begin(), I2 = NewF->arg_begin(), E = F->arg_end(); I != E; ++I, ++I2) {
        I2->setName(I->getName());
    }

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", NewF, 0);
    BasicBlock* old = builder.GetInsertBlock();
    builder.SetInsertPoint(block);
    Value* rhs = r.codeGen();
    builder.CreateRet(rhs);
    builder.SetInsertPoint(old);
    return NewF;
}

llvm::Value *NComparisonOperator::codeGen() {
    return ErrorV("NComparisonOperator codegen not implemented yet");
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
    if (llvm::isa<UnresolvedType>(ret_type)) {
        if (llvm::cast<UnresolvedType>(ret_type)->x == index) {
            ret_type = type;
        } else if (llvm::cast<UnresolvedType>(ret_type)->x >= 200) {
        }
    }
    FunctionType *FTy = FunctionType::get(ret_type, ArgTypes, F->getFunctionType()->isVarArg());

    // Create the new function...
    Function *NewF = Function::Create(FTy, F->getLinkage(), F->getName(), put_in_module ? mod : NULL);

    ValueToValueMapTy vmap;
    Function::arg_iterator DestI = NewF->arg_begin();
    for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        DestI->setName(I->getName());
        vmap[I] = DestI++;
    }
    SmallVector<ReturnInst *, 0> ri;
    CloneFunctionInto(NewF, F, vmap, false, ri);

    return NewF;
}

llvm::Value *NUnaryOperator::codeGen() {
    return ErrorV("NUnaryOperator codegen not implemented yet");
}

llvm::Value *NApply::codeGen() {
    static std::map<std::string, int> projection_operators;
    static bool projection_operators_generated;
    if (!projection_operators_generated) {
        projection_operators["α"] = 0;
        projection_operators["β"] = 1;
        projection_operators_generated = true;
    }

    llvm::Value *func = lhs.codeGen();
    llvm::Value *apply = rhs.codeGen();
    if (isa<Argument>(func) && projection_operators.find(func->getName()) != projection_operators.end()) {
        if (apply->getType()->isStructTy()) {
            return builder.CreateExtractValue(apply, projection_operators[func->getName()]);
        } else if (apply->getType()->isArrayTy()) {
            std::vector<Constant *> result;
            for (size_t i = 0; i < cast<ArrayType>(apply->getType())->getNumElements(); ++i) {
                result.push_back(cast<Constant>(builder.CreateExtractValue(builder.CreateExtractValue(apply, i), projection_operators[func->getName()])));
            }
            return ConstantArray::get(ArrayType::get(result[0]->getType(), result.size()), result);
        }
    }
    if (isa<Argument>(func) || (isa<ConstantStruct>(func) && isa<Argument>(builder.CreateExtractValue(func, 0)))) {
        if (isa<Argument>(apply)) {
            if (!isa<NIdentifier>(rhs)) {
                return ErrorV("Expected identifier in declaration");
            }
            std::cerr << "apply getname: " << apply->getName().str() << std::endl;
            return new Argument(func->getType(), apply->getName());
        }
    } else if (is_aplc_array(func) && apply->getType()->isIntegerTy()) {
        Value *array_data = builder.CreateStructGEP(func, 1);
        Value *Idxs[] = { builder.getInt64(0), apply };
        return builder.CreateLoad(builder.CreateGEP(array_data, ArrayRef<Value *>(Idxs)));
    } else if (func->getType()->isArrayTy() && isa<ConstantInt>(apply)) {
        return builder.CreateExtractValue(func, cast<ConstantInt>(apply)->getZExtValue());
    } else if (apply->getType()->isArrayTy()) {
        std::vector<Value *> results;
        for (size_t i = 0; i < cast<ArrayType>(apply->getType())->getNumElements(); ++i) {
            Value *arg = builder.CreateExtractValue(apply, i);
            if (arg->getType()->isStructTy()) {
                std::vector<Value *> args;
                for (size_t i = 0; i < cast<StructType>(arg->getType())->getNumElements(); ++i) {
                    args.push_back(builder.CreateExtractValue(arg, i));
                }
                results.push_back(builder.CreateCall(func, args));
            } else {
                results.push_back(builder.CreateCall(func, builder.CreateExtractValue(apply, i)));
            }
        }
        if (!cast<Function>(func)->getReturnType()->isVoidTy()) {
            Value *array_alloc = builder.CreateAlloca(cast<Function>(func)->getReturnType(), builder.getInt64(results.size()));
            for (size_t i = 0; i < results.size(); ++i) {
                Value *Idxs[] = { builder.getInt64(i) };
                builder.CreateStore(results[i], builder.CreateGEP(array_alloc, ArrayRef<Value *>(Idxs)));
            }
            return builder.CreateLoad(
                    builder.CreateBitCast(array_alloc,
                        PointerType::getUnqual(ArrayType::get(cast<Function>(func)->getReturnType(), results.size()))));
        } else {
            return NULL;
        }
    } else {
        llvm::Function *func_func;
        std::cerr << "I was here func func" << std::endl;
        if (!(func_func = llvm::dyn_cast<llvm::Function>(func))) {
            return ErrorV("Non-function left arguments to NApply not supported yet");
        }

        if (is_aplc_array(apply)) {
            Value *array_size = builder.CreateLoad(builder.CreateStructGEP(apply, 0));
            Value *array_data = builder.CreateStructGEP(apply, 1);
            Type *element_type = cast<ArrayType>(cast<PointerType>(array_data->getType())
                                                 ->getElementType())->getContainedType(0);
            if (!is_resolved(func_func)) {
                func_func = replace_unresolved(func_func, 0, element_type, true);
            }

            Value *ret = NULL;
            if (!func_func->getReturnType()->isVoidTy()) {
                Type *types[] = { builder.getInt64Ty(), ArrayType::get(func_func->getReturnType(), 0)};
                Type *array_real = StructType::get(mod->getContext(), types, false);
                Value *alloca_size = ConstantExpr::getSizeOf(array_real);
                alloca_size = builder.CreateAdd(alloca_size, builder.CreateMul(array_size, ConstantExpr::getSizeOf(element_type)));
                Value *mem = builder.CreateAlloca(builder.getInt8Ty(), alloca_size);
                ret = builder.CreateBitCast(mem, PointerType::getUnqual(array_real));
                builder.CreateStore(array_size, builder.CreateStructGEP(ret, 0));
            }

            BasicBlock* label_loop = BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            BasicBlock* label_loop_exit = BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            builder.CreateBr(label_loop);
            BasicBlock* old = builder.GetInsertBlock();
            builder.SetInsertPoint(label_loop);
            PHINode* indvar = builder.CreatePHI(builder.getInt64Ty(), 2);
            indvar->addIncoming(builder.getInt64(0), old);
            Value *Idxs[] = { builder.getInt64(0), indvar };
            Value *arg = builder.CreateLoad(builder.CreateGEP(array_data, ArrayRef<Value *>(Idxs)));
            Value *result = builder.CreateCall(func_func, arg);
            if (ret) builder.CreateStore(result, builder.CreateGEP(builder.CreateStructGEP(ret, 1), ArrayRef<Value *>(Idxs)));
            Value* nextindvar = builder.CreateBinOp(Instruction::Add, indvar, builder.getInt64(1));
            indvar->addIncoming(nextindvar, label_loop);
            builder.CreateCondBr(builder.CreateICmpEQ(nextindvar, array_size), label_loop_exit, label_loop);
            builder.SetInsertPoint(label_loop_exit);

            return ret;
        } else if (apply->getType()->isStructTy()) {
            std::vector<Value *> args;
            for (size_t i = 0; i < cast<StructType>(apply->getType())->getNumElements(); ++i) {
                args.push_back(builder.CreateExtractValue(apply, i));
                if (!is_resolved(func_func)) {
                    func_func = replace_unresolved(func_func, 0, args.back()->getType(), true);
                }
            }
            func_func = replace_unresolved(func_func, 0, builder.getVoidTy(), true);
            return builder.CreateCall(func_func, args);
        } else {
            return builder.CreateCall(func_func, apply);
        }
    }
    return ErrorV("Something bad happened in NApply codegen");
}

llvm::Value *NFuncType::codeGen() {
    Value *l = lhs.codeGen();
    Value *r = rhs.codeGen();

    std::vector<Type *> arg_types;
    if (l->getType()->isStructTy()) {
        for (size_t i = 0; i < cast<StructType>(l->getType())->getNumElements(); ++i) {
            arg_types.push_back(cast<StructType>(l->getType())->getContainedType(i));
        }
    } else {
        arg_types.push_back(l->getType());
    }
    Function *dummy = Function::Create(FunctionType::get(r->getType(), arg_types, false), GlobalValue::InternalLinkage);
    return new Argument(dummy->getType());
}

llvm::Value *NInteger::codeGen() {
    return ConstantInt::get(builder.getInt64Ty(), value);
}

llvm::Value *NIdentifier::codeGen() {
    if (name == "int") {
        return new Argument(builder.getInt64Ty());
    } else {
        symbolTable::iterator it = named_values.find(name);
        if (named_values.find(name) == named_values.end()) {
            return new Argument(builder.getInt64Ty(), name);
        }
        return it->second;
    }
}

llvm::Value *NTuple::codeGen() {
    std::vector<Value *> values;
    std::vector<Constant *> constants;
    std::vector<Type *> types;
    int isTypeExpr = 0;

    for (size_t i = 0; i < l.size(); ++i) {
        llvm::Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!isa<Constant>(val) && !isa<Argument>(val)) { return ErrorV("Tuple elements must be constants or arguments"); }
        if (!isTypeExpr) {
            if (isa<Argument>(val)) {
                isTypeExpr = 1;
            } else {
                isTypeExpr = 2;
            }
        } else if ((isa<Argument>(val) && isTypeExpr != 1) ||
                  (!isa<Argument>(val) && isTypeExpr != 2)) {
            return ErrorV("mixed types and non-types in tuple");
        }
        values.push_back(val);
        constants.push_back(dyn_cast<Constant>(val));
        types.push_back(val->getType());
    }

    if (isTypeExpr == 1 && cast<Argument>(values[0])->hasName()) {  /* Function arguments */
        FunctionType *ft = FunctionType::get(builder.getVoidTy(), types, false);
        Function *f = Function::Create(ft, GlobalValue::InternalLinkage);
        Function::arg_iterator args = f->arg_begin();

        for (size_t i = 0; i < l.size(); ++i) {
            args++->setName(cast<Argument>(values[i])->getName());
        }
        return f;
    } else if (isTypeExpr == 1) {
        return new Argument(StructType::get(mod->getContext(), types, false));
    }

    Constant *tuple = ConstantStruct::get(llvm::StructType::get(mod->getContext(), types, false), constants);

    // GlobalVariable* gvar = new GlobalVariable(*mod, tuple->getType(), true, GlobalValue::PrivateLinkage, 0, "struct");
    // gvar->setInitializer(tuple);
    // return gvar;
    return tuple;
}

llvm::Value *NArray::codeGen() {
    std::vector<Constant *> values;
    Type *type = NULL;
    for (size_t i = 0; i < l.size(); ++i) {
        Value *val = l[i]->codeGen();
        if (!val) { return ErrorV("Bad Tuple"); }
        if (!isa<Constant>(val) && !isa<Argument>(val)) return ErrorV("Tuple elements must be constants or arguments");
        values.push_back(cast<Constant>(val));
        if (i == 0) type = val->getType();
        else if (val->getType() != type) return ErrorV("Types of array elements must be the same");
    }
    if (values.size() && isa<Argument>(values[0])) {
        return new Argument(ArrayType::get(values[0]->getType(), 0));
    }

    ArrayType *array = ArrayType::get(type, values.size());
    return ConstantArray::get(array, values);
    Type *types[] = { builder.getInt64Ty(), array };
    Type *array_real = StructType::get(mod->getContext(), types, false);
    Constant *alloca_size = ConstantExpr::getSizeOf(array_real);

    types[1] = ArrayType::get(type, 0);
    Type *array_type = StructType::get(mod->getContext(), types, false);

    Value *mem = builder.CreateAlloca(builder.getInt8Ty(), alloca_size);
    Value *ret = builder.CreateBitCast(mem, PointerType::getUnqual(array_type));

    builder.CreateStore(builder.getInt64(values.size()), builder.CreateStructGEP(ret, 0));
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
        return builder.CreateAdd(L, R);
    case TMINUS:
        std::cerr << "TMINUS" << std::endl;
        return builder.CreateSub(L, R);
    case TMUL:
        std::cerr << "TMUL" << std::endl;
        return builder.CreateMul(L, R);
    default:
        return ErrorV("invalid binary operator");
    }

    return NULL;
}


static void print_value(llvm::Function *printf, llvm::Value *val)
{
    static bool strings_generated = false;
    static Value *format_int, *format_str, *format_newline, *struct_beg, *struct_del, *struct_end, *array_beg, *array_end;
    if (!strings_generated) {
        format_int = builder.CreateGlobalStringPtr("%d");
        format_str = builder.CreateGlobalStringPtr("%s");
        format_newline = builder.CreateGlobalStringPtr("\n");
        struct_beg = builder.CreateGlobalStringPtr("(");
        struct_del = builder.CreateGlobalStringPtr(", ");
        struct_end = builder.CreateGlobalStringPtr(")");
        array_beg = builder.CreateGlobalStringPtr("[");
        array_end = builder.CreateGlobalStringPtr("]");
        strings_generated = true;
    }

    if (val->getType()->isIntegerTy()) {
        builder.CreateCall2(printf, format_int, val);
    } else if (val->getType()->isPointerTy()) {
        if (is_aplc_array(val)) {
            Value *array_size = builder.CreateLoad(builder.CreateStructGEP(val, 0));
            Value *array_data = builder.CreateStructGEP(val, 1);

            builder.CreateCall(printf, array_beg);

            BasicBlock* label_loop =        BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);
            BasicBlock* label_loop_exit =   BasicBlock::Create(mod->getContext(), "", builder.GetInsertBlock()->getParent(), 0);

            builder.CreateBr(label_loop);
            BasicBlock* old = builder.GetInsertBlock();
            builder.SetInsertPoint(label_loop);

            // Block  (label_loop)
            PHINode* indvar = builder.CreatePHI(builder.getInt64Ty(), 2);
            indvar->addIncoming(builder.getInt64(0), old);

            Value *Idxs[] = { builder.getInt64(0), indvar };
            builder.CreateCall2(printf, format_int, builder.CreateLoad(builder.CreateGEP(array_data, ArrayRef<Value *>(Idxs))));
            builder.CreateCall(printf, struct_del);

            Value* nextindvar = builder.CreateBinOp(Instruction::Add, indvar, builder.getInt64(1));
            indvar->addIncoming(nextindvar, label_loop);
            builder.CreateCondBr(builder.CreateICmpEQ(nextindvar, array_size), label_loop_exit, label_loop);
            builder.SetInsertPoint(label_loop_exit);

            builder.CreateCall(printf, array_end);
        } else if (builder.CreateLoad(val)->getType()->isStructTy()) {
            builder.CreateCall(printf, struct_beg);
            size_t size = cast<StructType>(builder.CreateLoad(val)->getType())->getNumElements();
            for (size_t i = 0; i < size; ++i) {
                builder.CreateCall2(printf, format_int, builder.CreateLoad(builder.CreateStructGEP(val, i)));
                if (i != size - 1) builder.CreateCall(printf, struct_del);
            }
            builder.CreateCall(printf, struct_end);
        }
    } else if (val->getType()->isArrayTy()) {
        builder.CreateCall(printf, array_beg);
        size_t size = cast<ArrayType>(val->getType())->getNumElements();
        for (size_t i = 0; i < size; ++i) {
            builder.CreateCall2(printf, format_int, builder.CreateExtractValue(val, i));
            if (i != size - 1) builder.CreateCall(printf, struct_del);
        }
        builder.CreateCall(printf, array_end);
        return;
    } else if (val->getType()->isStructTy()) {
        builder.CreateCall(printf, struct_beg);
        size_t size = cast<StructType>(val->getType())->getNumElements();
        for (size_t i = 0; i < size; ++i) {
            builder.CreateCall2(printf, format_int, builder.CreateExtractValue(val, i));
            if (i != size - 1) builder.CreateCall(printf, struct_del);
        }
        builder.CreateCall(printf, struct_end);
    }
    builder.CreateCall(printf, format_newline);
}

void generate_code(ExpressionList *exprs)
{
    mod = new Module("main", getGlobalContext());

    // main
    Type *func_main_args[] = { builder.getInt32Ty(), PointerType::get(builder.getInt8PtrTy(), 0) };
    FunctionType* func_main_type = FunctionType::get(builder.getInt32Ty(), func_main_args, false);
    Function *func_main = Function::Create(func_main_type, GlobalValue::ExternalLinkage, "main", mod);

    // printf
    FunctionType *func_printf_type = FunctionType::get(builder.getInt32Ty(), builder.getInt8PtrTy(), true);
    named_values["__printf"] = Function::Create(func_printf_type, GlobalValue::ExternalLinkage, "printf", mod);

    // setlocale
    Type *func_setlocale_args[] = { builder.getInt32Ty(), builder.getInt8PtrTy() };
    FunctionType *func_setlocale_type = FunctionType::get(builder.getInt8PtrTy(), func_setlocale_args, false);
    named_values["__setlocale"] = Function::Create(func_setlocale_type, GlobalValue::ExternalLinkage, "setlocale", mod);

    // main block
    BasicBlock *bblock = BasicBlock::Create(mod->getContext(), "", func_main, 0);
    builder.SetInsertPoint(bblock);

    static Value* zero_initializer = builder.CreateGlobalStringPtr("");
    builder.CreateCall2(named_values["__setlocale"], builder.getInt32(LC_ALL), zero_initializer);

    // globals
    named_values["ι"] = generate_identity();
    // named_values["α"] = generate_alpha();
    // named_values["β"] = generate_beta();
    named_values["putchar"] = generate_putchar();

    for (size_t i = 0; i < exprs->size(); ++i) {
        Value *val = (*exprs)[i]->codeGen();
        if (!isa<NAssign>((*exprs)[i]) && val && !val->getType()->isVoidTy() &&
                !(val->getType()->isArrayTy() && cast<ArrayType>(val->getType())->getContainedType(0)->isVoidTy())) {
            print_value(cast<Function>(named_values["__printf"]), val);
        }
    }
    builder.CreateRet(builder.getInt32(0));

    std::cerr << "Code is generated." << std::endl;

    PassManager pm;
    pm.add(createFunctionInliningPass());
    pm.add(createPrintModulePass(&outs()));
    pm.run(*mod);
}
