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

typedef std::map<std::string, llvm::Value *> symbolTable;
static symbolTable named_values;
static std::string last_ident;

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
    func_args.push_back(UnresolvedType::get(0));
    FunctionType *func_type = FunctionType::get(UnresolvedType::get(0), func_args, false);
    Function *func = Function::Create(func_type, GlobalValue::InternalLinkage, "ι");

    BasicBlock *block = BasicBlock::Create(mod->getContext(), "entry", func, 0);
    llvm::IRBuilder<> builder(llvm::getGlobalContext());
    builder.SetInsertPoint(block);
    builder.CreateRet(func->arg_begin());

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

    static Value* const_ptr_12 = builder.CreateGlobalStringPtr("%c");
    builder.CreateCall2(named_values["__printf"], const_ptr_12, func->arg_begin());
    builder.CreateRetVoid();

    return func;
}

llvm::Value *NAssign::codeGen() {
    last_ident = "";
    if (last_ident == "") {
        return ErrorV("Identifier expected");
    }
    std::string ident = last_ident;
    llvm::Value *R = r.codeGen();
    if (!R) return NULL;

    if (named_values[last_ident]->getType() != R->getType()) {
        return ErrorV("Types don't match in assignment");
    }

    named_values[last_ident] = R;
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

    ValueToValueMapTy vmap;
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

    llvm::Value *func = lhs.codeGen();
    llvm::Value *apply = rhs.codeGen();
    if (!func) {
        return ErrorV("Error in NApply");
    }
    if (isa<UndefValue>(func)) {
        if (!apply) {
            if (!isa<NIdentifier>(rhs)) {
                return ErrorV("Expected identifier in declaration");
            }
            named_values[dyn_cast<NIdentifier>(&rhs)->name] = llvm::UndefValue::get(dyn_cast<UndefValue>(func)->getType());
        }
        return NULL;
    } else {
        llvm::Function *func_func;
        if (!(func_func = llvm::dyn_cast<llvm::Function>(func))) {
            return ErrorV("Non-function left arguments to NApply not supported yet");
        }

        if (!is_resolved(func_func)) {
            func_func = replace_unresolved(func_func, 0, apply->getType(), true);
        }

        return builder.CreateCall(func_func, apply);
    }
}

llvm::Value *NInteger::codeGen() {
    return ConstantInt::get(builder.getInt64Ty(), value);
}

llvm::Value *NIdentifier::codeGen() {
    if (name == "int") {
        return llvm::UndefValue::get(builder.getInt64Ty());
    } else {
        last_ident = name;
        symbolTable::iterator it = named_values.find(name);
        if (named_values.find(name) == named_values.end()) {
            return NULL;
        }
        return it->second;
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
    ArrayType *array = ArrayType::get(type, values.size());
    Type *types[] = { builder.getInt64Ty(), array };
    Type *array_real = StructType::get(mod->getContext(), types, false);
    Constant *alloca_size = ConstantExpr::getSizeOf(array_real);

    types[1] = ArrayType::get(type, 0);
    Type *array_type = StructType::get(mod->getContext(), types, false);

    Value *mem = builder.CreateAlloca(builder.getInt8Ty(), builder.CreateIntCast(alloca_size, builder.getInt32Ty(), false));
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

static void print_value(llvm::Function *printf, llvm::Value *val)
{
    static Value *format_int = builder.CreateGlobalStringPtr("%d");
    static Value *format_str = builder.CreateGlobalStringPtr("%s");
    static Value *format_newline = builder.CreateGlobalStringPtr("\n");
    static Value *struct_beg = builder.CreateGlobalStringPtr("(");
    static Value *struct_del = builder.CreateGlobalStringPtr(", ");
    static Value *struct_end = builder.CreateGlobalStringPtr(")");
    static Value *array_beg = builder.CreateGlobalStringPtr("[");
    static Value *array_end = builder.CreateGlobalStringPtr("]");

    PointerType *tmp1;
    if (val->getType()->isIntegerTy()) {
        builder.CreateCall2(printf, format_int, val);
    } else if ((tmp1 = dyn_cast<PointerType>(val->getType()))) {
        ArrayType *tmp2;
        if (tmp1->getElementType()->isStructTy() &&
                tmp1->getElementType()->getNumContainedTypes() == 2 &&
                (tmp2 = dyn_cast<ArrayType>(tmp1->getElementType()->getContainedType(1))) &&
                tmp2->getNumElements() == 0) {
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

    // main
    Type *func_main_args[] = { IntegerType::get(mod->getContext(), 32), PointerType::get(builder.getInt8PtrTy(), 0) };
    FunctionType* func_main_type = FunctionType::get(builder.getInt32Ty(), func_main_args, false);
    Function *func_main = Function::Create(func_main_type, GlobalValue::ExternalLinkage, "main", mod);

    // printf
    FunctionType *func_printf_type = FunctionType::get(builder.getInt32Ty(), builder.getInt8PtrTy(), true);
    Function *func_printf = Function::Create(func_printf_type, GlobalValue::ExternalLinkage, "printf", mod);

    // main block
    BasicBlock *bblock = BasicBlock::Create(mod->getContext(), "", func_main, 0);
    builder.SetInsertPoint(bblock);

    // globals
    named_values["__printf"] = func_printf;
    named_values["ι"] = generate_identity();
    named_values["putchar"] = generate_putchar();

    for (size_t i = 0; i < exprs->size(); ++i) {
        Value *val = (*exprs)[i]->codeGen();
        if (!isa<NAssign>((*exprs)[i]) && val) {
            print_value(func_printf, val);
        }
    }
    builder.CreateRet(builder.getInt32(0));

    std::cerr << "Code is generated." << std::endl;
    PassManager pm;
    pm.add(createPrintModulePass(&outs()));
    pm.run(*mod);
}
