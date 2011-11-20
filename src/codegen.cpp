#include "node.h"

#include <map>

#include <llvm/DerivedTypes.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/IRBuilder.h>

llvm::Value *ErrorV(const char *str) { std::cerr << "Error: " << str << std::endl; return 0; }

static llvm::Module *the_module;
static llvm::IRBuilder<> builder(llvm::getGlobalContext());
static std::map<std::string, llvm::Value *> named_values;
