find_package(LLVM REQUIRED)

include_directories(SYSTEM ${LLVM_INCLUDE_DIR})
add_executable(aplc main)
set_target_properties(aplc PROPERTIES
  COMPILE_FLAGS ${LLVM_CFLAGS}
  LINK_FLAGS ${LLVM_LD_FLAGS})
target_link_libraries(aplc
  icuuc LLVMCore LLVMSupport LLVMSystem)
