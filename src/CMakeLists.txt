find_package(LLVM REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(SYSTEM ${CMAKE_CURRENT_BINARY_DIR})
include_directories(SYSTEM ${LLVM_INCLUDE_DIR})

BISON_TARGET(MyParser parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
             COMPILE_FLAGS "-v")
FLEX_TARGET(MyScanner lexer.lpp  ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyScanner MyParser)
execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/parser.hpp ${CMAKE_CURRENT_BINARY_DIR}/parser.h)

add_executable(aplc main print codegen ${BISON_MyParser_OUTPUTS} ${FLEX_MyScanner_OUTPUTS})
set_target_properties(aplc PROPERTIES
    COMPILE_FLAGS ${LLVM_CFLAGS}
    LINK_FLAGS ${LLVM_LD_FLAGS})
target_link_libraries(aplc
    LLVMipo
    LLVMInstrumentation
    LLVMScalarOpts
    LLVMInstCombine
    LLVMTransformUtils
    LLVMipa
    LLVMAnalysis
    LLVMTarget
    LLVMCore
    LLVMSupport
    LLVMSystem
)
