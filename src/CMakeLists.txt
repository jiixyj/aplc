find_package(LLVM REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(SYSTEM ${CMAKE_CURRENT_BINARY_DIR})
include_directories(SYSTEM ${LLVM_INCLUDE_DIR})

BISON_TARGET(MyParser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp)
FLEX_TARGET(MyScanner lexer.l  ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyScanner MyParser)

add_executable(aplc main print ${BISON_MyParser_OUTPUTS} ${FLEX_MyScanner_OUTPUTS})
set_target_properties(aplc PROPERTIES
  COMPILE_FLAGS ${LLVM_CFLAGS}
  LINK_FLAGS ${LLVM_LD_FLAGS})
target_link_libraries(aplc
  LLVMCore LLVMSupport LLVMSystem)
