if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    file(READ "${CMAKE_ROOT}/Modules/FindBISON.cmake" FIND_BISON_FILE)
    string(REPLACE "bison" "yacc" FIND_BISON_FILE "${FIND_BISON_FILE}")
    string(REGEX REPLACE "EXECUTE_PROCESS\\(.*\\-\\-version[^\\)]*\\)" ""
                         FIND_BISON_FILE "${FIND_BISON_FILE}")
    string(REPLACE "INCLUDE(\${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)"
                   "INCLUDE(FindPackageHandleStandardArgs)"
                   FIND_BISON_FILE "${FIND_BISON_FILE}")
    file(WRITE "${CMAKE_BINARY_DIR}/FindBISON.cmake" "${FIND_BISON_FILE}")
endif()

find_package(LLVM REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_library(ICONV_LIBRARY iconv)
if(NOT ICONV_LIBRARY)
    set(ICONV_LIBRARY "")
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(SYSTEM ${CMAKE_CURRENT_BINARY_DIR})
include_directories(SYSTEM ${LLVM_INCLUDE_DIR})

BISON_TARGET(MyParser parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
             COMPILE_FLAGS "-v")
FLEX_TARGET(MyScanner lexer.lpp  ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyScanner MyParser)
if("${BISON_EXECUTABLE}" MATCHES ".*yacc")
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp.h ${CMAKE_CURRENT_BINARY_DIR}/parser.hpp)
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp.h ${CMAKE_CURRENT_BINARY_DIR}/parser.h)
else()
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_BINARY_DIR}/parser.hpp ${CMAKE_CURRENT_BINARY_DIR}/parser.h)
endif()

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
    ${ICONV_LIBRARY}
)

if(LLVM_VERSION STRGREATER "3.0")
    target_link_libraries(aplc LLVMVectorize)
endif()
