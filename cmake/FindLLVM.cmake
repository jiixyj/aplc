# Find LLVM - Find the LLVM includes and library.
#
# This module defines
#  LLVM_INCLUDE_DIR, where to find LLVM headers.
#  LLVM_LIBRARY_DIR, where to find LLVM libraries.
#  LLVM_BIN_DIR, where to find LLVM binaries.
#  LLVM_CONFIG, the llvm-config executable
#  LLVM_FOUND, If false, do not try to use LLVM.

find_program(LLVM_CONFIG NAMES llvm-config DOC "llvm-config executable")

if (LLVM_CONFIG)
  message(STATUS "LLVM llvm-config found at: ${LLVM_CONFIG}")
else (LLVM_CONFIG)
  message(FATAL_ERROR "Could NOT find llvm-config executable")
endif (LLVM_CONFIG)

execute_process(
  COMMAND ${LLVM_CONFIG} --includedir
  OUTPUT_VARIABLE LLVM_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG} --libdir
  OUTPUT_VARIABLE LLVM_LIBRARY_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG} --bindir
  OUTPUT_VARIABLE LLVM_BIN_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG} --cppflags
  OUTPUT_VARIABLE LLVM_CFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG} --ldflags
  OUTPUT_VARIABLE LLVM_LD_FLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG} --targets-built
  OUTPUT_VARIABLE LLVM_TARGETS_STR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Convert from whitespace-delimited words to a list
string(REGEX MATCHALL "[a-zA-Z0-9]+" LLVM_TARGETS ${LLVM_TARGETS_STR})

# handle the QUIETLY and REQUIRED arguments and set LLVM_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLVM DEFAULT_MSG LLVM_INCLUDE_DIR LLVM_LIBRARY_DIR)
