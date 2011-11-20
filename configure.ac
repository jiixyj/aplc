AC_PREREQ([2.68])
AC_INIT(aplc, 0.0.1)
AC_CONFIG_SRCDIR([src/main.cpp])
AM_INIT_AUTOMAKE
# AC_CONFIG_MACRO_DIR([m4])
# AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile])

# Checks for programs.
AC_PROG_CXX
AM_PROG_LEX
AC_PROG_YACC
AC_CHECK_PROG(LLVM_CONFIG, llvm-config, llvm-config)
if [[ -z "$LLVM_CONFIG" ]]; then
    AC_MSG_ERROR([Could not find llvm-config, quitting!])
fi
AC_SUBST(LLVM_CPPFLAGS, [`llvm-config --cppflags`])
AC_SUBST(LLVM_LDFLAGS, [`llvm-config --ldflags`])
AC_SUBST(LLVM_LDADD, [`llvm-config --libs core`])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
