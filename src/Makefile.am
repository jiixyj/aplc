BUILT_SOURCES = parser.h
AM_YFLAGS = -d
bin_PROGRAMS = aplc
aplc_SOURCES = main.cpp parser.ypp lexer.lpp print.cpp node.h codegen.cpp
aplc_CPPFLAGS = $(LLVM_CPPFLAGS)
aplc_LDFLAGS = $(LLVM_LDFLAGS)
aplc_LDADD = $(LLVM_LDADD) $(LEXLIB)
