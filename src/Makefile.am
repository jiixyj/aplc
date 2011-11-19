BUILT_SOURCES = parser.h
AM_YFLAGS = -d
bin_PROGRAMS = aplc
aplc_SOURCES = main.cpp parser.ypp lexer.lpp print.cpp node.h
aplc_CPPFLAGS = `llvm-config --cxxflags`
aplc_LDFLAGS = `llvm-config --ldflags`
aplc_LDADD = @LEXLIB@
