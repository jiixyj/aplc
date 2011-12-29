PROG_CXX       = aplc
GENERATED      = parser.cpp parser.h lexer.cpp
SRCS           = ${GENERATED} main.cpp print.cpp codegen.cpp
CLEANFILES     = ${GENERATED} parser.c y.tab.h lexer.c parser.y lexer.l
NO_MAN         =

BINDIR        ?= /bin
DESTDIR       ?= /usr/local

# setup llvm
LLVM_CXXFLAGS != llvm-config --cxxflags | sed "s/-I/-isystem/g"
LLVM_LDFLAGS  != llvm-config --ldflags
LLVM_LDADD    != llvm-config --libs
CXXFLAGS      += ${LLVM_CXXFLAGS}
LDFLAGS       += ${LLVM_LDFLAGS}
LDADD         += ${LLVM_LDADD}

# setup iconv
LDADD         += -liconv

# add our includes
CXXFLAGS      += -I${.CURDIR} -I${.OBJDIR}


parser.y: parser.ypp
	ln -s parser.ypp parser.y
lexer.l: lexer.lpp
	ln -s lexer.lpp lexer.l

.y.h:
	ln -s y.tab.h ${.TARGET}
.c.cpp:
	ln -s ${.IMPSRC} ${.TARGET}

.include <bsd.prog.mk>
