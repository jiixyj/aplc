PROG_CXX       = aplc
GENERATED      = parser.cpp parser.h lexer.cpp
SRCS           = ${GENERATED} main.cpp print.cpp
CLEANFILES     = ${GENERATED} parser.c y.tab.h lexer.c
NO_MAN         =

BINDIR        ?= /bin
DESTDIR       ?= /usr/local

LLVM_CXXFLAGS != llvm-config --cxxflags
LLVM_LDFLAGS  != llvm-config --ldflags

CXXFLAGS      += -I${.CURDIR} -I${.OBJDIR}
CXXFLAGS      += ${LLVM_CXXFLAGS}
LDFLAGS       += ${LLVM_LDFLAGS}

.c.cpp:
	ln -s ${.IMPSRC} ${.TARGET}
.y.h:
	ln -s y.tab.h ${.TARGET}

.include <bsd.prog.mk>
