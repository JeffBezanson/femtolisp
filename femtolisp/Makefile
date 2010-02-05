CC = gcc

NAME = flisp
SRCS = $(NAME).c builtins.c string.c equalhash.c table.c iostream.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
EXENAME = $(NAME)
LLTDIR = ../llt
LLT = $(LLTDIR)/libllt.a

FLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(LLTDIR) $(CFLAGS) -DUSE_COMPUTED_GOTO
LIBFILES = $(LLT)
LIBS = $(LIBFILES) -lm

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O2 -DNDEBUG $(FLAGS)

default: release test

test:
	./flisp unittest.lsp

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

flisp.o:  flisp.c cvalues.c types.c flisp.h print.c read.c equal.c
flisp.do: flisp.c cvalues.c types.c flisp.h print.c read.c equal.c

$(LLT):
	cd $(LLTDIR) && make

debug: $(DOBJS) $(LIBFILES)
	$(CC) $(DEBUGFLAGS) $(DOBJS) -o $(EXENAME) $(LIBS)
	make test

release: $(OBJS) $(LIBFILES)
	$(CC) $(SHIPFLAGS) $(OBJS) -o $(EXENAME) $(LIBS)

clean:
	rm -f *.o
	rm -f *.do
	rm -f $(EXENAME)
