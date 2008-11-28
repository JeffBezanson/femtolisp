CC = gcc

NAME = flisp
SRCS = $(NAME).c equal.c builtins.c string.c equalhash.c table.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
EXENAME = $(NAME)
LLTDIR = ../llt
LLT = $(LLTDIR)/libllt.a

FLAGS = -Wall -Wextra -Wno-strict-aliasing -I$(LLTDIR) $(CFLAGS)
LIBS = $(LLT) -lm

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG -fomit-frame-pointer $(FLAGS)

default: release test

test:
	./flisp unittest.lsp

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

flisp.o: flisp.c cvalues.c flisp.h print.c read.c
flisp.do: flisp.c cvalues.c flisp.h print.c read.c

$(LLT):
	cd $(LLTDIR) && make

debug: $(DOBJS) $(LIBS)
	$(CC) $(DEBUGFLAGS) $(DOBJS) -o $(EXENAME) $(LIBS)
	make test

release: $(OBJS) $(LIBS)
	$(CC) $(SHIPFLAGS) $(OBJS) -o $(EXENAME) $(LIBS)

clean:
	rm -f *.o
	rm -f *.do
	rm -f $(EXENAME)
