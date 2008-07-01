CC = gcc

NAME = flisp
SRCS = $(NAME).c builtins.c equal.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
EXENAME = $(NAME)
LLT = llt/libllt.a

FLAGS = -Wall -Wextra -Wno-strict-aliasing -I./llt $(CFLAGS)
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
	cd llt && make

debug: $(DOBJS) $(LIBS)
	$(CC) $(DEBUGFLAGS) $(DOBJS) -o $(EXENAME) $(LIBS)

release: $(OBJS) $(LIBS)
	$(CC) $(SHIPFLAGS) $(OBJS) -o $(EXENAME) $(LIBS)

clean:
	rm -f *.o
	rm -f *.do
	rm -f $(EXENAME)
