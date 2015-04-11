FREEBSD-GE-10 = $(shell test `uname` = FreeBSD -a `uname -r | cut -d. -f1` -ge 10 && echo YES)
CC = $(if $(FREEBSD-GE-10),clang,gcc)

NAME = lisp
SRC = $(NAME).c
EXENAME = $(NAME)

FLAGS = -Wall -Wextra
LIBS =

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -fomit-frame-pointer $(FLAGS)

default: release

debug: $(SRC)
	$(CC) $(DEBUGFLAGS) $(SRC) -o $(EXENAME) $(LIBS)

release: $(SRC)
	$(CC) $(SHIPFLAGS) $(SRC) -o $(EXENAME) $(LIBS)

clean:
	rm -f $(EXENAME)
