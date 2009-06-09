CC = gcc

SRCS = bitvector.c hashing.c socket.c timefuncs.c dblprint.c ptrhash.c \
	utf8.c ios.c operators.c cplxprint.c dirpath.c htable.c \
	bitvector-ops.c fp.c int2str.c dump.c random.c bswap.c memalign.c \
	swapreverse.c
OBJS = $(SRCS:%.c=%.o)
DOBJS = $(SRCS:%.c=%.do)
TARGET = libllt.a
TESTSRC = unittest.c
TESTER = llttest

FLAGS = -Wall -Wextra -Wno-strict-aliasing $(CFLAGS)
LIBS =

DEBUGFLAGS = -g -DDEBUG $(FLAGS)
SHIPFLAGS = -O3 -DNDEBUG $(FLAGS)

default: release

%.o: %.c
	$(CC) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(DEBUGFLAGS) -c $< -o $@

debug: $(DOBJS)
	rm -rf $(TARGET)
	ar rs $(TARGET) $(DOBJS)

release: $(OBJS)
	rm -rf $(TARGET)
	ar rs $(TARGET) $(OBJS)

test:
	make clean
	make debug CFLAGS=-DENABLE_LLT_TEST
	gcc $(TESTSRC) $(TARGET) -o $(TESTER) -lm
	./$(TESTER)

clean:
	rm -f *.o
	rm -f *.do
	rm -f *~
	rm -f core*
	rm -f $(TARGET)
	rm -f $(TESTER)
