# femtolisp/Makefile.am

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = -falign-functions -Wall -Wno-strict-aliasing -I$(top_srcdir) \
-I$(top_srcdir)/llt -DUSE_COMPUTED_GOTO



noinst_LTLIBRARIES = libllt.la # libflisp.la

libllt_la_SOURCES = llt/bitvector.h llt/htable.h llt/ptrhash.h llt/utf8.h	\
	llt/dirpath.h llt/ieee754.h llt/random.h llt/utils.h llt/dtypes.h	\
	llt/ios.h llt/socket.h llt/hashing.h llt/llt.h llt/timefuncs.h		\
	llt/bitvector.c llt/hashing.c llt/socket.c llt/timefuncs.c		\
	llt/ptrhash.c llt/utf8.c llt/ios.c llt/dirpath.c llt/htable.c		\
	llt/bitvector-ops.c llt/int2str.c llt/dump.c llt/random.c llt/lltinit.c



bin_PROGRAMS = flisp

flisp_SOURCES = flmain.c flisp.h opcodes.h flisp.c builtins.c string.c	\
	equalhash.c table.c iostream.c

flisp_LDADD = libllt.la



EXTRA_DIST = equal.c read.c print.c equalhash.c equalhash.h types.c operators.c	\
	cvalues.c llt/lookup3.c llt/htableh.inc llt/htable.inc llt/mt19937ar.c	\
	mkboot1.lsp mkboot0.lsp system.lsp compiler.lsp flisp.boot tests



flispbootdir = $(bindir)

flispboot_DATA = flisp.boot

flisp.boot: flisp
	cp -vft . $(top_srcdir)/*.lsp $(top_srcdir)/flisp.boot
	./$< mkboot0.lsp system.lsp compiler.lsp > flisp.boot.new
	mv -vf flisp.boot.new flisp.boot
	./$< mkboot1.lsp



check_SCRIPTS = unittest.sh

TESTS = $(check_SCRIPTS)

unittest.sh: flisp flisp.boot
	echo '#!/usr/bin/env bash' > $@
	echo						>> $@
	echo "echo $@ && \\"				>> $@
	echo "cd $(abs_srcdir)/tests && \\"		>> $@
	echo "$(abs_builddir)/flisp unittest.lsp"	>> $@
	chmod +x $@


CLEANFILES = mkboot1.lsp mkboot0.lsp system.lsp compiler.lsp flisp.boot	\
	unittest.sh

