PREFIX = /usr/local/stow/bdlisp
VERSION = 4.6
DISTNAME = bdlisp-$(VERSION)

CFLAGS += -Wall -rdynamic -DLISP_VERSION='"$(VERSION)"'
CFLAGS += -O3 -fomit-frame-pointer
# CFLAGS += -g

LDFLAGS += -lm -ldl

LISPLIB = apropos.lsp clos.lsp debug.lsp geom.lsp hash.lsp lint.lsp \
	match.lsp math.lsp perm.lsp pprint.lsp save.lsp struct.lsp cgi.lsp

LIBOBJS = lispread.o lispeval.o lispprnt.o lispfrmt.o lispdrvr.o lispfncs.o \
	lispstrg.o lispseqn.o lispvect.o lispmapf.o lispusrf.o lispmath.o \
	lispusrt.o lispsyst.o lispolst.o lispgetc.o lispmem.o readline.o

all: bdlisp0 bdlisp

# %.o: %.c
#	$(CC) -c $(CFLAGS) $<

%.c: %.lsp bdlisp1 packer.lsp
	./bdlisp1 -s packer.lsp LISP_packed_$* $*.lsp > $*.c.tmp
	mv $*.c.tmp $*.c

bdlisp: lispmain.o lisplib.o lispboot.o bdlisp.a
	$(CC) $(CFLAGS) -o bdlisp $+ $(LDFLAGS)

bdlisp1: lispm1.o bdlisp.a
	$(CC) $(CFLAGS) -o bdlisp1 $+ $(LDFLAGS)

bdlisp0: lispm0.o bdlisp.a
	$(CC) $(CFLAGS) -o bdlisp0 $+ $(LDFLAGS)

bdlisp.a: $(LIBOBJS)
	ar rs bdlisp.a $?

install: bdlisp
	mkdir -p $(PREFIX)/bin
	cp bdlisp $(PREFIX)/bin
	mkdir -p $(PREFIX)/include
	cp bdlisp.h $(PREFIX)/include
	mkdir -p $(PREFIX)/lib/bdlisp
	cp $(LISPLIB) $(PREFIX)/lib/bdlisp

uninstall:
	rm -f $(PREFIX)/bin/bdlisp
	rm -f $(PREFIX)/include/bdlisp.h
	cd $(PREFIX)/lib/bdlisp && rm -f $(LISPLIB)
	-rmdir $(PREFIX)/lib/bdlisp

ctags:
	ctags *.h *.c

clean:
	rm -f bdlisp bdlisp0 bdlisp1 *.o *.a

dist:
	rm -rf dist-tmp/$(DISTNAME)
	mkdir -p dist-tmp/$(DISTNAME)
	ln Makefile *.h *.c *.lsp dist-tmp/$(DISTNAME)
	tar cf $(DISTNAME).tar -C dist-tmp $(DISTNAME)
	gzip -f $(DISTNAME).tar
	rm -rf dist-tmp

lispm1.o: lispmain.c bdlisp.h
	$(CC) -c $(CFLAGS) -o lispm1.o -DNO_STATIC_LIB $<

lispm0.o: lispmain.c bdlisp.h
	$(CC) -c $(CFLAGS) -o lispm0.o -DNO_LIB $<

lispmain.o: bdlisp.h lispmain.c

lispread.o: bdlisp.h lispread.c

lispeval.o: bdlisp.h lispeval.c

lispprnt.o: bdlisp.h lispprnt.c

lispfrmt.o: bdlisp.h lispfrmt.c

lispdrvr.o: bdlisp.h lispdrvr.c

lispfncs.o: bdlisp.h lispfncs.c

lispstrg.o: bdlisp.h lispstrg.c

lispseqn.o: bdlisp.h lispseqn.c

lispvect.o: bdlisp.h lispvect.c

lispmapf.o: bdlisp.h lispmapf.c

lispusrf.o: bdlisp.h lispusrf.c

lispmath.o: bdlisp.h lispmath.c

lispusrt.o: bdlisp.h lispusrt.c

lispsyst.o: bdlisp.h lispsyst.c

lispolst.o: bdlisp.h lispolst.c

lispgetc.o: bdlisp.h lispgetc.c

lispmem.o: bdlisp.h lispmem.c

readline.o: readline.c

