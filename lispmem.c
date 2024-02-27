/*

	LISPMEM.C - 1997 - Bigdogs Internescional.

	Interprete LISP - gestione memoria.

*/

#include "bdlisp.h"

#ifdef WIN32_ENV
#include <windows.h>
#endif

void *LISPstackbase;

long LISPncells, LISPnsymbols, LISPnstreams, LISPnpool;

long LISPstringbytes;

#ifdef STATISTICS
unsigned long LISPrefcount;
#endif

static PTR freelist = NULL;

struct cellblock {
	struct cellblock *next;
	struct LISPcell blk[CELLBLOCKSIZE];
};

static struct cellblock *corehead = NULL;

LISPstatic void morecells(void);

PTR LISP_room(PTR args)
{
	static char buf[81];

	GETARGS(("", args));

	sprintf(buf, "; %ld used cells in %ld core block%s;",
		LISPncells, LISPnpool, LISPnpool == 1 ? "" : "s");
	LISPputs(NULL, buf);
	sprintf(buf, " %ld free cells.\n", LISPnpool * CELLBLOCKSIZE - LISPncells);
	LISPputs(NULL, buf);
	sprintf(buf, "; %d bytes per cell, %d cells per block.\n",
		(int) sizeof(struct LISPcell), CELLBLOCKSIZE);
	LISPputs(NULL, buf);
	sprintf(buf, "; %ld symbols, %ld streams, %ld string bytes.\n",
		LISPnsymbols + 1, LISPnstreams, LISPstringbytes);
	LISPputs(NULL, buf);
	sprintf(buf, "; %ld internal symbols in %u hash buckets.\n",
		LISPnintern, LISPhash_module);
	LISPputs(NULL, buf);
	sprintf(buf, "; struct size:");
	LISPputs(NULL, buf);
	sprintf(buf, " symbol=%u,", (unsigned) sizeof(struct LISPsymbol_ext));
	LISPputs(NULL, buf);
	sprintf(buf, " stream=%u.\n", (unsigned) sizeof(struct LISPstream_ext));
	LISPputs(NULL, buf);
	LISPusertyperoom();
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = linkPTR(NILptr);
	return linkPTR(NILptr);
}

PTR LISPnewstructure(PTR ptr)
{
	PTR ret;

	CHKPTR(ptr);
	if (LISPfullstack(ret)) {
		LISPputs(unlinkLISPerror(), FULL_STACK_MSG);
		LISPstatus |= LS_ABORT;
		return linkPTR(NILptr);
	}
	if (ptr->type == LT_NODE) {
		ret = newnodePTR();
		nodePTR(ret)->car = LISPnewstructure(nodePTR(ptr)->car);
		nodePTR(ret)->cdr = LISPnewstructure(nodePTR(ptr)->cdr);
	} else {
		ret = linkPTR(ptr);
	}
	return ret;
}

PTR LISPnewlist(PTR ptr)
{
	PTR ret = NULL, tail = NULL, tmp;

	while (ptr->type == LT_NODE) {
		tmp = newnodePTR();
		nodePTR(tmp)->car = linkPTR(nodePTR(ptr)->car);
		if (tail)
			nodePTR(tail)->cdr = tmp;
		else
			ret = tmp;
		tail = tmp;
		ptr = nodePTR(ptr)->cdr;
	}
	if (tail)
		nodePTR(tail)->cdr = linkPTR(ptr);
	else
		ret = linkPTR(ptr);
	return ret;
}

#ifdef LISPDEBUG

PTR linkPTR(PTR ptr)
{
	CHKPTR(ptr);
	if (ptr->nl >= (INT_MAX - 2))
		LISPfatal("link counter overflow");
#ifdef STATISTICS
	LISPrefcount++;
#endif
	(ptr->nl)++;
	return ptr;
}

void unlinkPTR(PTR ptr)
{
	CHKPTR(ptr);
	LISPassert(ptr->nl > 0);
#ifdef STATISTICS
	LISPrefcount++;
#endif
	if (--(ptr->nl) <= 0)
		freePTR(ptr);
}

#endif

LISPstatic void unlinklist(PTR ptr);

void freePTR(PTR ptr)
{
	CHKPTR(ptr);
	LISPassert(ptr->nl == 0);
	LISPassert(ptr != NILptr);

	/*
	if (LISPfullstack(ptr))
		LISPfatal("full CPU stack in freePTR");
	*/

	switch (ptr->type) { /* TYPESWITCH */
	case LT_NODE:
		unlinklist(ptr);
		return;
	case LT_NUMBER:
	case LT_BUILTIN:
	case LT_SPECIALFORM:
		break;
	case LT_CLOSURE:
		unlinkPTR(closurePTR(ptr)->fun);
		unlinkPTR(closurePTR(ptr)->env);
		break;
	case LT_SYMBOL:
		if (LISPkeepsymbol(ptr))
			return;
		LISPerasesymbol(ptr);
		NILptr->nl -= 5;
#ifdef STATISTICS
		LISPrefcount += 5;
#endif
#ifdef LISPDEBUG
		ptr->c.symbol.sym->magic = ~LISP_MAGIC_EXT;
#endif
		free(ptr->c.symbol.sym);
		LISPnsymbols--;
		break;
	case LT_STRING:
		if (stringPTR(ptr)->len >= sizeof(char *)) {
			LISPstringbytes -= stringPTR(ptr)->len;
			free(stringPTRstr(ptr));
		}
		break;
	case LT_STREAM:
		LISPerasestream(ptr);
#ifdef LISPDEBUG
		ptr->c.stream.str->magic = ~LISP_MAGIC_EXT;
#endif
		free(ptr->c.stream.str);
		LISPnstreams--;
		break;
	case LT_REF:
		unlinkPTR(*(refPTR(ptr)->ptr));
		*(refPTR(ptr)->ptr) = NULL;
		unlinkPTR(refPTR(ptr)->nextref);
		break;
	case LT_OBJECT:
		unlinkPTR(objectPTR(ptr)->isa);
		unlinkPTR(objectPTR(ptr)->fields);
		break;
	case LT_FREE:
		LISPfatal("freeing freed in freePTR");
		break;
	default:
		LISPuserfree(ptr);
		break;
	}

#ifdef LISPDEBUG
	ptr->magic = ~LISP_MAGIC;
#endif
	ptr->type = LT_FREE;
	ptr->c.node.cdr = freelist;
	freelist = ptr;
	LISPncells--;
}

LISPstatic void unlinklist(PTR ptr)
{
	PTR first = ptr, last = NULL;
	int n = 0;

	while (ptr->type == LT_NODE && ptr->nl == 0) {
		CHKPTR(ptr);
		last = ptr;
		unlinkPTR(nodePTR(last)->car);
		ptr = nodePTR(last)->cdr;
		last->type = LT_FREE;
		n++;
		ptr->nl--;
#ifdef STATISTICS
		LISPrefcount++;
#endif
	}
	if (last) {
		last->c.node.cdr = freelist;
		freelist = first;
	}
	if (ptr->nl <= 0)
		freePTR(ptr);
	LISPncells -= n;
}

/************************************/

LISPstatic void morecells(void)
{
	int i;
	struct cellblock *p;

	p = ecalloc(1, sizeof(*p));
	p->next = corehead;
	corehead = p;
	for (i = 0; i < CELLBLOCKSIZE; i++) {
		p->blk[i].type = LT_FREE;
		p->blk[i].nl = 0;
		p->blk[i].c.node.cdr = freelist;
		freelist = p->blk + i;
	}
	LISPnpool++;
}

PTR newnodePTR(void)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_NODE;
	ret->nl = 1;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	return ret;
}

PTR newrefPTR(PTR *ptr)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_REF;
	ret->nl = 1;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	refPTR(ret)->ptr = ptr;
	return ret;
}

PTR newbuiltinPTR(LISPbuiltinfun fun)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_BUILTIN;
	ret->nl = 1;
	builtinPTR(ret)->fun = fun;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	return ret;
}

PTR newspecialformPTR(LISPspecialform spform)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_SPECIALFORM;
	ret->nl = 1;
	specialformPTR(ret)->spform = spform;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	return ret;
}

PTR newclosurePTR(PTR fun, PTR env)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_CLOSURE;
	ret->nl = 1;
	closurePTR(ret)->fun = linkPTR(fun);
	closurePTR(ret)->env = linkPTR(env);
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	return ret;
}

PTR newobjectPTR(PTR isa)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_OBJECT;
	ret->nl = 1;
	objectPTR(ret)->isa = linkPTR(isa);
	objectPTR(ret)->fields = linkPTR(NILptr);
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	return ret;
}

PTR newnumberPTR(double val)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
	ret->type = LT_NUMBER;
	ret->nl = 1;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	numberPTR(ret)->val = val;
	return ret;
}

PTR LISPnewcell(void)
{
	PTR ret;

	if (!freelist)
		morecells();
	ret = freelist;
	freelist = ret->c.node.cdr;
	LISPncells++;
#ifdef LISPDEBUG
	ret->magic = LISP_MAGIC;
#endif
	ret->nl = 1;
	ret->type = -1;
	return ret;
}

void LISPeraseallsymbols(void)
{
	struct cellblock *p;
	int i;

	for (p = corehead; p; p = p->next) {
		for (i = 0; i < CELLBLOCKSIZE; i++) {
			if (p->blk[i].type == LT_SYMBOL) {
				(void) linkPTR(p->blk + i);
				LISPerasesymbol(p->blk + i);
				unlinkPTR(p->blk + i);
			}
		}
	}
	LISPerasesymbol(NILptr);
}

void LISPfreecore(void)
{
	struct cellblock *p;
	int i;

	for (p = corehead; p; p = p->next) {
		for (i = 0; i < CELLBLOCKSIZE; i++) {
			if (p->blk[i].type != LT_FREE) {
				switch (p->blk[i].type) { /* TYPESWITCH */
				case LT_SYMBOL:
					free(p->blk[i].c.symbol.sym);
					break;
				case LT_STREAM:
					free(p->blk[i].c.stream.str);
					break;
				case LT_STRING:
					if (p->blk[i].c.string.len >= sizeof(char *))
						free(p->blk[i].c.string.cnt.cptr);
					break;
				case LT_NUMBER:
				case LT_BUILTIN:
				case LT_SPECIALFORM:
				case LT_OBJECT:
				case LT_NODE:
				case LT_REF:
				default:
					LISPuserfree(p->blk + i);
					break;
				}
			}
		}
	}

	while (corehead) {
		p = corehead;
		corehead = p->next;
		free(p);
	}
}

/************************************/

LISPstatic void sweep(void);

void LISPreclaim()
{
	long n;
	static char buf[80];
	struct cellblock *p;
	int i;
	PTR ptr;

	n = LISPncells;

	for (p = corehead; p; p = p->next) {
		for (i = 0; i < CELLBLOCKSIZE; i++) {
			ptr = p->blk + i;
			ptr->mark = 0;
		}
	}

	/* puts("MARK1"); */
	LISPmarkobjects();

	/* puts("SWEEP"); */
	sweep();

	n -= LISPncells;
	if (LISPonterminal && n > 0) {
		sprintf(buf, "; garbage collection: %ld reclaimed cell%s.\n",
			n, (n == 1 ? "" : "s"));
		LISPputs(LISPconsole, buf);
	}
}

void LISPmark(PTR ptr)
{
	if (LISPfullstack(ptr))
		LISPfatal("full stack in mark");

tail_recurse:

	CHKPTR(ptr);
	if (ptr->mark == 0) {
		ptr->mark = 1;
		switch (ptr->type) { /* TYPESWITCH */
		case LT_SYMBOL:
			LISPmark(symbolPTR(ptr)->key);
			LISPmark(symbolPTR(ptr)->bind);
			LISPmark(symbolPTR(ptr)->prop);
			LISPmark(symbolPTR(ptr)->fun);
			LISPmark(symbolPTR(ptr)->name);
			break;
		case LT_CLOSURE:
			LISPmark(closurePTR(ptr)->env);
			ptr = closurePTR(ptr)->fun;
			goto tail_recurse;
		case LT_OBJECT:
			LISPmark(objectPTR(ptr)->isa);
			ptr = objectPTR(ptr)->fields;
			goto tail_recurse;
		case LT_NODE:
			LISPmark(nodePTR(ptr)->car);
			ptr = nodePTR(ptr)->cdr;
			goto tail_recurse;
		case LT_REF:
			LISPmark(*(refPTR(ptr)->ptr));
			ptr = refPTR(ptr)->nextref;
			goto tail_recurse;
		case LT_STREAM:
			switch (streamPTR(ptr)->strtype) {
			case ST_INSTRING:
				LISPmark(streamPTR(ptr)->str.instring.string);
				break;
			default:
				/* nothing */
				break;
			}
			break;
		default:
			if (ptr->type >= LT_USER_FIRST)
				LISPusermark(ptr);
			break;
		}
	}
}

LISPstatic void sweep(void)
{
	struct cellblock *p;
	int i;
	PTR ptr, car, cdr;

	for (p = corehead; p; p = p->next) {
		for (i = 0; i < CELLBLOCKSIZE; i++) {
			ptr = p->blk + i;
			if (ptr->mark == 0 && ptr->type == LT_NODE) {

				car = nodePTR(ptr)->car;
				cdr = nodePTR(ptr)->cdr;

				nodePTR(ptr)->car = linkPTR(NILptr);
				nodePTR(ptr)->cdr = linkPTR(NILptr);

				unlinkPTR(car);
				unlinkPTR(cdr);

			}
		}
	}
}

/************************************/

LISPstatic void checktype(PTR ptr, int type, int line, char *file)
{
	if (ptr->type != type)
		LISPfatal("wrong type (%d instead of %d) at [%d]%s",
			ptr->type, type, line, file);
}

struct LISPsymbol_ext *symbolPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_SYMBOL, line, file);
	LISPassert(ptr->c.symbol.sym->magic == LISP_MAGIC_EXT);
	return ptr->c.symbol.sym;
}

struct LISPstring *stringPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_STRING, line, file);
	return &(ptr->c.string);
}

struct LISPnumber *numberPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_NUMBER, line, file);
	return &(ptr->c.number);
}

struct LISPstream_ext *streamPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_STREAM, line, file);
	LISPassert(ptr->c.stream.str->magic == LISP_MAGIC_EXT);
	return ptr->c.stream.str;
}

struct LISPbuiltin *builtinPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_BUILTIN, line, file);
	return &(ptr->c.builtin);
}

struct LISPspecialform *specialformPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_SPECIALFORM, line, file);
	return &(ptr->c.specialform);
}

struct LISPclosure *closurePTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_CLOSURE, line, file);
	return &(ptr->c.closure);
}

struct LISPobject *objectPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_OBJECT, line, file);
	return &(ptr->c.object);
}

struct LISPnode *nodePTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_NODE, line, file);
	return &(ptr->c.node);
}

struct LISPref *refPTRfnc(PTR ptr, int line, char *file)
{
	checktype(ptr, LT_REF, line, file);
	return &(ptr->c.ref);
}

struct LISPuser *userPTRfnc(PTR ptr, int line, char *file)
{
	if (ptr->type < LT_USER_FIRST)
		LISPfatal("not user type %d at [%d]%s",
			ptr->type, line, file);
	return &(ptr->c.user);
}

void LISPfatal(char *fmt, ...)
{
	va_list vl;
#ifdef WIN32_ENV
	char sBuf[1024];
#endif

	va_start(vl, fmt);
	fprintf(stderr, "Fatal: ");
	vfprintf(stderr, fmt, vl);
	va_end(vl);
	fprintf(stderr, "\n");

#ifdef WIN32_ENV
	va_start(vl, fmt);
	vsprintf(sBuf, fmt, vl);
	va_end(vl);
	MessageBox(NULL, sBuf, "LISPfatal", MB_OK);
#endif

#ifdef LISPDEBUG
	abort();
#else
	exit(1);
#endif
}

void *emalloc(size_t size)
{
	void *ret;

	ret = malloc(size);
	if (!ret)
		LISPfatal(NO_MEM_MSG);
	return ret;
}

void *ecalloc(size_t nitems, size_t size)
{
	void *ret;

	ret = calloc(nitems, size);
	if (!ret)
		LISPfatal(NO_MEM_MSG);
	return ret;
}

char *estrdup(char *str)
{
	char *ret;

	ret = strdup(str);
	if (!ret)
		LISPfatal(NO_MEM_MSG);
	return ret;
}

char NO_MEM_MSG[] = "out of memory";
char FULL_STACK_MSG[] = "aborting: full CPU stack\n";
char NON_SYMBOL_MSG[] = "non symbol argument";
char NON_STRING_MSG[] = "non string argument";
char NON_VECTOR_MSG[] = "non vector argument";
char NON_CHAR_MSG[] = "non character argument";
char NON_NUMERIC_MSG[] = "non numeric argument";
char NON_STREAM_MSG[] = "non stream argument";
char NON_NODE_MSG[] = "non node argument";
char NON_LIST_MSG[] = "non list argument";
char NON_SAME_MSG[] = "not same type arguments";
char UNBOUND_SYMBOL_MSG[] = "unbound symbol";
char ILLEGAL_TYPE_MSG[] = "illegal type";
char ILL_FORM_HEAD_MSG[] = "illegal form head";
char CANT_SET_MSG[] = "illegal symbol setting";
char UNK_TYPE_IN_MSG[] = "unknown type in %s";
char BAD_FUNCTION_MSG[] = "bad function definition";
char BAD_VECT_IDX_MSG[] = "vector subscript out of bounds";
char CIRCULAR_MSG[] = "circular list detected";

