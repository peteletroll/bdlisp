/*

	BDLISP.H - 1997 - Bigdogs Internescional.

	Interprete LISP.

*/

#ifndef BDLISP_H
#define BDLISP_H

/*************************************/

/* #define LISPDEBUG */

/*************************************/

/* environment detection */

#if defined(__TURBOC__)
#define BORLANDC_ENV
#elif defined(_WIN32)
#define WIN32_ENV
#elif defined(DJGPP)
#define DJGPP_ENV
#elif defined(__GNUC__)
#define UNIX_GCC_ENV
#elif defined(__TINYC__)
#define UNIX_TCC_ENV
#else
#error BDLISP: Unidentified environment
#endif

/*************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <float.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <malloc.h>
#include <limits.h>

#ifdef DJGPP_ENV
#include <libm/math.h>
#else
#include <math.h>
#endif

/*************************************/

#define LISPstatic static

#ifdef WIN32_ENV
#define LISP_ENVIRONMENT "win32"
#define CELLBLOCKSIZE 8192
#include <io.h>
#include <direct.h>
#define F_OK 0
#define STACKSIZE 8000000
#define FPRESET _clear87
#define DIR_SEP "\\"
#define PATH_SEP ";"
#define LISP_HIST "_bdlisp.hst"
#define MAXINT INT_MAX
#define PATH_MAX _MAX_PATH
#define exception _exception
#define M_PI 3.14159265358979
#endif

#ifdef BORLANDC_ENV
#define LISP_ENVIRONMENT "borlandc"
#define CELLBLOCKSIZE 1024
#include <io.h>
#define F_OK 0
#include "cmdline.h"
#define READLINE
#define CONIO
#define HEAPCHECK
#define STACKSIZE 60000u
#define CTRL_C_CHECK
#define FPRESET _clear87
#define NO_STATIC_LIB
#define STATISTICS
#define DIR_SEP "\\"
#define PATH_SEP ";"
#define PATH_MAX MAXPATH
#define LISP_HIST "_bdlisp.hst"
/* #define REGEX */
#endif

#ifdef DJGPP_ENV
#define LISP_ENVIRONMENT "djgpp"
#define CELLBLOCKSIZE 8192
#include <go32.h>
#include <dpmi.h>
#include <unistd.h>
#include <sys/stat.h>
#define READLINE
#define POPEN
#define CONIO
#define STACKSIZE 8000000
#define CTRL_C_CHECK
#define FPRESET _clear87
#define DIR_SEP "\\"
#define PATH_SEP ";"
#define LISP_HIST "_bdlisp.hst"
#define REGEX
#endif

#ifdef UNIX_GCC_ENV
#define LISP_ENVIRONMENT "unix-gcc"
#define CELLBLOCKSIZE 8192
#include <unistd.h>
#include <sys/stat.h>
#define READLINE
#define POPEN
#define STACKSIZE 8000000
#define DIR_SEP "/"
#define PATH_SEP ":"
#define LISP_HIST ".bdlisp.hst"
#define REGEX
#define DYNLINK
#endif

#ifdef UNIX_TCC_ENV
#define LISP_ENVIRONMENT "unix-tcc"
#define CELLBLOCKSIZE 8192
#include <unistd.h>
#include <sys/stat.h>
#define READLINE
#define POPEN
#define STACKSIZE 8000000
#define DIR_SEP "/"
#define PATH_SEP ":"
#define LISP_HIST ".bdlisp.hst"
#define REGEX
#define DYNLINK
#endif

/*************************************/

#ifdef NOREADLINE
#undef READLINE
#endif

/*************************************/

#ifdef REGEX
#include <sys/types.h>
#include <regex.h>
#endif

/*************************************/

#ifdef DYNLINK
#include <dlfcn.h>
#endif

/*************************************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CONIO
#include <conio.h>
#endif

#ifdef LISPDEBUG

#define LISP_MAGIC 0x1a1a
#define LISP_MAGIC_EXT 0x1b1b

#define LISPassertfail(strcnd) { \
	LISPfatal("LISP assertion failed: %s, line %d, file %s\n", \
		strcnd, __LINE__, __FILE__); \
}

#define CHKPTR(ptr) \
	if ((ptr)->magic != LISP_MAGIC) \
		LISPassertfail("magic check")

#define LISPassert(cnd) \
	if (!(cnd)) LISPassertfail(#cnd)

#else

#define CHKPTR(ptr) { }
#define LISPassert(cnd) { }

#endif /* defined(LISPDEBUG) */

#if defined(HEAPCHECK)
#define CHKMEM() LISPassert(heapcheck() >= 0)
#else
#define CHKMEM() { }
#endif

/**************************************/

#define STRBUFBLKLEN 2048

struct StrBufBlk {
	struct StrBufBlk *next;
	char blk[STRBUFBLKLEN];
};

typedef struct {
	int curptr, length;
	struct StrBufBlk first, *last;
} LStrBuf;

void lsbInit(LStrBuf *sb);
void lsbAddChar(LStrBuf *sb, char c);
void lsbAddBlk(LStrBuf *sb, int len, char *blk);
void lsbAddStr(LStrBuf *sb, char *str);
void lsbPut(LStrBuf *sb, char *dst);
char *lsbGet(LStrBuf *sb);
void lsbDone(LStrBuf *sb);

/**************************************/

enum LISPtype {
	LT_FREE = 1,
	LT_SYMBOL,
	LT_NUMBER,
	LT_STRING,
	LT_STREAM,
	LT_CLOSURE,
	LT_OBJECT,
	LT_NODE,
	LT_REF,
	LT_SPECIALFORM,
	LT_BUILTIN,
	LT_TAILREPEAT,
	LT_USER_FIRST
};

enum LISPflags {
	LF_INTERN = 1,
	LF_DONTFREE = 2,
	LF_ISSPECIALFORM = 4,
	LF_ISMACRO = 8,
	LF_MULRET = 16,
	LF_CONST = 32,
	LF_ESCAPE = 64,
	LF_BINDCALLBACK = 128,
	LF_SPECIAL = 256,
	LF_SETNIL = 512,
	LF_SETUNIQ = 1024,
	LF_ARGKEYWORD = 2048,
	LF_COLON = 4096
};

#define LF_FUNCTION_FLAGS (LF_ISSPECIALFORM | LF_ISMACRO | LF_MULRET)

enum LISPstatus {
	LS_EXIT = 1,
	LS_ABORT = 2,
	LS_BREAK = 4,
	LS_USERBREAK = 8
};
extern enum LISPstatus LISPstatus;

/**************************************/

typedef struct LISPcell *PTR;

typedef PTR (*LISPbuiltinfun)(PTR);
typedef PTR (*LISPspecialform)(PTR, PTR);

struct LISPcell {
#ifdef LISPDEBUG
	unsigned magic;
#endif
	int nl;
	unsigned char mark, type;
	union {
		struct LISPnumber {
			double val;
		} number;
		struct {
			struct LISPsymbol_ext *sym;
		} symbol;
		struct LISPstring {
			int len;
			union {
				char *cptr;
				char str[sizeof(char *)];
			} cnt;
		} string;
		struct {
			struct LISPstream_ext *str;
		} stream;
		struct LISPbuiltin {
			LISPbuiltinfun fun;
		} builtin;
		struct LISPspecialform {
			LISPspecialform spform;
		} specialform;
		struct LISPclosure {
			PTR fun, env;
		} closure;
		struct LISPobject {
			PTR isa, fields;
		} object;
		struct LISPnode {
			PTR cdr, car;
		} node;
		struct LISPref {
			PTR nextref, *ptr;
		} ref;
		struct LISPuser {
			int info;
			void *ext;
		} user;
	} c;
};

#ifdef LISPDEBUG
struct LISPsymbol_ext *symbolPTRfnc(PTR ptr, int line, char *file);
#define symbolPTR(ptr) symbolPTRfnc(ptr, __LINE__, __FILE__)
#else
#define symbolPTR(ptr) ((ptr)->c.symbol.sym)
#endif

struct LISPsymbol_ext {
#ifdef LISPDEBUG
	unsigned magic;
#endif
	PTR bind;
	PTR prop;
	PTR fun;
	PTR key;
	PTR name;
	enum LISPflags flags;
	int kord;
	PTR unext, uprev; /* unlinked! */
	unsigned hash;
};

#ifdef LISPDEBUG
struct LISPstring *stringPTRfnc(PTR ptr, int line, char *file);
#define stringPTR(ptr) stringPTRfnc((ptr), __LINE__, __FILE__)
#else
#define stringPTR(ptr) (&((ptr)->c.string))
#endif

#define stringPTRstr(ptr) \
	(stringPTR(ptr)->len < sizeof(char *) ? \
		stringPTR(ptr)->cnt.str : \
		stringPTR(ptr)->cnt.cptr)

#ifdef LISPDEBUG
struct LISPnumber *numberPTRfnc(PTR ptr, int line, char *file);
#define numberPTR(ptr) numberPTRfnc(ptr, __LINE__, __FILE__)
#else
#define numberPTR(ptr) (&((ptr)->c.number))
#endif

#ifdef LISPDEBUG
struct LISPstream_ext *streamPTRfnc(PTR ptr, int line, char *file);
#define streamPTR(ptr) streamPTRfnc(ptr, __LINE__, __FILE__)
#else
#define streamPTR(ptr) ((ptr)->c.stream.str)
#endif

#define MAX_STREAM_UNGETC 10

enum LISPstreamtype {
	ST_CONSOLE,
	ST_FILE,
#ifdef POPEN
	ST_PIPE,
#endif
	ST_INSTRING,
	ST_OUTSTRING,
	ST_CLOSED
};

struct LISPstream_ext {
#ifdef LISPDEBUG
	unsigned magic;
#endif
	int seekflag, row, col;
	int eof;
	enum LISPstreamtype strtype;
	char ubuf[MAX_STREAM_UNGETC];
	int ubufptr;
	union {
		FILE *file;
		LStrBuf outstring;
		struct {
			PTR string;
			char *ptr;
			int len;
		} instring;
	} str;
};

#ifdef LISPDEBUG
struct LISPbuiltin *builtinPTRfnc(PTR ptr, int line, char *file);
#define builtinPTR(ptr) builtinPTRfnc(ptr, __LINE__, __FILE__)
#else
#define builtinPTR(ptr) (&((ptr)->c.builtin))
#endif

#ifdef LISPDEBUG
struct LISPspecialform *specialformPTRfnc(PTR ptr, int line, char *file);
#define specialformPTR(ptr) specialformPTRfnc(ptr, __LINE__, __FILE__)
#else
#define specialformPTR(ptr) (&((ptr)->c.specialform))
#endif

#ifdef LISPDEBUG
struct LISPclosure *closurePTRfnc(PTR ptr, int line, char *file);
#define closurePTR(ptr) closurePTRfnc(ptr, __LINE__, __FILE__)
#else
#define closurePTR(ptr) (&((ptr)->c.closure))
#endif

#ifdef LISPDEBUG
struct LISPobject *objectPTRfnc(PTR ptr, int line, char *file);
#define objectPTR(ptr) objectPTRfnc(ptr, __LINE__, __FILE__)
#else
#define objectPTR(ptr) (&((ptr)->c.object))
#endif

#ifdef LISPDEBUG
struct LISPnode *nodePTRfnc(PTR ptr, int line, char *file);
#define nodePTR(ptr) nodePTRfnc(ptr, __LINE__, __FILE__)
#else
#define nodePTR(ptr) (&((ptr)->c.node))
#endif

#ifdef LISPDEBUG
struct LISPref *refPTRfnc(PTR ptr, int line, char *file);
#define refPTR(ptr) refPTRfnc(ptr, __LINE__, __FILE__)
#else
#define refPTR(ptr) (&((ptr)->c.ref))
#endif

#ifdef LISPDEBUG
struct LISPuser *userPTRfnc(PTR ptr, int line, char *file);
#define userPTR(ptr) userPTRfnc(ptr, __LINE__, __FILE__)
#else
#define userPTR(ptr) (&((ptr)->c.user))
#endif

/**************************************/

struct LISPiterator {
	PTR ptr, slow;
	int state;
};
void LISPiterinit(struct LISPiterator *it, PTR ptr);
int LISPiternext(struct LISPiterator *it);

/**************************************/

int LISPaddusertype(char *name,
	int (*cmp)(PTR p1, PTR p2),
	void (*write)(PTR p, PTR stream),
	void (*mark)(PTR p), void (*free)(PTR p));

PTR LISPusertypes(void);

void LISPusertyperoom(void);

PTR unlinkLISPusertype(PTR ptr);
int LISPusercmp(PTR p1, PTR p2);
void LISPuserwrite(PTR ptr, PTR stream);
void LISPusermark(PTR ptr);
void LISPuserfree(PTR ptr);

void LISPclearusertypes(void);

PTR newuserPTR(int type);

/**************************************/

extern struct LISPcell LISPnil;
#define NILptr (&LISPnil)

/**************************************/

enum LISPinittabtype {
	LI_SYM, LI_STR, LI_NUM, LI_STM, LI_UNQ, LI_STA,
	LI_CST, LI_VAR, LI_FUN, LI_SPC,
	LI_END
};

struct LISPinittabelem {
	enum LISPinittabtype type;
	PTR *ptr;
	char *name;
	double *num;
	LISPbuiltinfun fun;
	LISPspecialform spform;
	int flags;
};

#define LISP_INIT_TAB(name)         struct LISPinittabelem name[] =
#define LISP_SYMBOL(name, ptr, fl)  { LI_SYM, &ptr, name, NULL, NULL, NULL, fl }
#define LISP_STRING(ptr, str)       { LI_STR, &ptr, str,  NULL, NULL, NULL, 0  }
#define LISP_NUMBER(ptr, num)       { LI_NUM, &ptr, NULL, &num, NULL, NULL, 0  }
#define LISP_UNIQ(ptr)              { LI_UNQ, &ptr, NULL, NULL, NULL, NULL, 0  }
#define LISP_STATIC(ptr)            { LI_STA, &ptr, NULL, NULL, NULL, NULL, 0  }
#define LISP_CONST(name, ptr)       { LI_CST, &ptr, name, NULL, NULL, NULL, 0  }
#define LISP_VAR(name, ptr)         { LI_VAR, &ptr, name, NULL, NULL, NULL, 0  }
#define LISP_FUNCT(name, fun, fl)   { LI_FUN, NULL, name, NULL, fun,  NULL, fl }
#define LISP_SPECIAL(name, fun, fl) { LI_SPC, NULL, name, NULL, NULL, fun,  fl }
#define LISP_TAB_LAST()             { LI_END, NULL, NULL, NULL, NULL, NULL, 0  }

extern void LISPprocessinittab(struct LISPinittabelem *tab);

/**************************************/

extern PTR CONSptr, NUMBERptr, SYMBOLptr, STRINGptr, VECTORptr,
	STREAMptr, BUILTINptr, SPECIAL_FORMptr, CLOSUREptr;

/**************************************/

extern PTR Tptr, EOFptr;
extern PTR PLUSptr, MINUSptr, STARptr;

#define nonNIL(ptr) ((ptr) != NILptr)
#define isNIL(ptr) ((ptr) == NILptr)

typedef struct {
	int len;
	unsigned char *blk;
} LISPpackedlib;
void LISPloadstaticlib(LISPpackedlib *lib, int print);

extern LISPpackedlib LISP_packed_lisplib;

void LISPinit(void);
void LISPdone(void);

extern PTR LISPstaticvars;

void LISPstoreargv(char **argv);
void LISPaddfeature(char *name);
void LISPaddbuiltinfun(char *name, LISPbuiltinfun fun, int flags);
void LISPaddspecialform(char *name, LISPspecialform fun, int flags);
void LISPaddstaticvar(PTR *ptr, PTR val);

void LISPmarkobjects(void);
void LISPmark(PTR ptr);

extern long LISPncells, LISPnsymbols, LISPnstreams, LISPnpool;
extern long LISPnintern;
extern long LISPstringbytes;
extern unsigned LISPhash_module;

void LISPinitstrings(void);
#ifdef REGEX
void LISPinitregexp(void);
#endif
	
PTR newsymbolPTR(PTR name);
PTR newstringPTR(int len, char *str);
PTR newstringPTRsb(LStrBuf *sb);
PTR newcharPTR(int c);
PTR newbuiltinPTR(LISPbuiltinfun fun);
PTR newspecialformPTR(LISPspecialform fun);
PTR newclosurePTR(PTR fun, PTR env);
PTR newobjectPTR(PTR isa);
PTR newnodePTR(void);
PTR newrefPTR(PTR *ptr);

PTR LISPnewcell(void);

#ifdef STATISTICS
extern unsigned long LISPrefcount;
#endif

#ifdef LISPDEBUG
PTR linkPTR(PTR ptr);
void unlinkPTR(PTR ptr);
#else
#define linkPTR(ptr) ((ptr)->nl++, (ptr))
#define unlinkPTR(ptr) { \
	if (--((ptr)->nl) <= 0) \
		freePTR((ptr)); \
}
#endif
void freePTR(PTR ptr);

PTR LISPnewstructure(PTR ptr);
PTR LISPnewlist(PTR ptr);

void LISPeraseallsymbols(void);

void LISPfreecore(void);

/*************************************/

void LISPreclaim(void);

/*************************************/

int LISPcomp(PTR ptr1, PTR ptr2);

/*************************************/

extern PTR LISPconsole;

PTR unlinkLISPinput(void);
void LISPpushinput(PTR input);
void LISPpopinput(void);
PTR unlinkLISPoutput(void);
void LISPpushoutput(PTR output);
void LISPpopoutput(void);

PTR unlinkLISPerror(void);
void LISPpusherror(PTR error);
void LISPpoperror(void);

extern char *LISPcurline;

int LISPgetc(PTR f);
int LISPungetc(int c, PTR f);

/*************************************/

#define GETARGS(parms) \
	if (LISPgetargs parms) \
		return LISPerrormsg(LISPgetargserror)

extern char *LISPgetargserror;
int LISPgetargs(char *spec, PTR args, ...);
PTR unlinkLISPkeyarg(PTR args, PTR key, PTR def);
int LISPvalidkeyargs(PTR args, PTR formal_key_args);

void LISPstackpush(PTR *stack, PTR ptr);
void LISPstackpushlist(PTR *stack, PTR ptr);
PTR LISPstackpop(PTR *stack);
void LISPpushval(PTR symbol, PTR value);
void LISPpopval(PTR symbol);
void LISPsetval(PTR symbol, PTR value);
void LISPbindcallback(PTR sym, PTR val);
void LISPerasesymbol(PTR sym);
void LISPuninternsymbol(PTR sym);
int LISPkeepsymbol(PTR sym);

/*************************************/

extern char *LISPstresc1, *LISPstresc2;
extern char *LISPsymesc1, *LISPsymesc2;

extern PTR LISPreadqueue;

extern PTR DOTptr, BEGIN_LISTptr, END_LISTptr, READ_EOFptr, READ_ABORTptr;

PTR LISP_read(PTR args);
PTR LISP_read_char(PTR args);
PTR LISP_peek_char(PTR args);
PTR LISP_read_line(PTR args);
PTR LISP_unread_char(PTR args);
PTR LISPreadobj(PTR f);

int LISPmustescape(PTR str);
int LISPisnumber(char *str);

PTR LISP_set_macro_character(PTR args);
PTR LISP_get_macro_character(PTR args);
void LISPinitcmacros(void);

PTR LISPbuiltincmacros(PTR args);
extern PTR QUOTEptr, BACKQUOTEptr, COMMAptr, COMMA_ATptr;

/*************************************/

extern PTR DISPLACE_MACROSptr;

extern PTR EVALHOOKptr, APPLYHOOKptr, DEBUGHOOKptr;
extern int LISPevalhookflag, LISPapplyhookflag;
extern int LISPallowreturn;

PTR LISP_eval(PTR ptr);
PTR LISPevalobj(PTR obj);
PTR LISPapplyfun(PTR fun, PTR args, int mulflag);

PTR LISP_evalhook(PTR args);
PTR LISP_applyhook(PTR args);

extern PTR LISPenvstack;
int LISPvalid_environment(PTR env);

PTR LISP_the_environment(PTR args);

PTR LISPmakelist(PTR first, ...);

PTR unlinkLISPgetval(PTR ptr);

/*************************************/

void LISPputprop(PTR *plist, PTR prop, PTR value);
void LISPremprop(PTR *plist, PTR prop);
PTR unlinkedLISPgetprop(PTR plist, PTR prop);

void LISPpushenvironment(PTR env);
void LISPpopenvironment(PTR env);

/*************************************/

PTR LISP_print(PTR ptr);
PTR LISP_prin1(PTR ptr);
PTR LISP_princ(PTR ptr);
PTR LISP_spaces(PTR args);
PTR LISP_terpri(PTR args);

PTR LISP_format(PTR args);

PTR LISP_error(PTR args);
PTR LISP_warn(PTR args);

void LISPwrite(PTR f, PTR ptr, int unescape);
void LISPnewline(PTR f);
int LISPputc(int c, PTR f);
int LISPputs(PTR f, char *str);
char *LISPnumformat(double n);
char *tostringPTR(PTR obj, int unescape);

void LISPdebugwrite(char *msg, ...);

extern int LISPdriverlevel;
extern int LISPreadlevel;

PTR LISP_driver(PTR args);

PTR LISP_car(PTR args);
PTR LISP_cdr(PTR args);
PTR LISP_caar(PTR args);
PTR LISP_cadr(PTR args);
PTR LISP_cdar(PTR args);
PTR LISP_cddr(PTR args);
PTR LISP_caaar(PTR args);
PTR LISP_caadr(PTR args);
PTR LISP_cadar(PTR args);
PTR LISP_caddr(PTR args);
PTR LISP_cdaar(PTR args);
PTR LISP_cdadr(PTR args);
PTR LISP_cddar(PTR args);
PTR LISP_cdddr(PTR args);

PTR LISP_quote(PTR args, PTR tail);
PTR LISP_backquote(PTR args, PTR tail);
PTR LISP_cons(PTR args);
PTR LISP_atom(PTR args);
PTR LISP_eq(PTR args);
PTR LISP_neq(PTR args);
PTR LISP_eql(PTR args);
PTR LISP_neql(PTR args);
PTR LISP_list(PTR args);
PTR LISP_list_star(PTR args);
PTR LISP_append(PTR args);
PTR LISP_reverse(PTR args);
PTR LISP_length(PTR args);
PTR LISP_elt(PTR args);
PTR LISP_nth(PTR args);
PTR LISP_nthcdr(PTR args);
PTR LISP_last(PTR args);

extern PTR TESTptr, EQLptr;

PTR LISP_assoc(PTR args);
PTR LISP_adjoin(PTR args);

PTR LISP_gensym(PTR args);
PTR LISP_gentemp(PTR args);
PTR LISP_make_symbol(PTR args);
PTR LISP_symbol_name(PTR args);
PTR LISP_symbol_value(PTR args);

PTR LISP_type_of(PTR args);
PTR LISP_types(PTR args);

PTR LISP_symbolp(PTR args);
PTR LISP_closurep(PTR args);
PTR LISP_stringp(PTR args);
PTR LISP_numberp(PTR args);
PTR LISP_listp(PTR args);
PTR LISP_consp(PTR args);
PTR LISP_endp(PTR args);
PTR LISP_streamp(PTR args);
PTR LISP_builtinp(PTR args);
PTR LISP_specialformp(PTR args);
PTR LISP_constantp(PTR args);
PTR LISP_keywordp(PTR args);
PTR LISP_less(PTR args);
PTR LISP_lesseq(PTR args);
PTR LISP_greater(PTR args);
PTR LISP_greatereq(PTR args);
PTR LISP_equalsign(PTR args);
PTR LISP_slashequal(PTR args);
PTR LISP_max(PTR args);
PTR LISP_min(PTR args);

PTR LISP_equal(PTR args);
PTR LISP_null(PTR args);
PTR LISP_identity(PTR args);

extern PTR OBJECT_PRINT_FUNCTIONptr;

PTR LISP_make_object(PTR args);
PTR LISP_objectp(PTR args);
PTR LISP_object_type(PTR args);

PTR LISP_get(PTR args);
PTR LISP_put(PTR args);
PTR LISP_remprop(PTR args);
PTR LISP_getprops(PTR args);
PTR LISP_getplist(PTR args);

PTR LISP_set(PTR args);
PTR LISP_setq(PTR args, PTR tail);
PTR LISP_psetq(PTR args, PTR tail);
PTR LISP_makunbound(PTR args);

PTR LISP_ascii(PTR args);

PTR LISP_copy_list(PTR args);
PTR LISP_copy_tree(PTR args);
PTR LISP_rplaca(PTR args);
PTR LISP_rplacd(PTR args);
PTR LISP_nconc(PTR args);

extern PTR SORTPREDptr;
PTR LISP_sort(PTR args);
PTR LISP_uniq(PTR args);

/**************************************/

PTR LISP_member(PTR args);
PTR LISP_member_if(PTR args);
PTR LISP_member_if_not(PTR args);
PTR LISP_position(PTR args);
PTR LISP_position_if(PTR args);
PTR LISP_position_if_not(PTR args);
PTR LISP_count(PTR args);
PTR LISP_count_if(PTR args);
PTR LISP_count_if_not(PTR args);
PTR LISP_remove(PTR args);
PTR LISP_remove_if(PTR args);
PTR LISP_remove_if_not(PTR args);
PTR LISP_delete(PTR args);
PTR LISP_delete_if(PTR args);
PTR LISP_delete_if_not(PTR args);
PTR LISP_find(PTR args);
PTR LISP_find_if(PTR args);
PTR LISP_find_if_not(PTR args);

/**************************************/

unsigned LISPstrhash(PTR str);
int LISPstreql(PTR str1, PTR str2);
int LISPstrcmp(PTR str1, PTR str2);

PTR LISP_string_hash(PTR args);
PTR LISP_concat(PTR args);
PTR LISP_substr(PTR args);
PTR LISP_findstr(PTR args);
PTR LISP_dupstr(PTR args);
PTR LISP_tokstr(PTR args);

PTR LISP_cgi_encode(PTR args);
PTR LISP_cgi_decode(PTR args);
PTR LISP_html_escape(PTR args);

PTR LISP_string_upcase(PTR args);
PTR LISP_string_downcase(PTR args);
PTR LISP_string_capitalize(PTR args);

void LISPscrambleblock(int len, char *blk);
PTR LISP_string_scramble(PTR args);

/**************************************/

void LISPinitvectors(void);

/**************************************/

PTR LISP_apply(PTR args);
PTR LISP_funcall(PTR args);
PTR LISP_mapl(PTR args);
PTR LISP_maplist(PTR args);
PTR LISP_mapcon(PTR args);
PTR LISP_mapc(PTR args);
PTR LISP_mapcar(PTR args);
PTR LISP_mapcan(PTR args);
PTR LISP_reduce(PTR args);

/**************************************/

extern PTR ZEROptr, ONEptr;

void LISPmathtry(void);
PTR LISPmathcheck(double val);
void LISPfpecatch(int sig);
extern void (*LISPoldfpecatch)(int sig);

PTR newnumberPTR(double val);

PTR LISP_plus(PTR args);
PTR LISP_difference(PTR args);
PTR LISP_times(PTR args);
PTR LISP_quotient(PTR args);
PTR LISP_oneplus(PTR args);
PTR LISP_oneminus(PTR args);
PTR LISP_evenp(PTR args);
PTR LISP_oddp(PTR args);
PTR LISP_zerop(PTR args);
PTR LISP_minusp(PTR args);
PTR LISP_plusp(PTR args);
PTR LISP_rem(PTR args);
PTR LISP_abs(PTR args);
PTR LISP_ceiling(PTR args);
PTR LISP_floor(PTR args);
PTR LISP_sqrt(PTR args);
PTR LISP_exp(PTR args);
PTR LISP_log(PTR args);
PTR LISP_expt(PTR args);
PTR LISP_sin(PTR args);
PTR LISP_cos(PTR args);
PTR LISP_tan(PTR args);
PTR LISP_asin(PTR args);
PTR LISP_acos(PTR args);
PTR LISP_atan(PTR args);
PTR LISP_atan2(PTR args);
PTR LISP_gcd(PTR args);
PTR LISP_logand(PTR args);
PTR LISP_logior(PTR args);
PTR LISP_logxor(PTR args);
PTR LISP_lognot(PTR args);
PTR LISP_random(PTR args);
PTR LISP_randomize(PTR args);

/**************************************/

PTR LISP_cls(PTR args);
PTR LISP_cursor(PTR args);

PTR LISP_room(PTR args);
PTR LISP_clock(PTR args);
PTR LISP_get_universal_time(PTR args);
PTR LISP_get_decoded_time(PTR args);
PTR LISP_abort(PTR args);
PTR LISP_error(PTR args);
PTR LISP_exit(PTR args);
PTR LISP_error_exit(PTR args);

/**************************************/

PTR LISP_oblist(PTR args);
PTR LISP_intern(PTR args);
PTR LISP_unintern(PTR args);

PTR LISPlookup(PTR name);
PTR LISPlookup_blk(int len, char *name);

void LISPchecksymbols(void);

/**************************************/

extern PTR LAMBDAptr, MACROptr;
extern PTR OPTIONALptr, RESTptr, KEYptr, AUXptr;

extern PTR LISPexitvalue;
extern PTR LISPcatchstack;
extern PTR LISPthrownsymbol, LISPthrownvalue;

extern PTR RETURN_THROWptr;

extern PTR LISPmret;

void LISPqueueinit(PTR *head, PTR *tail);
void LISPqueueadd(PTR *head, PTR *tail, PTR ptr);
void LISPqueueaddassoc(PTR *head, PTR *tail, PTR ptr1, PTR ptr2);
void LISPqueueappend(PTR *head, PTR *tail, PTR ptr);

PTR LISP_getd(PTR args);
PTR LISP_putd(PTR args);
PTR LISP_movd(PTR args);
PTR LISP_function(PTR args, PTR tail);
PTR LISP_specialp(PTR args);
PTR LISP_macrop(PTR args);
PTR LISP_valid_lambda(PTR args);
PTR LISP_valid_lambda_args(PTR args);
PTR LISP_cond(PTR args, PTR tail);
PTR LISP_do_loop(PTR args, PTR tail);
PTR LISP_prog1(PTR args, PTR tail);
PTR LISP_multiple_value_prog1(PTR args, PTR tail);
PTR LISP_progn(PTR args, PTR tail);
PTR LISP_and(PTR args, PTR tail);
PTR LISP_or(PTR args, PTR tail);
PTR LISP_catch(PTR args, PTR tail);
PTR LISP_throw(PTR args);
PTR LISP_return(PTR args);
PTR LISP_unwind_protect(PTR args, PTR tail);
PTR LISP_values(PTR args);
PTR LISP_values_list(PTR args);
PTR LISP_multiple_value_list(PTR args, PTR tail);
PTR LISP_macroexpand(PTR args);
PTR LISP_macroexpand_1(PTR args);

PTR LISPevaluserfun(PTR fun, PTR args, PTR env, PTR tail);

void LISPprintmret(PTR f, PTR single);

#define LISPresetmret() { \
	if (LISPmret) { \
		unlinkPTR(LISPmret); \
		LISPmret = NULL; \
	} \
}

int LISPvalidfunction(PTR ptr);

/**************************************/

char *LISPstreamtostr(PTR f);

PTR newconsolestreamPTR(void);
PTR newfilestreamPTR(FILE *f);
PTR newinstringstreamPTR(PTR str);
PTR newoutstringstreamPTR(void);
void LISPerasestream(PTR stream);

extern PTR STANDARD_INPUTptr, STANDARD_OUTPUTptr, STANDARD_ERRORptr;
extern PTR TERMINAL_IOptr;
extern PTR LISPstdin, LISPstdout, LISPstderr;
extern int LISPonterminal;

extern char *LISPdirsep;
extern char *LISPpathsep;
extern PTR PATHptr;

extern PTR VERBOSEptr, PRINTptr;
extern PTR LOAD_SOURCEptr;

void LISPdirname(char *buf, char *path);
void LISPpathjoin(char *buf, char *path, char *name);
FILE *LISPpathfopen(char *name, char **actual);
char *LISPpathfind(char *name, PTR path_list);

PTR LISP_stream_row(PTR args);
PTR LISP_stream_column(PTR args);
PTR LISP_fopen(PTR args);
PTR LISP_popen(PTR args);
PTR LISP_fread(PTR args);
PTR LISP_fwrite(PTR args);
PTR LISP_make_string_input_stream(PTR args);
PTR LISP_make_string_output_stream(PTR args);
PTR LISP_get_output_stream_string(PTR args);
PTR LISP_close(PTR args);
PTR LISP_fflush(PTR args);
PTR LISP_ftell(PTR args);
PTR LISP_fseek(PTR args);
PTR LISP_feof(PTR args);
PTR LISP_isatty(PTR args);
PTR LISP_load(PTR args);
PTR LISP_shell(PTR args);
PTR LISP_getenv(PTR args);
PTR LISP_path_find(PTR args);
PTR LISP_getcwd(PTR args);
PTR LISP_chdir(PTR args);
PTR LISP_mkdir(PTR args);
PTR LISP_unlink(PTR args);

#ifdef DYNLINK
PTR LISP_load_dynamic_library(PTR args);
#endif

extern int LISPloadlevel;
int LISPloadpathfile(char *name, int print);
PTR LISPloadfile(FILE *f, int print, PTR source);
PTR LISPloadstream(PTR stream, int print, PTR source);

PTR LISPsystemerror(void);

/**************************************/

PTR LISP_fopen(PTR args);
PTR LISP_fclose(PTR args);

/**************************************/

extern PTR BREAKptr, RETURNptr, ABORTptr, CONTINUEptr, ERROR_EXITptr;
extern PTR unlinkedLISPbreakfun, unlinkedLISPbreakargs;
extern int LISPbreakeval;

void LISPintcatch(int sig);
extern void (*LISPoldintcatch)(int sig);
void LISPbreakcheck(void);

extern int LISPdebuglevel;

#define MAX_ERROR_MSG 1024

PTR LISP_break(PTR args);
PTR LISP_boundp(PTR args);
PTR LISP_getvals(PTR args);
PTR LISPerrormsg(char *msg, ...);
PTR LISP_debug_driver(PTR ptr);

/**************************************/

extern void *LISPstackbase;
#define LISPfullstack(var) ((char *) LISPstackbase - (char *) &var > STACKSIZE)

void LISPfatal(char *msg, ...);

extern char NO_MEM_MSG[];
extern char FULL_STACK_MSG[];
extern char NON_SYMBOL_MSG[];
extern char NON_STRING_MSG[];
extern char NON_VECTOR_MSG[];
extern char NON_CHAR_MSG[];
extern char NON_NUMERIC_MSG[];
extern char NON_STREAM_MSG[];
extern char NON_NODE_MSG[];
extern char NON_LIST_MSG[];
extern char NON_SAME_MSG[];
extern char UNBOUND_SYMBOL_MSG[];
extern char ILLEGAL_TYPE_MSG[];
extern char ILL_FORM_HEAD_MSG[];
extern char CANT_SET_MSG[];
extern char UNK_TYPE_IN_MSG[];
extern char BAD_FUNCTION_MSG[];
extern char BAD_VECT_IDX_MSG[];
extern char CIRCULAR_MSG[];

void *emalloc(size_t size);
void *ecalloc(size_t nitems, size_t size);
char *estrdup(char *str);

#ifdef __cplusplus
}
#endif

#endif

