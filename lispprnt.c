/*

	LISPPRNT.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzione PRINT.

*/

#include "bdlisp.h"

PTR OBJECT_PRINT_FUNCTIONptr;

PTR unlinkLISPoutput(void)
{
	return unlinkLISPgetval(STANDARD_OUTPUTptr);
}

void LISPpushoutput(PTR output)
{
	if (!output)
		output = LISPonterminal ? LISPconsole : LISPstdout;

	LISPpushval(STANDARD_OUTPUTptr, output);
}

void LISPpopoutput(void)
{
	LISPpopval(STANDARD_OUTPUTptr);
}

PTR unlinkLISPerror(void)
{
	return unlinkLISPgetval(STANDARD_ERRORptr);
}

void LISPpusherror(PTR error)
{
	if (!error)
		error = LISPonterminal ? LISPconsole : LISPstdout;

	LISPpushval(STANDARD_ERRORptr, error);
}

void LISPpoperror(void)
{
	LISPpopval(STANDARD_ERRORptr);
}

PTR LISP_print(PTR args)
{
	PTR ret, stream;

	GETARGS(("a?f", args, &ret, &stream, unlinkLISPoutput()));
	LISPwrite(stream, ret, 0);
	LISPnewline(stream);
	return linkPTR(ret);
}

PTR LISP_prin1(PTR args)
{
	PTR ret, stream;

	GETARGS(("a?f", args, &ret, &stream, unlinkLISPoutput()));
	LISPwrite(stream, ret, 0);
	return linkPTR(ret);
}

PTR LISP_princ(PTR args)
{
	PTR ret, stream;

	GETARGS(("a?f", args, &ret, &stream, unlinkLISPoutput()));
	LISPwrite(stream, ret, 1);
	return linkPTR(ret);
}

PTR LISP_spaces(PTR args)
{
	PTR num, stream;
	int n;

	GETARGS(("n?f", args, &num, &stream, unlinkLISPoutput()));
	n = (int) numberPTR(num)->val;
	while (n-- > 0)
		LISPputc(' ', stream);
	return linkPTR(NILptr);
}

PTR LISP_terpri(PTR args)
{
	PTR stream;

	GETARGS(("?f", args, &stream, unlinkLISPoutput()));
	LISPnewline(stream);
	return linkPTR(NILptr);
}

void LISPwrite(PTR f, PTR ptr, int unescape)
{
	char *c, *key;
	static char buf[50];
	int i;
	PTR str, isa, printer, tmp, ret;
	struct LISPiterator it;
	
	if (!f)
		f = unlinkLISPoutput();
	if (LISPstatus & LS_BREAK)
		return;
	if (!ptr) {
		LISPputs(f, "#<null>");
		return;
	}
	CHKPTR(ptr);
	switch (ptr->type) { /* TYPESWITCH */
	case LT_FREE: /* just for debugging, will crash soon... */
		LISPputs(f, "#<free-cell>");
		break;
	case LT_SYMBOL:
		str = symbolPTR(ptr)->name;
		i = stringPTR(str)->len;
		c = stringPTRstr(str);
		if (unescape || !(symbolPTR(ptr)->flags & LF_ESCAPE
			|| LISPmustescape(str))
		) {
			while (i > 0) {
				LISPputc(*c, f);
				i--;
				c++;
			}
		} else {
			LISPputc('|', f);
			while (i > 0) {
				if (isprint((unsigned char) *c)) {
					if (*c == '|' || *c == '\\')
						LISPputc('\\', f);
					LISPputc(*c, f);
				} else {
					sprintf(buf, "\\%03o", (unsigned char) *c);
					LISPputs(f, buf);
				}
				i--;
				c++;
			}
			LISPputc('|', f);
		}
		break;
	case LT_STRING:
		i = stringPTR(ptr)->len;
		c = stringPTRstr(ptr);
		if (unescape) {
			while (i > 0) {
				LISPputc(*c, f);
				i--;
				c++;
			}
		} else {
			LISPputc('"', f);
			while (i > 0) {
				key = strchr(LISPstresc2, *c);
				if (key && !*key)
					key = NULL;
				if (!key && !isprint((unsigned char) *c)) {
					sprintf(buf, "\\%03o", (unsigned char) *c);
					LISPputs(f, buf);
				} else {
					if (key && *key) {
						LISPputc('\\', f);
						LISPputc(LISPstresc1[(int) (key - LISPstresc2)], f);
					} else {
						LISPputc(*c, f);
					}
				}
				i--;
				c++;
			}
			LISPputc('"', f);
		}
		break;
	case LT_NUMBER:
		sprintf(buf, LISPnumformat(numberPTR(ptr)->val),
			numberPTR(ptr)->val);
		LISPputs(f, buf);
		break;
	case LT_BUILTIN:
		sprintf(buf, "#<builtin:%p>", builtinPTR(ptr)->fun);
		LISPputs(f, buf);
		break;
	case LT_SPECIALFORM:
		sprintf(buf, "#<special-form:%p>", specialformPTR(ptr)->spform);
		LISPputs(f, buf);
		break;
	case LT_CLOSURE:
		sprintf(buf, "#<closure:%p>", ptr);
		LISPputs(f, buf);
		break;
	case LT_STREAM:
		LISPputs(f, LISPstreamtostr(ptr));
		break;
	case LT_OBJECT:
		isa = objectPTR(ptr)->isa;
		printer = unlinkedLISPgetprop(symbolPTR(isa)->prop, OBJECT_PRINT_FUNCTIONptr);
		if (printer && !isNIL(printer)) {
			tmp = LISPmakelist(ptr, f, NULL);
			ret = LISPapplyfun(printer, tmp, 0);
			unlinkPTR(ret);
			unlinkPTR(tmp);
		} else {
			LISPputs(f, "#<");
			LISPwrite(f, isa, 0);
			sprintf(buf, ":%p>", ptr);
			LISPputs(f, buf);
		}
		break;
	case LT_NODE:
		LISPputc('(', f);
		LISPiterinit(&it, ptr);
		while (it.ptr->type == LT_NODE) {
			LISPwrite(f, nodePTR(it.ptr)->car, unescape);
#ifdef CTRL_C_CHECK
			LISPbreakcheck();
#endif
			if (LISPstatus & LS_BREAK) {
				LISPputs(f, "^C\n");
				return;
			}
			if (LISPiternext(&it)) {
				LISPputs(f, " #<circular>");
				it.ptr = NILptr;
				break;
			}
			if (it.ptr != NILptr)  LISPputc(' ', f);
		}
		if (!(LISPstatus & LS_BREAK) && it.ptr != NILptr) {
			LISPputs(f, ". ");
			LISPwrite(f, it.ptr, unescape);
		}
		if (!(LISPstatus & LS_BREAK))
			LISPputc(')', f);
		break;
	case LT_REF:
		sprintf(buf, "#<reference:%p>", ptr);
		LISPputs(f, buf);
		break;
	default:
		LISPuserwrite(ptr, f);
		break;
	}
}

void LISPdebugwrite(char *msg, ...)
{
	va_list vl;

	va_start(vl, msg);
	while (*msg) {
		if (*msg == '~') {
			LISPwrite(unlinkLISPerror(), va_arg(vl, PTR), 0);
		} else {
			LISPputc(*msg, unlinkLISPerror());
		}
		msg++;
	}
	va_end(vl);
}

void LISPnewline(PTR f)
{
	if (!f)
		f = unlinkLISPoutput();
	LISPputc('\n', f);
}

int LISPputc(int c, PTR f)
{
	int ret = 0;

	if (!f)
		f = unlinkLISPoutput();
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
		ret = fputc(c, stdout) == EOF;
		break;
	case ST_FILE:
#ifdef POPEN
	case ST_PIPE:
#endif
		ret = fputc(c, streamPTR(f)->str.file) == EOF;
		break;
	case ST_INSTRING:
		ret = 1;
		break;
	case ST_OUTSTRING:
		lsbAddChar(&(streamPTR(f)->str.outstring), (char) c);
		break;
	case ST_CLOSED:
		ret = 1;
		break;
	default:
		LISPfatal("illegal stream type in LISPputc");
		break;
	}

	if (ret == 0) {
		if (c == '\r') {
			streamPTR(f)->col = 0;
		} else if (c == '\n') {
			streamPTR(f)->row++;
			streamPTR(f)->col = 0;
		} else if (c == '\t') {
			streamPTR(f)->col = (streamPTR(f)->col & ~7) + 8;
		} else {
			streamPTR(f)->col++;
		}
	}

	return ret;
}

int LISPputs(PTR f, char *str)
{
	int res;

	if (!f)
		f = unlinkLISPoutput();
	while (*str) {
		res = LISPputc(*(str++), f);
		if (res == EOF)
			return res;
	}
	return 0;
}

char *LISPnumformat(double n)
{
	if (fabs(n) <= INT_MAX && n == floor(n))
		return "%1.0f";
	return "%g";
}

/*******************************************/

LISPstatic PTR newstream(void)
{
	PTR ret;

	ret = LISPnewcell();
	ret->type = LT_STREAM;
	ret->c.stream.str = ecalloc(1, sizeof(*(ret->c.stream.str)));
#ifdef LISPDEBUG
	ret->c.stream.str->magic = LISP_MAGIC_EXT;
#endif
	LISPnstreams++;
	return ret;
}

PTR newconsolestreamPTR(void)
{
	PTR ret;

	ret = newstream();
	streamPTR(ret)->strtype = ST_CONSOLE;
	return ret;
}

PTR newfilestreamPTR(FILE *f)
{
	PTR ret;

	ret = newstream();
	streamPTR(ret)->strtype = ST_FILE;
	streamPTR(ret)->str.file = f;
	return ret;
}

PTR newinstringstreamPTR(PTR str)
{
	PTR ret;

	ret = newstream();
	streamPTR(ret)->strtype = ST_INSTRING;
	streamPTR(ret)->str.instring.string = linkPTR(str);
	streamPTR(ret)->str.instring.len = stringPTR(str)->len;
	streamPTR(ret)->str.instring.ptr = stringPTRstr(str);
	return ret;
}

PTR newoutstringstreamPTR(void)
{
	PTR ret;

	ret = newstream();
	streamPTR(ret)->strtype = ST_OUTSTRING;
	lsbInit(&(streamPTR(ret)->str.outstring));
	return ret;
}

void LISPerasestream(PTR ptr)
{
	switch (streamPTR(ptr)->strtype) {
	case ST_CONSOLE:
		break;
	case ST_FILE:
		if (streamPTR(ptr)->str.file
			&& streamPTR(ptr)->str.file != stdin
			&& streamPTR(ptr)->str.file != stdout
			&& streamPTR(ptr)->str.file != stderr)
			fclose(streamPTR(ptr)->str.file);
		break;
#ifdef POPEN
	case ST_PIPE:
		if (streamPTR(ptr)->str.file)
			pclose(streamPTR(ptr)->str.file);
		break;
#endif
	case ST_INSTRING:
		unlinkPTR(streamPTR(ptr)->str.instring.string);
		break;
	case ST_OUTSTRING:
		lsbDone(&(streamPTR(ptr)->str.outstring));
		break;
	case ST_CLOSED:
		break;
	default:
		LISPfatal("unknown stream type in erasestreamPTR");
		break;
	}
}

char *tostringPTR(PTR obj, int unescape)
{
	PTR stream;
	char *ret;

	stream = newoutstringstreamPTR();
	LISPwrite(stream, obj, unescape);
	ret = lsbGet(&(streamPTR(stream)->str.outstring));
	unlinkPTR(stream);
	return ret;
}

