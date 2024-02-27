/*

	LISPREAD.H - 1997 - Bigdogs Internescional.

	Interprete LISP - funzione READ.

*/

#include "bdlisp.h"

int LISPreadlevel = 0;

static char conv1[] = "|ntvbrfa\"\\";
static char conv2[] = "|\n\t\v\b\r\f\a\"\\";

char *LISPsymesc1 = conv1;
char *LISPsymesc2 = conv2;

char *LISPstresc1 = conv1 + 1;
char *LISPstresc2 = conv2 + 1;

#define isodigit(c) (isdigit(c) && (c) < '8')

PTR LISPreadqueue;

LISPstatic int escape(LStrBuf *sb, PTR f);

PTR DOTptr, BEGIN_LISTptr, END_LISTptr, READ_EOFptr, READ_ABORTptr;

LISPstatic PTR s_expr(PTR f, int dot_allowed);
LISPstatic void queue_s_expr(PTR f, int dot_allowed);
LISPstatic PTR list(PTR f);
LISPstatic PTR atom(PTR f, int dot_allowed);

static PTR cmacro[256];

void LISPdefbuiltincmacro(char c, LISPbuiltinfun fun);

PTR LISP_read(PTR args)
{
	PTR stream;

	GETARGS(("?f", args, &stream, unlinkLISPinput()));
	return LISPreadobj(stream);
}

PTR LISP_read_char(PTR args)
{
	PTR stream;
	int c;

	GETARGS(("?f", args, &stream, unlinkLISPinput()));
	c = LISPgetc(stream);
	if (c == EOF)
		return linkPTR(READ_EOFptr);
	return newcharPTR(c);
}

PTR LISP_peek_char(PTR args)
{
	PTR stream, skipwhite;
	int c;

	GETARGS(("a?f", args, &skipwhite, &stream, unlinkLISPinput()));

	c = LISPgetc(stream);
	if (skipwhite != NILptr)
		while (c != EOF && isspace(c))
			c = LISPgetc(stream);
	LISPungetc(c, stream);
	if (c == EOF)
		return linkPTR(READ_EOFptr);
	return newcharPTR(c);
}

PTR LISP_read_line(PTR args)
{
	PTR ret, stream;
	int c;
	LStrBuf sb;

	GETARGS(("?f", args, &stream, unlinkLISPinput()));
	lsbInit(&sb);
	while ((c = LISPgetc(stream)) != '\n' && c != EOF)
		lsbAddChar(&sb, (char) c);
	if (c == EOF && sb.length == 0) {
		lsbDone(&sb);
		return linkPTR(READ_EOFptr);
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

PTR LISP_unread_char(PTR args)
{
	PTR ch, stream;

	GETARGS(("s?f", args, &ch, &stream, unlinkLISPinput()));
	if (stringPTR(ch)->len != 1)
		return LISPerrormsg(NON_CHAR_MSG);
	return LISPungetc(*stringPTRstr(ch), stream) ?
		linkPTR(NILptr) : linkPTR(Tptr);
}

PTR LISPreadobj(PTR f)
{
	PTR ret;

	if (!f)
		f = unlinkLISPinput();

	ret = s_expr(f, 0);
	if (ret == END_LISTptr) {
		unlinkPTR(ret);
		LISPputs(unlinkLISPerror(), "Warning: extra ')'\n");
		ret = linkPTR(NILptr);
	}
	return ret;

}

PTR QUOTEptr, BACKQUOTEptr, COMMAptr, COMMA_ATptr;

LISPstatic PTR bmc_args(PTR args, PTR *f, int *c)
{
	PTR ftmp, ctmp;

	GETARGS(("fs", args, &ftmp, &ctmp));
	if (stringPTR(ctmp)->len != 1)
		return LISPerrormsg(NON_CHAR_MSG);
	if (f)
		*f = ftmp;
	if (c)
		*c = (unsigned char) *stringPTRstr(ctmp);

	return NULL;
}

LISPstatic PTR bmc_enclose(PTR head, PTR args)
{
	PTR f = NULL, tmp, ret;

	ret = bmc_args(args, &f, NULL);
	if (ret)
		return ret;

	tmp = LISPreadobj(f);
	ret = LISPmakelist(head, tmp, NULL);
	unlinkPTR(tmp);
	return ret;
}

LISPstatic PTR bmc_quote(PTR args)
{
	return bmc_enclose(QUOTEptr, args);
}

LISPstatic PTR bmc_backquote(PTR args)
{
	return bmc_enclose(BACKQUOTEptr, args);
}

LISPstatic PTR bmc_comma(PTR args)
{
	PTR f = NULL, tmp, ret;
	int at;

	ret = bmc_args(args, &f, NULL);
	if (ret)
		return ret;

	at = LISPgetc(f);
	if (at == '@') {
		at = 1;
	} else {
		LISPungetc(at, f);
		at = 0;
	}

	tmp = LISPreadobj(f);
	ret = LISPmakelist((at ? COMMA_ATptr : COMMAptr), tmp, NULL);
	unlinkPTR(tmp);
	return ret;
}

LISPstatic PTR bmc_begin_list(PTR args)
{
	PTR f = NULL, ret;

	ret = bmc_args(args, &f, NULL);
	if (ret)
		return ret;

	LISPreadlevel++;
	ret = list(f);
	LISPreadlevel--;
	return ret;
}

LISPstatic PTR bmc_end_list(PTR args)
{
	PTR f, ret;

	ret = bmc_args(args, &f, NULL);
	if (ret)
		return ret;

	return linkPTR(END_LISTptr);
}

LISPstatic PTR bmc_string(PTR args)
{
	PTR f = NULL, ret;
	int c, term = 0, n, i;
	LStrBuf sb;
	char *key;

	ret = bmc_args(args, &f, &term);
	if (ret)
		return ret;

	lsbInit(&sb);
	for (;;) {
		c = LISPgetc(f);
		if (c == term)
			break;
		switch (c) {
		case EOF:
			lsbDone(&sb);
			LISPputs(unlinkLISPerror(), "Read abort: unterminated string\n");
			return linkPTR(READ_EOFptr);
		case '\\':
			c = LISPgetc(f);
			if (c == EOF) {
				c = '\\';
			} else if (isodigit(c)) {
				n = c - '0';
				for (i = 0; i < 2; i++) {
					c = LISPgetc(f);
					if (c == EOF)
						break;
					if (!isodigit(c)) {
						LISPungetc(c, f);
						break;
					}
					n = n * 8 + (c - '0');
				}
				c = n;
			} else {
				key = strchr(LISPstresc1, c);
				if (key && *key)
					c = LISPstresc2[(int) (key - LISPstresc1)];
			}
			lsbAddChar(&sb, (char) c);
			break;
		default:
			lsbAddChar(&sb, (char) c);
			break;
		}
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

LISPstatic PTR bmc_semicolon(PTR args)
{
	PTR f = NULL, tmp;
	int c;

	tmp = bmc_args(args, &f, NULL);
	if (tmp)
		return tmp;

	do {
		c = LISPgetc(f);
	} while (c != EOF && c != '\n');
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = linkPTR(NILptr);
	return linkPTR(NILptr);
}

void LISPdefbuiltincmacro(char c, LISPbuiltinfun fun)
{
	unlinkPTR(cmacro[(unsigned char) c]);
	cmacro[(unsigned char) c] = newbuiltinPTR(fun);
}

void LISPinitcmacros(void)
{
	int i;

	for (i = 0; i < 256; i++) 
		LISPaddstaticvar(cmacro + i, NILptr);
		/* cmacro[i] = linkPTR(NILptr); */

	LISPdefbuiltincmacro('\'', bmc_quote);
	LISPdefbuiltincmacro('`', bmc_backquote);
	LISPdefbuiltincmacro(',', bmc_comma);
	LISPdefbuiltincmacro('(', bmc_begin_list);
	LISPdefbuiltincmacro(')', bmc_end_list);
	LISPdefbuiltincmacro('"', bmc_string);
	LISPdefbuiltincmacro(';', bmc_semicolon);
}

PTR LISP_set_macro_character(PTR args)
{
	PTR chr, fun;
	int c;

	GETARGS(("sa", args, &chr, &fun));
	if (stringPTR(chr)->len != 1)
		return LISPerrormsg(NON_CHAR_MSG);
	c = (unsigned char) *stringPTRstr(chr);

	if ((fun->type != LT_SYMBOL)
			&& (fun->type != LT_BUILTIN)
			&& !LISPvalidfunction(fun))
		return LISPerrormsg(BAD_FUNCTION_MSG);

	unlinkPTR(cmacro[c]);
	cmacro[c] = linkPTR(fun);

	return linkPTR(chr);
}

PTR LISP_get_macro_character(PTR args)
{
	PTR chr, ret;

	GETARGS(("s", args, &chr));
	if (stringPTR(chr)->len != 1)
		return LISPerrormsg(NON_CHAR_MSG);

	ret = cmacro[(unsigned char) *stringPTRstr(chr)];

	return linkPTR(ret);
}

LISPstatic PTR s_expr(PTR f, int dot_allowed)
{
	while (LISPreadqueue->type != LT_NODE)
		queue_s_expr(f, dot_allowed);
	return LISPstackpop(&LISPreadqueue);
}

LISPstatic int skipspace(PTR f)
{
	int c;

	while ((c = LISPgetc(f)) != EOF && isascii(c) && isspace(c))
		;
	LISPungetc(c, f);
	return c == EOF;
}

LISPstatic void queue_s_expr(PTR f, int dot_allowed)
{
	int c;
	PTR ret, args, cc;

	if (skipspace(f)) {
		LISPstackpush(&LISPreadqueue, READ_EOFptr);
		return;
	}

	c = LISPgetc(f);
	if (c == EOF) {
		LISPstackpush(&LISPreadqueue, READ_EOFptr);
		return;
	}

	ret = cmacro[(unsigned char) c];
	CHKPTR(ret);
	if (nonNIL(ret)) {
		cc = newcharPTR(c);
		args = LISPmakelist(f, cc, NULL);
		ret = LISPapplyfun(ret, args, 1);
		unlinkPTR(args);
		unlinkPTR(cc);
		if (LISPmret) {
			unlinkPTR(ret);
			ret = linkPTR(LISPmret);
			LISPresetmret();
			LISPstackpushlist(&LISPreadqueue, ret);
			unlinkPTR(ret);
			return;
		}
		LISPstackpush(&LISPreadqueue, ret);
		unlinkPTR(ret);
		return;
	}
	LISPungetc(c, f);
	ret = atom(f, dot_allowed);
	LISPstackpush(&LISPreadqueue, ret);
	unlinkPTR(ret);
}

LISPstatic PTR list(PTR f)
{
	int start_row;
	static char rbuf[20];
	PTR head, tail, elem, tmp;

	start_row = streamPTR(f)->row;
	LISPqueueinit(&head, &tail);
	for (;;) {
		elem = s_expr(f, tail != NULL);
		if (elem == READ_EOFptr) {
			unlinkPTR(elem);
			sprintf(rbuf, "%d", start_row);
			LISPputs(unlinkLISPerror(),
				"Read warning: terminating list at eof from line ");
			LISPputs(unlinkLISPerror(), rbuf);
			LISPputc('\n', unlinkLISPerror());
			return head;
		}
		if (elem == END_LISTptr) {
			unlinkPTR(elem);
			return head;
		}
		if (elem == DOTptr) {
			unlinkPTR(elem);
			elem = list(f);
			if (elem == READ_EOFptr) {
				unlinkPTR(head);
				return elem;
			}

			if (elem->type == LT_NODE && isNIL(nodePTR(elem)->cdr)) {
				tmp = linkPTR(nodePTR(elem)->car);
				unlinkPTR(elem);
				elem = tmp;
			}

			if (tail) {
				unlinkPTR(nodePTR(tail)->cdr);
				nodePTR(tail)->cdr = elem;
			} else {
				unlinkPTR(head);
				head = elem;
			}

			return head;
		}
		LISPqueueadd(&head, &tail, elem);
		unlinkPTR(elem);
	}
}

/***********************************************/

LISPstatic PTR atom(PTR f, int dot_allowed)
{
	int c, n, i, escapeflag, numberflag;
	PTR str, ret;
	double val;
	char *end, *tmp;
	LStrBuf sb;
	static char buf[51];

	if (skipspace(f))
		return linkPTR(READ_EOFptr);

	lsbInit(&sb);
	escapeflag = 0;
	for (;;) {
		c = LISPgetc(f);
		if (c == EOF) {
			break;
		} else if (c == '|') {
			escapeflag = 1;
			if (escape(&sb, f)) {
				lsbDone(&sb);
				LISPputs(unlinkLISPerror(), "Read abort: unterminated symbol\n");
				return linkPTR(READ_EOFptr);
			}
		} else if (c == '\\') {
			escapeflag = 1;
			c = LISPgetc(f);
			if (isodigit(c)) {
				n = c - '0';
				for (i = 0; i < 2; i++) {
					c = LISPgetc(f);
					if (c == EOF)
						break;
					if (!isodigit(c)) {
						LISPungetc(c, f);
						break;
					}
					n = n * 8 + (c - '0');
				}
				c = n;
			}
			if (c == EOF)
				c = '\\';
			lsbAddChar(&sb, (char) c);
		} else if (isspace(c) || nonNIL(cmacro[(unsigned char) c])) {
			break;
		} else {
			if (c >= 'A' && c <= 'Z')
				c += 'a' - 'A';
			lsbAddChar(&sb, (char) c);
		}
	}
	if (sb.length == 0 && !escapeflag) {
		lsbDone(&sb);
		if (c == EOF)
			return linkPTR(READ_EOFptr);
		sprintf(buf, "'%c' (%u)\n", c, c & 0xff);
		LISPputs(unlinkLISPerror(), "Read abort: character not allowed ");
		LISPputs(unlinkLISPerror(), buf);
		return linkPTR(READ_EOFptr);
	}
	LISPungetc(c, f);
	str = newstringPTRsb(&sb);
	lsbDone(&sb);
	tmp = stringPTRstr(str);
	if (dot_allowed && !escapeflag &&
			stringPTR(str)->len == 1 && *tmp == '.') {
		unlinkPTR(str);
		/* puts("got DOT"); */
		return linkPTR(DOTptr);
	}
	val = strtod(tmp, &end);
	numberflag = !*end && strcmp(tmp, "+")
		&& strcmp(tmp, "-") && strcmp(tmp, ".");
	if (!escapeflag && numberflag) {
		ret = newnumberPTR(val);
	} else {
		ret = LISPlookup(str);
		if (numberflag)
			symbolPTR(ret)->flags |= LF_ESCAPE;
	}
	unlinkPTR(str);
	return ret;
}

LISPstatic int escape(LStrBuf *sb, PTR f)
{
	int c;
	char *key;

	for (;;) {
		c = LISPgetc(f);
		switch (c) {
		case EOF:
			return 1;
		case '|':
			return 0;
		case '\\':
			c = LISPgetc(f);
			if (c == EOF)
				return 1;
			key = strchr(LISPsymesc1, c);
			if (key && *key)
				c = LISPsymesc2[(int) (key - LISPsymesc1)];
			lsbAddChar(sb, (char) c);
			break;
		default:
			lsbAddChar(sb, (char) c);
			break;
		}
	}
}

int LISPmustescape(PTR str)
{
	int l, c;
	char *s;

	l = stringPTR(str)->len;
	if (l <= 0)
		return 1;
	s = stringPTRstr(str);
	if (l == 1 && *s == '.')
		return 1;
	while (l > 0) {
		c = (unsigned char) *s;
		if (c == '\\' || c == '|' || isspace(c)
				|| (c >= 'A' && c <= 'Z')
				|| !isprint(c) || nonNIL(cmacro[c]))
			return 1;
		s++;
		l--;
	}
	return 0;
}

int LISPisnumber(char *str)
{
	char *end;

	(void) strtod(str, &end);
	return !*end && strcmp(str, "+")
		&& strcmp(str, "-") && strcmp(str, ".");
}

