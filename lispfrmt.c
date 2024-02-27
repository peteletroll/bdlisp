/*

	LISPFRMT.C - 1997 - Bigdogs Internescional.

	Interprete LISP - (format ...).

*/

#include "bdlisp.h"

LISPstatic int format(PTR stream, char *fmtstr, PTR fmtargs);

PTR LISP_format(PTR args)
{
	PTR stream, fmt, ret;
	int retstring = 0;

	GETARGS(("as-", args, &stream, &fmt, &args));

	if (stream == Tptr)
		stream = unlinkLISPoutput();
	if (stream == NILptr) {
		stream = newoutstringstreamPTR();
		retstring = 1;
	} else {
		if (stream->type != LT_STREAM)
			return LISPerrormsg(NON_STREAM_MSG);
		(void) linkPTR(stream);
	}

	format(stream, stringPTRstr(fmt), args);

	if (retstring) {
		ret = newstringPTRsb(&(streamPTR(stream)->str.outstring));
		unlinkPTR(stream);
		return ret;
	}

	unlinkPTR(stream);
	return linkPTR(NILptr);
}

PTR LISP_error(PTR args)
{
	PTR stream, fmt, ret;
	char *msg;
	int l;

	GETARGS(("s-", args, &fmt, &args));

	stream = newoutstringstreamPTR();

	format(stream, stringPTRstr(fmt), args);

	msg = lsbGet(&(streamPTR(stream)->str.outstring));
	unlinkPTR(stream);

	l = strlen(msg);
	if (msg[l - 1] == '\n')
		msg[--l] = 0;
	if (l > MAX_ERROR_MSG)
		msg[MAX_ERROR_MSG] = 0;
	unlinkedLISPbreakargs = NILptr;
	ret = LISPerrormsg(msg);
	free(msg);
	return ret;
}

PTR LISP_warn(PTR args)
{
	PTR fmt;

	GETARGS(("s-", args, &fmt, &args));
	format(unlinkLISPerror(), stringPTRstr(fmt), args);
	return linkPTR(NILptr);
}

#define MAX_PAR 10
typedef struct format_context_tag {
	char *fmt;
	PTR args, arg;
	int at_flag, colon_flag;
	int npar, par[MAX_PAR], parflag[MAX_PAR];
} format_context;

LISPstatic void parseparms(format_context *fc);
LISPstatic int getpar(format_context *fc, int n, int def);

LISPstatic void getarg(format_context *fc);

LISPstatic void symbolic(format_context *fc, PTR stream, int unescape);
LISPstatic void decimal(format_context *fc, PTR stream);
LISPstatic void floating(format_context *fc, PTR stream);

LISPstatic int format(PTR stream, char *fmtstr, PTR fmtargs)
{
	int count = 0;
	int par;
	format_context fc;

	fc.fmt = fmtstr;
	fc.args = fmtargs;

	while (*(fc.fmt)) {
		if (*(fc.fmt) == '~') {
			(fc.fmt)++;
			parseparms(&fc);
			/* printf("spec %c\n", *fmt); */
			switch (*(fc.fmt)) {
			case 0:
			case '~':
				LISPputc('~', stream);
				break;
			case 'a':
			case 'A':
				getarg(&fc);
				symbolic(&fc, stream, 1);
				break;
			case 'd':
			case 'D':
				getarg(&fc);
				decimal(&fc, stream);
				break;
			case 'f':
			case 'F':
				getarg(&fc);
				floating(&fc, stream);
				break;
			case 's':
			case 'S':
				getarg(&fc);
				symbolic(&fc, stream, 0);
				break;
			case '%':
				LISPputc('\n', stream);
				break;
			case '&':
				if (streamPTR(stream)->col != 0)
					LISPputc('\n', stream);
				break;
			case 't':
			case 'T':
				par = getpar(&fc, 0, 0);
				if (streamPTR(stream)->col > par)
					LISPputc('\n', stream);
				while (streamPTR(stream)->col < par)
					LISPputc(' ', stream);
				break;
			case '\n':
				while (*(fc.fmt) && isspace((unsigned char) *(fc.fmt)))
					(fc.fmt)++;
				(fc.fmt)--;
				break;
			default:
				LISPputc('~', stream);
				LISPputc(*(fc.fmt), stream);
				break;
			}
			count++;
			if (*(fc.fmt))
				(fc.fmt)++;
		} else {
			LISPputc(*((fc.fmt)++), stream);
		}
	}

	return count;
}

/*
	~mincol,colinc,minpad,padcharA is the full form of ~A, which allows 
	elaborate control of the padding. The string is padded on the right (or on 
	the left if the @ modifier is used) with at least minpad copies of padchar; 
	padding characters are then inserted colinc characters at a time until the 
	total width is at least mincol. The defaults are 0 for mincol and minpad, 1 
	for colinc, and the space character for padchar. 
*/

LISPstatic void symbolic(format_context *fc, PTR stream, int unescape)
{
	char *strarg;
	int l, pad;
	int mincol, colinc, minpad, padchar;

	mincol = getpar(fc, 0, 0);
	colinc = getpar(fc, 1, 1);
	minpad = getpar(fc, 2, 0);
	padchar = getpar(fc, 3, ' ');

	if (fc->colon_flag && isNIL(fc->arg)) {
		strarg = estrdup("()");
	} else {
		strarg = tostringPTR(fc->arg, unescape);
	}
	l = strlen(strarg);

	pad = minpad;
	while (l + pad < mincol)
		pad += colinc;

	if (!fc->at_flag)
		LISPputs(stream, strarg);
	while (pad > 0) {
		LISPputc(padchar, stream);
		pad--;
	}
	if (fc->at_flag)
		LISPputs(stream, strarg);

	free(strarg);
}

/*
	The @ modifier causes the number's sign to be printed always; the default is
	to print it only if the number is negative. The : modifier causes commas to
	be printed between groups of three digits; the third prefix parameter may be
	used to change the character used as the comma. Thus the most general form
	of ~D is ~mincol,padchar,commacharD.
*/

LISPstatic void decimal(format_context *fc, PTR stream)
{
	double n;
	int sign, l, lp;
	int mincol, padchar, commachar, commainterval;
	char *ebuf, *buf, *p;

	mincol = getpar(fc, 0, 0);
	padchar = getpar(fc, 1, ' ');
	commachar = getpar(fc, 2, ',');
	commainterval = getpar(fc, 3, 3);

	n = 0.0;
	if (fc->arg->type == LT_NUMBER)
		n = numberPTR(fc->arg)->val;

	l = 10 + (int) (log(fabs(n) + 1) / log(10));
	ebuf = buf = emalloc(l);

	sign = 1;
	sprintf(buf, "%.0f", n);
	if (*buf == '-') {
		sign = -1;
		buf++;
	}

	lp = strlen(buf);
	if (lp >= l)
		LISPfatal("wrong buffer size in decimal()");
	if (buf[lp] == '.')
		buf[--lp] = 0;

	l = lp;
	
	if (fc->at_flag || sign < 0)
		lp++;
	if (fc->colon_flag)
		lp += ((l - 1) / commainterval);

	lp = mincol - lp;
	while (lp-- > 0)
		LISPputc(padchar, stream);

	if (fc->at_flag) {
		LISPputc((sign < 0) ? '-' : '+', stream);
	} else {
		if (sign < 0)
			LISPputc('-', stream);
	}

	for (p = buf; *p; p++) {
		LISPputc(*p, stream);
		l--;
		if (fc->colon_flag && l > 0 && !(l % commainterval))
			LISPputc(commachar, stream);
	}
	
	free(ebuf);
}

/*
	The full form is ~w,d,k,overflowchar,padcharF. The parameter w is
	the width of the field to be printed; d is the number of digits to
	print after the decimal point; k is a scale factor that defaults
	to zero.
*/

LISPstatic void floating(format_context *fc, PTR stream)
{
	double n;
	int l, lp;
	int w, d, k;
	char *ebuf;
	char mask[50], tmp[50];

	w = getpar(fc, 0, -1);
	d = getpar(fc, 1, -1);
	k = getpar(fc, 2, 0);

	n = 0.0;
	if (fc->arg->type == LT_NUMBER)
		n = numberPTR(fc->arg)->val;
	if (k)
		n *= pow(10, k);

	l = (int) (log(fabs(n) + 1) / log(10));
	if (l < w)
		l = w;
	l += 10;
	if (d > 0)
		l += d;
	ebuf = emalloc(l);

	strcpy(mask, "%");
	if (fc->at_flag)
		strcat(mask, "+");
	if (w >= 0) {
		sprintf(tmp, "%d", w);
		strcat(mask, tmp);
	}
	if (d >= 0) {
		strcat(mask, ".");
		sprintf(tmp, "%d", d);
		strcat(mask, tmp);
	}
	strcat(mask, "f");
	/* fprintf(stderr, "MASK '%s'\n", mask); */
	sprintf(ebuf, mask, n);

	lp = strlen(ebuf);
	if (lp >= l)
		LISPfatal("wrong buffer size in floating()");

	LISPputs(stream, ebuf);
	free(ebuf);
}

LISPstatic void parseparms(format_context *fc)
{
	int p, pf, n, c;

	fc->at_flag = fc->colon_flag = 0;
	fc->npar = 0;

	for (;;) {
		pf = 0;
		if (*(fc->fmt) == '\'' && (fc->fmt)[1]) {
			p = (unsigned char) (fc->fmt)[1];
			pf = 1;
			fc->fmt += 2;
		} else if (*(fc->fmt) == ',') {
			p = -1;
		} else if (*(fc->fmt) == 'v' || *(fc->fmt) == 'V') {
			getarg(fc);
			switch (fc->arg->type) {
			case LT_NUMBER:
				p = (int) numberPTR(fc->arg)->val;
				pf = 1;
				break;
			case LT_STRING:
				p = *stringPTRstr(fc->arg);
				pf = 1;
				break;
			case LT_SYMBOL:
				p = *stringPTRstr(symbolPTR(fc->arg)->name);
				pf = 1;
				break;
			default:
				p = -1;
			}
			(fc->fmt)++;
		} else {
			n = sscanf(fc->fmt, "%d%n", &p, &c);
			if (n <= 0)
				break;
			pf = 1;
			fc->fmt += c;
		}
		/* printf("par %d %d\n", p, pf); */
		if (fc->npar < MAX_PAR) {
			(fc->par)[fc->npar] = p;
			(fc->parflag)[fc->npar] = pf;
			(fc->npar)++;
		}
		if (*(fc->fmt) != ',')
			break;
		(fc->fmt)++;
	}

	while (*(fc->fmt) == ':' || *(fc->fmt) == '@') {
		switch (*(fc->fmt)) {
		case ':':
			fc->colon_flag = 1;
			break;
		case '@':
			fc->at_flag = 1;
			break;
		}
		(fc->fmt)++;
	}
}

LISPstatic int getpar(format_context *fc, int n, int def)
{
	if (n < fc->npar && (fc->parflag)[n])
		return (fc->par)[n];
	return def;
}

LISPstatic void getarg(format_context *fc)
{
	if (fc->args->type == LT_NODE) {
		fc->arg = nodePTR(fc->args)->car;
		fc->args = nodePTR(fc->args)->cdr;
	} else {
		fc->arg = NILptr;
	}
}

