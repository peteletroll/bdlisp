/*

	LISPGETC.C - 1997 - Bigdogs Internescional.

	Interprete LISP - lettura caratteri.

*/

#include "bdlisp.h"

#ifdef READLINE
#include "readline.h"
#endif

static int getline_abort;
LISPstatic int LISPgetline(void);

PTR unlinkLISPinput(void)
{
	return unlinkLISPgetval(STANDARD_INPUTptr);
}

void LISPpushinput(PTR input)
{
	if (!input)
		input = LISPonterminal ? LISPconsole : LISPstdin;

	LISPpushval(STANDARD_INPUTptr, input);
}

void LISPpopinput(void)
{
	LISPpopval(STANDARD_INPUTptr);
}

char *LISPcurline;
static int lineptr = 0;

#define BUF_SIZE 1
static int buf[BUF_SIZE];
static int bufptr = 0;

int LISPgetc(PTR f)
{
	int ret = EOF;

	if (streamPTR(f)->ubufptr > 0) {
		ret = streamPTR(f)->ubuf[--(streamPTR(f)->ubufptr)];
	} else {
		if (streamPTR(f)->eof)
			return EOF;

		switch (streamPTR(f)->strtype) {
		case ST_CONSOLE:
			if (bufptr > 0) {
				ret = buf[--bufptr];
			} else if (LISPcurline && LISPcurline[lineptr]) {
				ret = LISPcurline[lineptr++];
			} else {
				if (!LISPgetline()) {
					ret = LISPcurline[lineptr++];
				}
			}
			break;
		case ST_FILE:
#ifdef POPEN
		case ST_PIPE:
#endif
			ret = getc(streamPTR(f)->str.file);
			break;
		case ST_INSTRING:
			if (streamPTR(f)->str.instring.len > 0) {
				ret = *((streamPTR(f)->str.instring.ptr)++);
				(streamPTR(f)->str.instring.len)--;
			}
			break;
		case ST_OUTSTRING:
			break;
		case ST_CLOSED:
			break;
		default:
			LISPfatal("unknown stream type in LISPgetc");
			break;
		}
	}

	if (ret == '\n') {
		streamPTR(f)->col = 0;
		streamPTR(f)->row++;
	} else if (ret == '\r') {
		streamPTR(f)->col = 0;
	} else if (ret == '\t') {
		streamPTR(f)->col = (streamPTR(f)->col & ~7) + 8;
	}

	if (ret == EOF) {
		streamPTR(f)->eof = 1;
	} else {
		ret = ret & 0xff;
	}
	return ret;
}

int LISPungetc(int c, PTR f)
{
	int ptr;

	if (c != EOF) {
		ptr = streamPTR(f)->ubufptr;
		if (ptr >= MAX_STREAM_UNGETC)
			return 1;
		streamPTR(f)->ubuf[ptr++] = c;
		streamPTR(f)->ubufptr = ptr;
		if (streamPTR(f)->col > 0)
			streamPTR(f)->col--;
		if (c == '\n')
			streamPTR(f)->row--;
	}
	return 0;
}

LISPstatic int LISPgetline(void)
{
	char *g, *p;
	static char prompt[80];
	static char buf[1026];
	int ret = 0;

	getline_abort = 0;

	p = prompt;
	/*
	if (LISPobjinfo_opt && LISPreadlevel == 0) {
		sprintf(p, "[o:%03ld n:%03ld p:%03ld]\n",
			LISPnobjs, LISPnnodes, LISPnpool);
		p += strlen(p);
	}
	*/
#ifdef STATISTICS
	sprintf(p, "[%lu] ", LISPrefcount);
	p += strlen(p);
#endif
	if (LISPdebuglevel) {
		sprintf(p, "%c%d ",
			LISPbreakeval ? 'E' : 'A',
			LISPdebuglevel);
		p += strlen(p);
	}
	if (LISPreadlevel) {
		sprintf(p, "%d> ", LISPreadlevel);
	} else {
		strcpy(p, "* ");
	}
	p += strlen(p);

	lineptr = 0;
	if (LISPcurline)
		free(LISPcurline);

#ifdef READLINE
	g = readline(prompt, &getline_abort);
	if (g) {
		strncpy(buf, g, 1024);
		strcat(buf, "\n");
		g = buf;
	} else {
		ret = 1;
	}
#else
	if (isatty(fileno(stdin)) && isatty(fileno(stdout)))
		fprintf(stdout, "%s", prompt);
	signal(SIGINT, LISPoldintcatch);
	g = fgets(buf, 1024, stdin);
	signal(SIGINT, LISPintcatch);
	if (!g)
		ret = 1;
#endif

	if (!g) {
		LISPcurline = NULL;
		return ret;
	}

	LISPcurline = estrdup(g);
	return 0;
}

