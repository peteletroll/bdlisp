/*

	LISPMAIN.C - 1997 - Bigdogs Internescional.

	Interprete LISP - ciclo principale.

*/

#include "bdlisp.h"

#ifdef BORLANDC_ENV
extern unsigned _stklen = 0xff00;
#endif

#ifdef HEAPCHECK
void LISPmemoryreport(void);
#endif

#ifndef NO_STATIC_LIB
extern LISPpackedlib LISP_packed_lispboot;
#endif

#ifdef READLINE
#include "readline.h"
#endif

int main(int argc, char *argv[])
{
	PTR tmp;
#ifdef HEAPCHECK
	long mem;
#endif
	int ret = 0;

	(void) argc;

	clock(); /* starts clock count */

#ifdef BORLANDC_ENV
	argexp(&argc, &argv, 0);
#endif

#ifdef HEAPCHECK
	mem = (long) coreleft();
	/* printf("_stklen=%u SP=%u mem=%ld\n", _stklen, _SP, mem); */
#endif

	CHKMEM();

	LISPinit();
	LISPstoreargv(argv);

#ifndef NO_LIB
#ifdef NO_STATIC_LIB
	fprintf(stderr, "%s: loading external library ...\n", argv[0]);
	if (LISPloadpathfile("lisplib.lsp", 0)) {
		fprintf(stderr, "Can't open external library\n");
		exit(1);
	}
	fprintf(stderr, "%s: loading external startup ...\n", argv[0]);
	if (LISPloadpathfile("lispboot.lsp", 0)) {
		fprintf(stderr, "Can't open external startup\n");
		exit(1);
	}
#else
	LISPloadstaticlib(&LISP_packed_lisplib, 0);
	LISPloadstaticlib(&LISP_packed_lispboot, 0);
#endif
#endif

	CHKMEM();

	if (!(LISPstatus & LS_EXIT)) {
#ifdef READLINE
		load_hist(LISP_HIST);
#endif
		tmp = LISP_driver(NILptr);
		unlinkPTR(tmp);
#ifdef READLINE
		save_hist(LISP_HIST);
#endif
	}

	tmp = LISPexitvalue;
	if (nonNIL(tmp)) {
		if (tmp->type == LT_NUMBER) {
			ret = (int) (numberPTR(tmp)->val);
		} else {
			ret = -1;
		}
	}

	CHKMEM();

	LISPdone();

#ifdef READLINE
	erase_hist();
#endif

	CHKMEM();

#ifdef HEAPCHECK
	mem = mem - (long) coreleft();
	if (mem) {
		fprintf("memory error %ld bytes\n", mem);
		LISPmemoryreport();
	}
#endif
	if (NILptr->nl != 6)
		fprintf(stderr, "nil reference counter = %d instead of 6.\n", NILptr->nl);
	if (LISPncells)
		fprintf(stderr, "%ld cells, %ld symbols, %ld streams left.\n",
			LISPncells, LISPnsymbols, LISPnstreams);
	if (LISPstringbytes)
		fprintf(stderr, "%ld string bytes left.\n", LISPstringbytes);
	return ret;
}

#ifdef HEAPCHECK

void LISPmemoryreport(void)
{
	FILE *f;
	struct heapinfo hi;

	free(malloc(10000));
	f = fopen("report.mem", "wt");
	if (f) {
		puts("Generating report.mem ...");
		hi.ptr = NULL;
		while (heapwalk(&hi) == _HEAPOK) {
			fprintf(f, "%p %5u bytes ", hi.ptr, hi.size);
			if (hi.in_use)  fprintf(f, "used");
			else  fprintf(f, "free");
			fputc('\n', f);
		}
		fclose(f);
	} else {
		puts("Can't open report.mem");
	}
}

#endif

