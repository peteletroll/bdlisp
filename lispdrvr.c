/*

	LISPDRVR.C - 1997 - Bigdogs Internescional.

	Interprete LISP - driver e debugging driver.

*/

#include "bdlisp.h"

PTR BREAKptr, CONTINUEptr, RETURNptr, ABORTptr, ERROR_EXITptr;
PTR unlinkedLISPbreakfun, unlinkedLISPbreakargs;
int LISPbreakeval;

int LISPdebuglevel;

PTR LISP_driver(PTR args)
{
	PTR inp, res;
	static int instances = 0;

	GETARGS(("", args));

	instances++;

	inp = linkPTR(NILptr);
	res = linkPTR(NILptr);

	LISPpushval(LOAD_SOURCEptr, Tptr);

	while (!(LISPstatus & LS_EXIT)) {
		LISPstatus = 0;

		if (instances <= 1 && LISPloadlevel <= 0)
			LISPreclaim();
		
		/* LISPchecksymbols(); */
		
		unlinkPTR(inp);
		inp = LISPreadobj(NULL);

		CHKMEM();

		if (inp == READ_EOFptr)
			break;

		LISPsetval(MINUSptr, inp);

		unlinkPTR(res);
		res = LISPevalobj(inp);

		CHKMEM();

		if (nonNIL(LISPthrownsymbol)) {
			LISPputs(NULL, "Uncatched throw ");
			LISPwrite(NULL, LISPthrownsymbol, 0);
			LISPnewline(NULL);
			unlinkPTR(LISPthrownsymbol);
			LISPthrownsymbol = linkPTR(NILptr);
			unlinkPTR(res);
			res = LISPthrownvalue;
			LISPthrownvalue = linkPTR(NILptr);
		}

		LISPsetval(PLUSptr, inp);
		LISPsetval(STARptr, res);

		if (!(LISPstatus & LS_EXIT))
			LISPprintmret(NULL, res);
	}

	LISPpopval(LOAD_SOURCEptr);

	unlinkPTR(inp);
	instances--;
	LISPstatus = 0;
	return res;
}

PTR LISPerrormsg(char *fmt, ...)
{
	va_list vl;
	PTR ret, task;
	PTR fnc;
	char *brk;
	int lbrk;
	static char buf[MAX_ERROR_MSG + 1];

	va_start(vl, fmt);
	vsprintf(buf, fmt, vl);
	va_end(vl);
	LISPputs(unlinkLISPerror(),
		LISPbreakeval ? "eval error: " : "apply error: ");
	LISPputs(unlinkLISPerror(), buf);
	LISPnewline(unlinkLISPerror());
	if (LISPdebuglevel > 20) {
		LISPputs(unlinkLISPerror(), "looks like something's wrong ...\n");
		LISPputs(unlinkLISPerror(), "exiting ...\n");
		LISPstatus |= (LS_EXIT | LS_ABORT);
		return linkPTR(NILptr);
	}
	LISPdebuglevel++;
	if (unlinkedLISPbreakargs) {
		task = newnodePTR();
		nodePTR(task)->car = linkPTR(unlinkedLISPbreakfun);
		nodePTR(task)->cdr = linkPTR(unlinkedLISPbreakargs);
	} else {
		task = linkPTR(unlinkedLISPbreakfun);
	}

	LISPstatus &= ~(LS_BREAK | LS_USERBREAK);
	LISPpushinput(NULL);
	LISPpushoutput(NULL);

	brk = tostringPTR(task, 0);
	lbrk = strlen(brk);
	if (lbrk > 66)
		strcpy(brk + 60, " ...");
	LISPputs(unlinkLISPerror(), "break task: ");
	LISPputs(unlinkLISPerror(), brk);
	LISPnewline(unlinkLISPerror());
	free(brk);
	
	LISPpushval(BREAKptr, task);
	unlinkPTR(task);

	fnc = newclosurePTR(unlinkLISPgetval(DEBUGHOOKptr),
		nodePTR(LISPenvstack)->car);
	ret = LISPapplyfun(fnc, NILptr, 1);
	unlinkPTR(fnc);

	LISPdebuglevel--;

	LISPpopval(BREAKptr);

	LISPpopinput();
	LISPpopoutput();

	return ret;
}

PTR LISP_debug_driver(PTR args)
{
	int exitdebug = 0;
	PTR inp, ret, task;
	int eval = LISPbreakeval;

	GETARGS(("", args));

	do {
		LISPbreakeval = eval;
		inp = LISPreadobj(unlinkLISPinput());
		if (inp == RETURNptr) {
			unlinkPTR(inp);
			inp = LISPreadobj(unlinkLISPinput());
			ret = LISPevalobj(inp);
			exitdebug = 1;
		} else if (inp == ABORTptr) {
			LISPstatus |= LS_ABORT;
			exitdebug = 1;
			ret = linkPTR(NILptr);
		} else if (inp == CONTINUEptr) {
			task = unlinkLISPgetval(BREAKptr);
			if (task->type == LT_NODE) {
				if (eval) {
					ret = LISPevalobj(task);
				} else {
					ret = LISPapplyfun(nodePTR(task)->car,
						nodePTR(task)->cdr, 1);
				}
			} else {
				ret = LISPevalobj(task);
			}
			exitdebug = 1;
		} else {
			ret = LISPevalobj(inp);
			if (!(LISPstatus & LS_ABORT))
				LISPprintmret(NULL, ret);
			if (LISPstatus & (LS_EXIT | LS_ABORT)) {
				exitdebug = 1;
			} else {
				unlinkPTR(ret);
			}
		}
		unlinkPTR(inp);
	} while (!exitdebug);

	return ret;
}

void (*LISPoldintcatch)(int sig);

void LISPintcatch(int sig)
{
	(void) sig;
	signal(SIGINT, LISPintcatch);
	LISPstatus |= LS_BREAK;
}

