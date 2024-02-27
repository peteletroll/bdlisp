/*

	LISPMAPF.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni di mappatura.

*/

#include "bdlisp.h"

LISPstatic PTR applylist(PTR args);

LISPstatic int listlist(PTR lst);
LISPstatic void shiftlist(PTR lst);
LISPstatic PTR carlist(PTR lst);

PTR LISP_apply(PTR args)
{
	PTR ret, fun, fargs;

	GETARGS(("a-", args, &fun, &args));
	fargs = applylist(args);
	ret = LISPapplyfun(fun, fargs, 1);
	unlinkPTR(fargs);
	return ret;
}

LISPstatic PTR applylist(PTR args)
{
	PTR ret;

	if (args->type != LT_NODE)
		return linkPTR(NILptr);
	if (nodePTR(args)->cdr->type != LT_NODE)
		return linkPTR(nodePTR(args)->car);
	ret = newnodePTR();
	nodePTR(ret)->car = linkPTR(nodePTR(args)->car);
	nodePTR(ret)->cdr = applylist(nodePTR(args)->cdr);
	return ret;
}

PTR LISP_funcall(PTR args)
{
	PTR fun;

	GETARGS(("a-", args, &fun, &args));
	return LISPapplyfun(fun, args, 1);
}

PTR LISP_mapl(PTR args)
{
	PTR ret, fun, fargs, thr, lst;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	ret = lst->type == LT_NODE ? nodePTR(lst)->car : NILptr;
	(void) linkPTR(ret);
	while (listlist(lst)) {
		fargs = LISPnewlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(ret);
			return thr;
		}
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return ret;
}

PTR LISP_maplist(PTR args)
{
	PTR head, tail, lst;
	PTR fun, fargs, thr;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	LISPqueueinit(&head, &tail);
	while (listlist(lst)) {
		fargs = LISPnewlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(head);
			return thr;
		}
		LISPqueueadd(&head, &tail, thr);
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return head;
}

PTR LISP_mapcon(PTR args)
{
	PTR head, tail, lst;
	PTR fun, fargs, thr;
	PTR tmp;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	LISPqueueinit(&head, &tail);
	while (listlist(lst)) {
		fargs = LISPnewlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(head);
			return thr;
		}
		tmp = thr;
		while (tmp->type == LT_NODE) {
			LISPqueueadd(&head, &tail, nodePTR(tmp)->car);
			tmp = nodePTR(tmp)->cdr;
		}
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return head;
}

PTR LISP_mapc(PTR args)
{
	PTR ret, fun, fargs, thr, lst;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	ret = lst->type == LT_NODE ? nodePTR(lst)->car : NILptr;
	(void) linkPTR(ret);
	while (listlist(lst)) {
		fargs = carlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(ret);
			return thr;
		}
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return ret;
}

PTR LISP_mapcar(PTR args)
{
	PTR head, tail, lst;
	PTR fun, fargs, thr;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	LISPqueueinit(&head, &tail);
	while (listlist(lst)) {
		fargs = carlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(head);
			return thr;
		}
		LISPqueueadd(&head, &tail, thr);
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return head;
}

PTR LISP_mapcan(PTR args)
{
	PTR head, tail, lst;
	PTR fun, fargs, thr;
	PTR tmp;

	GETARGS(("a-", args, &fun, &args));
	lst = LISPnewlist(args);
	LISPqueueinit(&head, &tail);
	while (listlist(lst)) {
		fargs = carlist(lst);
		thr = LISPapplyfun(fun, fargs, 0);
		unlinkPTR(fargs);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(lst);
			unlinkPTR(head);
			return thr;
		}

		tmp = thr;
		while (tmp->type == LT_NODE) {
			LISPqueueadd(&head, &tail, nodePTR(tmp)->car);
			tmp = nodePTR(tmp)->cdr;
		}
		unlinkPTR(thr);
		shiftlist(lst);
	}
	unlinkPTR(lst);
	return head;
}

LISPstatic int listlist(PTR lst)
{
	if (lst->type != LT_NODE)
		return 0;
	while (lst->type == LT_NODE) {
		if ((nodePTR(lst)->car)->type != LT_NODE)
			return 0;
		lst = nodePTR(lst)->cdr;
	}
	return 1;
}

LISPstatic void shiftlist(PTR lst)
{
	PTR tmp;

	while (lst->type == LT_NODE) {
		tmp = nodePTR(lst)->car;
		nodePTR(lst)->car = linkPTR(nodePTR(tmp)->cdr);
		unlinkPTR(tmp);
		lst = nodePTR(lst)->cdr;
	}
}

LISPstatic PTR carlist(PTR lst)
{
	PTR ret, tail;
	PTR tmp;

	ret = linkPTR(NILptr);
	tail = NULL;
	while (lst->type == LT_NODE) {
		tmp = newnodePTR();
		nodePTR(tmp)->car = linkPTR(nodePTR(nodePTR(lst)->car)->car);
		nodePTR(tmp)->cdr = NILptr;
		if (tail) {
			nodePTR(tail)->cdr = tmp;
		} else {
			ret = tmp;
		}
		tail = tmp;
		lst = nodePTR(lst)->cdr;
	}
	return ret;
}

PTR LISP_reduce(PTR args)
{
	PTR fun, ret, tmp;

	GETARGS(("aa", args, &fun, &args));
	if (args->type != LT_NODE)
		return LISPapplyfun(fun, NILptr, 0);
	ret = linkPTR(nodePTR(args)->car);
	args = nodePTR(args)->cdr;
	while (args->type == LT_NODE) {
		tmp = LISPmakelist(ret, nodePTR(args)->car, NULL);
		unlinkPTR(ret);
		ret = LISPapplyfun(fun, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			break;
		args = nodePTR(args)->cdr;
	}
	return ret;
}

