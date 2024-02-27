/*

	LISPSEQN.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni sulle sequenze.

*/

#include "bdlisp.h"

void LISPiterinit(struct LISPiterator *it, PTR ptr)
{
	it->ptr = it->slow = ptr;
	it->state = 0;
}

int LISPiternext(struct LISPiterator *it)
{
	if (it->ptr->type != LT_NODE)
		return 0;
	it->ptr = nodePTR(it->ptr)->cdr;
	it->state = !it->state;
	if (it->state)
		return 0;
	it->slow = nodePTR(it->slow)->cdr;
	return it->ptr == it->slow;
}

PTR LISP_position(PTR args)
{
	PTR obj, lst, test, tmp, ret;
	long pos = 0;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return newnumberPTR(pos);
		lst = nodePTR(lst)->cdr;
		pos++;
	}
	return linkPTR(NILptr);
}

PTR LISP_position_if(PTR args)
{
	PTR lst, test, tmp, ret;
	long pos = 0;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return newnumberPTR(pos);
		lst = nodePTR(lst)->cdr;
		pos++;
	}
	return linkPTR(NILptr);
}

PTR LISP_position_if_not(PTR args)
{
	PTR lst, test, tmp, ret;
	long pos = 0;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (isNIL(ret))
			return newnumberPTR(pos);
		lst = nodePTR(lst)->cdr;
		pos++;
	}
	return linkPTR(NILptr);
}

PTR LISP_member(PTR args)
{
	PTR obj, lst, test, tmp, ret;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return linkPTR(lst);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_member_if(PTR args)
{
	PTR lst, test, tmp, ret;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return linkPTR(lst);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_member_if_not(PTR args)
{
	PTR lst, test, tmp, ret;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (isNIL(ret))
			return linkPTR(lst);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_count(PTR args)
{
	PTR obj, lst, test, tmp, tret;
	long ret = 0;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return tret;
		unlinkPTR(tret);
		if (nonNIL(tret))
			ret++;
		lst = nodePTR(lst)->cdr;
	}
	return newnumberPTR(ret);
}

PTR LISP_count_if(PTR args)
{
	PTR lst, test, tmp, tret;
	long ret = 0;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return tret;
		unlinkPTR(tret);
		if (nonNIL(tret))
			ret++;
		lst = nodePTR(lst)->cdr;
	}
	return newnumberPTR(ret);
}

PTR LISP_count_if_not(PTR args)
{
	PTR lst, test, tmp, tret;
	long ret = 0;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return tret;
		unlinkPTR(tret);
		if (isNIL(tret))
			ret++;
		lst = nodePTR(lst)->cdr;
	}
	return newnumberPTR(ret);
}

PTR LISP_remove(PTR args)
{
	PTR obj, lst, test, tmp, tret;
	PTR ret, tail;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	LISPqueueinit(&ret, &tail);
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (isNIL(tret))
			LISPqueueadd(&ret, &tail, nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return ret;
}

PTR LISP_remove_if(PTR args)
{
	PTR lst, test, tmp, tret;
	PTR ret, tail;

	GETARGS(("aa", args, &test, &lst));
	LISPqueueinit(&ret, &tail);
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (isNIL(tret))
			LISPqueueadd(&ret, &tail, nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return ret;
}

PTR LISP_remove_if_not(PTR args)
{
	PTR lst, test, tmp, tret;
	PTR ret, tail;

	GETARGS(("aa", args, &test, &lst));
	LISPqueueinit(&ret, &tail);
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (nonNIL(tret))
			LISPqueueadd(&ret, &tail, nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return ret;
}

PTR LISP_delete(PTR args)
{
	PTR obj, lst, test, tmp, tret;
	PTR ret, prev;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	ret = lst;
	prev = NULL;
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (nonNIL(tret)) {
			if (prev) {
				nodePTR(prev)->cdr = linkPTR(nodePTR(lst)->cdr);
				unlinkPTR(lst);
				lst = nodePTR(prev)->cdr;
			} else {
				ret = lst = nodePTR(lst)->cdr;
			}
		} else {
			prev = lst;
			lst = nodePTR(lst)->cdr;
		}
	}
	return linkPTR(ret);
}

PTR LISP_delete_if(PTR args)
{
	PTR lst, test, tmp, tret;
	PTR ret, prev;

	GETARGS(("aa", args, &test, &lst));
	ret = lst;
	prev = NULL;
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (nonNIL(tret)) {
			if (prev) {
				nodePTR(prev)->cdr = linkPTR(nodePTR(lst)->cdr);
				unlinkPTR(lst);
				lst = nodePTR(prev)->cdr;
			} else {
				ret = lst = nodePTR(lst)->cdr;
			}
		} else {
			prev = lst;
			lst = nodePTR(lst)->cdr;
		}
	}
	return linkPTR(ret);
}

PTR LISP_delete_if_not(PTR args)
{
	PTR lst, test, tmp, tret;
	PTR ret, prev;

	GETARGS(("aa", args, &test, &lst));
	ret = lst;
	prev = NULL;
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		tret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tret;
		}
		unlinkPTR(tret);
		if (isNIL(tret)) {
			if (prev) {
				nodePTR(prev)->cdr = linkPTR(nodePTR(lst)->cdr);
				unlinkPTR(lst);
				lst = nodePTR(prev)->cdr;
			} else {
				ret = lst = nodePTR(lst)->cdr;
			}
		} else {
			prev = lst;
			lst = nodePTR(lst)->cdr;
		}
	}
	return linkPTR(ret);
}

PTR LISP_find(PTR args)
{
	PTR obj, lst, test, tmp, ret;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return linkPTR(nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_find_if(PTR args)
{
	PTR lst, test, tmp, ret;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return linkPTR(nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_find_if_not(PTR args)
{
	PTR lst, test, tmp, ret;

	GETARGS(("aa", args, &test, &lst));
	while (lst->type == LT_NODE) {
		tmp = LISPmakelist(nodePTR(lst)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (isNIL(ret))
			return linkPTR(nodePTR(lst)->car);
		lst = nodePTR(lst)->cdr;
	}
	return linkPTR(NILptr);
}

