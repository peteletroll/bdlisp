/*

	LISPVECT.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni sui vettori.

*/

#include "bdlisp.h"

static int LT_VECTOR;

#define vectelem(ptr, i) ((PTR *) userPTR(ptr)->ext)[i]

static int vectcmp(PTR ptr1, PTR ptr2)
{
	int i = 0, tmp;

	while (i < userPTR(ptr1)->info && i < userPTR(ptr2)->info) {
		tmp = LISPcomp(vectelem(ptr1, i), vectelem(ptr2, i));
		if (tmp)
			return tmp;
		i++;
	}
	return userPTR(ptr1)->info - userPTR(ptr2)->info;
}

static void vectwrite(PTR ptr, PTR stream)
{
	int i;

	LISPputs(stream, "#V(");
	for (i = 0; i < userPTR(ptr)->info; i++) {
		if (i > 0)
			LISPputs(stream, " ");
		LISPwrite(stream, vectelem(ptr, i), 0);
	}
	LISPputs(stream, ")");
}

static void vectmark(PTR ptr)
{
	int i;

	for (i = 0; i < userPTR(ptr)->info; i++)
		LISPmark(vectelem(ptr, i));
}

static void vectfree(PTR ptr)
{
	int i;

	for (i = 0; i < userPTR(ptr)->info; i++)
		unlinkPTR(vectelem(ptr, i));
	if (userPTR(ptr)->ext)
		free(userPTR(ptr)->ext);
}

LISPstatic PTR newvectorPTR(int size, PTR init)
{
	PTR ret;
	int i;
	PTR *arr;

	if (size < 0)
		size = 0;
	ret = newuserPTR(LT_VECTOR);
	userPTR(ret)->info = size;
	if (size > 0) {
		userPTR(ret)->ext = arr = ecalloc(sizeof(PTR), size);
		for (i = 0; i < size; i++)
			arr[i] = linkPTR(init);
	} else {
		userPTR(ret)->ext = NULL;
	}
	return ret;
}

LISPstatic PTR LISP_make_vector(PTR args)
{
	PTR size, init;

	GETARGS(("n?a", args, &size, &init, NILptr));
	return newvectorPTR((int) numberPTR(size)->val, init);
}

LISPstatic PTR LISP_resize_vector(PTR args)
{
	PTR vect, psize, init;
	int oldsize, size, i, m;
	PTR *oldarr, *arr;

	GETARGS(("an?a", args, &vect, &psize, &init, NILptr));
	if (vect->type != LT_VECTOR)
		return LISPerrormsg("non vector argument");

	oldarr = (PTR *) userPTR(vect)->ext;
	oldsize = userPTR(vect)->info;

	size = (int) numberPTR(psize)->val;
	if (size < 0)
		size = 0;
	if (size > 0) {
		arr = ecalloc(sizeof(PTR), size);
	} else {
		arr = NULL;
	}

	m = oldsize < size ? oldsize : size;

	for (i = 0; i < m; i++)
		arr[i] = oldarr[i];
	for (i = m; i < oldsize; i++)
		unlinkPTR(oldarr[i]);
	for (i = m; i < size; i++)
		arr[i] = linkPTR(init);

	free(oldarr);

	userPTR(vect)->ext = arr;
	userPTR(vect)->info = size;

	return linkPTR(vect);
}

LISPstatic PTR LISP_vector_size(PTR args)
{
	PTR vect;

	GETARGS(("a", args, &vect));
	if (vect->type != LT_VECTOR)
		return LISPerrormsg("non vector argument");
	return newnumberPTR(userPTR(vect)->info);
}

LISPstatic PTR LISP_vector_list(PTR args)
{
	PTR vect, ret, tail;
	int size, i;

	GETARGS(("a", args, &vect));
	if (vect->type != LT_VECTOR)
		return LISPerrormsg("non vector argument");
	size = userPTR(vect)->info;
	LISPqueueinit(&ret, &tail);
	for (i = 0; i < size; i++)
		LISPqueueadd(&ret, &tail, vectelem(vect, i));
	return ret;
}

LISPstatic PTR LISP_vref(PTR args)
{
	PTR vect, pidx;
	int idx;

	GETARGS(("an", args, &vect, &pidx));
	if (vect->type != LT_VECTOR)
		return LISPerrormsg("non vector argument");
	idx = (int) numberPTR(pidx)->val;
	if (idx < 0)
		idx += userPTR(vect)->info;
	if (idx < 0 || idx >= userPTR(vect)->info)
		return LISPerrormsg(BAD_VECT_IDX_MSG);

	return linkPTR(vectelem(vect, idx));
}

LISPstatic PTR LISP_set_vref(PTR args)
{
	PTR vect, pidx, val;
	int idx;

	GETARGS(("ana", args, &vect, &pidx, &val));
	if (vect->type != LT_VECTOR)
		return LISPerrormsg("non vector argument");

	idx = (int) numberPTR(pidx)->val;
	if (idx < 0)
		idx += userPTR(vect)->info;
	if (idx < 0 || idx >= userPTR(vect)->info)
		return LISPerrormsg(BAD_VECT_IDX_MSG);

	(void) linkPTR(val);
	unlinkPTR(vectelem(vect, idx));
	vectelem(vect, idx) = val;
	return linkPTR(val);
}

LISPstatic PTR LISP_vectorp(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return arg->type == LT_VECTOR ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

/*************************************/

static LISP_INIT_TAB(vect_init) {
	LISP_SYMBOL("vector", VECTORptr, 0),
	LISP_FUNCT("make-vector", LISP_make_vector, 0),
	LISP_FUNCT("resize-vector", LISP_resize_vector, 0),
	LISP_FUNCT("vector-size", LISP_vector_size, 0),
	LISP_FUNCT("vector-list", LISP_vector_list, 0),
	LISP_FUNCT("vref", LISP_vref, 0),
	LISP_FUNCT("set-vref", LISP_set_vref, 0),
	LISP_FUNCT("vectorp", LISP_vectorp, 0),
	LISP_TAB_LAST()
};

void LISPinitvectors(void)
{
	LT_VECTOR = LISPaddusertype("vector", vectcmp, vectwrite, vectmark, vectfree);
	LISPprocessinittab(vect_init);
}

