/*

	LISPUSRT.C - 1999 - Bigdogs Internescional.

	Interprete LISP - gestioni tipi aggiuntivi.

*/

#include "bdlisp.h"

#define MAX_TYPES 32

static struct usertypeinfo {
	PTR sym;
	int (*cmp)(PTR p1, PTR p2);
	void (*write)(PTR p, PTR stream);
	void (*mark)(PTR p);
	void (*free)(PTR p);
	long count;
} usertype[MAX_TYPES];

static int ntype = 0;

static void checktype(PTR ptr, char *fun)
{
	static char buf[80];

	if (ptr->type < LT_USER_FIRST || ptr->type >= LT_USER_FIRST + ntype) {
		sprintf(buf, "bad user type %d in %s", ptr->type, fun);
		LISPfatal(buf);
	}
}

int LISPaddusertype(char *name,
	int (*cmp)(PTR p1, PTR p2),
	void (*write)(PTR p, PTR stream),
	void (*mark)(PTR p),
	void (*free)(PTR p))
{
	PTR sym;
	int i, ret;

	if (ntype >= MAX_TYPES)
		LISPfatal("too many user types");

	for (i = 0; i < ntype; i++)
		if (!strcmp(name, stringPTRstr(symbolPTR(usertype[i].sym)->name)))
			LISPfatal("readding user type %s", name);

	sym = LISPlookup_blk(-1, name);
	LISPaddstaticvar(&(usertype[ntype].sym), sym);
	unlinkPTR(sym);

	usertype[ntype].cmp = cmp;
	usertype[ntype].write = write;
	usertype[ntype].mark = mark;
	usertype[ntype].free = free;
	usertype[ntype].count = 0;
	ret = LT_USER_FIRST + ntype++;
	/* printf("user %s %d\n", name, ret); */
	return ret;
}

PTR newuserPTR(int type)
{
	PTR ret;

	if (type < LT_USER_FIRST || type >= LT_USER_FIRST + ntype)
		LISPfatal("illegal type in newuserPTR");
	ret = LISPnewcell();
	ret->type = type;
	userPTR(ret)->info = 0;
	userPTR(ret)->ext = NULL;
	usertype[ret->type - LT_USER_FIRST].count++;
	return ret;
}

PTR LISPusertypes(void)
{
	int i;
	PTR head, tail;

	LISPqueueinit(&head, &tail);
	for (i = 0; i < ntype; i++)
		LISPqueueadd(&head, &tail, usertype[i].sym);
	return head;
}

void LISPusertyperoom(void)
{
	int i;
	char buf[20];

	LISPputs(NULL, "; extern types: ");
	for (i = 0; i < ntype; i++) {
		if (i)
			LISPputs(NULL, (i % 4) ? ", " : ",\n    ");
		LISPwrite(NULL, usertype[i].sym, 0);
		sprintf(buf, " (%ld)", usertype[i].count);
		LISPputs(NULL, buf);
	}
	if (ntype <= 0)
		LISPputs(NULL, "none");
	LISPputs(NULL, ".\n");
}

PTR unlinkLISPusertype(PTR ptr)
{
	checktype(ptr, "unlinkLISPusertype");
	return usertype[ptr->type - LT_USER_FIRST].sym;
}

int LISPusercmp(PTR p1, PTR p2)
{
	checktype(p1, "LISPusercmp - p1");
	checktype(p2, "LISPusercmp - p2");
	if (usertype[p1->type - LT_USER_FIRST].cmp)
		return usertype[p1->type - LT_USER_FIRST].cmp(p1, p2);
	return (p1 < p2) ? -1 : (p1 > p2) ? 1 : 0;
}

void LISPuserwrite(PTR ptr, PTR stream)
{
	static char buf[50];

	checktype(ptr, "LISPuserwrite");
	if (usertype[ptr->type - LT_USER_FIRST].write) {
		usertype[ptr->type - LT_USER_FIRST].write(ptr, stream);
	} else {
		LISPputs(stream, "#<");
		LISPwrite(stream, unlinkLISPusertype(ptr), 0);
		sprintf(buf, ":%p>", ptr);
		LISPputs(stream, buf);
	}
}

void LISPusermark(PTR ptr)
{
	checktype(ptr, "LISPusermark");
	if (usertype[ptr->type - LT_USER_FIRST].mark)
		usertype[ptr->type - LT_USER_FIRST].mark(ptr);
}

void LISPuserfree(PTR ptr)
{
	checktype(ptr, "LISPuserfree");
	if (usertype[ptr->type - LT_USER_FIRST].free)
		usertype[ptr->type - LT_USER_FIRST].free(ptr);
	usertype[ptr->type - LT_USER_FIRST].count--;
}

void LISPclearusertypes(void)
{
	ntype = 0;
}

