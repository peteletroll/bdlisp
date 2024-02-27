/*

	LISPFNCS.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni di libreria.

*/

#include "bdlisp.h"

PTR LISPexitvalue;

LISPstatic int equalaux(PTR ptr1, PTR ptr2);
LISPstatic int equallist(PTR ptr1, PTR ptr2);

PTR TESTptr, EQLptr;

LISPstatic PTR cxxxr(char *mask, PTR args)
{
	int i;
	PTR ptr;
	
	GETARGS(("a", args, &ptr));

	for (i = strlen(mask) - 1; i >= 0; i--) {
		switch (mask[i]) {
		case 'A':
			if (ptr->type == LT_NODE) {
				ptr = nodePTR(ptr)->car;
			}
			break;
		case 'D':
			if (ptr->type == LT_NODE) {
				ptr = nodePTR(ptr)->cdr;
			} else {
				ptr = NILptr;
			}
			break;
		default:
			LISPfatal("illegal operation character in cxxxr");
		}
	}
	return linkPTR(ptr);
}

PTR LISP_car(PTR args)
{
	return cxxxr("A", args);
}

PTR LISP_cdr(PTR args)
{
	return cxxxr("D", args);
}

PTR LISP_cadr(PTR args)
{
	return cxxxr("AD", args);
}

PTR LISP_cdar(PTR args)
{
	return cxxxr("DA", args);
}

PTR LISP_cddr(PTR args)
{
	return cxxxr("DD", args);
}

PTR LISP_caar(PTR args)
{
	return cxxxr("AA", args);
}

PTR LISP_caaar(PTR args)
{
	return cxxxr("AAA", args);
}

PTR LISP_caadr(PTR args)
{
	return cxxxr("AAD", args);
}

PTR LISP_cadar(PTR args)
{
	return cxxxr("ADA", args);
}

PTR LISP_caddr(PTR args)
{
	return cxxxr("ADD", args);
}

PTR LISP_cdaar(PTR args)
{
	return cxxxr("DAA", args);
}

PTR LISP_cdadr(PTR args)
{
	return cxxxr("DAD", args);
}

PTR LISP_cddar(PTR args)
{
	return cxxxr("DDA", args);
}

PTR LISP_cdddr(PTR args)
{
	return cxxxr("DDD", args);
}

PTR LISP_quote(PTR args, PTR tail)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return linkPTR(arg);
}

LISPstatic PTR backquote(PTR ptr)
{
	PTR ret, tail;
	PTR tmp, arg, exp;

	if (ptr && ptr->type == LT_NODE) {
		tmp = nodePTR(ptr)->car;
		if (tmp == COMMAptr) {
			tmp = nodePTR(ptr)->cdr;
			if (!tmp || tmp->type != LT_NODE)
				return linkPTR(NILptr);
			tmp = nodePTR(tmp)->car;
			ret = LISPevalobj(tmp);
			return ret;
		} else {
			ret = tail = NULL;
			while (ptr->type == LT_NODE) {
				arg = nodePTR(ptr)->car;
				ptr = nodePTR(ptr)->cdr;
				if (arg->type == LT_NODE) {
					tmp = nodePTR(arg)->car;
					if (tmp == COMMA_ATptr) {
						tmp = nodePTR(arg)->cdr;
						if (!tmp || tmp->type != LT_NODE)
							continue;
						tmp = nodePTR(tmp)->car;
						exp = LISPevalobj(tmp);
						for (arg = exp; arg && arg->type == LT_NODE; arg = nodePTR(arg)->cdr) {
							tmp = newnodePTR();
							nodePTR(tmp)->car = linkPTR(nodePTR(arg)->car);
							if (tail) {
								nodePTR(tail)->cdr = tmp;
							} else {
								ret = tmp;
							}
							tail = tmp;
						}
						unlinkPTR(exp);
						/* comma-at expansion */
						continue;
					}
				}
				tmp = newnodePTR();
				if (tail) {
					nodePTR(tail)->cdr = tmp;
				} else {
					ret = tmp;
				}
				tail = tmp;
				nodePTR(tmp)->car = backquote(arg);
			}
			if (tail) {
				nodePTR(tail)->cdr = linkPTR(NILptr);
			} else {
				ret = linkPTR(NILptr);
			}
			return ret;
		}
	}
	return linkPTR(ptr);
}

PTR LISP_backquote(PTR args, PTR tail)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return backquote(arg);
}

PTR LISP_cons(PTR args)
{
	PTR car, cdr, ret;

	GETARGS(("aa", args, &car, &cdr));
	ret = newnodePTR();
	nodePTR(ret)->car = linkPTR(car);
	nodePTR(ret)->cdr = linkPTR(cdr);
	return ret;
}

PTR LISP_atom(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return arg->type == LT_NODE ?
		linkPTR(NILptr) : linkPTR(Tptr);
}

PTR LISP_eq(PTR args)
{
	PTR arg1, arg2;

	GETARGS(("aa", args, &arg1, &arg2));
	return arg1 == arg2 ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_neq(PTR args)
{
	PTR arg1, arg2;

	GETARGS(("aa", args, &arg1, &arg2));
	return arg1 != arg2 ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_eql(PTR args)
{
	int eq = 0;
	PTR arg1, arg2;

	GETARGS(("aa", args, &arg1, &arg2));

	if (arg1->type != arg2->type)
		return linkPTR(NILptr);

	switch (arg1->type) { /* TYPESWITCH */
	case LT_NUMBER:
		eq = (numberPTR(arg1)->val == numberPTR(arg2)->val);
		break;
	case LT_STRING:
		eq = LISPstreql(arg1, arg2);
		break;
	case LT_OBJECT:
		eq = equalaux(arg1, arg2);
		break;
	default:
		eq = (arg1 == arg2);
		break;
	}
	return eq ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_neql(PTR args)
{
	PTR eq;

	eq = LISP_eql(args);
	if (LISPstatus & LS_ABORT)
		return eq;
	unlinkPTR(eq);
	return isNIL(eq) ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_list(PTR args)
{
	return linkPTR(args);
}

PTR LISP_list_star(PTR args)
{
	PTR ret, tail;

	LISPqueueinit(&ret, &tail);
	while (args->type == LT_NODE) {
		if (nodePTR(args)->cdr->type == LT_NODE) {
			LISPqueueadd(&ret, &tail, nodePTR(args)->car);
		} else {
			LISPqueueappend(&ret, &tail, nodePTR(args)->car);
		}
		args = nodePTR(args)->cdr;
	}
	return ret;
}

PTR LISP_append(PTR args)
{
	PTR ret, tail;
	struct LISPiterator it;

	LISPqueueinit(&ret, &tail);
	while (args->type == LT_NODE) {
		if (nodePTR(args)->cdr->type != LT_NODE)
			break;
		LISPiterinit(&it, nodePTR(args)->car);
		while (it.ptr->type == LT_NODE) {
			LISPqueueadd(&ret, &tail, nodePTR(it.ptr)->car);
			if (LISPiternext(&it)) {
				unlinkPTR(ret);
				return LISPerrormsg(CIRCULAR_MSG);
			}
		}
		args = nodePTR(args)->cdr;
	}
	if (args->type == LT_NODE && nodePTR(args)->car->type == LT_NODE)
		LISPqueueappend(&ret, &tail, nodePTR(args)->car);
	return ret;
}

PTR LISP_reverse(PTR args)
{
	PTR ret, tmp, node;
	char *p1, *p2, c;

	GETARGS(("a?a", args, &tmp, &ret, NILptr));
	if (tmp->type == LT_STRING) {
		if (stringPTR(tmp)->len <= 1)
			return linkPTR(tmp);
		ret = newstringPTR(stringPTR(tmp)->len, stringPTRstr(tmp));
		p1 = stringPTRstr(ret);
		p2 = p1 + stringPTR(ret)->len - 1;
		while (p1 < p2) {
			c = *p1;
			*p1 = *p2;
			*p2 = c;
			p1++, p2--;
		}
		return ret;
	}
	if (tmp->type != LT_NODE)
		return linkPTR(tmp);
	(void) linkPTR(ret);
	while (tmp->type == LT_NODE) {
		node = newnodePTR();
		nodePTR(node)->car = linkPTR(nodePTR(tmp)->car);
		nodePTR(node)->cdr = ret;
		ret = node;
		tmp = nodePTR(tmp)->cdr;
	}
	return ret;
}

PTR LISP_length(PTR args)
{
	PTR arg;
	struct LISPiterator it;
	long ret;

	GETARGS(("a", args, &arg));
	if (arg->type == LT_STRING)
		return newnumberPTR(stringPTR(arg)->len);
	LISPiterinit(&it, arg);
	ret = 0;
	while (it.ptr->type == LT_NODE) {
		if (LISPiternext(&it))
			return LISPerrormsg(CIRCULAR_MSG);
		ret++;
	}
	return newnumberPTR(ret);
}

PTR LISP_elt(PTR args)
{
	PTR num, seq;
	long n;

	GETARGS(("an", args, &seq, &num));
	n = (long) numberPTR(num)->val;
	if (seq->type == LT_STRING) {
		if (n < 0)
			return newstringPTR(-1, "");
		if (n > stringPTR(seq)->len)
			return newstringPTR(-1, "");
		return newcharPTR(stringPTRstr(seq)[(int) n]);
	}
	if (n < 0)
		return linkPTR(NILptr);
	while (n > 0 && seq->type == LT_NODE) {
		seq = nodePTR(seq)->cdr;
		n--;
	}
	if (seq->type != LT_NODE)
		return linkPTR(NILptr);
	return linkPTR(nodePTR(seq)->car);
}

PTR LISP_nth(PTR args)
{
	PTR num, lst;
	long n;

	GETARGS(("na", args, &num, &lst));
	n = (long) numberPTR(num)->val;
	if (n < 0)
		return linkPTR(NILptr);
	while (n > 0 && lst->type == LT_NODE) {
		lst = nodePTR(lst)->cdr;
		n--;
	}
	if (lst->type != LT_NODE)
		return linkPTR(NILptr);
	return linkPTR(nodePTR(lst)->car);
}

PTR LISP_nthcdr(PTR args)
{
	PTR num, lst;
	long n;

	GETARGS(("na", args, &num, &lst));
	n = (long) numberPTR(num)->val;
	while (n > 0 && lst->type == LT_NODE) {
		lst = nodePTR(lst)->cdr;
		n--;
	}
	if (lst->type != LT_NODE)
		return linkPTR(NILptr);
	return linkPTR(lst);
}

PTR LISP_last(PTR args)
{
	PTR arg;
	struct LISPiterator it;

	GETARGS(("a", args, &arg));
	if (arg->type != LT_NODE)
		return linkPTR(NILptr);
	LISPiterinit(&it, arg);
	while (nodePTR(it.ptr)->cdr->type == LT_NODE)
		if (LISPiternext(&it))
			return LISPerrormsg(CIRCULAR_MSG);
	return linkPTR(it.ptr);
}

PTR LISP_assoc(PTR args)
{
	PTR key, lst, test, tmp, ret;
	struct LISPiterator it;

	GETARGS(("aa:a", args, &key, &lst, TESTptr, &test, EQLptr));
	LISPiterinit(&it, lst);
	while (it.ptr->type == LT_NODE) {
		tmp = nodePTR(it.ptr)->car;
		if (tmp->type == LT_NODE) {
			tmp = LISPmakelist(key, nodePTR(tmp)->car, NULL);
			ret = LISPapplyfun(test, tmp, 0);
			unlinkPTR(tmp);
			if (LISPstatus & LS_ABORT)
				return ret;
			unlinkPTR(ret);
			if (nonNIL(ret))
				return linkPTR(nodePTR(it.ptr)->car);
		}
		if (LISPiternext(&it))
			break;
	}
	return linkPTR(NILptr);
}

PTR LISP_adjoin(PTR args)
{
	PTR obj, lst, test, tmp, ret;
	struct LISPiterator it;

	GETARGS(("aa:a", args, &obj, &lst, TESTptr, &test, EQLptr));
	LISPiterinit(&it, lst);
	while (it.ptr->type == LT_NODE) {
		tmp = LISPmakelist(obj, nodePTR(it.ptr)->car, NULL);
		ret = LISPapplyfun(test, tmp, 0);
		unlinkPTR(tmp);
		if (LISPstatus & LS_ABORT)
			return ret;
		unlinkPTR(ret);
		if (nonNIL(ret))
			return linkPTR(lst);
		if (LISPiternext(&it))
			break;
	}
	tmp = newnodePTR();
	nodePTR(tmp)->cdr = linkPTR(lst);
	nodePTR(tmp)->car = linkPTR(obj);
	return tmp;
}

PTR LISP_make_symbol(PTR args)
{
	PTR str;

	GETARGS(("s", args, &str));
	return newsymbolPTR(str);
}

PTR LISP_symbol_name(PTR args)
{
	PTR sym;

	GETARGS(("y", args, &sym));
	return linkPTR(symbolPTR(sym)->name);
}

PTR LISP_symbol_value(PTR args)
{
	PTR sym;

	GETARGS(("y", args, &sym));
	sym = symbolPTR(sym)->bind;
	if (sym->type != LT_NODE)
		return LISPerrormsg("unbound symbol");
	return linkPTR(nodePTR(sym)->car);
}

PTR LISP_make_object(PTR args)
{
	PTR isa;

	GETARGS(("y", args, &isa));
	return newobjectPTR(isa);
}

PTR LISP_objectp(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return arg->type == LT_OBJECT ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_object_type(PTR args)
{
	PTR arg;

	GETARGS(("o", args, &arg));
	return linkPTR(objectPTR(arg)->isa);
}

void LISPpushenvironment(PTR env)
{
	PTR var, val;

	while (env->type == LT_NODE) {
		var = nodePTR(env)->car;
		val = NILptr;
		env = nodePTR(env)->cdr;
		if (env->type == LT_NODE) {
			val = nodePTR(env)->car;
			env = nodePTR(env)->cdr;
		}
		if (var->type == LT_SYMBOL) {
			LISPpushval(var, val);
		}
	}
}

void LISPpopenvironment(PTR env)
{
	PTR var;

	while (env->type == LT_NODE) {
		var = nodePTR(env)->car;
		env = nodePTR(env)->cdr;
		if (env->type == LT_NODE) {
			env = nodePTR(env)->cdr;
		}
		if (var->type == LT_SYMBOL) {
			LISPpopval(var);
		}
	}
}


PTR unlinkedLISPgetprop(PTR plist, PTR prop)
/* returns non linked PTR */
{
	while (plist->type == LT_NODE) {
		if (nodePTR(plist)->car == prop)
			return nodePTR(nodePTR(plist)->cdr)->car;
		plist = nodePTR(nodePTR(plist)->cdr)->cdr;
	}
	return NULL;
}

void LISPputprop(PTR *plist, PTR prop, PTR value)
{
	PTR tmp;

	while ((tmp = *plist)->type == LT_NODE) {
		if (nodePTR(tmp)->car == prop) {
			tmp = nodePTR(tmp)->cdr;
			unlinkPTR(nodePTR(tmp)->car);
			nodePTR(tmp)->car = linkPTR(value);
			return;
		}

		tmp = nodePTR(tmp)->cdr;
		plist = &(nodePTR(tmp)->cdr);
	}

	unlinkPTR(*plist);
	tmp = newnodePTR();
	nodePTR(tmp)->car = linkPTR(prop);
	*plist = tmp;
	nodePTR(tmp)->cdr = newnodePTR();
	tmp = nodePTR(tmp)->cdr;
	nodePTR(tmp)->car = linkPTR(value);
	nodePTR(tmp)->cdr = linkPTR(NILptr);
}

void LISPremprop(PTR *plist, PTR prop)
{
	PTR tmp;

	while ((tmp = *plist)->type == LT_NODE) {
		if (nodePTR(tmp)->car == prop) {
			tmp = nodePTR(nodePTR(tmp)->cdr)->cdr;
			(void) linkPTR(tmp);
			unlinkPTR(*plist);
			*plist = tmp;
			return;
		}

		tmp = nodePTR(tmp)->cdr;
		plist = &(nodePTR(tmp)->cdr);
	}
}

PTR LISP_get(PTR args)
{
	PTR sym, prop, def;
	PTR ret, plist;

	GETARGS(("ay?a", args, &sym, &prop, &def, NILptr));
	switch (sym->type) {
	case LT_SYMBOL:
		plist = symbolPTR(sym)->prop;
		break;
	case LT_OBJECT:
		plist = objectPTR(sym)->fields;
		break;
	default:
		return LISPerrormsg(NON_SYMBOL_MSG);
	}

	ret = unlinkedLISPgetprop(plist, prop);
	if (!ret)
		ret = def;

	return linkPTR(ret);
}

PTR LISP_put(PTR args)
{
	PTR sym, prop, val;
	PTR *plist = NULL;

	GETARGS(("aya", args, &sym, &prop, &val));
	switch (sym->type) {
	case LT_SYMBOL:
		plist = &(symbolPTR(sym)->prop);
		break;
	case LT_OBJECT:
		plist = &(objectPTR(sym)->fields);
		break;
	default:
		return LISPerrormsg(NON_SYMBOL_MSG);
	}

	LISPputprop(plist, prop, val);
	
	return linkPTR(val);
}

PTR LISP_remprop(PTR args)
{
	PTR sym, prop, *plist;

	GETARGS(("ay", args, &sym, &prop));
	switch (sym->type) {
	case LT_SYMBOL:
		plist = &(symbolPTR(sym)->prop);
		break;
	case LT_OBJECT:
		plist = &(objectPTR(sym)->fields);
		break;
	default:
		return LISPerrormsg(NON_SYMBOL_MSG);
	}

	LISPremprop(plist, prop);
	
	return linkPTR(NILptr);
}

PTR LISP_getprops(PTR args)
{
	PTR plist, ret, tail;

	GETARGS(("a", args, &plist));
	switch (plist->type) {
	case LT_SYMBOL:
		plist = symbolPTR(plist)->prop;
		break;
	case LT_OBJECT:
		plist = objectPTR(plist)->fields;
		break;
	default:
		return LISPerrormsg(NON_SYMBOL_MSG);
	}

	LISPqueueinit(&ret, &tail);
	while (plist->type == LT_NODE) {
		LISPqueueadd(&ret, &tail, nodePTR(plist)->car);
		plist = nodePTR(nodePTR(plist)->cdr)->cdr;
	}
	return ret;
}

PTR LISP_getplist(PTR args)
{
	PTR plist;

	GETARGS(("a", args, &plist));
	switch (plist->type) {
	case LT_SYMBOL:
		plist = symbolPTR(plist)->prop;
		break;
	case LT_OBJECT:
		plist = objectPTR(plist)->fields;
		break;
	default:
		return LISPerrormsg(NON_SYMBOL_MSG);
	}
	return LISPnewlist(plist);
}

PTR CONSptr, NUMBERptr, SYMBOLptr, STRINGptr, VECTORptr,
	STREAMptr, BUILTINptr, SPECIAL_FORMptr, CLOSUREptr;

PTR LISP_type_of(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	switch (arg->type) { /* TYPESWITCH */
	case LT_NODE:
		arg = CONSptr;
		break;
	case LT_NUMBER:
		arg = NUMBERptr;
		break;
	case LT_SYMBOL:
		arg = SYMBOLptr;
		break;
	case LT_STRING:
		arg = STRINGptr;
		break;
	case LT_STREAM:
		arg = STREAMptr;
		break;
	case LT_BUILTIN:
		arg = BUILTINptr;
		break;
	case LT_SPECIALFORM:
		arg = SPECIAL_FORMptr;
		break;
	case LT_CLOSURE:
		arg = CLOSUREptr;
		break;
	case LT_OBJECT:
		arg = objectPTR(arg)->isa;
		break;
	default:
		arg = unlinkLISPusertype(arg);
		break;
	}
	return linkPTR(arg);
}

PTR LISP_types(PTR args)
{
	PTR head, tail, user;

	LISPqueueinit(&head, &tail);
	LISPqueueadd(&head, &tail, CONSptr);
	LISPqueueadd(&head, &tail, NUMBERptr);
	LISPqueueadd(&head, &tail, SYMBOLptr);
	LISPqueueadd(&head, &tail, STRINGptr);
	LISPqueueadd(&head, &tail, VECTORptr);
	LISPqueueadd(&head, &tail, STREAMptr);
	LISPqueueadd(&head, &tail, BUILTINptr);
	LISPqueueadd(&head, &tail, SPECIAL_FORMptr);
	LISPqueueadd(&head, &tail, CLOSUREptr);
	user = LISPusertypes();
	LISPqueueappend(&head, &tail, user);
	unlinkPTR(user);
	return head;
}

LISPstatic PTR typecheck(PTR args, int type)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return arg->type == type ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_symbolp(PTR args)
{
	return typecheck(args, LT_SYMBOL);
}

PTR LISP_closurep(PTR args)
{
	return typecheck(args, LT_CLOSURE);
}

PTR LISP_consp(PTR args)
{
	return typecheck(args, LT_NODE);
}

PTR LISP_numberp(PTR args)
{
	return typecheck(args, LT_NUMBER);
}

PTR LISP_streamp(PTR args)
{
	return typecheck(args, LT_STREAM);
}

PTR LISP_builtinp(PTR args)
{
	return typecheck(args, LT_BUILTIN);
}

PTR LISP_specialformp(PTR args)
{
	return typecheck(args, LT_SPECIALFORM);
}

PTR LISP_listp(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return (isNIL(arg) || arg->type == LT_NODE) ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_endp(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	if (arg->type == LT_NODE)
		return linkPTR(NILptr);
	if (nonNIL(arg))
		return LISPerrormsg(NON_LIST_MSG);
	return linkPTR(Tptr);
}

PTR LISP_constantp(PTR args)
{
	PTR arg, ret;

	GETARGS(("a", args, &arg));

	ret = Tptr;
	
	switch (arg->type) {
	case LT_SYMBOL:
		if (!(symbolPTR(arg)->flags & LF_CONST))
			ret = NILptr;
		break;
	case LT_NODE:
		if (nodePTR(arg)->car != QUOTEptr)
			ret = NILptr;
		break;
	}

	return linkPTR(ret);
}

PTR LISP_keywordp(PTR args)
{
	PTR arg, ret;

	GETARGS(("a", args, &arg));

	ret = NILptr;
	if (arg->type == LT_SYMBOL
				&& symbolPTR(arg)->flags & LF_COLON)
			ret = Tptr;
	return linkPTR(ret);
}

LISPstatic int numcomp(double val1, double val2)
{
	return (val1 < val2) ? -1 :
		(val1 > val2) ? 1 : 0;
}

int LISPcomp(PTR ptr1, PTR ptr2)
{
	int ret;

	if (LISPfullstack(ptr1))
		LISPfatal("full CPU stack in LISPcomp");

tail_recurse:

	if (ptr1 == ptr2)
		return 0;
	if (ptr1->type != ptr2->type)
		return ptr1->type < ptr2->type ? -1 : 1;
	switch (ptr1->type) { /* TYPESWITCH */
	case LT_NUMBER:
		return numcomp(numberPTR(ptr1)->val, numberPTR(ptr2)->val);
	case LT_SYMBOL:
		return LISPstrcmp(symbolPTR(ptr1)->name, symbolPTR(ptr2)->name);
	case LT_STRING:
		return LISPstrcmp(ptr1, ptr2);
	case LT_STREAM:
		return numcomp((unsigned long) ptr1,
			(unsigned long) ptr2);
	case LT_BUILTIN:
		return numcomp((unsigned long) builtinPTR(ptr1)->fun,
			(unsigned long) builtinPTR(ptr2)->fun);
	case LT_SPECIALFORM:
		return numcomp((unsigned long) specialformPTR(ptr1)->spform,
			(unsigned long) specialformPTR(ptr2)->spform);
	case LT_CLOSURE:
		ret = LISPcomp(closurePTR(ptr1)->fun, closurePTR(ptr2)->fun);
		if (ret)
			return ret;
		return LISPcomp(closurePTR(ptr1)->env, closurePTR(ptr2)->env);
	case LT_OBJECT:
		ret = LISPcomp(objectPTR(ptr1)->isa, objectPTR(ptr2)->isa);
		if (ret)
			return ret;
		return LISPcomp(objectPTR(ptr1)->fields, objectPTR(ptr2)->fields);
	case LT_NODE:
		ret = LISPcomp(nodePTR(ptr1)->car, nodePTR(ptr2)->car);
		if (ret)
			return ret;
		ptr1 = nodePTR(ptr1)->cdr;
		ptr2 = nodePTR(ptr2)->cdr;
		goto tail_recurse;
	case LT_REF:
		return (ptr1 < ptr2) ? -1 : (ptr1 > ptr2) ? 1 : 0;
	}
	return LISPusercmp(ptr1, ptr2);
}

LISPstatic PTR cmpargs(PTR args, int sign, int eq)
{
	PTR arg1, arg2;
	int c;

	if (args->type != LT_NODE)
		return linkPTR(Tptr);
	arg1 = nodePTR(args)->car;
	args = nodePTR(args)->cdr;
	while (args->type == LT_NODE) {
		arg2 = nodePTR(args)->car;
		c = LISPcomp(arg1, arg2);
		/* printf("c:%d sign:%d\n", c, sign); */
		if (eq) {
			if (sign > 0 && c < 0) {
				return linkPTR(NILptr);
			} else if (sign < 0 && c > 0) {
				return linkPTR(NILptr);
			}
		} else {
			if (sign > 0 && c <= 0) {
				return linkPTR(NILptr);
			} else if (sign < 0 && c >= 0) {
				return linkPTR(NILptr);
			}
		}
		/* printf("ok\n"); */
		arg1 = arg2;
		args = nodePTR(args)->cdr;
	}
	return linkPTR(Tptr);
}

PTR LISP_less(PTR args)
{
	return cmpargs(args, -1, 0);
}

PTR LISP_lesseq(PTR args)
{
	return cmpargs(args, -1, 1);
}

PTR LISP_greater(PTR args)
{
	return cmpargs(args, +1, 0);
}

PTR LISP_greatereq(PTR args)
{
	return cmpargs(args, +1, 1);
}

PTR LISP_equalsign(PTR args)
{
	PTR arg1, arg2;

	if (args->type != LT_NODE)
		return linkPTR(Tptr);
	arg1 = nodePTR(args)->car;
	args = nodePTR(args)->cdr;
	while (args->type == LT_NODE) {
		arg2 = nodePTR(args)->car;
		if (LISPcomp(arg1, arg2))
			return linkPTR(NILptr);
		arg1 = arg2;
		args = nodePTR(args)->cdr;
	}
	return linkPTR(Tptr);
}

PTR LISP_slashequal(PTR args)
{
	PTR arg1, tmp;

	while (args->type == LT_NODE) {
		arg1 = nodePTR(args)->car;
		for (tmp = nodePTR(args)->cdr; tmp->type == LT_NODE; tmp = nodePTR(tmp)->cdr) {
			if (!LISPcomp(arg1, nodePTR(tmp)->car))
				return linkPTR(NILptr);
		}
		args = nodePTR(args)->cdr;
	}
	return linkPTR(Tptr);
}

PTR LISP_max(PTR args)
{
	PTR ret, arg;

	ret = NULL;
	while (args->type == LT_NODE) {
		arg = nodePTR(args)->car;
		if (ret) {
			if (LISPcomp(ret, arg) < 0)
				ret = arg;
		} else {
			ret = arg;
		}
		args = nodePTR(args)->cdr;
	}
	return ret ? linkPTR(ret) : linkPTR(NILptr);
}

PTR LISP_min(PTR args)
{
	PTR ret, arg;

	ret = NULL;
	while (args->type == LT_NODE) {
		arg = nodePTR(args)->car;
		if (ret) {
			if (LISPcomp(ret, arg) > 0)
				ret = arg;
		} else {
			ret = arg;
		}
		args = nodePTR(args)->cdr;
	}
	return ret ? linkPTR(ret) : linkPTR(NILptr);
}

PTR LISP_equal(PTR args)
{
	PTR arg1, arg2;

	GETARGS(("aa", args, &arg1, &arg2));

	return equalaux(arg1, arg2) ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

LISPstatic int equalaux(PTR ptr1, PTR ptr2)
{
	if (LISPfullstack(ptr1))
		LISPfatal("full CPU stack in equalaux");
	if (ptr1 == ptr2)
		return 1;
	if (ptr1->type != ptr2->type)
		return 0;
	switch (ptr1->type) { /* TYPESWITCH */
	case LT_NUMBER:
		return numberPTR(ptr1)->val == numberPTR(ptr2)->val;
	case LT_STRING:
		return !strcmp(stringPTRstr(ptr1), stringPTRstr(ptr2));
	case LT_BUILTIN:
		return builtinPTR(ptr1)->fun == builtinPTR(ptr2)->fun;
	case LT_SPECIALFORM:
		return specialformPTR(ptr1)->spform == specialformPTR(ptr2)->spform;
	case LT_CLOSURE:
		return equalaux(closurePTR(ptr1)->env, closurePTR(ptr2)->env)
			&& equalaux(closurePTR(ptr1)->fun, closurePTR(ptr2)->fun);
	case LT_OBJECT:
		return equalaux(objectPTR(ptr1)->isa, objectPTR(ptr2)->isa)
			&& equalaux(objectPTR(ptr1)->fields, objectPTR(ptr2)->fields);
	case LT_NODE:
		return equallist(ptr1, ptr2);
	case LT_REF:
		return ptr1 == ptr2;
	}
	if (ptr1->type >= LT_USER_FIRST)
		return !LISPusercmp(ptr1, ptr2);
	return 0;
}

LISPstatic int equallist(PTR ptr1, PTR ptr2)
{
	while (ptr1->type == LT_NODE && ptr2->type == LT_NODE) {
		if (ptr1 == ptr2)
			return 1;
		if (!equalaux(nodePTR(ptr1)->car, nodePTR(ptr2)->car))
			return 0;
		ptr1 = nodePTR(ptr1)->cdr;
		ptr2 = nodePTR(ptr2)->cdr;
	}
	return equalaux(ptr1, ptr2);
}

PTR LISP_null(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return isNIL(arg) ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_identity(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return linkPTR(arg);
}

LISPstatic void setspecialsym(PTR sym, PTR val)
{
	PTR tmp;

	(void) linkPTR(val);
	if (isNIL(symbolPTR(sym)->bind)) {
		unlinkPTR(NILptr);
		tmp = (symbolPTR(sym)->bind = newnodePTR());
		nodePTR(tmp)->cdr = linkPTR(NILptr);
		symbolPTR(sym)->flags |= LF_SPECIAL;
	} else {
		tmp = symbolPTR(sym)->bind;
		unlinkPTR(nodePTR(tmp)->car);
	}
	/* printf("SPECIALBIND %p\n", tmp); */
	nodePTR(tmp)->car = val;
	if (symbolPTR(sym)->flags & LF_BINDCALLBACK)
		LISPbindcallback(sym, val);
}

PTR LISP_set(PTR args)
{
	PTR ret, sym;

	GETARGS(("ya", args, &sym, &ret));
	if (symbolPTR(sym)->flags & LF_CONST)
		return LISPerrormsg(CANT_SET_MSG);
	setspecialsym(sym, ret);
	return linkPTR(ret);
}

LISPstatic PTR *symbind(PTR sym)
{
	PTR tmp;

	tmp = nodePTR(LISPenvstack)->car;
	while (tmp->type == LT_NODE) {
		if (nodePTR(tmp)->car->type == LT_NODE && nodePTR(nodePTR(tmp)->car)->car == sym) {
			return &(nodePTR(nodePTR(tmp)->car)->cdr);
		}
		tmp = nodePTR(tmp)->cdr;
	}

	tmp = symbolPTR(sym)->bind;
	if (tmp->type != LT_NODE) {
		/* return NULL; */
		tmp = newnodePTR();
		nodePTR(tmp)->car = linkPTR(NILptr);
		nodePTR(tmp)->cdr = linkPTR(NILptr);
		unlinkPTR(symbolPTR(sym)->bind);
		symbolPTR(sym)->bind = tmp;
		symbolPTR(sym)-> flags |= LF_SPECIAL;
	}

	return &(nodePTR(tmp)->car);
}

PTR LISP_setq(PTR args, PTR tail)
{
	PTR ret, sym, *bind;

	ret = linkPTR(NILptr);

	while (args->type == LT_NODE) {
		unlinkPTR(ret);

		sym = nodePTR(args)->car;
		args = nodePTR(args)->cdr;
		if (args->type != LT_NODE)
			return LISPerrormsg("not enough arguments");
		ret = nodePTR(args)->car;
		args = nodePTR(args)->cdr;

		if (sym->type != LT_SYMBOL)
			return LISPerrormsg(NON_SYMBOL_MSG);
		if (symbolPTR(sym)->flags & LF_CONST)
			return LISPerrormsg(CANT_SET_MSG);

		bind = symbind(sym);
		if (!bind)
			return LISPerrormsg(UNBOUND_SYMBOL_MSG);
		/* printf("BIND %p\n", bind); */

		ret = LISPevalobj(ret);
		if (LISPstatus & LS_ABORT)
			return ret;

		unlinkPTR(*bind);
		*bind = linkPTR(ret);

		if (symbolPTR(sym)->flags & LF_BINDCALLBACK)
			LISPbindcallback(sym, ret);
	}

	return ret;
}

PTR LISP_psetq(PTR args, PTR tail)
{
	PTR set_head, set_tail;
	PTR sym, val, *bind;

	LISPqueueinit(&set_head, &set_tail);
	while (args->type == LT_NODE) {
		sym = nodePTR(args)->car;
		args = nodePTR(args)->cdr;
		if (sym->type != LT_SYMBOL) {
			unlinkPTR(set_head);
			return LISPerrormsg(NON_SYMBOL_MSG);
		}
		LISPqueueadd(&set_head, &set_tail, sym);

		if (args->type == LT_NODE) {
			val = LISPevalobj(nodePTR(args)->car);
			args = nodePTR(args)->cdr;
			if (LISPstatus & LS_ABORT) {
				unlinkPTR(set_head);
				return val;
			}
		} else {
			val = linkPTR(NILptr);
		}
		LISPqueueadd(&set_head, &set_tail, val);
		unlinkPTR(val);
	}

	val = linkPTR(NILptr);
	args = set_head;
	while (args->type == LT_NODE) {
		sym = nodePTR(args)->car;
		args = nodePTR(args)->cdr;
		unlinkPTR(val);
		val = linkPTR(nodePTR(args)->car);
		args = nodePTR(args)->cdr;

		bind = symbind(sym);
		if (!bind) {
			unlinkPTR(set_head);
			return LISPerrormsg(UNBOUND_SYMBOL_MSG);
		}
		unlinkPTR(*bind);
		*bind = linkPTR(val);

		if (symbolPTR(sym)->flags & LF_BINDCALLBACK)
			LISPbindcallback(sym, val);
	}

	unlinkPTR(set_head);
	return val;
}

PTR LISP_makunbound(PTR args)
{
	PTR sym;

	GETARGS(("y", args, &sym));
	if (symbolPTR(sym)->flags & LF_CONST)
		return LISPerrormsg(CANT_SET_MSG);
	unlinkPTR(symbolPTR(sym)->bind);
	symbolPTR(sym)->bind = linkPTR(NILptr);
	symbolPTR(sym)->flags &= ~LF_SPECIAL;
	return linkPTR(sym);
}

void LISPpushval(PTR symbol, PTR value)
{
	PTR tmp;

	CHKPTR(symbol);
#ifdef LISPDEBUG
	if (symbol->type != LT_SYMBOL)
		LISPfatal("not symbol in LISPpushval");
#endif
	tmp = newnodePTR();
	nodePTR(tmp)->car = linkPTR(value);
	nodePTR(tmp)->cdr = symbolPTR(symbol)->bind;
	symbolPTR(symbol)->bind = tmp;
	if (symbolPTR(symbol)->flags & LF_BINDCALLBACK)
		LISPbindcallback(symbol, value);
}

void LISPpopval(PTR symbol)
{
	PTR tmp;

	CHKPTR(symbol);
#ifdef LISPDEBUG
	if (symbol->type != LT_SYMBOL)
		LISPfatal("not symbol in LISPpopval");
#endif
	if (symbolPTR(symbol)->bind->type == LT_NODE) {
		tmp = symbolPTR(symbol)->bind;
		symbolPTR(symbol)->bind = linkPTR(nodePTR(tmp)->cdr);
		unlinkPTR(tmp);
	}
	if (symbolPTR(symbol)->flags & LF_BINDCALLBACK) {
		tmp = symbolPTR(symbol)->bind;
		if (tmp->type == LT_NODE) {
			tmp = nodePTR(tmp)->car;
		} else {
			tmp = symbol;
		}
		LISPbindcallback(symbol, tmp);
	}
}

void LISPsetval(PTR symbol, PTR value)
{
	LISPpopval(symbol);
	LISPpushval(symbol, value);
}

void LISPstackpush(PTR *stack, PTR ptr)
{
	PTR tmp;

	tmp = newnodePTR();
	nodePTR(tmp)->car = linkPTR(ptr);
	nodePTR(tmp)->cdr = *stack;
	*stack = tmp;
}

void LISPstackpushlist(PTR *stack, PTR ptr)
{
	if (ptr->type == LT_NODE) {
		LISPstackpushlist(stack, nodePTR(ptr)->cdr);
		LISPstackpush(stack, nodePTR(ptr)->car);
	}
}

PTR LISPstackpop(PTR *stack)
{
	PTR ret, tmp;

	tmp = *stack;
#ifdef LISPDEBUG
	if (isNIL(tmp))
		LISPfatal("empty stack in LISPstackpop");
#endif
	*stack = linkPTR(nodePTR(tmp)->cdr);
	/* nodePTR(tmp)->cdr = linkPTR(NILptr); */
	ret = linkPTR(nodePTR(tmp)->car);
	unlinkPTR(tmp);
	return ret;
}

PTR LISP_ascii(PTR args)
{
	PTR ret, arg;
	int a;

	GETARGS(("a", args, &arg));
	switch (arg->type) {
	case LT_SYMBOL:
		arg = symbolPTR(arg)->name;
	case LT_STRING:
		ret = newnumberPTR((unsigned char) *stringPTRstr(arg));
		break;
	case LT_NUMBER:
		a = (int) fmod(numberPTR(arg)->val, 256.0);
		if (a < 0)
			a += 256;
		ret = newcharPTR(a);
		break;
	default:
		ret = LISPerrormsg(ILLEGAL_TYPE_MSG);
		break;
	}
	return ret;
}

PTR LISP_copy_list(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return LISPnewlist(arg);
}

PTR LISP_copy_tree(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return LISPnewstructure(arg);
}

PTR LISP_rplaca(PTR args)
{
	PTR ret, val;

	GETARGS(("ca", args, &ret, &val));
	unlinkPTR(nodePTR(ret)->car);
	nodePTR(ret)->car = linkPTR(val);
	return linkPTR(ret);
}

PTR LISP_rplacd(PTR args)
{
	PTR ret, val;

	GETARGS(("ca", args, &ret, &val));
	unlinkPTR(nodePTR(ret)->cdr);
	nodePTR(ret)->cdr = linkPTR(val);
	return linkPTR(ret);
}

PTR LISP_nconc(PTR args)
{
	PTR ret, tail, arg;

	ret = linkPTR(NILptr);
	tail = NULL;
	while (1) {
		if (!args || args->type != LT_NODE)
			break;
		arg = nodePTR(args)->car;
		if (arg && arg->type == LT_NODE) {
			if (tail) {
				unlinkPTR(nodePTR(tail)->cdr);
				nodePTR(tail)->cdr = linkPTR(arg);
			} else {
				unlinkPTR(ret);
				ret = linkPTR(arg);
			}
			tail = arg;
			if (nodePTR(args)->cdr && nodePTR(args)->cdr->type == LT_NODE) {
				while (nodePTR(tail)->cdr->type == LT_NODE) {
					tail = nodePTR(tail)->cdr;
				}
			} else {
				break;
			}
		}
		args = nodePTR(args)->cdr;
	}
	return ret;
}

PTR SORTPREDptr;

LISPstatic PTR sort_aux(PTR lst, PTR pred);
LISPstatic void put_nil(PTR lst);

PTR LISP_sort(PTR args)
{
	PTR arg, pred;
	PTR tmp, lst, tail, ret;

	GETARGS(("a?a", args, &arg, &pred, SORTPREDptr));

	if (arg->type != LT_NODE)
		return linkPTR(arg);

	/* creates a new NULL terminated list for sort_aux */
	lst = tail = NULL;
	ret = arg;
	while (ret->type == LT_NODE) {
		tmp = newnodePTR();
		nodePTR(tmp)->car = linkPTR(nodePTR(ret)->car);
		if (tail)
			nodePTR(tail)->cdr = tmp;
		else
			lst = tmp;
		tail = tmp;
		ret = nodePTR(ret)->cdr;
	}
	nodePTR(tail)->cdr = NULL;

	ret = sort_aux(lst, pred);
	if (LISPstatus & LS_ABORT)
		return ret;

	for (tmp = ret; nodePTR(tmp)->cdr; tmp = nodePTR(tmp)->cdr)
		;
	nodePTR(tmp)->cdr = linkPTR(NILptr);
	return ret;
}

LISPstatic PTR sort_aux(PTR lst, PTR pred)
/* lst must be NULL terminated */
/* non standard linking */
{
	PTR lst1, lst2, tmp;
	PTR tail, predargs;

	if (LISPfullstack(lst1))
		LISPfatal("full CPU stack in sort_aux");
	if (!lst)
		return lst;
	if (!(nodePTR(lst)->cdr))
		return lst;
	lst1 = lst2 = NULL;
	while (lst) {
		LISPassert(lst->nl == 1);
		tmp = lst;
		lst = nodePTR(tmp)->cdr;
		nodePTR(tmp)->cdr = lst1;
		lst1 = lst2;
		lst2 = tmp;
	}

	lst1 = sort_aux(lst1, pred);
	if (LISPstatus & LS_ABORT) {
		put_nil(lst2);
		unlinkPTR(lst2);
		return lst1;
	}
	
	lst2 = sort_aux(lst2, pred);
	if (LISPstatus & LS_ABORT) {
		put_nil(lst1);
		unlinkPTR(lst1);
		return lst2;
	}

	lst = tail = NULL;
	while (lst1 && lst2) {
		predargs = LISPmakelist(nodePTR(lst1)->car, nodePTR(lst2)->car, NULL);
		tmp = LISPapplyfun(pred, predargs, 0);
		unlinkPTR(predargs);
		if (LISPstatus & LS_ABORT) {
			put_nil(lst1);
			unlinkPTR(lst1);
			put_nil(lst2);
			unlinkPTR(lst2);
			if (tail)
				nodePTR(tail)->cdr = linkPTR(NILptr);
			if (lst)
				unlinkPTR(lst);
			return tmp;
		}
		if (nonNIL(tmp)) {
			unlinkPTR(tmp);
			tmp = lst1;
			lst1 = nodePTR(lst1)->cdr;
		} else {
			unlinkPTR(tmp);
			tmp = lst2;
			lst2 = nodePTR(lst2)->cdr;
		}
		if (tail) {
			nodePTR(tail)->cdr = tmp;
		} else {
			lst = tmp;
		}
		tail = tmp;
	}
	
	if (lst1) {
		if (tail) {
			nodePTR(tail)->cdr = lst1;
		} else {
			lst = lst1;
		}
	} else {
		if (tail) {
			nodePTR(tail)->cdr = lst2;
		} else {
			lst = lst2;
		}
	}
	
	return lst;
}

LISPstatic void put_nil(PTR lst)
{
	while (nodePTR(lst)->cdr)
		lst = nodePTR(lst)->cdr;
	nodePTR(lst)->cdr = linkPTR(NILptr);
}

PTR LISP_uniq(PTR args)
{
	PTR lst, test, tmp, last;
	PTR tres;
	PTR ret, tail;

	GETARGS(("a:a", args, &lst, TESTptr, &test, EQLptr));
	LISPqueueinit(&ret, &tail);
	last = NULL;
	while (lst->type == LT_NODE) {
		if (last) {
			tmp = LISPmakelist(last, nodePTR(lst)->car, NULL);
			tres = LISPapplyfun(test, tmp, 0);
			unlinkPTR(tmp);
			if (LISPstatus & LS_ABORT) {
				unlinkPTR(ret);
				return tres;
			}
			if (isNIL(tres)) {
				LISPqueueadd(&ret, &tail, nodePTR(lst)->car);
				last = nodePTR(lst)->car;
			}
			unlinkPTR(tres);
		} else {
			LISPqueueadd(&ret, &tail, nodePTR(lst)->car);
			last = nodePTR(lst)->car;
		}
		lst = nodePTR(lst)->cdr;
	}
	return ret;
}

PTR LISP_break(PTR args)
{
	GETARGS(("", args));
	LISPstatus |= (LS_BREAK | LS_USERBREAK);
	return linkPTR(NILptr);
}

PTR LISP_abort(PTR args)
{
	GETARGS(("", args));
	LISPstatus |= LS_ABORT;
	return linkPTR(NILptr);
}

PTR LISP_exit(PTR args)
{
	PTR arg;

	GETARGS(("?n", args, &arg, ZEROptr));
	LISPstatus |= (LS_EXIT | LS_ABORT);
	unlinkPTR(LISPexitvalue);
	LISPexitvalue = linkPTR(arg);
	return linkPTR(NILptr);
}

PTR LISP_error_exit(PTR args)
{
	GETARGS(("", args));

	LISPstatus |= (LS_EXIT | LS_ABORT);
	unlinkPTR(LISPexitvalue);
	LISPexitvalue = newnumberPTR(1);
	return linkPTR(NILptr);
}

