/*

	LISPEVAL.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzione EVAL.

*/

#include "bdlisp.h"

PTR LISPenvstack;

PTR DISPLACE_MACROSptr;

PTR EVALHOOKptr, APPLYHOOKptr, DEBUGHOOKptr;
int LISPevalhookflag, LISPapplyhookflag;

PTR LISP_eval(PTR args)
{
	PTR arg, env, ret;

	GETARGS(("a?a", args, &arg, &env, NILptr));

	if (!LISPvalid_environment(env))
		return LISPerrormsg("non valid environment");
	/* puts("push environment"); */
	LISPstackpush(&LISPenvstack, env);

	ret = LISPevalobj(arg);
	/* puts("pop environment") */;
	env = LISPstackpop(&LISPenvstack);
	unlinkPTR(env);
	return ret;
}

LISPstatic PTR evalsymbol(PTR sym);
LISPstatic PTR evalform(PTR form);
LISPstatic PTR evalargs(PTR args);

PTR LISPstatic evalhook(PTR ptr);
PTR LISPstatic applyhook(PTR fun, PTR args);

LISPstatic PTR evalobj_no_hook(PTR ptr);

int LISPvalid_environment(PTR env)
{
	/* LISPdebugwrite("TEST ~\n", env); */
	while (env->type == LT_NODE) {
		if (nodePTR(env)->car->type == LT_NODE && nodePTR(nodePTR(env)->car)->car->type == LT_SYMBOL) {
			env = nodePTR(env)->cdr;
		} else {
			return 0;
		}
	}
	return 1;
}

PTR LISPevalobj(PTR ptr)
{
	CHKPTR(ptr);

	if (LISPevalhookflag)
		return evalhook(ptr);

	/* it could just call evalobj_no_hook */
	/* code duplicated for efficiency     */

	switch (ptr->type) {
	case LT_NODE:
		return evalform(ptr);
	case LT_SYMBOL:
		return evalsymbol(ptr);
	default:
		LISPresetmret();
		return linkPTR(ptr);
	}
}

LISPstatic PTR evalobj_no_hook(PTR ptr)
{
	CHKPTR(ptr);

	switch (ptr->type) {
	case LT_NODE:
		return evalform(ptr);
	case LT_SYMBOL:
		return evalsymbol(ptr);
	default:
		LISPresetmret();
		return linkPTR(ptr);
	}
}

LISPstatic PTR evalhook(PTR ptr)
{
	PTR tmp, ret;

	tmp = LISPmakelist(unlinkLISPgetval(EVALHOOKptr), ptr, NULL);
	LISPpushval(EVALHOOKptr, NILptr);

	ret = LISPapplyfun(nodePTR(tmp)->car, nodePTR(tmp)->cdr, 1);

	unlinkPTR(tmp);
	LISPpopval(EVALHOOKptr);
	return ret;
}

LISPstatic PTR evalsymbol(PTR sym)
{
	PTR tmp;
	
	LISPresetmret();

	/* ricerca nell'environment */

	tmp = nodePTR(LISPenvstack)->car;
	/* LISPdebugwrite("SEARCH ~ in ~\n", sym, tmp); */
	while (tmp->type == LT_NODE) {
		if (nodePTR(tmp)->car->type == LT_NODE && nodePTR(nodePTR(tmp)->car)->car == sym) {
			tmp = nodePTR(nodePTR(tmp)->car)->cdr;
			/* LISPdebugwrite("FOUND ~ = ~\n", sym, tmp); */
			return linkPTR(tmp);
		}
		tmp = nodePTR(tmp)->cdr;
	}

	/* ricerca nelle variabili speciali */

	if (isNIL(symbolPTR(sym)->bind)) {
		if (symbolPTR(sym)->flags & LF_CONST)
			return linkPTR(sym);
		unlinkedLISPbreakfun = sym;
		unlinkedLISPbreakargs = NULL;
		LISPbreakeval = 1;
		return LISPerrormsg("unbound symbol");
	}
	return linkPTR(nodePTR(symbolPTR(sym)->bind)->car);
}

LISPstatic PTR evalform(PTR form)
{
	PTR ret, actualargs;
	PTR fun;
	int flags, old_allowreturn;
#ifdef CTRL_C_CHECK
	static int brkcount;
#endif

	CHKPTR(form);
#ifdef CTRL_C_CHECK
	if (++brkcount > 1000) {
		LISPbreakcheck();
		brkcount = 0;
	}
#endif

	if (LISPfullstack(ret)) {
		LISPputs(NULL, FULL_STACK_MSG);
		LISPstatus |= LS_ABORT;
		return linkPTR(NILptr);
	}

	fun = nodePTR(form)->car;

	if (LISPstatus & (LS_USERBREAK | LS_BREAK)) {
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = nodePTR(form)->cdr;
		LISPbreakeval = 1;
		if (LISPstatus & LS_USERBREAK)
			return LISPerrormsg("ctrl-c pressed");
		else
			return LISPerrormsg("program break");
	}

	if (LISPstatus & LS_ABORT) {
		fputs("LS_ABORT ???\n", stderr);
		return linkPTR(NILptr);
	}

	if (fun->type != LT_SYMBOL) {
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = nodePTR(form)->cdr;
		LISPbreakeval = 1;
		return LISPerrormsg(ILL_FORM_HEAD_MSG);
	}

	if (isNIL(symbolPTR(fun)->fun)) {
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = nodePTR(form)->cdr;
		LISPbreakeval = 1;
		return LISPerrormsg("evaluating undefined function");
	}
	
	flags = symbolPTR(fun)->flags;
	
	if (flags & LF_ISSPECIALFORM) {
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = nodePTR(form)->cdr;
		LISPbreakeval = 1;
		fun = symbolPTR(fun)->fun;
		switch (fun->type) {
		case LT_SPECIALFORM:
			ret = (specialformPTR(fun)->spform)(nodePTR(form)->cdr, NULL);
			if (!(flags & LF_MULRET) && !(LISPstatus & LS_ABORT))
				LISPresetmret();
			break;
		default:
			LISPfatal("illegal type %d in evalform/special", fun->type);
			break;
		}
	} else if (flags & LF_ISMACRO) {
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = nodePTR(form)->cdr;
		LISPbreakeval = 1;
		fun = symbolPTR(fun)->fun;
		switch (fun->type) {
		case LT_NODE:
			old_allowreturn = LISPallowreturn;
			LISPallowreturn = 0;
			ret = LISPevaluserfun(fun, nodePTR(form)->cdr, NILptr, NULL);
			LISPallowreturn = old_allowreturn;
			break;
		default:
			LISPfatal("illegal type %d in evalform/macro", fun->type);
			break;
		}

		if (LISPstatus & LS_ABORT)
			return ret;

		LISPresetmret();

		if (ret->type == LT_NODE
				&& nonNIL(unlinkLISPgetval(DISPLACE_MACROSptr))) {
			unlinkPTR(nodePTR(form)->car);
			unlinkPTR(nodePTR(form)->cdr);
			nodePTR(form)->car = linkPTR(nodePTR(ret)->car);
			nodePTR(form)->cdr = linkPTR(nodePTR(ret)->cdr);
		}
			
		fun = ret;
		ret = LISPevalobj(fun);
		unlinkPTR(fun);
	} else {
		actualargs = evalargs(nodePTR(form)->cdr);
		/* LISPdebugwrite("ACTUAL ~\n", actualargs); */
		if (LISPstatus & LS_ABORT)
			return actualargs;
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = actualargs;
		LISPbreakeval = 0;
		if (LISPapplyhookflag) {
			ret = applyhook(fun, actualargs);
		} else {
			fun = symbolPTR(fun)->fun;
			switch (fun->type) {
			case LT_BUILTIN:
				ret = (builtinPTR(fun)->fun)(actualargs);
				if (!(flags & LF_MULRET) && !(LISPstatus & LS_ABORT))
					LISPresetmret();
				break;
			case LT_NODE:
				old_allowreturn = LISPallowreturn;
				LISPallowreturn = 0;
				ret = LISPevaluserfun(fun, actualargs, NILptr, NULL);
				LISPallowreturn = old_allowreturn;
				break;
			default:
				LISPfatal("illegal type %d in evalform/function", fun->type);
				break;
			}
		}
		unlinkPTR(actualargs);
	}

	return ret;
}

LISPstatic PTR applyhook(PTR fun, PTR args)
{
	PTR ret, form;

	form = LISPmakelist(unlinkLISPgetval(APPLYHOOKptr), fun, args, NULL);
	LISPpushval(APPLYHOOKptr, NILptr);
	ret = LISPapplyfun(nodePTR(form)->car, nodePTR(form)->cdr, 1);
	unlinkPTR(form);
	LISPpopval(APPLYHOOKptr);
	return ret;
}

PTR LISP_evalhook(PTR args)
{
	PTR obj, evalhook, applyhook, env;
	PTR ret;

	GETARGS(("aaa?a", args, &obj, &evalhook, &applyhook, &env, NILptr));

	LISPstackpush(&LISPenvstack, env);
	LISPpushval(EVALHOOKptr, evalhook);
	LISPpushval(APPLYHOOKptr, applyhook);

	ret = evalobj_no_hook(obj);

	env = LISPstackpop(&LISPenvstack);
	unlinkPTR(env);
	LISPpopval(EVALHOOKptr);
	LISPpopval(APPLYHOOKptr);
	return ret;
}

PTR LISP_applyhook(PTR args)
{
	PTR fun, arglist, evalhook, applyhook;
	PTR ret;

	GETARGS(("aaaa", args, &fun, &arglist, &evalhook, &applyhook));

	LISPpushval(EVALHOOKptr, evalhook);
	LISPpushval(APPLYHOOKptr, applyhook);
	ret = LISPapplyfun(fun, arglist, 1);
	LISPpopval(EVALHOOKptr);
	LISPpopval(APPLYHOOKptr);
	return ret;
}

PTR LISPapplyfun(PTR fun, PTR args, int mulflag)
{
	PTR ret = NULL, env = NILptr;
	int flags = 0;

	if (fun->type == LT_CLOSURE) {
		env = closurePTR(fun)->env;
		fun = closurePTR(fun)->fun;
	}
	switch (fun->type) {
	case LT_SYMBOL:
		flags = symbolPTR(fun)->flags;
		fun = symbolPTR(fun)->fun;
		if (isNIL(fun)) {
			ret = LISPerrormsg("applying undefined function");
			break;
		}
		switch (fun->type) {
		case LT_BUILTIN:
			ret = (builtinPTR(fun)->fun)(args);
			if (!(flags & LF_MULRET))
				LISPresetmret();
			break;
		case LT_SPECIALFORM:
			ret = LISPerrormsg("can't apply a special form");
			break;
		case LT_NODE:
			ret = LISPevaluserfun(fun, args, NILptr, NULL);
			break;
		default:
			LISPfatal("bad function type %d", fun->type);
			break;
		}
		break;
	case LT_BUILTIN:
		LISPresetmret();
		ret = (builtinPTR(fun)->fun)(args);
		break;
	case LT_NODE:
		if (flags || LISPvalidfunction(fun)) {
			ret = LISPevaluserfun(fun, args, env, NULL);
			break;
		}
	default: /* prevents compiler warning */
		unlinkedLISPbreakfun = fun;
		unlinkedLISPbreakargs = args;
		LISPbreakeval = 0;
		return LISPerrormsg(ILL_FORM_HEAD_MSG);
	}

	if (!mulflag)
		LISPresetmret();

	return ret;
}

LISPstatic PTR evalargs(PTR args)
{
	PTR ret, tail, ptr, tmp;

	ret = linkPTR(NILptr);
	tail = NULL;
	while (args->type == LT_NODE) {
		tmp = LISPevalobj(nodePTR(args)->car);
		if (LISPstatus & LS_ABORT) {
			unlinkPTR(ret);
			return tmp;
		}
		ptr = newnodePTR();
		nodePTR(ptr)->car = tmp;
		nodePTR(ptr)->cdr = NILptr;
		if (tail) {
			nodePTR(tail)->cdr = ptr;
		} else {
			ret = ptr;
		}
		tail = ptr;
		args = nodePTR(args)->cdr;
	}
	return ret;
}

PTR LISPmakelist(PTR first, ...)
{
	PTR ret, tail, node, tmp;
	va_list vl;

	CHKPTR(first);
	ret = tail = newnodePTR();
	nodePTR(ret)->car = linkPTR(first);
	va_start(vl, first);
	while (NULL != (tmp = va_arg(vl, PTR))) {
		CHKPTR(tmp);
		node = newnodePTR();
		nodePTR(node)->car = linkPTR(tmp);
		nodePTR(tail)->cdr = node;
		tail = node;
	}
	va_end(vl);
	nodePTR(tail)->cdr = linkPTR(NILptr);
	return ret;
}

char *LISPgetargserror;

int LISPgetargs(char *spec, PTR args, ...)
{
	va_list vl;
	int status = 0;
	PTR *arg, key, keyargs = NULL;
	PTR keylist = NULL;

	/*
		types:
		a = any
		n = LT_NUMBER
		y = LT_SYMBOL
		s = LT_STRING
		f = LT_STREAM
		o = LT_OBJECT
		c = LT_NODE
		? = &optional
		: = &key
		- = &rest
	*/

	va_start(vl, args);
	while (*spec) {
		if (*spec == '-') {
			arg = va_arg(vl, PTR *);
			*arg = keyargs ? keyargs : args;
			if ((*arg)->type != LT_NODE)
				*arg = NILptr;
			args = NILptr;
			break;
		}

		if (*spec == '?') {
			status = '?';
			spec++;
			continue;
		}

		if (*spec == ':') {
			status = ':';
			keyargs = args;
			keylist = linkPTR(NILptr);
			args = NILptr;
			spec++;
			continue;
		}

		arg = va_arg(vl, PTR *);

		switch (status) {
		default:
			if (args->type == LT_NODE) {
				*arg = nodePTR(args)->car;
				args = nodePTR(args)->cdr;
			} else {
				LISPgetargserror = "not enough arguments";
				va_end(vl);
				if (keylist)
					unlinkPTR(keylist);
				return 1;
			}
			break;
		case '?':
			if (args->type == LT_NODE) {
				*arg = nodePTR(args)->car;
				args = nodePTR(args)->cdr;
				(void) va_arg(vl, PTR);
			} else {
				*arg = va_arg(vl, PTR);
			}
			break;
		case ':':
			key = (PTR) arg;
			arg = va_arg(vl, PTR *);
			*arg = unlinkLISPkeyarg(keyargs, key, va_arg(vl, PTR));
			LISPstackpush(&keylist, key);
			break;
		}

		if (*arg) {
			CHKPTR(*arg);
			switch (*spec) {
			case 'a':
				break;
			case 'n':
				if ((*arg)->type != LT_NUMBER) {
					LISPgetargserror = NON_NUMERIC_MSG;
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			case 'y':
				if ((*arg)->type != LT_SYMBOL) {
					LISPgetargserror = NON_SYMBOL_MSG;
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			case 's':
				if ((*arg)->type == LT_SYMBOL)
					*arg = symbolPTR(*arg)->name;
				if ((*arg)->type != LT_STRING) {
					LISPgetargserror = NON_STRING_MSG;
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			case 'f':
				if ((*arg)->type != LT_STREAM) {
					LISPgetargserror = NON_STREAM_MSG;
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			case 'o':
				if ((*arg)->type != LT_OBJECT) {
					LISPgetargserror = "non object argument";
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			case 'c':
				if ((*arg)->type != LT_NODE) {
					LISPgetargserror = NON_NODE_MSG;
					va_end(vl);
					if (keylist)
						unlinkPTR(keylist);
					return 1;
				}
				break;
			default:
				LISPfatal("unknown key '%c' in LISPgetargs", *spec);
				break;
			}
		}
		spec++;
	}

	va_end(vl);

	if (keylist && !LISPvalidkeyargs(keyargs, keylist)) {
		unlinkPTR(keylist);
		LISPgetargserror = "bad key arguments";
		return 1;
	}

	if (keylist)
		unlinkPTR(keylist);

	if (args != NILptr) {
		LISPgetargserror = "too many arguments";
		return 1;
	}

	return 0;
}

PTR unlinkLISPkeyarg(PTR args, PTR key, PTR def)
{
	key = symbolPTR(key)->key;
	if (isNIL(key))
		return def;

	while (args->type == LT_NODE) {
		if (nodePTR(args)->car == key) {
			args = nodePTR(args)->cdr;
			if (args->type != LT_NODE)
				return NILptr;
			return nodePTR(args)->car;
		}
		args = nodePTR(args)->cdr;
		if (args->type == LT_NODE)
			args = nodePTR(args)->cdr;
	}

	return def;
}


PTR unlinkLISPgetval(PTR ptr)
{
	if (symbolPTR(ptr)->bind->type == LT_NODE)
		return nodePTR(symbolPTR(ptr)->bind)->car;
	return ptr;
}

PTR LISP_the_environment(PTR args)
{
	GETARGS(("", args));

	args = nodePTR(LISPenvstack)->car;
	/* return linkPTR(LISPenvstack); */
	return linkPTR(args);
}

