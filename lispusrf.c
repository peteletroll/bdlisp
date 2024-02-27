/*

	LISPUSRF.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni definite dall'utente.

*/

#include "bdlisp.h"

PTR LAMBDAptr, MACROptr;
PTR OPTIONALptr, RESTptr, KEYptr, AUXptr;

PTR LISPcatchstack;
PTR LISPthrownsymbol, LISPthrownvalue;

PTR RETURN_THROWptr;

PTR LISPmret;

int LISPallowreturn = 0;

LISPstatic PTR LISPtrap(void);

enum funtype { FT_BAD = 0, FT_LAMBDA, FT_MACRO };
LISPstatic enum funtype findfuntype(PTR fun);
LISPstatic int validargs(PTR ptr);
LISPstatic PTR setlambdafargs(PTR fargs, PTR actualargs, PTR env);
LISPstatic void clearspecialvars(PTR fargs);

LISPstatic PTR processtasks(PTR tasks, PTR def, PTR tail);

int LISPvalidfunction(PTR ptr)
{
	return findfuntype(ptr) != FT_BAD;
}

PTR LISP_cond(PTR args, PTR tail)
{
	PTR ret, test, task;

	while (args->type == LT_NODE) {
		task = nodePTR(args)->car;
		if (task->type == LT_NODE) {
			test = LISPevalobj(nodePTR(task)->car);
			if (LISPstatus & LS_ABORT)
				return test;
			if (nonNIL(test)) {
				ret = processtasks(nodePTR(task)->cdr, test, NULL);
				unlinkPTR(test);
				return ret;
			}
			unlinkPTR(test);
		}
		args = nodePTR(args)->cdr;
	}
	return linkPTR(NILptr);
}

PTR LISP_and(PTR args, PTR tail)
{
	PTR ret;

	ret = linkPTR(Tptr);
	while (args->type == LT_NODE) {
		unlinkPTR(ret);
		ret = LISPevalobj(nodePTR(args)->car);
		if (LISPstatus & LS_ABORT)
			return ret;
		if (isNIL(ret))
			break;
		args = nodePTR(args)->cdr;
	}
	return ret;
}

PTR LISP_or(PTR args, PTR tail)
{
	PTR ret;

	ret = linkPTR(NILptr);
	while (args->type == LT_NODE) {
		unlinkPTR(ret);
		ret = LISPevalobj(nodePTR(args)->car);
		if (LISPstatus & LS_ABORT)
			return ret;
		if (nonNIL(ret))
			break;
		args = nodePTR(args)->cdr;
	}
	return ret;
}

PTR LISP_do_loop(PTR args, PTR tail)
{
	PTR ret, curr, task;

	curr = args;
	LISPallowreturn++;
	for (;;) {
		task = nodePTR(curr)->car;
		ret = LISPevalobj(task);
		if (LISPstatus & (LS_ABORT | LS_BREAK)) {
			if (LISPthrownsymbol == RETURN_THROWptr) {
				unlinkPTR(ret);
				ret = LISPtrap();
			}
			break;
		}
		unlinkPTR(ret);
		curr = nodePTR(curr)->cdr;
		if (curr->type != LT_NODE)
			curr = args;
	}
	LISPallowreturn--;
	return ret;
}

PTR LISP_prog1(PTR args, PTR tail)
{
	PTR ret, thr;

	if (args->type != LT_NODE)
		return linkPTR(NILptr);
	ret = LISPevalobj(nodePTR(args)->car);
	if (LISPstatus & LS_ABORT)
		return ret;

	thr = processtasks(nodePTR(args)->cdr, NILptr, NULL);
	if (LISPstatus & LS_ABORT) {
		unlinkPTR(ret);
		return thr;
	}
	unlinkPTR(thr);
	return ret;
}

PTR LISP_multiple_value_prog1(PTR args, PTR tail)
{
	PTR ret, thr;

	if (args->type != LT_NODE)
		return linkPTR(NILptr);
	thr = LISPevalobj(nodePTR(args)->car);
	if (LISPstatus & LS_ABORT)
		return thr;

	if (LISPmret) {
		ret = LISPmret;
		LISPmret = NULL;
		unlinkPTR(thr);
	} else {
		ret = newnodePTR();
		nodePTR(ret)->car = thr;
		nodePTR(ret)->cdr = linkPTR(NILptr);
	}

	thr = processtasks(nodePTR(args)->cdr, NILptr, NULL);
	if (LISPstatus & LS_ABORT) {
		unlinkPTR(ret);
		return thr;
	}
	unlinkPTR(thr);

	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = ret;
	return (ret->type == LT_NODE) ?
		linkPTR(nodePTR(ret)->car) : linkPTR(NILptr);
}

PTR LISP_progn(PTR args, PTR tail)
{
	return processtasks(args, NILptr, NULL);
}

PTR LISP_throw(PTR args)
{
	PTR sym, ret, ptr;

	GETARGS(("ya", args, &sym, &ret));

	if (sym == NILptr)
		return LISPerrormsg("can't throw nil");

	for (ptr = LISPcatchstack; ptr->type == LT_NODE; ptr = nodePTR(ptr)->cdr)
		if (nodePTR(ptr)->car == sym)
			break;
	if (ptr->type != LT_NODE)
		return LISPerrormsg("uncatched throw");

	unlinkPTR(LISPthrownsymbol);
	LISPthrownsymbol = linkPTR(sym);
	unlinkPTR(LISPthrownvalue);
	LISPthrownvalue = linkPTR(ret);
	LISPstatus |= LS_ABORT;
	return linkPTR(LISPthrownvalue);
}

PTR LISP_catch(PTR args, PTR tail)
{
	PTR catchsym, ret;

	GETARGS(("a-", args, &catchsym, &args));
	catchsym = LISPevalobj(catchsym);
	if (LISPstatus & LS_ABORT)
		return catchsym;
	if (catchsym->type != LT_SYMBOL) {
		unlinkPTR(catchsym);
		return LISPerrormsg(NON_SYMBOL_MSG);
	}
	if (catchsym == NILptr) {
		unlinkPTR(catchsym);
		return LISPerrormsg("can't catch nil");
	}
	LISPstackpush(&LISPcatchstack, catchsym);
	unlinkPTR(catchsym);

	ret = processtasks(args, NILptr, NULL);
	if (LISPstatus & LS_ABORT) {
		if (LISPthrownsymbol == catchsym) {
			unlinkPTR(ret);
			ret = LISPtrap();
		}
	}

	catchsym = LISPstackpop(&LISPcatchstack);
	unlinkPTR(catchsym);
	return ret;
}

PTR LISP_return(PTR args)
{
	PTR ret;

	if (LISPallowreturn <= 0)
		return LISPerrormsg("misplaced return");

	GETARGS(("?a", args, &ret, NILptr));

	unlinkPTR(LISPthrownsymbol);
	LISPthrownsymbol = linkPTR(RETURN_THROWptr);
	unlinkPTR(LISPthrownvalue);
	LISPthrownvalue = linkPTR(ret);
	LISPstatus |= LS_ABORT;
	return linkPTR(LISPthrownvalue);
}

LISPstatic PTR LISPtrap(void)
{
	PTR ret;

	LISPstatus &= ~LS_ABORT;
	unlinkPTR(LISPthrownsymbol);
	LISPthrownsymbol = linkPTR(NILptr);
	ret = LISPthrownvalue;
	LISPthrownvalue = linkPTR(NILptr);
	LISPresetmret();
	return ret;
}

PTR LISP_unwind_protect(PTR args, PTR tail)
{
	PTR ret, thr;
	int abort;
	PTR thsym, thval;

	if (args->type != LT_NODE)
		return linkPTR(NILptr);
	thr = LISPevalobj(nodePTR(args)->car);

	if (LISPmret) {
		ret = LISPmret;
		LISPmret = NULL;
		unlinkPTR(thr);
	} else {
		ret = newnodePTR();
		nodePTR(ret)->car = thr;
		nodePTR(ret)->cdr = linkPTR(NILptr);
	}

	abort = LISPstatus & LS_ABORT;
	LISPstatus &= ~LS_ABORT;
	thsym = LISPthrownsymbol;
	LISPthrownsymbol = linkPTR(NILptr);
	thval = LISPthrownvalue;
	LISPthrownvalue = linkPTR(NILptr);

	thr = processtasks(nodePTR(args)->cdr, NILptr, NULL);
	if (LISPstatus & LS_ABORT) {
		unlinkPTR(thsym);
		unlinkPTR(thval);
		unlinkPTR(ret);
		return thr;
	}
	unlinkPTR(thr);

	LISPstatus |= abort;
	unlinkPTR(LISPthrownsymbol);
	LISPthrownsymbol = thsym;
	unlinkPTR(LISPthrownvalue);
	LISPthrownvalue = thval;

	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = ret;
	return (ret->type == LT_NODE) ?
		linkPTR(nodePTR(ret)->car) : linkPTR(NILptr);
}

PTR LISP_values(PTR args)
{
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = linkPTR(args);
	if (args->type == LT_NODE)
		return linkPTR(nodePTR(args)->car);
	return linkPTR(NILptr);
}

PTR LISP_values_list(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = linkPTR(arg);
	if (arg->type == LT_NODE)
		return linkPTR(nodePTR(arg)->car);
	return linkPTR(NILptr);
}

PTR LISP_multiple_value_list(PTR args, PTR tail)
{
	PTR ret, tmp;

	GETARGS(("a", args, &tmp));

	/*
	LISPputc('>', NULL);
	LISPwrite(NULL, tmp, 0);
	LISPnewline(NULL);
	*/

	tmp = LISPevalobj(tmp);
	if (LISPmret) {
		ret = LISPmret;
		LISPmret = NULL;
		if (ret->type == LT_NODE && nodePTR(ret)->car != tmp) {
			LISPputs(NULL, "Single: ");
			LISPwrite(NULL, tmp, 0);
			LISPputs(NULL, " Multiple: ");
			LISPwrite(NULL, ret, 0);
			LISPnewline(NULL);
			LISPfatal("incoherent multiple return values in LISP_multiple_value_list");
		}
	} else {
		ret = newnodePTR();
		nodePTR(ret)->car = linkPTR(tmp);
		nodePTR(ret)->cdr = linkPTR(NILptr);
	}

	/*
	LISPputc('<', NULL);
	LISPwrite(NULL, tmp, 0);
	LISPwrite(NULL, ret, 0);
	LISPnewline(NULL);
	*/

	unlinkPTR(tmp);
	return ret;
}

void LISPprintmret(PTR f, PTR single)
{
	PTR mret, ptr;

	if (!f)
		f = unlinkLISPoutput();
	if (LISPmret) {
		if (LISPmret->type == LT_NODE && nodePTR(LISPmret)->car != single) {
			LISPputs(NULL, "Single: ");
			LISPwrite(NULL, single, 0);
			LISPputs(NULL, " Multiple: ");
			LISPwrite(NULL, LISPmret, 0);
			LISPnewline(NULL);
			LISPfatal("incoherent multiple return values in LISPprintmret");
		}
		mret = linkPTR(LISPmret);
		for (ptr = mret; ptr->type == LT_NODE; ptr = nodePTR(ptr)->cdr) {
			LISPwrite(f, nodePTR(ptr)->car, 0);
			if (LISPstatus & LS_ABORT)
				break;
			if (nodePTR(ptr)->cdr->type == LT_NODE)
				LISPputc(';', f);
			LISPnewline(f);
		}
		unlinkPTR(mret);
		LISPresetmret();
	} else {
		LISPwrite(f, single, 0);
		LISPnewline(f);
	}
}

PTR LISP_macroexpand(PTR args)
{
	PTR form, head, mret;
	int n = 0, flag;

	GETARGS(("a", args, &form));
	(void) linkPTR(form);

	for (;;) {
		if (form->type != LT_NODE)
			break;
		head = nodePTR(form)->car;
		flag = (head->type == LT_SYMBOL && (symbolPTR(head)->flags & LF_ISMACRO))
			|| (head->type == LT_NODE && findfuntype(head) == FT_MACRO);
		if (!flag)
			break;
		mret = LISPapplyfun(head, nodePTR(form)->cdr, 0);
		unlinkPTR(form);
		form = mret;
		n++;
	}

	mret = LISPmakelist(form, (n > 0) ? Tptr : NILptr, NULL);
	unlinkPTR(form);
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = mret;
	return linkPTR(nodePTR(mret)->car);
}

PTR LISP_macroexpand_1(PTR args)
{
	PTR form, head, mret;
	int flag;

	GETARGS(("a", args, &form));
	if (form->type != LT_NODE) {
		mret = LISPmakelist(form, NILptr, NULL);
	} else {
		head = nodePTR(form)->car;
		flag = (head->type == LT_SYMBOL && (symbolPTR(head)->flags & LF_ISMACRO))
			|| (head->type == LT_NODE && findfuntype(head) == FT_MACRO);
		if (flag) {
			form = LISPapplyfun(head, nodePTR(form)->cdr, 0);
			mret = LISPmakelist(form, Tptr, NULL);
			unlinkPTR(form);
		} else {
			mret = LISPmakelist(form, NILptr, NULL);
		}
	}
	if (LISPmret)
		unlinkPTR(LISPmret);
	LISPmret = mret;
	return linkPTR(nodePTR(mret)->car);
}

PTR LISP_getd(PTR args)
{
	PTR sym;

	GETARGS(("a", args, &sym));
	if (sym->type == LT_CLOSURE)
		return linkPTR(closurePTR(sym)->fun);
	if (sym->type != LT_SYMBOL)
		return LISPerrormsg(NON_SYMBOL_MSG);
	return linkPTR(symbolPTR(sym)->fun);
}

PTR LISP_putd(PTR args)
{
	PTR symbol, fun;
	int type;

	GETARGS(("ya", args, &symbol, &fun));
	type = findfuntype(fun);
	if (nonNIL(fun) && type == FT_BAD)
		return LISPerrormsg(BAD_FUNCTION_MSG);
	unlinkPTR(symbolPTR(symbol)->fun);
	symbolPTR(symbol)->fun = linkPTR(fun);
	symbolPTR(symbol)->flags &= ~(LF_ISSPECIALFORM | LF_ISMACRO);
	if (type == FT_MACRO)
		symbolPTR(symbol)->flags |= LF_ISMACRO;

	return linkPTR(fun);
}

PTR LISP_movd(PTR args)
{
	PTR symbol1, symbol2;
	PTR ret;

	GETARGS(("yy", args, &symbol1, &symbol2));
	symbolPTR(symbol2)->flags = (symbolPTR(symbol2)->flags & ~LF_FUNCTION_FLAGS)
		| (symbolPTR(symbol1)->flags & LF_FUNCTION_FLAGS);
	unlinkPTR(symbolPTR(symbol2)->fun);
	ret = symbolPTR(symbol2)->fun = linkPTR(symbolPTR(symbol1)->fun);
	return linkPTR(ret);
}

PTR LISP_function(PTR args, PTR tail)
{
	PTR fun;

	GETARGS(("a", args, &fun));

	switch (fun->type) {
	case LT_SYMBOL:
		if (!(symbolPTR(fun)->flags & (LF_ISMACRO | LF_ISSPECIALFORM))) {
			/* return linkPTR(symbolPTR(fun)->fun); */
			return linkPTR(fun);
		}
		break;
	case LT_BUILTIN:
		return linkPTR(fun);
	case LT_NODE:
		if (findfuntype(fun) == FT_LAMBDA)
			return newclosurePTR(fun, nodePTR(LISPenvstack)->car);
	default:
		/* nothing */
		break;
	}
	return LISPerrormsg(BAD_FUNCTION_MSG);
}

PTR LISP_specialp(PTR args)
{
	PTR symbol;

	GETARGS(("y", args, &symbol));
	return symbolPTR(symbol)->flags & LF_ISSPECIALFORM ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_macrop(PTR args)
{
	PTR symbol;

	GETARGS(("y", args, &symbol));
	return symbolPTR(symbol)->flags & LF_ISMACRO ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_valid_lambda(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return FT_BAD == findfuntype(arg) ?
		linkPTR(NILptr) : linkPTR(Tptr);
}

PTR LISP_valid_lambda_args(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return validargs(arg) ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISPevaluserfun(PTR fun, PTR args, PTR env, PTR tail)
{
	PTR tmp, ret;
	PTR fargs;

	(void) linkPTR(fun); /* protezione in caso di funzioni automodificanti */
	tmp = nodePTR(fun)->cdr;
	fargs = setlambdafargs(nodePTR(tmp)->car, args, env);
	if (LISPstatus & LS_ABORT) {
		unlinkPTR(fun);
		return fargs;
	}
	
	tmp = nodePTR(tmp)->cdr;
	ret = processtasks(tmp, NILptr, tail);

	clearspecialvars(fargs);
	unlinkPTR(fun);
	return ret;
}

LISPstatic PTR processtasks(PTR tasks, PTR def, PTR tail)
{
	PTR task, ret;

	ret = linkPTR(def);
	while (tasks->type == LT_NODE) {
		task = nodePTR(tasks)->car;
		unlinkPTR(ret);
		ret = LISPevalobj(task);
		if (LISPstatus & LS_ABORT)
			break;
		tasks = nodePTR(tasks)->cdr;
	}
	return ret;
}

LISPstatic enum funtype findfuntype(PTR fun)
{
	enum funtype ret;
	PTR tmp;

	if (fun->type != LT_NODE)
		return FT_BAD;
	tmp = nodePTR(fun)->car;
	if (tmp == LAMBDAptr) {
		ret = FT_LAMBDA;
	} else if (tmp == MACROptr) {
		ret = FT_MACRO;
	} else {
		return FT_BAD;
	}
	tmp = nodePTR(fun)->cdr;
	if (tmp->type != LT_NODE)
		return FT_BAD;
	tmp = nodePTR(tmp)->car;
	switch (tmp->type) {
	case LT_SYMBOL:
		if (tmp != NILptr)
			return FT_BAD;
		break;
	case LT_NODE:
		if (!validargs(tmp))
			return FT_BAD;
		break;
	default:
		return FT_BAD;
	}
	return ret;
}

/********************************************/

typedef struct lambdaenv_tag {
	PTR envhead, envtail;
	PTR specialhead, specialtail;
	PTR abortval;
	PTR closureenv;
} lambdaenv;

LISPstatic void lambdaenvadd(lambdaenv *env, PTR sym, PTR val)
{
	if (symbolPTR(sym)->flags & LF_SPECIAL) {
		LISPpushval(sym, val);
		LISPqueueadd(&(env->specialhead), &(env->specialtail), sym);
	} else {
		/* LISPdebugwrite("~ <- ~\n", sym, val); */
		LISPqueueaddassoc(&(env->envhead), &(env->envtail), sym, val);
	}
}

LISPstatic PTR evaldefault(lambdaenv *lenv, PTR ptr)
{
	PTR ret;
	
	PTR tmp;
	if (lenv->envtail) {
		nodePTR(lenv->envtail)->cdr = lenv->closureenv;
	} else {
		lenv->envhead = lenv->closureenv;
	}
	LISPstackpush(&LISPenvstack, lenv->envhead);
	ret = LISPevalobj(ptr);
	tmp = LISPstackpop(&LISPenvstack);
	unlinkPTR(tmp);
	if (lenv->envtail) {
		nodePTR(lenv->envtail)->cdr = NILptr;
	} else {
		lenv->envhead = NILptr;
	}

	return ret;
}

LISPstatic PTR walkargserror(lambdaenv *lenv, char *msg)
{
	if (lenv)
		return LISPerrormsg(msg);
	return Tptr;
}

int LISPvalidkeyargs(PTR args, PTR formal_key_args)
{
	PTR arg, farg, ptr;
	int found;

	/*
	if (formal_key_args)
		LISPdebugwrite("checking ~ against ~\n", args, formal_key_args);
	*/

	while (args->type == LT_NODE) {
		arg = nodePTR(args)->car;
		if (arg->type != LT_SYMBOL)
			return 0;
		if (!(symbolPTR(arg)->flags & LF_COLON))
			return 0;

		if (formal_key_args) {
			found = 0;
			ptr = formal_key_args;
			while (ptr->type == LT_NODE) {
				farg = nodePTR(ptr)->car;
				if (farg->type == LT_NODE)
					farg = nodePTR(farg)->car;
				if (farg->type != LT_SYMBOL)
					LISPfatal("bad formal args in LISPvalidkeyargs()");
				if (symbolPTR(farg)->flags & LF_ARGKEYWORD)
					break;
				/* LISPdebugwrite("\tchecking ~ against ~\n", farg, arg); */
				if (symbolPTR(farg)->key == arg) {
					found = 1;
					break;
				}
				ptr = nodePTR(ptr)->cdr;
			}
			if (!found)
				return 0;
		}

		args = nodePTR(args)->cdr;
		if (args->type != LT_NODE)
			return 0;
		args = nodePTR(args)->cdr;
	}

	return 1;
}

LISPstatic PTR walkargs(lambdaenv *lenv, PTR fargs, PTR aargs)
{
	PTR var, val, flag, keyvalue, flagvalue;
	PTR keyword = NULL, keyargs = NULL;
	int varflags;

	while (fargs->type == LT_NODE) {
		var = nodePTR(fargs)->car;
		/* LISPdebugwrite("VAR ~\n", var); */
		val = keyword ? NILptr : NULL;
		flag = NULL;
		flagvalue = NILptr;
		fargs = nodePTR(fargs)->cdr;
		if (!keyargs && (keyword == KEYptr || keyword == RESTptr)) {
			keyargs = aargs;
			aargs = NILptr;
		}
		if (keyword && keyword != RESTptr && var->type == LT_NODE) {
			val = nodePTR(var)->cdr;
			var = nodePTR(var)->car;
			if (val->type == LT_NODE) {
				flag = nodePTR(val)->cdr;
				val = nodePTR(val)->car;
				if (flag->type == LT_NODE) {
					flag = nodePTR(flag)->car;
				} else {
					flag = NULL;
				}
			} else {
				val = NILptr;
			}
		}
		if (var->type != LT_SYMBOL)
			return walkargserror(lenv, "illegal parameter");
		varflags = symbolPTR(var)->flags;
		if (varflags & LF_ARGKEYWORD) {
			if (keyword && symbolPTR(keyword)->kord >= symbolPTR(var)->kord)
				return walkargserror(lenv, "misplaced keyword");
			keyword = var;
			if (keyword == KEYptr && !LISPvalidkeyargs(aargs, fargs))
				return walkargserror(lenv, "bad key arguments");
			continue;
		}
		if (varflags & LF_CONST)
			return walkargserror(lenv, "illegal parameter");
		if (flag) {
			if (flag->type != LT_SYMBOL)
				return walkargserror(lenv, "illegal parameter");
			if (symbolPTR(flag)->flags & LF_CONST)
				return walkargserror(lenv, "illegal parameter");
		}
		if (lenv) {
			if (keyword == RESTptr) {
				val = linkPTR(keyargs);
			} else if (keyword == KEYptr) {
				keyvalue = unlinkLISPkeyarg(keyargs, var, NULL);
				if (keyvalue) {
					val = keyvalue;
					flagvalue = Tptr;
					(void) linkPTR(val);
				} else {
					val = evaldefault(lenv, val);
					if (LISPstatus & LS_ABORT)
						return val;
				}
			} else if (keyword != AUXptr && aargs->type == LT_NODE) {
				val = linkPTR(nodePTR(aargs)->car);
				aargs = nodePTR(aargs)->cdr;
				flagvalue = Tptr;
			} else if (val) {
				/* qui devo fare eval(val) */
				val = evaldefault(lenv, val);
				if (LISPstatus & LS_ABORT)
					return val;
			}
			if (!val)
				return walkargserror(lenv, "not enough arguments");
			lambdaenvadd(lenv, var, val);
			if (flag)
				lambdaenvadd(lenv, flag, flagvalue);
			unlinkPTR(val);
		}
		/*
		LISPdebugwrite("VAR ~ <- ~\n", var, val);
		if (flag)
			LISPdebugwrite("FLAG ~ <- ~\n", flag, flagvalue);
		*/
	}
	if (aargs != NILptr)
		return walkargserror(lenv, "too many arguments");

	return NULL;
}

LISPstatic int validargs(PTR ptr)
{
	return walkargs(NULL, ptr, NILptr) == NULL;
}

/********************************************/

void LISPqueueinit(PTR *head, PTR *tail)
{
	*head = linkPTR(NILptr);
	*tail = NULL;
}

void LISPqueueadd(PTR *head, PTR *tail, PTR ptr)
{
	PTR tmp = newnodePTR();
	nodePTR(tmp)->car = linkPTR(ptr);
	nodePTR(tmp)->cdr = NILptr;
	if (*tail) {
		/*
		while ((*tail)->type == LT_NODE)
			tail = &(nodePTR(*tail)->cdr);
		*/
		nodePTR(*tail)->cdr = tmp;
	} else {
		*head = tmp;
	}
	*tail = tmp;
}

void LISPqueueaddassoc(PTR *head, PTR *tail, PTR ptr1, PTR ptr2)
{
	PTR tmp = newnodePTR();
	nodePTR(tmp)->car = linkPTR(ptr1);
	nodePTR(tmp)->cdr = linkPTR(ptr2);
	LISPqueueadd(head, tail, tmp);
	unlinkPTR(tmp);
}

void LISPqueueappend(PTR *head, PTR *tail, PTR ptr)
{
	unlinkPTR(NILptr);
	if (*tail) {
		nodePTR(*tail)->cdr = linkPTR(ptr);
	} else {
		*head = linkPTR(ptr);
	}
}

/********************************************/

LISPstatic PTR setlambdafargs(PTR fargs, PTR actualargs, PTR env)
{
	lambdaenv lenv;
	PTR res;

	LISPqueueinit(&lenv.envhead, &lenv.envtail);
	LISPqueueinit(&lenv.specialhead, &lenv.specialtail);
	lenv.closureenv = env;

	res = walkargs(&lenv, fargs, actualargs);
	if (res) {
		unlinkPTR(lenv.envhead);
		LISPstackpush(&LISPenvstack, NILptr);
		clearspecialvars(lenv.specialhead);
		return res;
	}

	LISPqueueappend(&lenv.envhead, &lenv.envtail, env);
	LISPstackpush(&LISPenvstack, lenv.envhead);
	unlinkPTR(lenv.envhead);
	return lenv.specialhead;
}

LISPstatic void clearspecialvars(PTR fargs)
{
	PTR tmp = fargs;

	if (fargs->type == LT_SYMBOL) {
		LISPpopval(fargs);
	} else {
		while (fargs->type == LT_NODE) {
			LISPpopval(nodePTR(fargs)->car);
			fargs = nodePTR(fargs)->cdr;
		}
	}
	unlinkPTR(tmp);
	tmp = LISPstackpop(&LISPenvstack);
	unlinkPTR(tmp);
}

