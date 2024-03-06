/*

	LISPMATH.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni matematiche.

*/

#include "bdlisp.h"

PTR ZEROptr, ONEptr;

double LISPinf, LISPnan;

static int matherrno;
enum { ME_FPE = 1, ME_RANGE, ME_DOMAIN, ME_TLOSS };

LISPstatic PTR mathfun(double (*fun)(double), PTR args);

PTR LISP_plus(PTR args)
{
	double val = 0.0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		val += numberPTR(nodePTR(args)->car)->val;
		args = nodePTR(args)->cdr;
	}
	return LISPmathcheck(val);
}

PTR LISP_difference(PTR args)
{
	double val = 0.0;
	unsigned n = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		if (n) {
			val -= numberPTR(nodePTR(args)->car)->val;
		} else {
			val = numberPTR(nodePTR(args)->car)->val;
		}
		args = nodePTR(args)->cdr;
		n++;
	}
	if (n == 1)
		val = -val;
	return LISPmathcheck(val);
}

PTR LISP_times(PTR args)
{
	double val = 1.0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		val *= numberPTR(nodePTR(args)->car)->val;
		args = nodePTR(args)->cdr;
	}
	return LISPmathcheck(val);
}

PTR LISP_quotient(PTR args)
{
	double val = 1.0, den;
	unsigned n = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		den = numberPTR(nodePTR(args)->car)->val;
		if (n) {
			if (den)
				val /= den;
			else
				matherrno = ME_FPE;
		} else {
			val = den;
		}
		args = nodePTR(args)->cdr;
		n++;
	}
	if (n == 1) {
		if (val)
			val = 1.0 / val;
		else
			matherrno = ME_FPE;
	}
	return LISPmathcheck(val);
}

PTR LISP_oneplus(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return newnumberPTR(numberPTR(arg)->val + 1);
}

PTR LISP_oneminus(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return newnumberPTR(numberPTR(arg)->val - 1);
}

PTR LISP_oddp(PTR args)
{
	PTR arg;
	double val;

	GETARGS(("n", args, &arg));
	val = fmod(numberPTR(arg)->val, 2.0);
	return fabs(val) == 1.0 ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_evenp(PTR args)
{
	PTR arg;
	double val;

	GETARGS(("n", args, &arg));
	val = fmod(numberPTR(arg)->val, 2.0);
	return fabs(val) == 0.0 ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_zerop(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return numberPTR(arg)->val == 0.0 ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_plusp(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return numberPTR(arg)->val > 0.0 ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_minusp(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return numberPTR(arg)->val < 0.0 ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_rem(PTR args)
{
	double val = 0.0;
	unsigned n = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		if (n) {
			val = fmod(val, numberPTR(nodePTR(args)->car)->val);
		} else {
			val = numberPTR(nodePTR(args)->car)->val;
		}
		args = nodePTR(args)->cdr;
		n++;
	}
	return LISPmathcheck(val);
}

PTR LISP_abs(PTR args)
{
	return mathfun(fabs, args);
}

PTR LISP_ceiling(PTR args)
{
	return mathfun(ceil, args);
}

PTR LISP_floor(PTR args)
{
	return mathfun(floor, args);
}

PTR LISP_sqrt(PTR args)
{
	return mathfun(sqrt, args);
}

PTR LISP_exp(PTR args)
{
	return mathfun(exp, args);
}

PTR LISP_log(PTR args)
{
	return mathfun(log, args);
}

PTR LISP_expt(PTR args)
{
	double val = 1.0;
	unsigned n = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		if (n) {
			val = pow(val, numberPTR(nodePTR(args)->car)->val);
		} else {
			val = numberPTR(nodePTR(args)->car)->val;
		}
		args = nodePTR(args)->cdr;
		n++;
	}
	return LISPmathcheck(val);
}

PTR LISP_sin(PTR args)
{
	return mathfun(sin, args);
}

PTR LISP_cos(PTR args)
{
	return mathfun(cos, args);
}

PTR LISP_tan(PTR args)
{
	return mathfun(tan, args);
}

PTR LISP_asin(PTR args)
{
	return mathfun(asin, args);
}

PTR LISP_acos(PTR args)
{
	return mathfun(acos, args);
}

PTR LISP_atan(PTR args)
{
	return mathfun(atan, args);
}

PTR LISP_atan2(PTR args)
{
	PTR y, x;
	double val;

	GETARGS(("nn", args, &y, &x));
	LISPmathtry();
	val = atan2(numberPTR(y)->val, numberPTR(x)->val);
	return LISPmathcheck(val);
}

PTR LISP_gcd(PTR args)
{
	double val = 1.0, arg, tmp;
	unsigned n = 0;

	LISPmathtry();
	while (!matherrno && args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		arg = numberPTR(nodePTR(args)->car)->val;
		if (n > 0) {
			if (arg > val) {
				tmp = arg;
				arg = val;
				val = tmp;
			}
			/* printf("%f %f\n", val, arg); */
			while (arg != 0) {
				tmp = fmod(val, arg);
				val = arg;
				arg = tmp;
				/* printf("%f %f\n", val, arg); */
			}
		} else {
			val = arg;
		}
		args = nodePTR(args)->cdr;
		n++;
	}
	return LISPmathcheck(val);
}

PTR LISP_logand(PTR args)
{
	int val = ~0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		val &= (int) numberPTR(nodePTR(args)->car)->val;
		args = nodePTR(args)->cdr;
	}
	return LISPmathcheck(val);
}

PTR LISP_logior(PTR args)
{
	int val = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		val |= (int) numberPTR(nodePTR(args)->car)->val;
		args = nodePTR(args)->cdr;
	}
	return LISPmathcheck(val);
}

PTR LISP_logxor(PTR args)
{
	int val = 0;

	LISPmathtry();
	while (args->type == LT_NODE) {
		if (nodePTR(args)->car->type != LT_NUMBER)
			return LISPerrormsg(NON_NUMERIC_MSG);
		val ^= (int) numberPTR(nodePTR(args)->car)->val;
		args = nodePTR(args)->cdr;
	}
	return LISPmathcheck(val);
}

PTR LISP_lognot(PTR args)
{
	PTR arg;

	GETARGS(("n", args, &arg));
	return newnumberPTR(~ (int) numberPTR(arg)->val);
}

PTR LISP_random(PTR args)
{
	PTR arg;
	double ret;

	GETARGS(("?n", args, &arg, NULL));
	
	ret = rand() / (RAND_MAX + 1.0);
	if (arg)
		ret = floor(numberPTR(arg)->val * ret);
	return newnumberPTR(ret);
}

PTR LISP_randomize(PTR args)
{
	PTR arg;
	int seed;

	GETARGS(("?n", args, &arg, ZEROptr));

	if (arg == ZEROptr) {
		seed = (int) time(NULL);
	} else {
		seed = (int) numberPTR(arg)->val;
	}

	srand(seed);
	return newnumberPTR(seed);
}

LISPstatic PTR mathfun(double (*fun)(double), PTR args)
{
	PTR arg;
	double val;

	GETARGS(("n", args, &arg));
	LISPmathtry();
	val = fun(numberPTR(arg)->val);
	return LISPmathcheck(val);
}

void (*LISPoldfpecatch)(int sig);

void LISPfpecatch(int sig)
{
	(void) sig;
#ifdef FPRESET
	FPRESET();
#endif
	signal(SIGFPE, LISPfpecatch);
	matherrno = ME_FPE;
	return;
}

void LISPmathtry(void)
{
	matherrno = errno = 0;
}

PTR LISPmathcheck(double val)
{
	PTR ret = NULL;
	char *msg;

	if (errno)
		matherrno = ME_FPE;
	if (matherrno) {
		switch (matherrno) {
		case ME_FPE:
			switch (errno) {
			case EDOM:
				msg = "domain";
				break;
			case ERANGE:
				msg = "range";
				break;
			default:
				msg = "floating point exception";
				break;
			}
			break;
		case ME_RANGE:
			msg = "range";
			break;
		case ME_DOMAIN:
			msg = "domain";
			break;
		case ME_TLOSS:
			msg = "total loss of precision";
			break;
		default:
			msg = "unknown math library error";
			break;
		}
		ret = LISPerrormsg(msg);
	} else {
		ret = newnumberPTR(val);
	}
	return ret;
}

