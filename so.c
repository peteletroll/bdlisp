#include "bdlisp.h"

PTR LISP_dyn_func(PTR args)
{
	return linkPTR(Tptr);
}

static LISP_INIT_TAB(init_tab) {
	LISP_FUNCT("dyn-func", LISP_dyn_func, 0),
	LISP_TAB_LAST()
};

void LISPinitlibrary(void)
{
	LISPprocessinittab(init_tab);
	LISPaddfeature(":dyn-func");
}

