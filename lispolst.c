/*

	LISPOLST.C - 1997 - Bigdogs Internescional.

	Interprete LISP - tabella dei simboli.

*/

#include "bdlisp.h"

enum LISPstatus LISPstatus;

struct bucket {
	PTR ptr;
	unsigned n;
};

static struct bucket *oblist;
unsigned LISPhash_module;
long LISPnintern;

LISPstatic void putsym(PTR sym, struct bucket *lst);
LISPstatic void remsym(PTR sym, struct bucket *lst);

LISPstatic void rehash(int n);

PTR Tptr, EOFptr;

PTR PLUSptr, MINUSptr, STARptr;

PTR ENVIRONMENTptr, CORE_VERSIONptr;
PTR ARGSptr;
PTR FEATURESptr;

PTR DEBUG_DRIVERptr;

PTR LISPstaticvars;

struct LISPcell LISPnil;
static struct LISPsymbol_ext nil_sym;

static double pi, epsilon, zero, one;
static PTR LISPpi, LISPepsilon;

static PTR LISPdirsep_cst, LISPpathsep_cst;

static LISP_INIT_TAB(main_init_tab) {
	LISP_STATIC(LISPenvstack),

	LISP_STATIC(LISPexitvalue),
	LISP_STATIC(LISPcatchstack),
	LISP_STATIC(LISPthrownsymbol),
	LISP_STATIC(LISPthrownvalue),

	LISP_STATIC(LISPstdin),
	LISP_STATIC(LISPstdout),
	LISP_STATIC(LISPstderr),
	LISP_STATIC(LISPconsole),

	LISP_STATIC(LISPreadqueue),

	LISP_UNIQ(DOTptr),
	LISP_UNIQ(BEGIN_LISTptr),
	LISP_UNIQ(END_LISTptr),
	LISP_UNIQ(READ_EOFptr),
	LISP_UNIQ(READ_ABORTptr),
	LISP_UNIQ(RETURN_THROWptr),

	LISP_SYMBOL("t", Tptr, LF_CONST),

	LISP_SYMBOL("&optional", OPTIONALptr, LF_CONST | LF_ARGKEYWORD),
	LISP_SYMBOL("&rest", RESTptr, LF_CONST | LF_ARGKEYWORD),
	LISP_SYMBOL("&key", KEYptr, LF_CONST | LF_ARGKEYWORD),
	LISP_SYMBOL("&aux", AUXptr, LF_CONST | LF_ARGKEYWORD),

	LISP_SYMBOL("*standard-input*", STANDARD_INPUTptr, LF_SPECIAL),
	LISP_SYMBOL("*standard-output*", STANDARD_OUTPUTptr, LF_SPECIAL),
	LISP_SYMBOL("*standard-error*", STANDARD_ERRORptr, LF_SPECIAL),
	LISP_SYMBOL("*terminal-io*", TERMINAL_IOptr, LF_SPECIAL),
	LISP_SYMBOL("*eof*", EOFptr, LF_CONST),

	LISP_SYMBOL("verbose", VERBOSEptr, 0),
	LISP_SYMBOL("print", PRINTptr, 0),

	LISP_SYMBOL("*load-source*", LOAD_SOURCEptr, LF_SPECIAL | LF_SETNIL),
	LISP_SYMBOL("*displace-macros*", DISPLACE_MACROSptr, LF_SPECIAL | LF_SETNIL),
	LISP_SYMBOL("*evalhook*", EVALHOOKptr, LF_SPECIAL | LF_BINDCALLBACK | LF_SETNIL),
	LISP_SYMBOL("*applyhook*", APPLYHOOKptr, LF_SPECIAL | LF_BINDCALLBACK | LF_SETNIL),
	LISP_SYMBOL("*debughook*", DEBUGHOOKptr, LF_SPECIAL | LF_SETNIL),
	LISP_SYMBOL("debug-driver", DEBUG_DRIVERptr, 0),

	LISP_SYMBOL("+", PLUSptr, LF_CONST | LF_SETNIL),
	LISP_SYMBOL("-", MINUSptr, LF_CONST | LF_SETNIL),
	LISP_SYMBOL("*", STARptr, LF_CONST | LF_SETNIL),

	LISP_SYMBOL("cons", CONSptr, 0),
	LISP_SYMBOL("number", NUMBERptr, 0),
	LISP_SYMBOL("symbol", SYMBOLptr, 0),
	LISP_SYMBOL("string", STRINGptr, 0),
	LISP_SYMBOL("stream", STREAMptr, 0),
	LISP_SYMBOL("builtin", BUILTINptr, 0),
	LISP_SYMBOL("special-form", SPECIAL_FORMptr, 0),
	LISP_SYMBOL("closure", CLOSUREptr, 0),

	LISP_SYMBOL("lambda", LAMBDAptr, 0),
	LISP_SYMBOL("macro", MACROptr, 0),
	LISP_SYMBOL("quote", QUOTEptr, 0),
	LISP_SYMBOL("backquote", BACKQUOTEptr, 0),
	LISP_SYMBOL("comma", COMMAptr, 0),
	LISP_SYMBOL("comma-at", COMMA_ATptr, 0),
	LISP_SYMBOL("return", RETURNptr, LF_MULRET),
	LISP_SYMBOL("abort", ABORTptr, 0),
	LISP_SYMBOL("break", BREAKptr, LF_SETNIL),
	LISP_SYMBOL("continue", CONTINUEptr, 0),
	LISP_SYMBOL("error-exit", ERROR_EXITptr, 0),
	LISP_SYMBOL("*environment*", ENVIRONMENTptr, LF_CONST),
	LISP_SYMBOL("*core-version*", CORE_VERSIONptr, LF_CONST),
	LISP_SYMBOL("*args*", ARGSptr, LF_SPECIAL),
	LISP_SYMBOL("*path*", PATHptr, LF_SPECIAL | LF_SETNIL),
	LISP_SYMBOL("*features*", FEATURESptr, LF_SPECIAL | LF_SETNIL),
	LISP_SYMBOL("<", SORTPREDptr, 0),
	LISP_SYMBOL("test", TESTptr, 0),
	LISP_SYMBOL("eql", EQLptr, 0),

	LISP_STRING(LISPdirsep_cst, DIR_SEP),
	LISP_STRING(LISPpathsep_cst, PATH_SEP),

	LISP_CONST("*directory-separator*", LISPdirsep_cst),
	LISP_CONST("*path-separator*", LISPpathsep_cst),

	LISP_NUMBER(LISPpi, pi),
	LISP_NUMBER(LISPepsilon, epsilon),
	LISP_NUMBER(ZEROptr, zero),
	LISP_NUMBER(ONEptr, one),

	LISP_CONST("pi", LISPpi),
	LISP_CONST("epsilon", LISPepsilon),

	LISP_FUNCT("driver", LISP_driver, 0),
	LISP_FUNCT("debug-driver", LISP_debug_driver, 0),

	LISP_FUNCT("car", LISP_car, 0),
	LISP_FUNCT("cdr", LISP_cdr, 0),
	LISP_FUNCT("caar", LISP_caar, 0),
	LISP_FUNCT("cadr", LISP_cadr, 0),
	LISP_FUNCT("cdar", LISP_cdar, 0),
	LISP_FUNCT("cddr", LISP_cddr, 0),
	LISP_FUNCT("caaar", LISP_caaar, 0),
	LISP_FUNCT("caadr", LISP_caadr, 0),
	LISP_FUNCT("cadar", LISP_cadar, 0),
	LISP_FUNCT("caddr", LISP_caddr, 0),
	LISP_FUNCT("cdaar", LISP_cdaar, 0),
	LISP_FUNCT("cdadr", LISP_cdadr, 0),
	LISP_FUNCT("cddar", LISP_cddar, 0),
	LISP_FUNCT("cdddr", LISP_cdddr, 0),

	LISP_SPECIAL("quote", LISP_quote, 0),
	LISP_SPECIAL("backquote", LISP_backquote, 0),

	LISP_FUNCT("cons", LISP_cons, 0),
	LISP_FUNCT("atom", LISP_atom, 0),
	LISP_FUNCT("eq", LISP_eq, 0),
	LISP_FUNCT("neq", LISP_neq, 0),
	LISP_FUNCT("eql", LISP_eql, 0),
	LISP_FUNCT("neql", LISP_neql, 0),
	LISP_FUNCT("equal", LISP_equal, 0),

	LISP_FUNCT("list", LISP_list, 0),
	LISP_FUNCT("list*", LISP_list_star, 0),
	LISP_FUNCT("append", LISP_append, 0),
	LISP_FUNCT("reverse", LISP_reverse, 0),
	LISP_FUNCT("length", LISP_length, 0),
	LISP_FUNCT("elt", LISP_elt, 0),
	LISP_FUNCT("nth", LISP_nth, 0),
	LISP_FUNCT("nthcdr", LISP_nthcdr, 0),
	LISP_FUNCT("last", LISP_last, 0),
	LISP_FUNCT("assoc", LISP_assoc, 0),
	LISP_FUNCT("adjoin", LISP_adjoin, 0),

	LISP_FUNCT("member", LISP_member, 0),
	LISP_FUNCT("member-if", LISP_member_if, 0),
	LISP_FUNCT("member-if-not", LISP_member_if_not, 0),
	LISP_FUNCT("position", LISP_position, 0),
	LISP_FUNCT("position-if", LISP_position_if, 0),
	LISP_FUNCT("position-if-not", LISP_position_if_not, 0),
	LISP_FUNCT("count", LISP_count, 0),
	LISP_FUNCT("count-if", LISP_count_if, 0),
	LISP_FUNCT("count-if-not", LISP_count_if_not, 0),
	LISP_FUNCT("remove", LISP_remove, 0),
	LISP_FUNCT("remove-if", LISP_remove_if, 0),
	LISP_FUNCT("remove-if-not", LISP_remove_if_not, 0),
	LISP_FUNCT("delete", LISP_delete, 0),
	LISP_FUNCT("delete-if", LISP_delete_if, 0),
	LISP_FUNCT("delete-if-not", LISP_delete_if_not, 0),
	LISP_FUNCT("find", LISP_find, 0),
	LISP_FUNCT("find-if", LISP_find_if, 0),
	LISP_FUNCT("find-if-not", LISP_find_if_not, 0),

	LISP_FUNCT("gensym", LISP_gensym, 0),
	LISP_FUNCT("gentemp", LISP_gentemp, 0),
	LISP_FUNCT("make-symbol", LISP_make_symbol, 0),
	LISP_FUNCT("symbol-name", LISP_symbol_name, 0),
	LISP_FUNCT("symbol-value", LISP_symbol_value, 0),

	LISP_SYMBOL("object-print-function", OBJECT_PRINT_FUNCTIONptr, 0),
	LISP_FUNCT("make-object", LISP_make_object, 0),
	LISP_FUNCT("objectp", LISP_objectp, 0),
	LISP_FUNCT("object-type", LISP_object_type, 0),

	LISP_FUNCT("get", LISP_get, 0),
	LISP_FUNCT("put", LISP_put, 0),
	LISP_FUNCT("remprop", LISP_remprop, 0),
	LISP_FUNCT("getprops", LISP_getprops, 0),
	LISP_FUNCT("getplist", LISP_getplist, 0),

	LISP_FUNCT("set", LISP_set, 0),
	LISP_SPECIAL("setq", LISP_setq, 0),
	LISP_SPECIAL("psetq", LISP_psetq, 0),
	LISP_FUNCT("makunbound", LISP_makunbound, 0),

	LISP_FUNCT("oblist", LISP_oblist, 0),
	LISP_FUNCT("intern", LISP_intern, 0),
	LISP_FUNCT("unintern", LISP_unintern, 0),

	LISP_FUNCT("ascii", LISP_ascii, 0),

	LISP_FUNCT("copy-list", LISP_copy_list, 0),
	LISP_FUNCT("copy-tree", LISP_copy_tree, 0),
	LISP_FUNCT("rplaca", LISP_rplaca, 0),
	LISP_FUNCT("rplacd", LISP_rplacd, 0),
	LISP_FUNCT("nconc", LISP_nconc, 0),

	LISP_FUNCT("sort", LISP_sort, 0),
	LISP_FUNCT("uniq", LISP_uniq, 0),

	LISP_FUNCT("string-hash", LISP_string_hash, 0),
	LISP_FUNCT("concat", LISP_concat, 0),
	LISP_FUNCT("substr", LISP_substr, 0),
	LISP_FUNCT("findstr", LISP_findstr, 0),
	LISP_FUNCT("dupstr", LISP_dupstr, 0),
	LISP_FUNCT("tokstr", LISP_tokstr, 0),

	LISP_FUNCT("cgi-encode", LISP_cgi_encode, 0),
	LISP_FUNCT("cgi-decode", LISP_cgi_decode, 0),
	LISP_FUNCT("html-escape", LISP_html_escape, 0),

	LISP_FUNCT("string-upcase", LISP_string_upcase, 0),
	LISP_FUNCT("string-downcase", LISP_string_downcase, 0),
	LISP_FUNCT("string-capitalize", LISP_string_capitalize, 0),

	LISP_FUNCT("string-scramble", LISP_string_scramble, 0),

	LISP_FUNCT("apply", LISP_apply, LF_MULRET),
	LISP_FUNCT("funcall", LISP_funcall, LF_MULRET),
	LISP_FUNCT("mapl", LISP_mapl, 0),
	LISP_FUNCT("maplist", LISP_maplist, 0),
	LISP_FUNCT("mapcon", LISP_mapcon, 0),
	LISP_FUNCT("mapc", LISP_mapc, 0),
	LISP_FUNCT("mapcar", LISP_mapcar, 0),
	LISP_FUNCT("mapcan", LISP_mapcan, 0),
	LISP_FUNCT("reduce", LISP_reduce, 0),

	LISP_FUNCT("read", LISP_read, 0),
	LISP_FUNCT("read-char", LISP_read_char, 0),
	LISP_FUNCT("peek-char", LISP_peek_char, 0),
	LISP_FUNCT("read-line", LISP_read_line, 0),
	LISP_FUNCT("unread-char", LISP_unread_char, 0),
	LISP_FUNCT("set-macro-character", LISP_set_macro_character, 0),
	LISP_FUNCT("get-macro-character", LISP_get_macro_character, 0),

	LISP_FUNCT("eval", LISP_eval, LF_MULRET),

	LISP_FUNCT("evalhook", LISP_evalhook, LF_MULRET),
	LISP_FUNCT("applyhook", LISP_applyhook, LF_MULRET),

	LISP_FUNCT("the-environment", LISP_the_environment, 0),

	LISP_FUNCT("print", LISP_print, 0),
	LISP_FUNCT("prin1", LISP_prin1, 0),
	LISP_FUNCT("princ", LISP_princ, 0),

	LISP_FUNCT("format", LISP_format, 0),
	LISP_FUNCT("error", LISP_error, 0),
	LISP_FUNCT("warn", LISP_warn, 0),

	LISP_FUNCT("type-of", LISP_type_of, 0),
	LISP_FUNCT("types", LISP_types, 0),
	LISP_FUNCT("symbolp", LISP_symbolp, 0),
	LISP_FUNCT("closurep", LISP_closurep, 0),
	LISP_FUNCT("stringp", LISP_stringp, 0),
	LISP_FUNCT("listp", LISP_listp, 0),
	LISP_FUNCT("consp", LISP_consp, 0),
	LISP_FUNCT("endp", LISP_endp, 0),
	LISP_FUNCT("numberp", LISP_numberp, 0),
	LISP_FUNCT("streamp", LISP_streamp, 0),
	LISP_FUNCT("builtinp", LISP_builtinp, 0),
	LISP_FUNCT("specialformp", LISP_specialformp, 0),
	LISP_FUNCT("constantp", LISP_constantp, 0),
	LISP_FUNCT("keywordp", LISP_keywordp, 0),
	LISP_FUNCT("<", LISP_less, 0),
	LISP_FUNCT("<=", LISP_lesseq, 0),
	LISP_FUNCT(">", LISP_greater, 0),
	LISP_FUNCT(">=", LISP_greatereq, 0),
	LISP_FUNCT("=", LISP_equalsign, 0),
	LISP_FUNCT("/=", LISP_slashequal, 0),
	LISP_FUNCT("max", LISP_max, 0),
	LISP_FUNCT("min", LISP_min, 0),
	LISP_FUNCT("not", LISP_null, 0),
	LISP_FUNCT("null", LISP_null, 0),
	LISP_FUNCT("identity", LISP_identity, 0),
	LISP_FUNCT("nonnull", LISP_identity, 0),
	LISP_SPECIAL("and", LISP_and, LF_MULRET),
	LISP_SPECIAL("or", LISP_or, LF_MULRET),

	LISP_FUNCT("+", LISP_plus, 0),
	LISP_FUNCT("-", LISP_difference, 0),
	LISP_FUNCT("*", LISP_times, 0),
	LISP_FUNCT("/", LISP_quotient, 0),
	LISP_FUNCT("1+", LISP_oneplus, 0),
	LISP_FUNCT("1-", LISP_oneminus, 0),
	LISP_FUNCT("rem", LISP_rem, 0),
	LISP_FUNCT("oddp", LISP_oddp, 0),
	LISP_FUNCT("evenp", LISP_evenp, 0),
	LISP_FUNCT("zerop", LISP_zerop, 0),
	LISP_FUNCT("minusp", LISP_minusp, 0),
	LISP_FUNCT("plusp", LISP_plusp, 0),
	LISP_FUNCT("abs", LISP_abs, 0),
	LISP_FUNCT("floor", LISP_floor, 0),
	LISP_FUNCT("ceiling", LISP_ceiling, 0),
	LISP_FUNCT("sqrt", LISP_sqrt, 0),
	LISP_FUNCT("exp", LISP_exp, 0),
	LISP_FUNCT("log", LISP_log, 0),
	LISP_FUNCT("expt", LISP_expt, 0),
	LISP_FUNCT("sin", LISP_sin, 0),
	LISP_FUNCT("cos", LISP_cos, 0),
	LISP_FUNCT("tan", LISP_tan, 0),
	LISP_FUNCT("asin", LISP_asin, 0),
	LISP_FUNCT("acos", LISP_acos, 0),
	LISP_FUNCT("atan", LISP_atan, 0),
	LISP_FUNCT("atan2", LISP_atan2, 0),
	LISP_FUNCT("gcd", LISP_gcd, 0),
	LISP_FUNCT("logand", LISP_logand, 0),
	LISP_FUNCT("logior", LISP_logior, 0),
	LISP_FUNCT("logxor", LISP_logxor, 0),
	LISP_FUNCT("lognot", LISP_lognot, 0),
	LISP_FUNCT("random", LISP_random, 0),
	LISP_FUNCT("randomize", LISP_randomize, 0),

	LISP_FUNCT("getd", LISP_getd, 0),
	LISP_FUNCT("putd", LISP_putd, 0),
	LISP_FUNCT("movd", LISP_movd, 0),
	LISP_SPECIAL("function", LISP_function, 0),
	LISP_FUNCT("valid-lambda", LISP_valid_lambda, 0),
	LISP_FUNCT("valid-lambda-args", LISP_valid_lambda_args, 0),
	LISP_FUNCT("specialp", LISP_specialp, 0),
	LISP_FUNCT("macrop", LISP_macrop, 0),
	LISP_SPECIAL("cond", LISP_cond, LF_MULRET),
	LISP_SPECIAL("do-loop", LISP_do_loop, LF_MULRET),
	LISP_SPECIAL("prog1", LISP_prog1, 0),
	LISP_SPECIAL("multiple-value-prog1", LISP_multiple_value_prog1, LF_MULRET),
	LISP_SPECIAL("progn", LISP_progn, LF_MULRET),
	LISP_SPECIAL("catch", LISP_catch, LF_MULRET),
	LISP_FUNCT("throw", LISP_throw, LF_MULRET),
	LISP_FUNCT("return", LISP_return, 0),
	LISP_SPECIAL("unwind-protect", LISP_unwind_protect, LF_MULRET),
	LISP_FUNCT("values", LISP_values, LF_MULRET),
	LISP_FUNCT("values-list", LISP_values_list, LF_MULRET),
	LISP_SPECIAL("multiple-value-list", LISP_multiple_value_list, 0),
	LISP_FUNCT("macroexpand", LISP_macroexpand, LF_MULRET),
	LISP_FUNCT("macroexpand-1", LISP_macroexpand_1, LF_MULRET),

	LISP_FUNCT("boundp", LISP_boundp, 0),
	LISP_FUNCT("getvals", LISP_getvals, 0),

	LISP_FUNCT("stream-row", LISP_stream_row, 0),
	LISP_FUNCT("stream-column", LISP_stream_column, 0),
	LISP_FUNCT("fopen", LISP_fopen, 0),
#ifdef POPEN
	LISP_FUNCT("popen", LISP_popen, 0),
#endif
	LISP_FUNCT("fread", LISP_fread, 0),
	LISP_FUNCT("fwrite", LISP_fwrite, 0),
	LISP_FUNCT("make-string-input-stream", LISP_make_string_input_stream, 0),
	LISP_FUNCT("make-string-output-stream", LISP_make_string_output_stream, 0),
	LISP_FUNCT("get-output-stream-string", LISP_get_output_stream_string, 0),
	LISP_FUNCT("close", LISP_close, 0),
	LISP_FUNCT("fflush", LISP_fflush, 0),
	LISP_FUNCT("ftell", LISP_ftell, 0),
	LISP_FUNCT("fseek", LISP_fseek, 0),
	LISP_FUNCT("feof", LISP_feof, 0),
	LISP_FUNCT("isatty", LISP_isatty, 0),
	LISP_FUNCT("load", LISP_load, 0),
	LISP_FUNCT("shell", LISP_shell, 0),
	LISP_FUNCT("getenv", LISP_getenv, 0),
	LISP_FUNCT("path-find", LISP_path_find, 0),
	LISP_FUNCT("getcwd", LISP_getcwd, 0),
	LISP_FUNCT("chdir", LISP_chdir, 0),
	LISP_FUNCT("mkdir", LISP_mkdir, 0),
	LISP_FUNCT("unlink", LISP_unlink, 0),

#ifdef DYNLINK
	LISP_FUNCT("load-dynamic-library", LISP_load_dynamic_library, 0),
#endif

	LISP_FUNCT("cls", LISP_cls, 0),
	LISP_FUNCT("cursor", LISP_cursor, 0),

	LISP_FUNCT("spaces", LISP_spaces, 0),
	LISP_FUNCT("terpri", LISP_terpri, 0),
	LISP_FUNCT("room", LISP_room, LF_MULRET),
	LISP_FUNCT("clock", LISP_clock, 0),
	LISP_FUNCT("get-universal-time", LISP_get_universal_time, 0),
	LISP_FUNCT("get-decoded-time", LISP_get_decoded_time, 0),
	LISP_FUNCT("break", LISP_break, 0),
	LISP_FUNCT("abort", LISP_abort, 0),
	LISP_FUNCT("exit", LISP_exit, 0),
	LISP_FUNCT("error-exit", LISP_error_exit, 0),

	LISP_TAB_LAST()
};

LISPstatic void nil_setup(void);

LISPstatic void setval(PTR sym, PTR val);

void LISPinit(void)
{
	PTR tmp;

	LISPapplyhookflag = LISPevalhookflag 
		= LISPallowreturn = 0;

	LISPstackbase = (void *) &tmp;
	
	LISPstatus = 0;
	LISPncells = LISPnsymbols = LISPnstreams = 0;

	LISPinitstrings();

	LISPhash_module = 65;
	LISPnintern = 0;
	oblist = ecalloc(LISPhash_module, sizeof(*oblist));

	nil_setup();

	LISPstaticvars = linkPTR(NILptr);

	pi = M_PI;
	epsilon = DBL_EPSILON;
	zero = 0;
	one = 1;

#ifdef DJGPP_ENV
	_go32_want_ctrl_break(1);
#endif

	LISPprocessinittab(main_init_tab);

	LISPinitcmacros();

	LISPonterminal = isatty(fileno(stdin))
			&& isatty(fileno(stdout))
			&& isatty(fileno(stderr));

	unlinkPTR(LISPstdin);
	LISPstdin = newfilestreamPTR(stdin);
	unlinkPTR(LISPstdout);
	LISPstdout = newfilestreamPTR(stdout);
	unlinkPTR(LISPstderr);
	LISPstderr = newfilestreamPTR(stderr);
	if (LISPonterminal) {
		unlinkPTR(LISPconsole);
		LISPconsole = newconsolestreamPTR();
	}

	setval(STANDARD_INPUTptr, LISPonterminal ? LISPconsole : LISPstdin);
	setval(STANDARD_OUTPUTptr, LISPonterminal ? LISPconsole : LISPstdout);
	setval(STANDARD_ERRORptr, LISPonterminal ? LISPconsole : LISPstderr);
	setval(TERMINAL_IOptr, LISPconsole);

	unlinkPTR(nodePTR(READ_EOFptr)->car);
	nodePTR(READ_EOFptr)->car = linkPTR(EOFptr);
	setval(EOFptr, READ_EOFptr);

	setval(DEBUGHOOKptr, ERROR_EXITptr);

	LISPaddfeature(":bdlisp");
#ifdef LISPDEBUG
	LISPaddfeature(":rtc");
#endif
#ifdef POPEN
	LISPaddfeature(":popen");
#endif
#ifdef CONIO
	LISPaddfeature(":conio");
#else
	LISPaddfeature(":ansi");
#endif
#ifdef READLINE
	LISPaddfeature(":readline");
#endif
#ifdef DYNLINK
	LISPaddfeature(":dynlink");
#endif

	symbolPTR(OPTIONALptr)->kord = 1;
	symbolPTR(KEYptr)->kord = 2;
	symbolPTR(RESTptr)->kord = 3;
	symbolPTR(AUXptr)->kord = 4;
	LISPstackpush(&LISPenvstack, NILptr);

	LISPinitvectors();

#ifdef REGEX
	LISPinitregexp();
#endif

	tmp = newstringPTR(-1, LISP_ENVIRONMENT);
	setval(ENVIRONMENTptr, tmp);
	unlinkPTR(tmp);

	tmp = newstringPTR(-1, LISP_VERSION);
	setval(CORE_VERSIONptr, tmp);
	unlinkPTR(tmp);

	LISPmret = NULL;

	LISPoldfpecatch = signal(SIGFPE, LISPfpecatch);
	if (LISPoldfpecatch == SIG_ERR)
		fprintf(stderr, "bdlisp can't trap SIGFPU\n");
	LISPoldintcatch = signal(SIGINT, LISPintcatch);
}

void LISPprocessinittab(struct LISPinittabelem *tab)
{
	PTR sym, ptr;

	for (;;) {
		switch (tab->type) {
		case LI_SYM:
			sym = LISPlookup_blk(-1, tab->name);
			LISPaddstaticvar(tab->ptr, sym);
			symbolPTR(sym)->flags |= LF_DONTFREE | tab->flags;
			if (tab->flags & LF_SETNIL) {
				LISPpopval(sym);
				LISPpushval(sym, NILptr);
			}
			unlinkPTR(sym);
			break;
		case LI_STR:
			ptr = newstringPTR(-1, tab->name);
			LISPaddstaticvar(tab->ptr, ptr);
			unlinkPTR(ptr);
			break;
		case LI_NUM:
			ptr = newnumberPTR(*(tab->num));
			LISPaddstaticvar(tab->ptr, ptr);
			unlinkPTR(ptr);
			break;
		case LI_STA:
			LISPaddstaticvar(tab->ptr, NILptr);
			break;
		case LI_UNQ:
			ptr = newnodePTR();
			nodePTR(ptr)->car = linkPTR(NILptr);
			nodePTR(ptr)->cdr = linkPTR(NILptr);
			LISPaddstaticvar(tab->ptr, ptr);
			unlinkPTR(ptr);
			break;
		case LI_CST:
			sym = LISPlookup_blk(-1, tab->name);
			symbolPTR(sym)->flags |= LF_DONTFREE | LF_CONST;
			LISPpopval(sym);
			LISPpushval(sym, *(tab->ptr));
			unlinkPTR(sym);
			break;
		case LI_FUN:
			LISPaddbuiltinfun(tab->name, tab->fun, tab->flags);
			break;
		case LI_SPC:
			LISPaddspecialform(tab->name, tab->spform, tab->flags);
			break;
		case LI_END:
			return;
		default:
			LISPfatal("LISPprocessinittab: unknown type %d", tab->type);
		}
		tab++;
	}
}

void LISPstoreargv(char **argv)
{
	PTR head, tail, tmp;

	LISPqueueinit(&head, &tail);
	while (argv && *argv) {
		tmp = newstringPTR(-1, *argv);
		LISPqueueadd(&head, &tail, tmp);
		unlinkPTR(tmp);
		argv++;
	}
	LISPpopval(ARGSptr);
	LISPpushval(ARGSptr, head);
	unlinkPTR(head);
}

void LISPaddfeature(char *name)
{
	PTR feat, *lst;

	feat = newnodePTR();
	nodePTR(feat)->cdr = linkPTR(NILptr);
	nodePTR(feat)->car = LISPlookup_blk(-1, name);
	lst = &(nodePTR(symbolPTR(FEATURESptr)->bind)->car);
	while ((*lst)->type == LT_NODE) {
		if (nodePTR(*lst)->car == nodePTR(feat)->cdr)
			return;
		lst = &(nodePTR(*lst)->cdr);
	}
	unlinkPTR(*lst);
	*lst = feat;
}

void LISPaddbuiltinfun(char *name, LISPbuiltinfun fun, int flags)
{
	PTR ptr;

	ptr = LISPlookup_blk(-1, name);
	symbolPTR(ptr)->flags |= flags | LF_DONTFREE;
	unlinkPTR(symbolPTR(ptr)->fun);
	symbolPTR(ptr)->fun = newbuiltinPTR(fun);
	unlinkPTR(ptr);
}

void LISPaddspecialform(char *name, LISPspecialform spform, int flags)
{
	PTR ptr;

	ptr = LISPlookup_blk(-1, name);
	symbolPTR(ptr)->flags |= flags | LF_DONTFREE | LF_ISSPECIALFORM;
	unlinkPTR(symbolPTR(ptr)->fun);
	symbolPTR(ptr)->fun = newspecialformPTR(spform);
	unlinkPTR(ptr);
}

void LISPaddstaticvar(PTR *ptr, PTR val)
{
	PTR ref;

	ref = newrefPTR(ptr);
	*ptr = linkPTR(val);
	refPTR(ref)->nextref = LISPstaticvars;
	LISPstaticvars = ref;
}

LISPstatic void setval(PTR sym, PTR val)
{
	LISPpopval(sym);
	LISPpushval(sym, val);
}

void LISPdone(void)
{
	if (LISPoldfpecatch)
		signal(SIGFPE, LISPoldfpecatch);
	if (LISPoldintcatch)
		signal(SIGINT, LISPoldintcatch);

	LISPresetmret();

	LISPeraseallsymbols();

	LISPreclaim();

	unlinkPTR(LISPstaticvars);

	LISPfreecore();

	LISPclearusertypes();

	if (LISPcurline) {
		free(LISPcurline);
		LISPcurline = NULL;
	}

	free(oblist);
}

void LISPmarkobjects(void)
{
	unsigned i;
	PTR ptr;

	for (i = 0; i < LISPhash_module; i++)
		for (ptr = oblist[i].ptr; ptr; ptr = symbolPTR(ptr)->unext)
			LISPmark(ptr);
	
	if (LISPmret)
		LISPmark(LISPmret);

	LISPmark(LISPstaticvars);
}

void LISPbindcallback(PTR sym, PTR val)
{
	/*
	LISPputs(LISPstdout, "Trapping ");
	LISPwrite(LISPstdout, sym, 0);
	LISPputs(LISPstdout, " -> ");
	LISPwrite(LISPstdout, val, 0);
	LISPputc('\n', LISPstdout);
	*/

	if (sym == EVALHOOKptr) {
		LISPevalhookflag = nonNIL(val);
	} else if (sym == APPLYHOOKptr) {
		LISPapplyhookflag = nonNIL(val);
	} else {
		LISPfatal("incoherent bind callback");
	}
	
}

PTR LISP_oblist(PTR args)
{
	unsigned i;
	int bybucket;
	PTR ptr;
	PTR head, tail;
	PTR buckethead, buckettail;

	GETARGS(("?a", args, &ptr, NILptr));
	bybucket = isNIL(ptr) ? 0 : 1;
	LISPqueueinit(&head, &tail);
	for (i = 0; i < LISPhash_module; i++) {
		if (bybucket) {
			LISPqueueinit(&buckethead, &buckettail);
			for (ptr = oblist[i].ptr; ptr; ptr = symbolPTR(ptr)->unext) {
				LISPqueueadd(&buckethead, &buckettail, ptr);
			}
			LISPqueueadd(&head, &tail, buckethead);
			unlinkPTR(buckethead);
		} else {
			for (ptr = oblist[i].ptr; ptr; ptr = symbolPTR(ptr)->unext) {
				LISPqueueadd(&head, &tail, ptr);
			}
		}
	}
	return head;
}

PTR LISP_intern(PTR args)
{
	PTR arg;

	GETARGS(("s", args, &arg));
	return LISPlookup(arg);
}

PTR LISP_unintern(PTR args)
{
	PTR arg;

	GETARGS(("y", args, &arg));
	LISPuninternsymbol(arg);
	return linkPTR(arg);
}

LISPstatic PTR find(PTR name, unsigned hash)
{
	PTR ptr;

	hash %= LISPhash_module;
	for (ptr = oblist[hash].ptr; ptr; ptr = symbolPTR(ptr)->unext) {
		if (LISPstreql(name, symbolPTR(ptr)->name))
			return linkPTR(ptr);
	}
	return NULL;
}

LISPstatic void intern(PTR sym, unsigned hash)
{
	struct bucket *bucket;

	CHKPTR(sym);
	if (symbolPTR(sym)->flags & LF_INTERN)
		return;
	symbolPTR(sym)->flags |= LF_INTERN;
	symbolPTR(sym)->hash = hash;
	LISPnintern++;
	bucket = oblist + (hash % LISPhash_module);
	putsym(sym, bucket);
	if (bucket->n > LISPhash_module / 2)
		rehash(2 * (LISPhash_module - 1) + 1);
}

PTR LISPlookup_blk(int len, char *name)
{
	PTR str, ret;

	if (len < 0)
		len = strlen(name);
	str = newstringPTR(len, name);
	ret = LISPlookup(str);
	unlinkPTR(str);
	return ret;
}

PTR LISPlookup(PTR name)
{
	PTR ret;
	unsigned hash;

	hash = LISPstrhash(name);
	ret = find(name, hash);
	if (!ret) {
		ret = newsymbolPTR(name);
		intern(ret, hash);
	}
	return ret;
}

PTR LISP_gensym(PTR args)
{
	PTR arg, str, ret;
	static unsigned long count = 1;
	LStrBuf sb;
	static char buf[51];

	GETARGS(("?a", args, &arg, NULL));
	str = NULL;
	if (arg) {
		switch (arg->type) {
		case LT_SYMBOL:
			str = symbolPTR(arg)->name;
			break;
		case LT_STRING:
			str = arg;
			break;
		case LT_NUMBER:
			count = (unsigned long) numberPTR(arg)->val;
			break;
		default:
			return LISPerrormsg(ILLEGAL_TYPE_MSG);
		}
	}
	lsbInit(&sb);
	if (str) {
		lsbAddBlk(&sb, stringPTR(str)->len, stringPTRstr(str));
	} else {
		lsbAddChar(&sb, 'g');
	}
	sprintf(buf, "%lu", count++);
	lsbAddStr(&sb, buf);
	str = newstringPTRsb(&sb);
	lsbDone(&sb);
	ret = newsymbolPTR(str);
	unlinkPTR(str);
	return ret;
}

PTR LISP_gentemp(PTR args)
{
	PTR arg, name, ptr, ret;
	int hash;
	static unsigned long count = 1;
	static char buf[51];
	LStrBuf sb;

	GETARGS(("?s", args, &arg, Tptr));
	for (;;) {
		lsbInit(&sb);
		lsbAddBlk(&sb, stringPTR(arg)->len, stringPTRstr(arg));
		sprintf(buf, "%lu", count++);
		lsbAddStr(&sb, buf);
		name = newstringPTRsb(&sb);
		lsbDone(&sb);

		hash = LISPstrhash(name);
		ptr = find(name, hash);
		if (!ptr)
			break;
		unlinkPTR(name);
		unlinkPTR(ptr);
	}
	ret = LISPlookup(name);
	unlinkPTR(name);
	return ret;
}

LISPstatic void nil_setup(void)
{
#ifdef LISPDEBUG
	LISPnil.magic = LISP_MAGIC;
#endif
	LISPnil.nl = 5;
	LISPnil.type = LT_SYMBOL;
	LISPnil.c.symbol.sym = &nil_sym;

#ifdef LISPDEBUG
	nil_sym.magic = LISP_MAGIC_EXT;
#endif

	symbolPTR(NILptr)->bind = symbolPTR(NILptr)->prop
		= symbolPTR(NILptr)->fun = NILptr;
	symbolPTR(NILptr)->key = NILptr;
	symbolPTR(NILptr)->flags = LF_DONTFREE | LF_CONST;
	symbolPTR(NILptr)->uprev = symbolPTR(NILptr)->unext = NULL;
	symbolPTR(NILptr)->name = newstringPTR(-1, "nil");

	intern(NILptr, LISPstrhash(symbolPTR(NILptr)->name));
}

void LISPuninternsymbol(PTR sym)
{
	CHKPTR(sym);
	if (symbolPTR(sym)->flags & LF_INTERN) {
		remsym(sym, oblist + (symbolPTR(sym)->hash % LISPhash_module));
		LISPnintern--;
		symbolPTR(sym)->flags &= ~LF_INTERN;
	}
}

LISPstatic void putsym(PTR sym, struct bucket *lst)
{
	symbolPTR(sym)->unext = lst->ptr;
	if (lst->ptr)
		symbolPTR(lst->ptr)->uprev = sym;
	lst->ptr = sym;
	(lst->n)++;
}

LISPstatic void remsym(PTR sym, struct bucket *lst)
{
	if (symbolPTR(sym)->uprev)
		symbolPTR(symbolPTR(sym)->uprev)->unext = symbolPTR(sym)->unext;
	else
		lst->ptr = symbolPTR(sym)->unext;

	if (symbolPTR(sym)->unext)
		symbolPTR(symbolPTR(sym)->unext)->uprev = symbolPTR(sym)->uprev;

	symbolPTR(sym)->unext = symbolPTR(sym)->uprev = NULL;
	(lst->n)--;
}

LISPstatic void rehash(int n)
{
	unsigned i;
	PTR tmp;
	struct bucket bucket;

	if (n < 1)
		n = 1;
	memset(&bucket, 0, sizeof(bucket));
	for (i = 0; i < LISPhash_module; i++) {
		while (oblist[i].ptr) {
			tmp = oblist[i].ptr;
			remsym(tmp, oblist + i);
			putsym(tmp, &bucket);
		}
	}
	free(oblist);
	LISPhash_module = n;
	oblist = ecalloc(LISPhash_module, sizeof(*oblist));

	while (bucket.ptr) {
		tmp = bucket.ptr;
		i = symbolPTR(tmp)->hash % LISPhash_module;
		remsym(tmp, &bucket);
		putsym(tmp, oblist + i);
	}
}

void LISPchecksymbols(void)
{
	PTR ptr;
	unsigned i;

	printf("Checking ");
	fflush(stdout);
	for (i = 0; i < LISPhash_module; i++) {
		printf("%d ", i);
		fflush(stdout);
		for (ptr = oblist[i].ptr; ptr; ptr = symbolPTR(ptr)->unext)
			;
	}
	printf("ok.\n");
}

PTR newsymbolPTR(PTR name)
{
	PTR ret, key;
	char *str;

	ret = LISPnewcell();
	ret->type = LT_SYMBOL;
	ret->c.symbol.sym = ecalloc(1, sizeof(*(ret->c.symbol.sym)));
#ifdef LISPDEBUG
	ret->c.symbol.sym->magic = LISP_MAGIC_EXT;
#endif
	LISPnsymbols++;

	symbolPTR(ret)->name = linkPTR(name);
	symbolPTR(ret)->bind = symbolPTR(ret)->fun
		= symbolPTR(ret)->prop = symbolPTR(ret)->key = NILptr;
#ifdef STATISTICS
	LISPrefcount += 4;
#endif
	NILptr->nl += 4;

	str = stringPTRstr(name);

	if (LISPisnumber(str))
		symbolPTR(ret)->flags |= LF_ESCAPE;
		
	if (*str == ':') {
		symbolPTR(ret)->flags |= LF_CONST | LF_COLON;
		key = LISPlookup_blk(stringPTR(name)->len - 1, str + 1);
		unlinkPTR(symbolPTR(key)->key);
		symbolPTR(key)->key = linkPTR(ret);
		unlinkPTR(key);
	}

	return ret;
}

PTR LISP_boundp(PTR args)
{
	PTR sym;

	GETARGS(("y", args, &sym));
	return nonNIL(symbolPTR(sym)->bind) ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_getvals(PTR args)
{
	PTR sym;

	GETARGS(("y", args, &sym));
	return LISPnewlist(symbolPTR(sym)->bind);
}

void LISPerasesymbol(PTR sym)
{
	PTR key, bind, fun, prop, name;

	/* printf("Erasing %s\n", sym->c.symbol.sym->name); */

	CHKPTR(sym);

	LISPuninternsymbol(sym);

	key = symbolPTR(sym)->key;
	symbolPTR(sym)->key = linkPTR(NILptr);

	bind = symbolPTR(sym)->bind;
	symbolPTR(sym)->bind = linkPTR(NILptr);

	fun = symbolPTR(sym)->fun;
	symbolPTR(sym)->fun = linkPTR(NILptr);

	prop = symbolPTR(sym)->prop;
	symbolPTR(sym)->prop = linkPTR(NILptr);

	name = symbolPTR(sym)->name;
	symbolPTR(sym)->name = linkPTR(NILptr);

	unlinkPTR(key);
	unlinkPTR(bind);
	unlinkPTR(fun);
	unlinkPTR(prop);
	unlinkPTR(name);
}

int LISPkeepsymbol(PTR sym)
{
	struct LISPsymbol_ext *ptr = symbolPTR(sym);

	if (ptr->flags & LF_INTERN) {
		if ((ptr->flags & LF_DONTFREE)
			|| nonNIL(ptr->bind)
			|| nonNIL(ptr->fun)
			|| nonNIL(ptr->prop)
			|| nonNIL(ptr->key)
		) {
			return 1;
		}
	}
	return 0;
}

