/*

	LISPSYST.C - 1997 - Bigdogs Internescional.

	Interprete LISP - interfaccia col sistema operativo.

*/

#include "bdlisp.h"

PTR STANDARD_INPUTptr, STANDARD_OUTPUTptr, STANDARD_ERRORptr;
PTR LISPstdin, LISPstdout, LISPstderr;

int LISPonterminal;

PTR TERMINAL_IOptr;
PTR LISPconsole;

PTR VERBOSEptr, PRINTptr;
PTR LOAD_SOURCEptr;

char *LISPdirsep = DIR_SEP;
char *LISPpathsep = PATH_SEP;
PTR PATHptr;

#ifdef CTRL_C_CHECK

void LISPbreakcheck(void)
{
#ifdef DJGPP_ENV
	if (_go32_was_ctrl_break_hit())
		LISPstatus |= LS_BREAK;
#endif
#ifdef BORLANDC_ENV
	kbhit();
#endif
}

#endif

PTR LISP_cls(PTR args)
{
	GETARGS(("", args));
	fflush(stdout);
#ifdef CONIO
	clrscr();
#else
	printf("\033[2J\033[1;1f");
#endif
	return linkPTR(NILptr);
}

PTR LISP_cursor(PTR args)
{
	PTR argr, argc;
	int r, c;

	fflush(stdout);

	GETARGS(("nn", args, &argr, &argc));

	r = (int) numberPTR(argr)->val;
	c = (int) numberPTR(argc)->val;

#ifdef CONIO
	gotoxy(c + 1, r + 1);
#else
	printf("\033[%d;%df", r + 1, c + 1);
#endif

	return linkPTR(NILptr);
}

#ifdef BORLANDC_ENV
#undef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC CLK_TCK
#endif

PTR LISP_clock(PTR args)
{
	GETARGS(("", args));
	return newnumberPTR((double) clock() / CLOCKS_PER_SEC);
}

PTR LISP_get_universal_time(PTR args)
{
	struct timeval tv;

	GETARGS(("", args));
	if (gettimeofday(&tv, NULL))
		return linkPTR(NILptr);
	return newnumberPTR(tv.tv_sec + tv.tv_usec * 1e-6);
}

PTR LISP_get_decoded_time(PTR args)
{
	time_t t;
	struct tm *tm;
	PTR ret, tail, tmp;

	GETARGS(("", args));

	time(&t);
	tm = localtime(&t);

	LISPqueueinit(&ret, &tail);

	tmp = newnumberPTR(tm->tm_sec);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_min);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_hour);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_mday);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_mon + 1);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_year + 1900);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	tmp = newnumberPTR(tm->tm_wday);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	LISPqueueadd(&ret, &tail, tm->tm_isdst ? Tptr : NILptr);

	tmp = newnumberPTR(timezone / 3600.0);
	LISPqueueadd(&ret, &tail, tmp);
	unlinkPTR(tmp);

	return ret;
}

PTR LISP_fflush(PTR args)
{
	PTR f;

	GETARGS(("?f", args, &f, unlinkLISPoutput()));
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
		fflush(stdout);
		break;
	case ST_FILE:
#ifdef POPEN
	case ST_PIPE:
#endif
		fflush(streamPTR(f)->str.file);
		break;
	default:
		break;
	}
	return linkPTR(Tptr);
}

PTR LISP_ftell(PTR args)
{
	PTR ret = NULL, f;

	GETARGS(("f", args, &f));
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
#ifdef POPEN
	case ST_PIPE:
#endif
	case ST_CLOSED:
		ret = newnumberPTR(-1);
		break;
	case ST_FILE:
		ret = newnumberPTR(ftell(streamPTR(f)->str.file));
		break;
	case ST_INSTRING:
		ret = newnumberPTR(streamPTR(f)->str.instring.ptr
				- stringPTRstr(streamPTR(f)->str.instring.string));
		break;
	case ST_OUTSTRING:
		ret = newnumberPTR(streamPTR(f)->str.outstring.length);
		break;	
	default:
		LISPfatal("unknown stream type in LISP_ftell");
		break;
	}
	return ret;
}

PTR LISP_fseek(PTR args)
{
	PTR f, pos;
	long p;
	int res = 0;

	GETARGS(("fn", args, &f, &pos));
	p = (long) numberPTR(pos)->val;
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
#ifdef POPEN
	case ST_PIPE:
#endif
	case ST_INSTRING:
	case ST_OUTSTRING:
	case ST_CLOSED:
		res = 1;
		break;
	case ST_FILE:
		res = fseek(streamPTR(f)->str.file, p, SEEK_SET);
		break;
	default:
		LISPfatal("unknown stream type in LISP_fseek");
		break;
	}
	return res ? linkPTR(NILptr) : linkPTR(Tptr);
}

PTR LISP_stream_row(PTR args)
{
	PTR f;

	GETARGS(("?f", args, &f, unlinkLISPoutput()));
	return newnumberPTR(streamPTR(f)->row);
}

PTR LISP_stream_column(PTR args)
{
	PTR f;

	GETARGS(("?f", args, &f, unlinkLISPoutput()));
	return newnumberPTR(streamPTR(f)->col);
}

PTR LISP_fopen(PTR args)
{
	PTR name, opts;
	FILE *f;

	GETARGS(("ss", args, &name, &opts));
	f = fopen(stringPTRstr(name), stringPTRstr(opts));
	if (!f)
		return linkPTR(NILptr);
	return newfilestreamPTR(f);
}

#ifdef POPEN

PTR LISP_popen(PTR args)
{
	PTR name, opts;
	FILE *f;
	PTR ret;

	GETARGS(("ss", args, &name, &opts));
	f = popen(stringPTRstr(name), stringPTRstr(opts));
	if (!f)
		return linkPTR(NILptr);
	ret = newfilestreamPTR(f);
	streamPTR(ret)->strtype = ST_PIPE;
	return ret;
}

#endif

PTR LISP_fread(PTR args)
{
	PTR stream, psize, ret;
	size_t size;
	FILE *f;
	char buf1, *buf;

	GETARGS(("fn", args, &stream, &psize));

	if (streamPTR(stream)->strtype != ST_FILE)
		return LISPerrormsg("non file argument");
	f = streamPTR(stream)->str.file;

	size = (size_t) numberPTR(psize)->val;

	if (size <= 0)
		return newstringPTR(0, "");

	if (size == 1) {
		size = fread(&buf1, 1, 1, f);
		if (size <= 0)
			return newstringPTR(0, "");
		return newcharPTR(buf1);
	}

	buf = emalloc(size);
	size = fread(buf, 1, size, f);
	if (size <= 0) {
		free(buf);
		return newstringPTR(0, "");
	}

	ret = newstringPTR(size, buf);
	free(buf);
	return ret;
}

PTR LISP_fwrite(PTR args)
{
	PTR stream, str;
	FILE *f;
	size_t ret;

	GETARGS(("fs", args, &stream, &str));
	
	if (streamPTR(stream)->strtype != ST_FILE)
		return LISPerrormsg("non file argument");
	f = streamPTR(stream)->str.file;

	ret = fwrite(stringPTRstr(str), 1, stringPTR(str)->len, f);
	return newnumberPTR(ret);
}

PTR LISP_make_string_input_stream(PTR args)
{
	PTR arg;

	GETARGS(("s", args, &arg));
	return newinstringstreamPTR(arg);
}

PTR LISP_make_string_output_stream(PTR args)
{
	GETARGS(("", args));
	return newoutstringstreamPTR();
}

PTR LISP_get_output_stream_string(PTR args)
{
	PTR stream, ret;
	LStrBuf *sb;

	GETARGS(("f", args, &stream));
	if (streamPTR(stream)->strtype != ST_OUTSTRING)
		return linkPTR(NILptr);
	sb = &(streamPTR(stream)->str.outstring);
	ret = newstringPTRsb(sb);
	lsbDone(sb);
	lsbInit(sb);
	return ret;
}

PTR LISP_close(PTR args)
{
	PTR f;
	int res = 0;

	GETARGS(("f", args, &f));
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
		res = 1;
		break;
	case ST_FILE:
		res = fclose(streamPTR(f)->str.file);
		break;
#ifdef POPEN
	case ST_PIPE:
		res = pclose(streamPTR(f)->str.file);
		break;
#endif
	case ST_INSTRING:
		unlinkPTR(streamPTR(f)->str.instring.string);
		break;
	case ST_OUTSTRING:
		lsbDone(&(streamPTR(f)->str.outstring));
		break;
	case ST_CLOSED:
		res = 1;
		break;
	default:
		LISPfatal("unknown stream type in LISP_fclose");
		break;
	}
	streamPTR(f)->strtype = ST_CLOSED;
	return res ? linkPTR(NILptr) : linkPTR(Tptr);
}

PTR LISP_feof(PTR args)
{
	PTR f;
	int res = 0;

	GETARGS(("f", args, &f));
	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
		res = feof(stdin);
		break;
	case ST_FILE:
#ifdef POPEN
	case ST_PIPE:
#endif
		res = feof(streamPTR(f)->str.file);
		break;
	case ST_INSTRING:
		res = !*(streamPTR(f)->str.instring.ptr);
		break;
	case ST_OUTSTRING:
		res = 0;
		break;
	case ST_CLOSED:
		res = 1;
		break;
	default:
		LISPfatal("unknown stream type in LISP_feof");
		break;
	}
	return res ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_isatty(PTR args)
{
	PTR f;
	int res = 0;

	GETARGS(("f", args, &f));
	if (streamPTR(f)->strtype == ST_FILE)
		res = isatty(fileno(streamPTR(f)->str.file));
	return res ? linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_load(PTR args)
{
	PTR verbose, print, ret;
	PTR stream;
	char *name, *actual;
	PTR source;
	FILE *f;

	GETARGS(("a:aa", args, &stream,
		VERBOSEptr, &verbose, Tptr,
		PRINTptr, &print, NILptr));

	if (stream->type == LT_STREAM) {
		ret = LISPloadstream(stream, nonNIL(print), NILptr);
		unlinkPTR(ret);
		return linkPTR(Tptr);
	}

	if (stream->type == LT_SYMBOL)
		stream = symbolPTR(stream)->name;

	if (stream->type != LT_STRING)
		return LISPerrormsg(ILLEGAL_TYPE_MSG);

	name = stringPTRstr(stream);

	f = LISPpathfopen(name, &actual);
	if (!f) {
		if (nonNIL(verbose)) {
			LISPputs(unlinkLISPerror(), "; can't open ");
			LISPputs(unlinkLISPerror(), name);
			LISPnewline(unlinkLISPerror());
		}
		return linkPTR(NILptr);
	}
	if (nonNIL(verbose)) {
		LISPputs(NULL, "; loading ");
		LISPputs(NULL, actual);
		LISPnewline(NULL);
	}
	source = newstringPTR(-1, actual);
	ret = LISPloadfile(f, nonNIL(print), source);
	unlinkPTR(source);
	fclose(f);
	if (LISPstatus & LS_ABORT)
		return ret;
	unlinkPTR(ret);
	return linkPTR(Tptr);
}

int LISPloadlevel;

int LISPloadpathfile(char *name, int print)
{
	PTR ptr;
	char *actual;
	FILE *f;
	PTR source;

	f = LISPpathfopen((char *) name, &actual);
	if (!f) {
		return 1;
	}
	/* printf("Reading %s ...\n", initfile); */

	source = newstringPTR(-1, actual);
	ptr = LISPloadfile(f, print, source);
	unlinkPTR(ptr);
	unlinkPTR(source);
	fclose(f);
	return 0;
}

PTR LISPloadfile(FILE *f, int print, PTR source)
{
	PTR ret;
	PTR stream;

	stream = newfilestreamPTR(f);

	ret = LISPloadstream(stream, print, source);

	streamPTR(stream)->strtype = ST_CLOSED;
	unlinkPTR(stream);
	return ret;
}

LISPstatic int validlib(LISPpackedlib *lib)
{
	int len = lib->len;
	unsigned char *blk = lib->blk;

	return blk[len + 0] == 255
		&& blk[len + 1] == 4
		&& blk[len + 2] == 3;
}

void LISPloadstaticlib(LISPpackedlib *lib, int print)
{
	char *blk;
	PTR str, stream, ptr;

	if (!validlib(lib))
		LISPfatal("bad static library");
	blk = emalloc(lib->len);
	memcpy(blk, lib->blk, lib->len);
	LISPscrambleblock(lib->len, blk);
	str = newstringPTR(lib->len, blk);
	free(blk);

	stream = newinstringstreamPTR(str);
	unlinkPTR(str);

	ptr = LISPloadstream(stream, print, BUILTINptr);
	unlinkPTR(stream);
	unlinkPTR(ptr);
}

PTR LISPloadstream(PTR stream, int print, PTR source)
{
	PTR obj, res = NULL;

	if (stream->type != LT_STREAM)
		LISPfatal("non stream in LISPloadstream");
	LISPloadlevel++;
	LISPpushval(LOAD_SOURCEptr, source);
	for (;;) {
		obj = LISPreadobj(stream);
		if (obj == READ_EOFptr) {
			unlinkPTR(obj);
			break;
		}
		res = LISPevalobj(obj);
		unlinkPTR(obj);
		if (LISPstatus & LS_ABORT)
			break;
		if (print)
			LISPprintmret(NULL, res);
		unlinkPTR(res);
	}
	LISPpopval(LOAD_SOURCEptr);
	LISPloadlevel--;
	return (res && (LISPstatus & LS_ABORT)) ? res : linkPTR(NILptr);
}

PTR LISP_shell(PTR args)
{
	PTR arg;
	int res;
	char *shell;

	GETARGS(("?s", args, &arg, NULL));
	errno = 0;
	if (arg == NULL) {
		shell = getenv("SHELL");
		if (!shell) {
			shell = getenv("COMSPEC");
			if (!shell) {
				return LISPerrormsg("no shell name in environment");
			}
		}
		res = system(shell);
	} else {
		res = system(stringPTRstr(arg));
	}
	if (res && errno)
		return LISPsystemerror();
	return linkPTR(Tptr);
}

PTR LISP_getenv(PTR args)
{
	PTR arg;
	char *cnt;

	GETARGS(("s", args, &arg));
	cnt = getenv(stringPTRstr(arg));
	if (cnt)
		return newstringPTR(-1, cnt);
	return linkPTR(NILptr);
}

PTR LISPsystemerror(void)
{
	perror("System error");
	errno = 0;
	return LISP_debug_driver(NILptr);
}

char *LISPstreamtostr(PTR f)
{
	static char buf[50];
	char *id;

	switch (streamPTR(f)->strtype) {
	case ST_CONSOLE:
		id = "console";
		break;
	case ST_INSTRING:
		id = "instring";
		break;
	case ST_OUTSTRING:
		id = "outstring";
		break;
#ifdef POPEN
	case ST_PIPE:
		id = "pipe";
		break;
#endif
	case ST_CLOSED:
		id = "closed-stream";
		break;
	default:
		id = "stream";
		break;
	}
	sprintf(buf, "#<%s:%p>", id, f);
	return buf;
}

void LISPdirname(char *buf, char *path)
{
	char *last;
	int n;

	last = strrchr(path, *LISPdirsep);
	if (last) {
		n = (int) (last - path);
		strncpy(buf, path, n);
		buf[n] = 0;
	} else {
		strcpy(buf, ".");
	}
}

void LISPpathjoin(char *buf, char *path, char *name)
{
	int l;

	strncpy(buf, path, PATH_MAX);
	l = strlen(buf);
	if (buf[l - 1] != *LISPdirsep)
		strncat(buf, LISPdirsep, PATH_MAX);
	strncat(buf, name, PATH_MAX);
}

FILE *LISPpathfopen(char *name, char **actual)
{
	char *path;

	if (actual)
		*actual = NULL;

	path = LISPpathfind(name, unlinkLISPgetval(PATHptr));
	if (!path)
		return NULL;

	if (actual)
		*actual = path;
	
	return fopen(path, "rt");
}

char *LISPpathfind(char *name, PTR pathlst)
{
	static char buf[PATH_MAX + 1];
	PTR ptr;

	if (!access(name, F_OK))
		return name;

	if (pathlst->type == LT_NODE && *name != *LISPdirsep) {
		while (pathlst->type == LT_NODE) {
			ptr = nodePTR(pathlst)->car;
			if (ptr->type == LT_SYMBOL)
				ptr = symbolPTR(ptr)->name;
			if (ptr->type == LT_STRING) {
				LISPpathjoin(buf, stringPTRstr(ptr), name);
				/* fprintf(stderr, "Trying %s\n", buf); */
				if (!access(buf, F_OK))
					return buf;
			}
			pathlst = nodePTR(pathlst)->cdr;
		}
	}
	
	return NULL;
}

PTR LISP_path_find(PTR args)
{
	PTR name, pathlist;
	char *ret;

	GETARGS(("s?a", args, &name, &pathlist, unlinkLISPgetval(PATHptr)));
	
	ret = LISPpathfind(stringPTRstr(name), pathlist);
	if (ret)
		return newstringPTR(-1, ret);
	return linkPTR(NILptr);
}

PTR LISP_getcwd(PTR args)
{
	static char buf[PATH_MAX + 1];

	GETARGS(("", args));
	if (!getcwd(buf, PATH_MAX))
		return linkPTR(NILptr);
	return newstringPTR(-1, buf);
}

PTR LISP_chdir(PTR args)
{
	PTR name, ret;

	GETARGS(("s", args, &name));
	ret = chdir(stringPTRstr(name)) ? NILptr : Tptr;
	return linkPTR(ret);
}

PTR LISP_mkdir(PTR args)
{
	PTR name, ret;

	GETARGS(("s", args, &name));
#if defined( BORLANDC_ENV ) || defined( WIN32_ENV )
	ret = mkdir(stringPTR(name)->str) ? NILptr : Tptr;
#else
	ret = mkdir(stringPTRstr(name), ~0) ? NILptr : Tptr;
#endif
	return linkPTR(ret);
}

PTR LISP_unlink(PTR args)
{
	PTR name, ret;

	GETARGS(("s", args, &name));
	ret = unlink(stringPTRstr(name)) ? NILptr : Tptr;
	return linkPTR(ret);
}

#ifdef DYNLINK

PTR LISP_load_dynamic_library(PTR args)
{
	PTR name, init_func_name;
	char *ifnc = "LISPinitlibrary";
	void *dl;
	void (*init)(void);
	
	GETARGS(("s?s", args, &name, &init_func_name, NULL));

	if (init_func_name)
		ifnc = stringPTRstr(init_func_name);

	dl = dlopen(stringPTRstr(name), RTLD_NOW | RTLD_GLOBAL);
	if (!dl)
		return LISPerrormsg("dynamic linker: %s", dlerror());

	init = (void (*)(void)) dlsym(dl, ifnc);
	if (!init) {
		dlclose(dl);
		return LISPerrormsg("no %s in dynamic library", ifnc);
	}

	init();

	return linkPTR(Tptr);
}

#endif

