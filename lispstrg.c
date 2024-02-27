/*

	LISPSTRG.C - 1997 - Bigdogs Internescional.

	Interprete LISP - funzioni sulle stringhe.

*/

#include "bdlisp.h"

static struct LISPcell emptystring, onecharstring[256];

void LISPinitstrings(void)
{
	int i;

#ifdef LISPDEBUG
	emptystring.magic = LISP_MAGIC;
#endif
	emptystring.nl = 1;
	emptystring.type = LT_STRING;
	emptystring.nl = 1;
	emptystring.c.string.len = 0;
	emptystring.c.string.cnt.str[0] = 0;

	for (i = 0; i < 256; i++) {
#ifdef LISPDEBUG
		onecharstring[i].magic = LISP_MAGIC;
#endif
		onecharstring[i].nl = 1;
		onecharstring[i].type = LT_STRING;
		onecharstring[i].c.string.len = 1;
		onecharstring[i].c.string.cnt.str[0] = i;
		onecharstring[i].c.string.cnt.str[1] = 0;
	}

	LISPstringbytes = 0;
}

LISPstatic char *memmem(int l1, char *c1, int l2, char *c2)
{
	int i, j, m;

	m = l1 - l2;
	for (i = 0; i <= m; i++) {
		for (j = 0; j < l2; j++) {
			if (c1[i + j] != c2[j])
				break;
		}
		if (j == l2)
			return c1 + i;
	}
	return NULL;
}
	
unsigned LISPstrhash(PTR str)
{
	int len;
	unsigned char *name;
	unsigned ret;

	len = stringPTR(str)->len;
	name = (unsigned char *) stringPTRstr(str);

	ret = 5381;
	while (len > 0) {
		/* ret = 33 * ret + *name; */
		ret += (ret << 5) + *name;
		name++;
		len--;
	}
	return ret;
}

int LISPstreql(PTR str1, PTR str2)
{
	int l;

	l = stringPTR(str1)->len;
	if (l != stringPTR(str2)->len)
		return 0;
	return !memcmp(stringPTRstr(str1), stringPTRstr(str2), l);
}

int LISPstrcmp(PTR str1, PTR str2)
{
	int l, l1, l2, ret;

	l1 = stringPTR(str1)->len;
	l2 = stringPTR(str2)->len;
	l = (l1 < l2) ? l1 : l2;
	ret = memcmp(stringPTRstr(str1), stringPTRstr(str2), l);
	if (ret)
		return ret;
	return l1 - l2;
}

PTR newstringPTR(int len, char *str)
{
	PTR ret;

	if (len < 0)
		len = strlen(str);
	if (len == 0)
		return linkPTR(&emptystring);
	if (len == 1 && str) 
		return newcharPTR(*str);

	ret = LISPnewcell();
	ret->type = LT_STRING;
	stringPTR(ret)->len = len;
	if (len >= sizeof(char *)) {
		stringPTR(ret)->cnt.cptr = emalloc(len + 1);
		LISPstringbytes += len;
	}
	if (str)
		memcpy(stringPTRstr(ret), str, len);
	stringPTRstr(ret)[len] = 0;

	return ret;
}

PTR newstringPTRsb(LStrBuf *sb)
{
	PTR ret;

	if (sb->length == 0)
		return linkPTR(&emptystring);
	if (sb->length == 1) 
		return newcharPTR(sb->first.blk[0]);

	ret = newstringPTR(sb->length, NULL);
	lsbPut(sb, stringPTRstr(ret));

	return ret;
}

PTR newcharPTR(int c)
{
	PTR ret;

	ret = onecharstring + (unsigned char) c;
	return linkPTR(ret);
}

PTR LISP_string_hash(PTR args)
{
	PTR str;

	GETARGS(("s", args, &str));

	return newnumberPTR(LISPstrhash(str));
}

PTR LISP_concat(PTR args)
{
	PTR ret;
	PTR str;
	LStrBuf sb;

	lsbInit(&sb);
	while (args->type == LT_NODE) {
		str = nodePTR(args)->car;
		if (str->type == LT_SYMBOL)
			str = symbolPTR(str)->name;
		if (str->type != LT_STRING) {
			lsbDone(&sb);
			return LISPerrormsg(NON_STRING_MSG);
		}
		lsbAddBlk(&sb, stringPTR(str)->len, stringPTRstr(str));
		args = nodePTR(args)->cdr;
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

PTR LISP_substr(PTR args)
{
	PTR str, n1, n2, ret;
	int c1, c2, l;

	GETARGS(("sn?n", args, &str, &n1, &n2, NULL));

	l = stringPTR(str)->len;
	c1 = (int) numberPTR(n1)->val;
	c2 = n2 ? (int) numberPTR(n2)->val : l;
	
	if (c1 < 0)  c1 = 0;
	if (c2 > l)  c2 = l;

	if (c1 >= l)
		return linkPTR(&emptystring);

	if (c2 <= c1)
		return linkPTR(&emptystring);

	if (c1 <= 0 && c2 >= l)
		return linkPTR(str);

	ret = newstringPTR(c2 - c1, stringPTRstr(str) + c1);
	
	return ret;
}

PTR LISP_findstr(PTR args)
{
	PTR str1, str2, n;
	int start;
	char *ret;

	GETARGS(("ss?n", args, &str1, &str2, &n, ZEROptr));

	start = (int) numberPTR(n)->val;
	if (start < 0)
		start = 0;
	if (start > stringPTR(str1)->len)
		start = stringPTR(str1)->len;
	ret = memmem(stringPTR(str1)->len - start, stringPTRstr(str1) + start,
		stringPTR(str2)->len, stringPTRstr(str2));
	if (ret)
		return newnumberPTR(ret - stringPTRstr(str1));
	return linkPTR(NILptr);
}

static int contains(PTR str, char c)
{
	int l;
	char *s;

	l = stringPTR(str)->len;
	s = stringPTRstr(str);
	while (l > 0) {
		if (*s == c)
			return 1;
		l--;
		s++;
	}
	return 0;
}

PTR LISP_dupstr(PTR args)
{
	PTR str, num, ret;
	int l, n;
	char *s;

	GETARGS(("sn", args, &str, &num));

	n = (int) numberPTR(num)->val;

	l = stringPTR(str)->len;
	if (n <= 0 || l <= 0)
		return linkPTR(&emptystring);
	if (n == 1)
		return linkPTR(str);
	
	ret = newstringPTR(n * l, NULL);
	s = stringPTRstr(ret);
	while (n-- > 0) {
		memcpy(s, stringPTRstr(str), l);
		s += l;
	}

	return ret;
}

PTR LISP_tokstr(PTR args)
{
	PTR str, sep;
	PTR head, tail;
	char *s, *start;
	int l;

	GETARGS(("ss", args, &str, &sep));

	s = stringPTRstr(str);
	l = stringPTR(str)->len;

	LISPqueueinit(&head, &tail);

	if (stringPTR(sep)->len <= 0) {
		while (l > 0) {
			str = newcharPTR(*s);
			LISPqueueadd(&head, &tail, str);
			unlinkPTR(str);
			s++;
			l--;
		}
	} else {
		while (l > 0) {
			while (l > 0 && contains(sep, *s))
				s++, l--;
			if (l <= 0)
				break;
			start = s;
			while (l > 0 && !contains(sep, *s))
				s++, l--;
			str = newstringPTR((int) (s - start), start);
			LISPqueueadd(&head, &tail, str);
			unlinkPTR(str);
		}
	}

	return head;
}

PTR LISP_stringp(PTR args)
{
	PTR arg;

	GETARGS(("a", args, &arg));
	return arg->type == LT_STRING ?
		linkPTR(Tptr) : linkPTR(NILptr);
}

PTR LISP_cgi_encode(PTR args)
{
	int escape = '%';
	PTR str, esc, ret;
	int l;
	char *c, buf[5];
	LStrBuf sb;

	GETARGS(("s?s", args, &str, &esc, NULL));

	if (esc) {
		if (stringPTR(esc)->len != 1)
			return LISPerrormsg(NON_CHAR_MSG);
		escape = *(stringPTRstr(esc));
		if (escape > 0 && escape < 128 && isalnum(escape))
			return LISPerrormsg("invalid escape character");
	}

	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	c = stringPTRstr(str);

	lsbInit(&sb);
	while (l > 0) {
		if (isalnum((unsigned char) *c)) {
			lsbAddChar(&sb, *c);
		} else if (*c == ' ' && escape != '+') {
			lsbAddChar(&sb, '+');
		} else {
			sprintf(buf, "%c%02x", escape, (unsigned char) *c);
			lsbAddStr(&sb, buf);
		}
		l--, c++;
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

PTR LISP_cgi_decode(PTR args)
{
	char escape = '%';
	PTR str, esc, ret;
	int l, t;
	char *c;
	LStrBuf sb;

	GETARGS(("s?s", args, &str, &esc, NULL));

	if (esc) {
		if (stringPTR(esc)->len != 1)
			return LISPerrormsg(NON_CHAR_MSG);
		escape = *stringPTRstr(esc);
	}


	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	c = stringPTRstr(str);

	lsbInit(&sb);
	while (l > 0) {
		if (*c == '+' && escape != '+') {
			lsbAddChar(&sb, ' ');
		} else if (*c == escape) {
			t = '?';
			sscanf(c + 1, "%2x", &t);
			lsbAddChar(&sb, (char) t);
			l -= 2, c += 2;
		} else {
			lsbAddChar(&sb, *c);
		}
		l--, c++;
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

PTR LISP_html_escape(PTR args)
{
	PTR str, ret;
	int l;
	unsigned char *c;
	LStrBuf sb;
	char entbuf[10];

	GETARGS(("s", args, &str));

	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	c = (unsigned char *) stringPTRstr(str);

	lsbInit(&sb);
	while (l > 0) {
		switch (*c) {
		case '&':
			lsbAddStr(&sb, "&amp;");
			break;
		case '"':
			lsbAddStr(&sb, "&quot;");
			break;
		case '\'':
			lsbAddStr(&sb, "&apos;");
			break;
		case '<':
			lsbAddStr(&sb, "&lt;");
			break;
		case '>':
			lsbAddStr(&sb, "&gt;");
			break;
		default:
			if (*c >= 127) {
				sprintf(entbuf, "&#x%02x;", *c);
				lsbAddStr(&sb, entbuf);
			} else {
				lsbAddChar(&sb, *c);
			}
			break;
		}
		l--, c++;
	}
	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

PTR LISP_string_upcase(PTR args)
{
	PTR str;
	char *c;
	int l;

	GETARGS(("s", args, &str));

	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	if (l == 1)
		return newcharPTR(toupper(*stringPTRstr(str)));
	str = newstringPTR(l, stringPTRstr(str));
	c = stringPTRstr(str);
	while (l > 0) {
		*c = toupper(*c);
		c++;
		l--;
	}
	return str;
}

PTR LISP_string_downcase(PTR args)
{
	PTR str;
	char *c;
	int l;

	GETARGS(("s", args, &str));

	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	if (l == 1)
		return newcharPTR(tolower(*stringPTRstr(str)));
	str = newstringPTR(l, stringPTRstr(str));
	c = stringPTRstr(str);
	while (l > 0) {
		*c = tolower(*c);
		c++;
		l--;
	}
	return str;
}

PTR LISP_string_capitalize(PTR args)
{
	PTR str;
	unsigned char *c;
	int l, flag;

	GETARGS(("s", args, &str));

	l = stringPTR(str)->len;
	if (l == 0)
		return linkPTR(str);
	if (l == 1)
		return newcharPTR(toupper(*stringPTRstr(str)));
	str = newstringPTR(l, stringPTRstr(str));
	c = (unsigned char *) stringPTRstr(str);
	flag = 0;
	while (l > 0) {
		*c = flag ? tolower(*c) : toupper(*c);
		flag = isalnum(*c);
		c++;
		l--;
	}
	return str;
}

void LISPscrambleblock(int len, char *blk)
{
	int i;
	unsigned char mask = 23;

	for (i = 0; i < len; i++) {
		blk[i] = blk[i] ^ (char) mask;
		mask = mask * 13 + 17;
	}
}

PTR LISP_string_scramble(PTR args)
{
	PTR arg, ret;
	int len;
	char *blk;

	GETARGS(("s", args, &arg));

	len = stringPTR(arg)->len;
	blk = emalloc(len + 1);
	memcpy(blk, stringPTRstr(arg), len);
	LISPscrambleblock(len, blk);
	ret = newstringPTR(len, blk);
	free(blk);
	return ret;
}

#ifdef REGEX

static int LT_REGEXP;

#define BUF_LEN 80
static char buf[BUF_LEN + 1];

#define MAX_SUB 10
static regmatch_t match[MAX_SUB];

LISPstatic PTR indexpair(int i1, int i2)
{
	PTR ret, tmp;

	ret = newnodePTR();
	nodePTR(ret)->car = newnumberPTR(i1);
	tmp = nodePTR(ret)->cdr = newnodePTR();
	nodePTR(tmp)->car = newnumberPTR(i2);
	nodePTR(tmp)->cdr = linkPTR(NILptr);
	return ret;
}

LISPstatic PTR make_regexp(PTR args)
{
	PTR str, ret;
	regex_t *re;
	int res;

	GETARGS(("s", args, &str));

	re = emalloc(sizeof(*re));
	res = regcomp(re, stringPTRstr(str), REG_EXTENDED);
	if (res) {
		regerror(res, re, buf, BUF_LEN);
		regfree(re);
		free(re);
		return LISPerrormsg("regular expression: %s", buf);
	}

	ret = newuserPTR(LT_REGEXP);
	userPTR(ret)->ext = re;

	return ret;
}

LISPstatic PTR fre_match(PTR args)
{
	PTR regexp, str;
	regex_t *re;
	int res;

	GETARGS(("as", args, &regexp, &str));
	if (regexp->type != LT_REGEXP)
		return LISPerrormsg("non regexp argument");
	re = (regex_t *) userPTR(regexp)->ext;
	res = regexec(re, stringPTRstr(str), 1, match, 0);
	if (res)
		return linkPTR(NILptr);
	return indexpair(match->rm_so, match->rm_eo);
}

LISPstatic PTR fre_gmatch(PTR args)
{
	PTR regexp, str;
	char *s;
	regex_t *re;
	PTR ret, tail, tmp;
	int ofs = 0;

	GETARGS(("as", args, &regexp, &str));
	if (regexp->type != LT_REGEXP)
		return LISPerrormsg("non regexp argument");
	re = (regex_t *) userPTR(regexp)->ext;
	s = stringPTRstr(str);
	LISPqueueinit(&ret, &tail);
	while (!regexec(re, s, 1, match, 0)) {
		tmp = indexpair(ofs + match->rm_so, ofs + match->rm_eo);
		LISPqueueadd(&ret, &tail, tmp);
		unlinkPTR(tmp);
		s += match->rm_eo;
		ofs += match->rm_eo;
	}
	return ret;
}

LISPstatic void put_repl(LStrBuf *sb, char *str, char *repl)
{
	int c;

	while ((c = *repl) != 0) {
		if (c == '\\') {
			c = *(++repl);
			if (isdigit(c)) {
				c -= '0';
				lsbAddBlk(sb, match[c].rm_eo - match[c].rm_so,
					str + match[c].rm_so);
			} else if (c) {
				lsbAddChar(sb, c);
			} else {
				repl--;
			}
		} else {
			lsbAddChar(sb, c);
		}
		repl++;
	}
}

LISPstatic PTR fre_sub(PTR args)
{
	PTR regexp, pstr, repl;
	regex_t *re;
	char *str;
	int res;
	LStrBuf sb;
	PTR ret;

	GETARGS(("ass", args, &regexp, &repl, &pstr));
	if (regexp->type != LT_REGEXP)
		return LISPerrormsg("non regexp argument");
	re = (regex_t *) userPTR(regexp)->ext;
	str = stringPTRstr(pstr);

	res = regexec(re, str, MAX_SUB, match, 0);
	if (res)
		return linkPTR(pstr);

	lsbInit(&sb);
	lsbAddBlk(&sb, match->rm_so, str);
	put_repl(&sb, str, stringPTRstr(repl));
	lsbAddStr(&sb, str + match->rm_eo);

	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

LISPstatic PTR fre_gsub(PTR args)
{
	PTR regexp, pstr, repl;
	regex_t *re;
	char *str;
	LStrBuf sb;
	PTR ret;

	GETARGS(("ass", args, &regexp, &repl, &pstr));
	if (regexp->type != LT_REGEXP)
		return LISPerrormsg("non regexp argument");
	re = (regex_t *) userPTR(regexp)->ext;
	str = stringPTRstr(pstr);

	lsbInit(&sb);
	while (!regexec(re, str, MAX_SUB, match, 0)) {
		lsbAddBlk(&sb, match->rm_so, str);
		put_repl(&sb, str, stringPTRstr(repl));
		str += match->rm_eo;
	}
	lsbAddStr(&sb, str);

	ret = newstringPTRsb(&sb);
	lsbDone(&sb);
	return ret;
}

LISPstatic PTR fre_split(PTR args)
{
	PTR regexp, pstr;
	regex_t *re;
	char *str;
	regmatch_t match;
	PTR ret, tail, tmp;

	GETARGS(("as", args, &regexp, &pstr));
	if (regexp->type != LT_REGEXP)
		return LISPerrormsg("non regexp argument");
	re = (regex_t *) userPTR(regexp)->ext;
	str = stringPTRstr(pstr);

	LISPqueueinit(&ret, &tail);

	while (0 == regexec(re, str, 1, &match, 0)
			&& match.rm_so != match.rm_eo) {
		tmp = newstringPTR(match.rm_so, str);
		LISPqueueadd(&ret, &tail, tmp);
		unlinkPTR(tmp);
		str += match.rm_eo;
	}

	if (*str) {
		tmp = newstringPTR(-1, str);
		LISPqueueadd(&ret, &tail, tmp);
		unlinkPTR(tmp);
	} 

	return ret;
}

LISPstatic void regexp_free(PTR ptr)
{
	regex_t *re;

	if (ptr->type != LT_REGEXP)
		LISPfatal("wrong type in regexp_free");
	re = (regex_t *) userPTR(ptr)->ext;
	regfree(re);
	free(re);
}

static LISP_INIT_TAB(re_init) {
	LISP_FUNCT("make-regexp", make_regexp, 0),
	LISP_FUNCT("re-match", fre_match, 0),
	LISP_FUNCT("re-gmatch", fre_gmatch, 0),
	LISP_FUNCT("re-sub", fre_sub, 0),
	LISP_FUNCT("re-gsub", fre_gsub, 0),
	LISP_FUNCT("re-split", fre_split, 0),
	LISP_TAB_LAST()
};

void LISPinitregexp(void)
{
	LT_REGEXP = LISPaddusertype("regexp", NULL, NULL, NULL, regexp_free);
	LISPprocessinittab(re_init);
	LISPaddfeature(":regexp");
}

#endif /* REGEX */

/*********************************************/

void lsbInit(LStrBuf *sb)
{
	sb->curptr = sb->length = 0;
	sb->last = &(sb->first);
	sb->first.next = NULL;
}

void lsbAddChar(LStrBuf *sb, char c)
{
	if (sb->curptr >= STRBUFBLKLEN) {
		sb->last->next = emalloc(sizeof(struct StrBufBlk));
		sb->last= sb->last->next;
		sb->last->next = NULL;
		sb->curptr = 0;
	}
	sb->last->blk[sb->curptr++] = c;
	sb->length++;
}

void lsbAddBlk(LStrBuf *sb, int len, char *blk)
{
	int n;

	while (len > 0) {
		n = STRBUFBLKLEN - sb->curptr;
		if (n > len)
			n = len;
		memcpy(sb->last->blk + sb->curptr, blk, n);
		blk += n;
		len -= n;
		sb->length += n;
		sb->curptr += n;
		if (len > 0) {
			sb->last->next = emalloc(sizeof(struct StrBufBlk));
			sb->last= sb->last->next;
			sb->last->next = NULL;
			sb->curptr = 0;
		}
	}
}

void lsbAddStr(LStrBuf *sb, char *str)
{
	lsbAddBlk(sb, strlen(str), str);
}

void lsbPut(LStrBuf *sb, char *dst)
{
	struct StrBufBlk *b;

	for (b = &(sb->first); b != sb->last; b = b->next) {
		memcpy(dst, b->blk, STRBUFBLKLEN);
		dst += STRBUFBLKLEN;
	}
	memcpy(dst, b->blk, sb->curptr);
	dst[sb->curptr] = 0;
}

char *lsbGet(LStrBuf *sb)
{
	char *ret;

	ret = emalloc(sb->length + 1);
	lsbPut(sb, ret);
	return ret;
}

void lsbDone(LStrBuf *sb)
{
	struct StrBufBlk *b;

	while (sb->first.next) {
		b = sb->first.next;
		sb->first.next = b->next;
		free(b);
	}
}

