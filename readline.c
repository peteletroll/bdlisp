/*

	READLINE.C - 1998 - Bigdogs Internescional.

	Input controllato.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

#include <limits.h>

#include <assert.h>

#include "readline.h"

/*****************************************/

#if defined(__TURBOC__)

#define CONIO
#include <io.h>
#include <dir.h>
#define PATH_MAX MAXPATH
#define DIR_SEP '\\'

#elif defined(DJGPP)

#define CONIO
#include <unistd.h>
#define DIR_SEP '\\'

#elif defined(__sun)

#define TERMIOS
#define DIR_SEP '/'

#else /* try unix with termios */

#define TERMIOS
#include <unistd.h>
#define DIR_SEP '/'

#endif

/*****************************************/

#ifdef TERMIOS
#include <termios.h>
#endif

#ifdef CONIO
#include <conio.h>
#endif

#define MAX_HIST 100

enum keycmd {
	FIRST = 10000,
	ENTER, CANCEL, REDRAW, ABORT,
	BACKSPACE, ERASE,
	BACK, FORWARD,
	BEFORE, AFTER,
	HOME, END,
	MATCHPAR
};

static struct keytab_elem {
	int in, out;
} keytab[] = {

	{ '\n',  ENTER     },
	{ '\r',  ENTER     },
	{ 8,     BACKSPACE },
	{ 127,   BACKSPACE },
	{ 24,    ERASE     },  /* ^X */
	{ 12,    REDRAW    },  /* ^L */
	{ 18,    REDRAW    },  /* ^R */
	{ 3,     ABORT     },  /* ^C */
	{ 6,     FORWARD   },  /* ^F */
	{ 2,     BACK      },  /* ^B */
	{ 16,    BEFORE    },  /* ^P */
	{ 14,    AFTER     },  /* ^N */
	{ 1,     HOME      },  /* ^A */
	{ 5,     END       },  /* ^E */
	{ 19,    MATCHPAR  },  /* ^S */

#ifdef CONIO

	/* PC keyboard */

	{ 1075,  BACK      },
	{ 1077,  FORWARD   },
	{ 1072,  BEFORE    },
	{ 1080,  AFTER     },
	{ 1083,  ERASE     },
	{ 1071,  HOME      },
	{ 1079,  END       },

#endif

#ifdef LINUX

	{ 127,   BACKSPACE },

#endif

	/* ANSI codes */

	{ 2067,  FORWARD   },  /* ESC[C */
	{ 2068,  BACK      },  /* ESC[D */
	{ 2065,  BEFORE    },  /* ESC[A */
	{ 2066,  AFTER     },  /* ESC[B */

	/* end of table */

	{ -1,    -1 }
};

/******************************************************/

#define BUF_LEN 1024
static char buf[BUF_LEN + 1];
static int buf_ptr, buf_len;
static int buf_mod;

/******************************************************/

static void (*oldint)();

/******************************************************/

#ifdef TERMIOS

struct termios term;

static void term_setup(void)
{
	struct termios t;

	tcgetattr(0, &term);
	t = term;
	t.c_iflag &= ~(BRKINT|PARMRK|INPCK|IXON|IXOFF);
	t.c_iflag |=  (IGNBRK|IGNPAR);
	t.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
	/* t.c_lflag |=  (ISIG); */
	t.c_lflag &= ~(ISIG);
	t.c_cc[VMIN] = 1;
	t.c_cc[VTIME] = 0;
	tcsetattr(0, TCSADRAIN, &t);
}

static void term_restore(void)
{
	tcsetattr(0, TCSADRAIN, &term);
}

static int read_key(void)
{
	int ret;

	ret = getc(stdin);
	if (ret == 27) {
		ret = getc(stdin);
		if (ret == '[')
			ret = 2000 + getc(stdin);
	}
	return ret;
}

#endif

#ifdef CONIO

static int read_key(void)
{
	int ret = getch();
	if (ret == 0)
		ret = 1000 + getch();
	return ret;
}

#endif

static int getkey(void)
{
	int ret;
	struct keytab_elem *k;

	ret = read_key();
	for (k = keytab; k->in >= 0; k++) {
		if (k->in == ret) {
			ret = k->out;
			break;
		}
	}
	return ret;
}

/******************************************************/

static struct hist_elem {
	struct hist_elem *prev, *next;
	char row[1];
} *hist_head = NULL, *hist_tail, *cur_hist = NULL;

static int nhist = 0;

static void add_hist(char *row)
{
	struct hist_elem *h;

	h = malloc(sizeof(*h) + strlen(row));
	if (!h)
		return;
	strcpy(h->row, row);
	h->next = NULL;
	h->prev = hist_head;
	if (hist_head) {
		hist_head->next = h;
	} else {
		hist_tail = h;
	}
	hist_head = h;
	nhist++;

	if (nhist > 2 && nhist > MAX_HIST) {
		h = hist_tail;
		hist_tail = h->next;
		hist_tail->prev = NULL;
		free(h);
		nhist--;
	}
}

void erase_hist(void)
{
	struct hist_elem *h;

	while (hist_head) {
		h = hist_head;
		hist_head = h->prev;
		free(h);
		nhist--;
	}
	assert(nhist == 0);
}

static char idbuf[PATH_MAX + 1];

static void setidbuf(char *id)
{
	static char s[2] = { DIR_SEP, 0 };
	char *h;
	int l;
	
	h = getenv("HOME");

	if (h) {
		strcpy(idbuf, h);
		l = strlen(idbuf);
		if (idbuf[l - 1] != *s)
			strcat(idbuf, s);
	} else {
		strcpy(idbuf, s);
	}
	strcat(idbuf, id);
}

int load_hist(char *id)
{
	FILE *f;
	int l;

	setidbuf(id);
	f = fopen(idbuf, "rt");
	if (!f)
		return 1;
	while (fgets(buf, BUF_LEN, f)) {
		l = strlen(buf);
		if (l <= 0)
			continue;
		l--;
		if (buf[l] == '\n') {
			if (l <= 0)
				continue;
			buf[l] = 0;
		}
		add_hist(buf);
	}
	fclose(f);
	return 0;
}

int save_hist(char *id)
{
	FILE *f;
	struct hist_elem *h;

	setidbuf(id);
	f = fopen(idbuf, "wt");
	if (!f)
		return 1;
	for (h = hist_tail; h; h = h->next)
		fprintf(f, "%s\n", h->row);
	fclose(f);
	return 0;
}

/******************************************************/

static void refresh_to_eol(void)
{
	int i;

	for (i = buf_ptr; i < buf_len; i++)
		putc(buf[i], stdout);
	putc(' ', stdout);
	for (i = buf_len; i >= buf_ptr; i--)
		putc(8, stdout);
	fflush(stdout);
}

static void move_to(int pos)
{
	if (pos < 0)
		pos = 0;
	if (pos > buf_len)
		pos = buf_len;
	while (buf_ptr > pos) {
		putc(8, stdout);
		buf_ptr--;
	}
	while (buf_ptr < pos) {
		putc(buf[buf_ptr], stdout);
		buf_ptr++;
	}
	fflush(stdout);
}

static void replace(char *new)
{
	int i;

	move_to(0);
	for (i = 0; i < buf_len; i++)
		putc(' ', stdout);
	for (i = 0; i < buf_len; i++)
		putc(8, stdout);
	strncpy(buf, new, BUF_LEN);
	buf[BUF_LEN] = 0;
	buf_len = strlen(buf);
	for (i = 0; i < buf_len; i++)
		putc(buf[i], stdout);
	fflush(stdout);
	buf_ptr = buf_len;
}

static void match_par(void)
{
	int i, n, c;
	int dir, incr, decr;

	switch (buf[buf_ptr]) {

	case '(':
		dir = +1; incr = '('; decr = ')';
		break;
	case ')':
		dir = -1; incr = ')'; decr = '(';
		break;

	case '[':
		dir = +1; incr = '['; decr = ']';
		break;
	case ']':
		dir = -1; incr = ']'; decr = '[';
		break;

	case '{':
		dir = +1; incr = '{'; decr = '}';
		break;
	case '}':
		dir = -1; incr = '}'; decr = '{';
		break;

	default:
		return;
	}
	
	n = 1;
	i = buf_ptr + dir;
	while (i >= 0 && i < buf_len) {
		c = buf[i];
		if (c == incr)
			n++;
		if (c == decr)
			n--;
		if (n == 0) {
			move_to(i);
			break;
		}
		i += dir;
	}
}

/******************************************************/

char *readline(char *prompt, int *status)
{
	int i, c;

	*status = RL_OK;

	if (!isatty(fileno(stdin)) || !isatty(fileno(stdout))) {
		if (!fgets(buf, BUF_LEN, stdin)) {
			*status = RL_EOF;
			return NULL;
		}
		i = strlen(buf);
		if (buf[i - 1] == '\n')
			buf[i - 1] = 0;
		return buf;
	}

#ifdef TERMIOS
	term_setup();
#endif
	oldint = signal(SIGINT, SIG_IGN);

	if (!prompt)
		prompt = "";

	printf("%s", prompt);
	fflush(stdout);

	buf_ptr = buf_len = 0;
	buf_mod = 0;
	cur_hist = NULL;
	
	while ((c = getkey()) != ENTER && c != EOF && c != ABORT) {
		switch (c) {
		case FORWARD:
			if (buf_ptr < buf_len) {
				putc(buf[buf_ptr++], stdout);
				fflush(stdout);
			}
			break;
		case BACK:
			if (buf_ptr > 0) {
				putc(8, stdout);
				buf_ptr--;
				fflush(stdout);
			}
			break;
		case HOME:
			move_to(0);
			break;
		case END:
			move_to(buf_len);
			break;
		case BACKSPACE:
			if (buf_ptr > 0) {
				for (i = buf_ptr; i < buf_len; i++)
					buf[i - 1] = buf[i];
				putc(8, stdout);
				buf_ptr--;
				buf_len--;
				refresh_to_eol();
				buf_mod = 1;
			}
			break;
		case ERASE:
			if (buf_ptr < buf_len) {
				for (i = buf_ptr + 1; i < buf_len; i++)
					buf[i - 1] = buf[i];
				buf_len--;
				refresh_to_eol();
				buf_mod = 1;
			}
			break;
		case REDRAW:
			printf("\n%s", prompt);
			for (i = 0; i < buf_len; i++)
				putc(buf[i], stdout);
			for (i = buf_len; i > buf_ptr; i--)
				putc(8, stdout);
			fflush(stdout);
			break;
		case MATCHPAR:
			if (buf_ptr < buf_len)
				match_par();
			break;
		case BEFORE:
			if (hist_head) {
				if (cur_hist) {
					cur_hist = cur_hist->prev;
				} else {
					cur_hist = hist_head;
				}
				replace(cur_hist ? cur_hist->row : "");
				buf_mod = 0;
			}
			break;
		case AFTER:
			if (hist_head && cur_hist) {
				cur_hist = cur_hist->next;
				replace(cur_hist ? cur_hist->row : "");
				buf_mod = 0;
			}
			break;
		default:
			if ((isprint(c) || (c >= 128 && c <= 255)) && buf_len < BUF_LEN) {
				for (i = buf_len - 1; i >= buf_ptr; i--)
					buf[i + 1] = buf[i];
				buf_len++;
				buf[buf_ptr++] = c;
				putc(c, stdout);
				refresh_to_eol();
				buf_mod = 1;
			} else {
				/* ignore key */
				/* printf("?%d?", c); */
			}
			break;
		}
	}

#ifdef TERMIOS
	term_restore();
#endif
	signal(SIGINT, oldint);

	buf[buf_len] = 0;

	if (c == EOF && buf_len == 0) {
		putc('\n', stdout);
		*status = RL_EOF;
		return NULL;
	}

	if (c == ABORT) {
		printf("^C\n");
		*status = RL_ABORT;
		return NULL;
	}

	/* if (buf_mod && buf_len > 0) */
	if (buf_len > 0)
		add_hist(buf);

	putc('\n', stdout);
	return buf;
}

