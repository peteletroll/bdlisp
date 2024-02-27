/*

	CMDLINE.C - 1997 - Bigdogs Internescional.

	Espansione wildcards in linea di comando.
	Analisi opzioni.

*/

#pragma hdrstop

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "cmdline.h"

struct listelem {
	struct listelem *next;
	char name[20];
};

static struct listelem *arghead, *argtail;
void addelem(struct listelem **head, struct listelem **tail,
	char *path, char *name);

static char *progname = NULL;
static int nelem;

static char *NO_MEM_MSG = "%s: no memory for wildcard expansion\n";

void argexp(int *argc, char **argv[], int attrib)
{
	int i;
	char *arg, *c, *c1;
	struct ffblk f;
	int found;
	static char path[MAXPATH + 1];
	char **nargv;
	struct listelem *p;
	struct listelem *tmphead, *tmptail;

	if (progname) {
		fprintf(stderr, "%s: argexp called twice\n", progname);
		exit(1);
	}
	progname = *argv[0];
	for (i = 0; i < *argc; i++) {
		arg = (*argv)[i];
		tmphead = tmptail = NULL;
		if (strchr(arg, '*') || strchr(arg, '?')) {
			strncpy(path, arg, MAXPATH);
			c = NULL;
			if (path[1] == ':')
				c = path + 1;
			if ((c1 = strrchr(path, '\\')) != NULL)
				c = c1;
			if (c) {
				*(c + 1) = 0;
			} else {
				path[0] = 0;
			}
			found = !findfirst(arg, &f, attrib);
			if (found) {
				while (found) {
					addelem(&tmphead, &tmptail, path, f.ff_name);
					found = !findnext(&f);
				}
			} else {
				addelem(&tmphead, &tmptail, NULL, arg);
			}
		} else {
			addelem(&tmphead, &tmptail, NULL, arg);
		}
		if (arghead)
			argtail->next = tmphead;
		else
			arghead = tmphead;
		argtail = tmptail;
	}
	nargv = calloc(nelem + 1, sizeof(*nargv));
	if (!nargv) {
		fprintf(stderr, NO_MEM_MSG, progname);
		exit(1);
	}
	for (i = 0, p = arghead; i < nelem; i++, p = p->next)
		nargv[i] = p->name;
	*argv = nargv;
	*argc = nelem;
}

static void addelem(struct listelem **head, struct listelem **tail,
	char *path, char *name)
{
	static char buf[MAXPATH + 1];
	struct listelem *p;

	if (path) {
		strncpy(buf, path, MAXPATH);
	} else {
		buf[0] = 0;
	}
	strncat(buf, name, MAXPATH);
	p = malloc(sizeof(*p) + strlen(buf));
	if (!p) {
		fprintf(stderr, NO_MEM_MSG, progname);
		exit(1);
	}
	strcpy(p->name, buf);
	/*
	p->next = NULL;
	if (*head)  (*tail)->next = p;
	else  *head = p;
	*tail = p;
	*/
	while (*head && strcmp((*head)->name, p->name) < 0)
		head = &((*head)->next);
	if (!*head)
		*tail = p;
	p->next = *head;
	*head = p;
	nelem++;
}

/*************************************************/

/*
	getopt.c -- Turbo C
 
	Copyright (c) 1986,1987 by Borland International Inc.
	All Rights Reserved.

	Minor mods - 1997 - Bigdogs Internescional.

*/
 
int	optind	= 1;	/* index of which argument is next	*/
char   *optarg;		/* pointer to argument of current option */
int	opterr	= 1;	/* allow error message	*/
 
static	char   *letP = NULL;	/* remember next option char's location */
/* static	char	SW = 0;	*/	/* DOS switch character, either '-' or '/' */
/* removed - 1997 - now accepts both '/' and '-' */

/*
  Parse the command line options, System V style.
 
  Standard option syntax is:
 
	option ::= SW [optLetter]* [argLetter space* argument]
 
  where
	- SW is either '/' or '-', according to the current setting
	  of the MSDOS switchar (int 21h function 37h).
	  (modified - 1997 - accepts both)
	- there is no space before any optLetter or argLetter.
	- opt/arg letters are alphabetic, not punctuation characters.
	- optLetters, if present, must be matched in optionS.
	- argLetters, if present, are found in optionS followed by ':'.
	- argument is any white-space delimited string.  Note that it
	  can include the SW character.
	- upper and lower case letters are distinct.

  There may be multiple option clusters on a command line, each
  beginning with a SW, but all must appear before any non-option
  arguments (arguments not introduced by SW).  Opt/arg letters may
  be repeated: it is up to the caller to decide if that is an error.
 
  The character SW appearing alone as the last argument is an error.
  The lead-in sequence SWSW ("--" or "//") causes itself and all the
  rest of the line to be ignored (allowing non-options which begin
  with the switch char).

  The string *optionS allows valid opt/arg letters to be recognized.
  argLetters are followed with ':'.  getopt() returns the value of
  the option character found, or EOF if no more options are in the
  command line.	 If option is an argLetter then the global optarg is
  set to point to the argument string (having skipped any white-space).
 
  The global optind is initially 1 and is always left as the index
  of the next argument of argv[] which getopt has not taken.  Note
  that if "--" or "//" are used then optind is stepped to the next
  argument before getopt() returns EOF.
 
  If an error occurs, that is an SW char precedes an unknown letter,
  then getopt() will return a '?' character and normally prints an
  error message via perror().  If the global variable opterr is set
  to false (zero) before calling getopt() then the error message is
  not printed.
 
  For example, if the MSDOS switch char is '/' (the MSDOS norm) and
 
	*optionS == "A:F:PuU:wXZ:"
 
  then 'P', 'u', 'w', and 'X' are option letters and 'F', 'U', 'Z'
  are followed by arguments.  A valid command line may be:
 
	aCommand  /uPFPi /X /A L someFile
 
  where:
	- 'u' and 'P' will be returned as isolated option letters.
	- 'F' will return with "Pi" as its argument string.
	- 'X' is an isolated option.
	- 'A' will return with "L" as its argument.
	- "someFile" is not an option, and terminates getopt.  The
	  caller may collect remaining arguments using argv pointers.
*/

static int isswitch(int c)
{
	return c == '-' || c == '/';
}

int	getopt(int argc, char *argv[], char *optionS)
{
	unsigned char ch;
	char *optP;
 
	if (argc > optind) {
		if (letP == NULL) {
			if ((letP = argv[optind]) == NULL || 
				!isswitch(*(letP++)))  goto gopEOF;
			if (isswitch(*letP)) {
				optind++;  goto gopEOF;
			}
		}
		if (0 == (ch = *(letP++))) {
			optind++;  goto gopEOF;
		}
		if (':' == ch  ||  (optP = strchr(optionS, ch)) == NULL)  
			goto gopError;
		if (':' == *(++optP)) {
			optind++;
			if (0 == *letP) {
				if (argc <= optind)  goto  gopError;
				letP = argv[optind++];
			}
			optarg = letP;
			letP = NULL;
		} else {
			if (0 == *letP) {
				optind++;
				letP = NULL;
			}
			optarg = NULL;
		}
		return ch;
	}
gopEOF:
	optarg = letP = NULL;  
	return EOF;
 
gopError:
	optarg = NULL;
	errno  = EINVAL;
	if (opterr)
		fprintf(stderr, "%s: Invalid option -%c\n", argv[0], ch);
	return ('?');
}

