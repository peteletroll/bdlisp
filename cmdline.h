/*

	CMDLINE.H - 1997 - Bigdogs Internescional.

	Espansione wildcards in linea di comando.
	Analisi opzioni.

*/

#ifndef CMDLINE_H
#define CMDLINE_H

#include <dir.h>

void argexp(int *argc, char **argv[], int attrib);

extern int optind;    /* index of which argument is next */
extern char *optarg;  /* pointer to argument of current option */
extern int opterr;    /* allow error message */
 
extern int getopt(int argc, char *argv[], char *optionS);

#endif
