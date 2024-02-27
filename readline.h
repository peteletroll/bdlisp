/*

	READLINE.H - 1998 - Bigdogs Internescional.

	Input controllato.

*/

#ifndef READLINE_H
#define READLINE_H

enum { RL_OK = 0, RL_EOF, RL_ABORT };

char *readline(char *prompt, int *status);
void erase_hist(void);

int load_hist(char *id);
int save_hist(char *id);

#endif
