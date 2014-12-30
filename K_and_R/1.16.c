#include <stdio.h>

/* Revise the main routine of the longest line program so it
   will correctly print the length of arbitrarily long input
   lines, and as much of the text as possible. */

#define MAXLINE 10000

int mygetline(char *, int);
void copy(char *, char *);

int
main() {

    int len, max;
    char line[MAXLINE];
    char longest[MAXLINE];

    max = 0;
    while ((len = mygetline(line, MAXLINE)) > 0) {
        if (len > max) {
            max = len;
            copy(longest, line);
        }
    }

    if (max > 0) {
        printf("%s", longest);  
    }

    return 0;
}

int
mygetline(char *line, int lim) {
    int c, i;

    for (i = 0; i < lim - 1 && (c = getchar()) != EOF; i++) {
        line[i] = c;
        if (c == '\n') {
            i++;
            break;
        }
    }

    line[i] = '\0';
    return i;
}

void
copy(char *to, char *from) {
    int i = 0;
    while ((to[i] = from[i]) != '\0') {
        i++;
    }
}
