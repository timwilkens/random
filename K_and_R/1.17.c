#include <stdio.h>

/* Write a program to print all input lines that are longer
   than 80 characters. */

#define MAXLINE 10000

int mygetline(char *, int);

int
main() {

    int len;
    char line[MAXLINE];

    while ((len = mygetline(line, MAXLINE)) > 0) {
        if (len > 80) {
            printf("%s", line);
        }
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
