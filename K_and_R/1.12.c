#include <stdio.h>

/* Write a program that prints its input one word per line */

#define IN 1
#define OUT 0

int
main() {

    int c, status;
    status = OUT;
    while ((c = getchar()) != EOF) {
        if (c == '\t' || c == '\n' || c == ' ') {
            if (status == IN) {
                printf("\n");
                status = OUT;
            }
        } else {
            status = IN;
            putchar(c);
        }
    }

    return 0;
}
