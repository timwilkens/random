#include <stdio.h>

/* Write a program to copy its input to its output,
   replacing each string of one or more blanks by a
   single blank. */

#define PRINT_SPACE 1
#define NO_SPACE 0

int
main() {
    int c, status;
    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            status = PRINT_SPACE;
        } else {
            /* Any consecutive spaces are done now.
               Print one space if we had some. */
            if (status == PRINT_SPACE) {
                putchar(' ');
                status = NO_SPACE;
            }
            putchar(c);
        }
    }

    return 0;
}
