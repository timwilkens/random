#include <stdio.h>

/* Write a program to count blanks, tabs, and newlines. */

int
main() {

    double spaces, tabs, new_lines;
    int c;

    while ((c = getchar()) != EOF) {
        if (c == '\n') {
            new_lines++;
        } else if (c == '\t') {
            tabs++;
        } else if (c == ' ') {
            spaces++;
        }
    }
    printf("Spaces: %0.f\n", spaces);
    printf("Tabs: %0.f\n", tabs);
    printf("New Lines: %0.f\n", new_lines);

    return 0;
}
