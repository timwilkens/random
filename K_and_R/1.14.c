#include <stdio.h>

/* Write a program to print a histogram of the lengths
   of words in its input. It is easy to draw the bars
   with bars horizontal; a vertical orientation is more
   challenging */

#define IN  1
#define OUT 0

#define MAX_CHAR 255

void print_bars(int);
void horizonal_histogram(int[], int);

void
horizontal_histogram(int frequency[], int length) {
    int i = 1;
    for (i = 1; i <= length; i++) {
        if (frequency[i] == 0) {
            continue;
        }

        if (i == 9) {
            printf("[ \\t ] ");
        } else if (i == 10) {
            printf("[ \\n ] ");
        } else {
            printf("[ %2c ] ", i);
        }
        print_bars(frequency[i]);
        printf("\n");
    }
}

void
print_bars(int count) {
    int i;
    for (i = 0; i < count; i ++) {
        printf("|");
    }
}

int
main() {

    int c, status, max_char_seen;
    /* Assume only ASCII */
    int frequency[MAX_CHAR + 1] = {0};

    while ((c = getchar()) != EOF) {
        frequency[c]++;
        if (c > max_char_seen) {
            max_char_seen = c;
        }
    }

    horizontal_histogram(frequency, max_char_seen);

    return 0;
}
