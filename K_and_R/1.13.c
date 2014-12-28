#include <stdio.h>

/* Write a program to print a histogram of the lengths
   of words in its input. It is easy to draw the bars
   with bars horizontal; a vertical orientation is more
   challenging */

#define IN  1
#define OUT 0

#define MAX_WORD_LEN 50

void print_bars(int);
void horizonal_histogram(int[], int);
void vertical_histogram(int[], int);

void
horizontal_histogram(int frequency[], int length) {
    int i = 1;
    for (i = 1; i <= length; i++) {
        printf("[%2d] ", i);
        print_bars(frequency[i]);
        printf("\n");
    }
}

void
vertical_histogram(int frequency[], int length) {

    int max_count, i;
    max_count = 0;
    /* Record the highest frequency */
    for (i = 1; i <= length; i++) {
        if (frequency[i] > max_count) {
            max_count = frequency[i];
        } 
    }

    int j;
    /* Loop through the frequencies starting at the highest.
       Print a star if the frequency matches, and a space otherwise.
       Decrement the frequency when we print a star. */
    for (j = max_count; j > 0; j--) {
        for (i = 1; i <= length; i++) {
            if (frequency[i] == j) {
                printf(" * ");
                frequency[i]--;
            } else {
                printf("   ");
            }
        }
        printf("\n");
    }

    /* Print the base with the word lengths */
    for (i = 1; i < length; i++) {
        printf("%2d ", i); 
    }

    printf("%2d\n", length);
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

    int c, status, length, max_length_seen;
    int frequency[MAX_WORD_LEN + 1] = {0};
    length = 0;

    while ((c = getchar()) != EOF) {
        if (c == '\n' || c == '\t' || c == ' ') {
            /* Just got out of a word if length is non zero */
            if (length != 0) {
                if (length > MAX_WORD_LEN) {
                    printf("Word exceeded maximum word length\n");
                    return 1;
                }
                frequency[length]++;
                if (length > max_length_seen) {
                    max_length_seen = length;
                }
                length = 0;
            }
            status = OUT;
        } else {
            status = IN;
            length++;
        }
    }

    vertical_histogram(frequency, max_length_seen);

    return 0;
}
