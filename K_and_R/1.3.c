#include <stdio.h>

/* Modify the temperature conversion program to print
   a heading above the table. */

#define UPPER 300
#define LOWER 0
#define STEP 20

void
print_heading() {
    printf("%3s\t%6s\n", "F", "C");

}

int
main() {

    print_heading();

    float fahr, celsius;

    fahr = LOWER;
    while (fahr <= UPPER) {
        celsius = (5.0 / 9.0) * (fahr - 32.0);
        printf("%3.0f\t%6.1f\n", fahr, celsius);
        fahr += STEP;
    }

    return 0;
}
