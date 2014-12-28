#include <stdio.h>

/* Write a program to print the corresponding
   Celsius to Fahrenheit table. */

#define UPPER 300
#define LOWER 0
#define STEP 20

void
print_heading() {
    printf("%3s\t%6s\n", "C", "F");

}

int
main() {

    print_heading();

    float fahr, celsius;

    celsius = LOWER;
    while (celsius <= UPPER) {
        fahr = ((celsius * 9.0) / 5.0) +  32.0;
        printf("%3.0f\t%6.1f\n", celsius, fahr);
        celsius += STEP;
    }

    return 0;
}
