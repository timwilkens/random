#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int options;
  int value;
} Cell;

typedef struct {
    Cell *cells[81];
    Cell *rows[9][9];
    Cell *columns[9][9];
    Cell *squares[9][9];
} Board;

static int number_of_cells = 81;
static int default_options = 0x01ff;
static int null_options = 0xfe00;

Cell *
new_cell(int value) {
    Cell *c = (Cell *)malloc(sizeof(Cell));
    if (value == 0) {
        c->options = default_options; 
        c->value = 0;
    } else {
        c->options = null_options;
        c->value = value;
    }
    return c;
}

Board *
new_board(int values[81]) {
    // Expect cells with no value to be passed in with a value of 0
    Board *b = (Board *)malloc(sizeof(Board));
    int i;
    for (i = 0; i < number_of_cells; i++) {
        b->cells[i] = new_cell(values[i]); 
    }

    int column, place, j;
    for (column = 0; column < 9; column++) {
        place = 0;
        for (j = column; j < column + 73; j += 9) {
            b->columns[column][place] = b->cells[j];
            place++;
        }
    }

    int row_number = 0;
    for (i = 0; i < 73; i += 9) {
        place = 0;
        for (j = i; j < i + 9; j++) {
            b->rows[row_number][place] = b->cells[j];
            place++;
        }
        row_number++;
    }

    int square_starts[9] = { 0,  3,  6,
                             27, 30, 33,
                             54, 57, 60
                           };

    int k;
    for (i = 0; i < 9; i++) {
        j = square_starts[i];
        k = 0;
        b->squares[i][k++] = b->cells[j];
        b->squares[i][k++] = b->cells[j + 1];
        b->squares[i][k++] = b->cells[j + 2];
        b->squares[i][k++] = b->cells[j + 9];
        b->squares[i][k++] = b->cells[j + 10];
        b->squares[i][k++] = b->cells[j + 11];
        b->squares[i][k++] = b->cells[j + 18];
        b->squares[i][k++] = b->cells[j + 19];
        b->squares[i][k] = b->cells[j + 20];
    }


    return b;
}

void
show_cell_options(Cell *c) {
    if (c->options == null_options) {
        printf("\tVALUE: %d\n", c->value);
    } else {
        int i;
        int j = 1;
        printf("\t");
        for (i = 1; i <= 256 ; i *= 2) {
            if (c->options & i) {
                printf("%d, ", j);
            }
            j++;
        }
        printf("\n");
    }
}

void show_board(Board *b) {
    int i;
    for (i = 0; i < number_of_cells; i++) {
        int value = b->cells[i]->value;
        if (value == 0) {
            printf(" - ");
        } else {
            printf(" %d ", value);
        }
        if (i % 9 == 8) {
            printf("\n");
        }
    }
}

void
set_cell_value(Cell *c, int value) {
    // Caller must not provide an invalid value (not 0-9)
    c->options = null_options;
    c->value = value;
}

void
remove_cell_option(Cell *c, int option) {
    int mask = (1 << (option - 1));
    if (!(c->options & mask)) {
        return; // Already removed this option.
    }
    c->options ^= mask;
}

int
value_from_options(int options) {
    // Return 0 for not found.
    if (options == null_options) {
        return 0;
    }

    if (options != 0 && (options & (options - 1)) == 0) {
        int i;
        for (i = 1; i <= 9; i++) {
            if (options & 1) {
                return i;
            }
            options >>= 1;
        }
    }
    return 0;
}

// Set values based on available options in a row.
void
remove_row_options(Board *b) {
    int row, j, k;
    for (row = 0; row < 9; row++) {
        for (j = 0; j < 9; j++) {
            if (b->rows[row][j]->options != null_options) {
                continue;
            }
            for (k = 0; k < 9; k++) {
                if (j == k) { continue; }
                remove_cell_option(b->rows[row][k], b->rows[row][j]->value);
            }
        }
    }
}

void
remove_column_options(Board *b) {
    int i, j, k;
    for (i = 0; i < 9; i++) {
        for (j = 0; j < 9; j++) {
            if (b->columns[i][j]->options != null_options) {
                continue;
            }
            for (k = 0; k < 9; k++) {
                if (j == k) { continue; }
                remove_cell_option(b->columns[i][k], b->columns[i][j]->value);
            }
        }
    }
}

void
remove_square_options(Board *b) {
    int i, j, k;
    for (i = 0; i < 9; i++) {
        for (j = 0; j < 9; j++) {
            if (b->squares[i][j]->options != null_options) {
                continue;
            }
            for (k = 0; k < 9; k++) {
                if (j == k) { continue; }
                remove_cell_option(b->squares[i][k], b->squares[i][j]->value);
            }
        }
    }
}

// Our routine that should set values.
// Only set a value once it has ONE option left.
void
condense_only_one(Board *b) {
    int i;
    for (i = 0; i < number_of_cells; i++) {
        int possible = value_from_options(b->cells[i]->options);
        if (possible) {
            set_cell_value(b->cells[i], possible);
        }
    }
}

int
main() {
    int values[81] = { 0,4,5,8,0,3,7,1,0,
                       8,1,0,0,0,0,0,2,4,
                       7,0,9,0,0,0,5,0,8,
                       0,0,0,9,0,7,0,0,0,
                       0,0,0,0,6,0,0,0,0,
                       0,0,0,4,0,2,0,0,0,
                       6,0,4,0,0,0,3,0,5,
                       3,2,0,0,0,0,0,8,7,
                       0,5,7,3,0,8,2,6,0
                     };

    Board *b = new_board(values);
    show_board(b);
    printf("\n\n");

    int i;
    for (i = 0; i < 10; i++) {
        condense_only_one(b);
        remove_row_options(b);
        remove_column_options(b);
        remove_square_options(b);
        condense_only_one(b);
    }
    show_board(b);

    return 0;
}
