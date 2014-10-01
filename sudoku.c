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

int
bit_vec_from_int(int n) {
    return n ? 1 << (n - 1) : 0;
}

int
int_from_bit_vec(int n) {
    if ((n & (n - 1)) == 0) {
        int i;
        for (i = 1; i <= 9; i++) {
            if (n & 1) {
                return i;
            }
            n >>= 1;
        }
    }
    return 0;
}

Cell *
new_cell(int value) {
    Cell *c = (Cell *)malloc(sizeof(Cell));
    if (value == 0) {
        c->options = default_options; 
        c->value = 0;
    } else {
        c->options = null_options;
        c->value = bit_vec_from_int(value);
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
show_board(Board *b) {
    int i;
    for (i = 0; i < number_of_cells; i++) {
        int value = int_from_bit_vec(b->cells[i]->value);
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
    c->options = null_options;
    c->value = bit_vec_from_int(value);
}

void
remove_cell_option(Cell *c, int option) {
    if (!(c->options & option)) {
        return; // Already removed this option.
    }
    c->options ^= option;
}

int
only_option(int options) {
    // Return 0 for not found.
    if (options == null_options) {
        return 0;
    }

    return int_from_bit_vec(options);
}

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
set_value_if_one_option(Board *b) {
    int i;
    for (i = 0; i < number_of_cells; i++) {
        int only_one = only_option(b->cells[i]->options);
        if (only_one) {
            set_cell_value(b->cells[i], only_one);
        }
    }
}

void
clean_up(Board *b) {
    int i;
    for (i = 0; i < number_of_cells; i++) {
        free(b->cells[i]);
    }
    free(b);
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
        set_value_if_one_option(b);
        remove_row_options(b);
        set_value_if_one_option(b);
        remove_column_options(b);
        set_value_if_one_option(b);
        remove_square_options(b);
    }

    set_value_if_one_option(b);
    show_board(b);

    clean_up(b);

    return 0;
}
