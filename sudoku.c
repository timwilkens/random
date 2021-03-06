#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int options;
} Cell;

typedef struct {
    Cell *cells[81];
    Cell *rows[9][9];
    Cell *columns[9][9];
    Cell *squares[9][9];
} Board;

Board *setup_metadata(Board *);

static int NUMBER_OF_CELLS = 81;
static int ALL_BITS = 0x01ff;

int
bit_vec_from_int(int n) {
    return n ? 1 << (n - 1) : 0;
}

// This doubles as a routine to check if a cell has it's value determined.
// Return 0 if there are still multiple options.
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
    c->options = value ? bit_vec_from_int(value) : ALL_BITS;
    return c;
}

Board *
new_board(int values[81]) {
    // Expect cells with no value to be passed in with a value of 0
    Board *b = (Board *)malloc(sizeof(Board));
    int i;
    for (i = 0; i < NUMBER_OF_CELLS; i++) {
        b->cells[i] = new_cell(values[i]); 
    }

    return setup_metadata(b);
}

Board *
setup_metadata(Board *b) {
    int column, place, i, j;
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

    for (i = 0; i < 9; i++) {
        j = square_starts[i];
        int k = 0;
        int loop = j;
        for (loop = j; loop < j + 19; loop += 9) {
            b->squares[i][k++] = b->cells[loop];
            b->squares[i][k++] = b->cells[loop + 1];
            b->squares[i][k++] = b->cells[loop + 2];
        }
    }

    return b;
}

Board *
clone_board(Board *b) {
    Board *clone = (Board *)malloc(sizeof(Board));
    int i;
    for (i = 0; i < 81; i++) {
        clone->cells[i] = new_cell(int_from_bit_vec(b->cells[i]->options));
    }
    return setup_metadata(clone);
}

void
show_board(Board *b) {
    int i;
    for (i = 0; i < NUMBER_OF_CELLS; i++) {
        int value = int_from_bit_vec(b->cells[i]->options);
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
    c->options = bit_vec_from_int(value);
}

void
remove_cell_option(Cell *c, int option) {
    c->options &= (c->options ^option);
}

void
remove_options(Board *b, int group_type) {
    Cell *(*section)[9][9];
    if (group_type == 0) {
        section = &b->rows;
    } else if (group_type == 1) {
        section = &b->columns;
    } else {
        section = &b->squares;
    }

    int i, j, k;
    for (i = 0; i < 9; i++) {
        for (j = 0; j < 9; j++) {
            if (!int_from_bit_vec((* section)[i][j]->options)) {
                continue;
            }
            for (k = 0; k < 9; k++) {
                if (j == k) { continue; }
                remove_cell_option((* section)[i][k], (* section)[i][j]->options);
            }
        }
    }
}

void
remove_row_options(Board *b) {
    remove_options(b, 0);
}

void
remove_column_options(Board *b) {
    remove_options(b, 1);
}

void
remove_square_options(Board *b) {
    remove_options(b, 2);
}

void
clean_up(Board *b) {
    int i;
    for (i = 0; i < NUMBER_OF_CELLS; i++) {
        free(b->cells[i]);
    }
    free(b);
}

int
is_solved(Board *b) {

    int i, j;
    for (i = 0; i < 9; i++) {
        int row_vec = 0;
        int column_vec = 0;
        int square_vec = 0;
        for (j = 0; j < 9; j++) {
            if (!int_from_bit_vec(b->rows[i][j]->options)) {
                return 0;
            }
            row_vec ^= b->rows[i][j]->options;
            if (!int_from_bit_vec(b->columns[i][j]->options)) {
                return 0;
            }
            column_vec ^= b->columns[i][j]->options;
            if (!int_from_bit_vec(b->squares[i][j]->options)) {
                return 0;
            }
            square_vec ^= b->squares[i][j]->options;
        }
        if ((row_vec & column_vec & square_vec) != ALL_BITS) {
            return 0;
        }
    }
    return 1;
}

int
remove_only_avail(Board *b, int group_type) {

    Cell *(*section)[9][9];
    if (group_type == 0) {
        section = &b->rows;
    } else if (group_type == 1) {
        section = &b->columns;
    } else {
        section = &b->squares;
    }

    int changes = 0;
    int group, place, neighbor;
    for (group = 0; group < 9; group++) {
        for (place = 0; place < 9; place++) {
            if (int_from_bit_vec((*section)[group][place]->options)) {
                continue; // Skip this cell since the value is set.
            }
            int bit_vec = (*section)[group][place]->options;
            for (neighbor = 0; neighbor < 9; neighbor++) {
                if (neighbor == place) { continue; }
                bit_vec &= (bit_vec ^ (*section)[group][neighbor]->options);
            }
            int value = int_from_bit_vec(bit_vec);
            if (int_from_bit_vec(bit_vec)) {
                set_cell_value((*section)[group][place], value);
                changes += 1;
            }
        }
    }
    return changes;
}

int
row_only_avail(Board *b) {
    return remove_only_avail(b, 0);
}

int
col_only_avail(Board *b) {
    return remove_only_avail(b, 1);
}

int
square_only_avail(Board *b) {
    return remove_only_avail(b, 2);
}

void
simple_solve(Board *b) {
    int i = 0;
    int changes;
    while (1) {
        changes = 0;

        // Remove options based on set values.
        remove_row_options(b);
        remove_column_options(b);
        remove_square_options(b);

        // Set values based on options available to neighbors.
        changes += row_only_avail(b);
        changes += col_only_avail(b);
        changes += square_only_avail(b);

        if (is_solved(b)) {
            return;
        }

        if (changes == 0) {
            return;
        }
    }
}

int
bits_set(int n) {
    int mask;
    int set = 0;
    for (mask = 1; mask < 512; mask <<= 1) {
        if (n & mask) {set++;}
    }
    return set;
}

void
_solve(Board *b, int depth) {
    if (depth >= 5) {
        return;
    }

    simple_solve(b);
    if (!is_solved(b)) {
        int i;
        for (i = 0; i < 81; i++) {
            if (int_from_bit_vec(b->cells[i]->options)) {
                continue; // Value already set.
            }
            int bit_vec = b->cells[i]->options;
            if (bits_set(bit_vec) <= 6) { // Proceed if we have less than 6 options left.
                int mask;
                for (mask = 1; mask < 512; mask <<= 1) {
                    if (bit_vec & mask) {
                        Board *clone = clone_board(b);
                        set_cell_value(clone->cells[i], int_from_bit_vec(mask));
                        _solve(clone, depth + 1);
                        if (is_solved(clone)) {
                            clean_up(clone);
                            set_cell_value(b->cells[i], int_from_bit_vec(mask));
                            _solve(b, depth + 1);
                            return;
                        }
                        clean_up(clone);
                    }
                }
            }
        }
    }
}

void
solve(Board *b) {
    _solve(b, 1);
}

int
main() {

    int hardest[81] = { 8,0,0,0,0,0,0,0,0,
                        0,0,3,6,0,0,0,0,0,
                        0,7,0,0,9,0,2,0,0,
                        0,5,0,0,0,7,0,0,0,
                        0,0,0,0,4,5,7,0,0,
                        0,0,0,1,0,0,0,3,0,
                        0,0,1,0,0,0,0,6,8,
                        0,0,8,5,0,0,0,1,0,
                        0,9,0,0,0,0,4,0,0,
                      };

    int extreme[81] = { 5,0,0,0,0,0,8,0,0,
                        0,0,0,7,5,0,0,9,0,
                        0,2,9,0,0,6,0,7,0,
                        0,0,0,0,0,4,0,0,0,
                        4,0,0,0,3,0,6,2,5,
                        0,6,5,9,0,0,0,0,0,
                        9,0,0,0,4,0,0,0,3,
                        0,1,0,0,2,5,9,0,0,
                        0,0,2,0,0,0,0,0,8
                      };

    int hard[81] = { 0,0,0,0,7,0,1,0,0,
                     0,0,0,9,0,0,0,0,2,
                     3,4,0,0,0,8,0,0,0,
                     6,7,1,0,0,0,0,2,0,
                     0,0,5,0,1,0,9,0,0,
                     0,2,0,0,0,0,6,8,1,
                     0,0,0,6,0,0,0,4,9,
                     5,0,0,0,0,9,0,0,0,
                     0,0,6,0,8,0,0,0,0
                   };

    Board *b = new_board(hardest);
    show_board(b);
    printf("\n\n");

    solve(b);
    show_board(b);
    clean_up(b);

    return 0;
}
