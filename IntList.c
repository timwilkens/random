/* 
*  The MIT License (MIT)
*
*  Copyright (c) 2014 Tim Wilkens
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  The above copyright notice and this permission notice shall be included in all
*  copies or substantial portions of the Software.
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*  SOFTWARE.
*/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int *elems;
  // Total allocated size. 1 based.
  uint32_t size;
  // Offset of most recently added item. 0 based.
  int32_t tick;
} IntList;

void
show_IntList(IntList *list);

IntList*
shrink_IntList(IntList *list);
IntList*
grow_IntList(IntList *list);

IntList*
new_IntList(int *elems, uint32_t size) {
    IntList *list = (IntList *)malloc(sizeof(IntList));
    list->elems = (int *)malloc(size * sizeof(int));
    memcpy(list->elems, elems, size * sizeof(int));
    list->size = size;
    list->tick = size - 1; 
    return list;
}

void
destroy_IntList(IntList *list) {
    free(list->elems);
    free(list);
}

void
push_IntList(IntList *list, int i) {
    // Grow only when full. Double in size.
    if (list->tick + 1 >= list->size) {
        list = grow_IntList(list);
    }
    *(list->elems + ++list->tick) = i;
}

int
pop_IntList(IntList *list) {
    if (list->tick < 0) {
        printf("Attempt to pop from empty list!\n");
        destroy_IntList(list);
        exit(1);
    }

    // Shrink by half when we are less than 25% full.
    // Invariant: between 25% and 100% full.
    if ((list->size - list->tick) > (list->size * 0.75)) {
        list = shrink_IntList(list);
    }
    return *(list->elems + list->tick--);
}

IntList*
grow_IntList(IntList *list) {
    uint32_t new_size = list->size == 0 ? 1 : list->size * 2;
    int *tmp = (int *)malloc(new_size * sizeof(int));
    memcpy(tmp, list->elems, list->size * sizeof(int));
    free(list->elems);
    list->elems = tmp;
    list->size = new_size;
    return list;
}

IntList*
shrink_IntList(IntList *list) {
    uint32_t new_size = list->size / 2;
    int *tmp = (int *)malloc(new_size * sizeof(int));
    memcpy(tmp, list->elems, new_size * sizeof(int));
    free(list->elems);
    list->elems = tmp;
    list->size = new_size;
    return list;
}

void
show_IntList(IntList *list) {
    printf("\n");
    printf("SIZE: %d\n", list->size);
    printf("CAPACITY: %d\n", list->tick);
    printf("ITEMS: %p\n", &list->elems);
    for (int i = 0; i <= list->tick; i++) {
        printf("[%d] =  %d\n", i, *(list->elems + i));
    }
    printf("\n");
}

int
main() {
    IntList *list = new_IntList(NULL, 0);
    for (int i = 0; i < 100; i++) {
        push_IntList(list, i);
    }

    for (int i = 99; i >= 0; i--) {
        printf("%d\n", pop_IntList(list));
    }

    push_IntList(list, 12345);
    printf("%d\n", pop_IntList(list));

    destroy_IntList(list);
    return 0;
}
