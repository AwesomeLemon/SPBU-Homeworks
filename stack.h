#include "list.h"
#include <stdlib.h>
#include <stdio.h>
#define stack list
#define stack_get_new getNewList
#define stack_pop pop
#define stack_push pushFront
#define stack_clear_exit clearExit
void stack_calculate(stack*);