/* Header file with functions for stack calculator
		by Alexander Chebykin
*/
#include "longNumList.h"
#include <stdlib.h>
#include <stdio.h>
#define stack listLong
#define stack_get_new getNewListLong
#define stack_pop popLong
#define stack_push pushFrontLong
#define stack_clear_exit clearExitLong
void stack_calculate(stack*);
void stack_true_calculte(stack*, char);