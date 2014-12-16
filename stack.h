/* Header file with functions for stack calculator
		by Alexander Chebykin
*/
#include "longNumList.h"
#define stack listLong
#define stack_get_new getNewListLong
#define stack_pop popLong
#define stack_push pushFrontLong
#define stack_clear_exit clearExitLong
void stack_start_to_calculate(stack*);
void stack_calculate(stack*, char);
char stack_scan_for_calc(stack*, char**);