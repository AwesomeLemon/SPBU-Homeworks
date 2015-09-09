/* Header file with functions for stack calculator
		by Alexander Chebykin
*/
#include "longNumList.h"
#define stack listLong
#define stack_get_new long_list_get_list
#define stack_pop long_list_pop
#define stack_push long_list_push_front
#define stack_exit long_list_exit
//#define stack_print long_list_print_exit
void stack_start_to_calculate(stack*);
void stack_calculate(stack*, char);
char stack_scan_for_calc(stack*, char**);
void stack_print_exit(stack*);