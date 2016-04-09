/* Main file for stack calculator for long numbers
		by Alexander Chebykin
*/
#include "stack.h"

int main() {
    stack* stack1;
    char c;
	  stack_get_new(&stack1);
	  printf("Please enter expression to calculate using reverse polish notation,\
	          every number or operation sign in new line.\n");
	  
    while (scanf("%c",&c)!= EOF) {
        ungetc(c, stdin);
		    stack_start_to_calculate(stack1);
    }
	  stack_print_exit(stack1);
	  return 0;
}
