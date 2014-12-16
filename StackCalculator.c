/* Main file for stack calculator for long numbers
		by Alexander Chebykin
*/
#include "stack.h"

int main() {
    stack* stack1;
    char c;
    int flag=1;
	stack_get_new(&stack1);
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'q':
					stack_clear_exit(stack1);
                    flag = 0;
					break;
			   case 'c':
				   stack_start_to_calculate(stack1);
				   printLongNum(stack1->head->val);
				   stack_pop(stack1);
				   break;
		}
    }
	return 0;
}