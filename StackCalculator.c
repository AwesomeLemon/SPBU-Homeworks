/* Main file for stack calculator for integers
		by Alexander Chebykin
*/
//#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
//#include <crtdbg.h>
#include <stdio.h>
#include "stack.h"
//#include "longMath.h"
//#include "longNumList.h"

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
				   stack_calculate(stack1);
				   printLongNum(stack1->head->val);
				   stack_pop(stack1);
				   break;
		}
    }
	//_CrtDumpMemoryLeaks();
	return 0;
}
