/* Main file for stack calculator for integers
		by Alexander Chebykin
*/
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#include <stdio.h>
#include "list.h"

int main() {
    list* list1 = getNewList();
    char c;
    vtype val;
    int flag=1;
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'q':
					clearExit(list1);
                    flag = 0;
					break;
			   case 'c':
				   calculate(list1);
				   printList(list1);
				   pop(list1);
		}
    }
	free(list1);
	_CrtDumpMemoryLeaks();
	return 0;
}
