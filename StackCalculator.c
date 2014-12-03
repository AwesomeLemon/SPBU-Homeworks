/* Main file for one-way list
		by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
#include "list.h"

int main() {
    list* list1 = getNewList();
    char c;
    vtype val;
    int flag=1;
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'p':
					printList(list1);
                    break;
               case 'q':
					clearExit(list1);
                    flag = 0;
					break;
			   case 'c':
				   calculate(list1);
		}
    }
	return 0;
}
