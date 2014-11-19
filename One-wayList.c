/* Main file for one-way list
		by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
#include "list.h"

int main() {
    list* list1 = getNewList();
    char c;
    float val;
    int flag=1;
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'a':
                    scanf("%f",&val);
					//pushFront(&(list1->head),val);
					pushBack(list1,val);
                    break;
               case 'r':
                    scanf("%f",&val);
                    removeValue(list1,val);
                    break;
               case 'p':
					printList(list1);
                    break;
               case 'q':
					clearExit(list1);
                    flag = 0;
                    }
    }
	return 0;
}
