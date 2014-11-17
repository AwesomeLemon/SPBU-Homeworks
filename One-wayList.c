/* Main file for one-way list
		by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
#include "list.h"

int main() {
    list* list1 = (list*) malloc(sizeof(list));
    char c;
    float val;
    int flag=1;
	list1->head=0;
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'a':
                    scanf("%f",&val);
					push(&(list1->head),val);
                    break;
               case 'r':
                    scanf("%f",&val);
                    pop(list1,val);
                    break;
               case 'p':
					printList(list1);
                    break;
               case 'q':
                    flag = 0;
                    }
    }
	return 0;
}
