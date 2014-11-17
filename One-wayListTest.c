/* One-way list with basic operations
										by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
#include "list.h"
void sendAlot(int n) {
	int i=0;
	freopen("in.txt","w+",stdout);
	for (i; i<n; i++) printf("a %i.5\n",i);
	for (i=0; i<n; i++) if (i%2) printf("r %i.5\n",i);
	printf("p q");
	fclose(stdout);
}

int main() {
	list* list1 = (list*) malloc(sizeof(list));
    char c;
    float val;
    int flag=1;
	list1->head=0;
	sendAlot(7500);
	freopen("in.txt","r",stdin);
	freopen("out.txt","w+",stdout);
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
	fclose(stdin);
	fclose(stdout);
	return 0;
}
