#include <stdio.h>
#include <stdlib.h>
typedef struct node {
	float val;
	struct node* next;
} node;

typedef struct list {
	struct node* head;
	int len;
} list;

node* getNewnode(float val) {
	node *res = (node*) malloc(sizeof(node));
	if (res) {
		res->val = val;
		res->next = 0;
	}
	return res;
}

void printList(list* list1) {
	node* curr;
	curr = list1->head;
	list1->len=0;
	while (curr) {
		printf("%f ",curr->val);
		curr = curr->next;
		list1->len++;
	}
	printf("\n");
}

int pop(list* list1,float val) {
	node* curr = list1->head;
	if (curr->val == val) {
            node* t = list1->head->next;
            free(list1->head);
            list1->head = t;   
            return 0;  
            }
	while (curr->next != 0) {
          if (curr->next->val != val)
              curr = curr->next;
          else {
               node* t = curr->next->next;
               free(curr->next);
               curr->next = t;
               return 0;
               }
          }
 printf("Such element doesn't exist!");
 return 1;
}
int main() {
	list* list1 = (list*) malloc(sizeof(list));
	//list1->head = getNewnode(2.7);
	//list1->head->next=getNewnode(-7.9);
	//getNewnod(2.3);
	//getNewnod(-0.1);
//	printList(list1);
//	pop(list1,2.7);
//	printList(list1);
    char c;
    float val;
    int flag=1;
    node *curr = list1->head;
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'a':
                    scanf("%f",&val);
                    curr = getNewnode(val);
                    curr = curr->next;
                    printList(list1);
                    break;
               case 'r':
                    scanf("%f",&val);
                    pop(list1,val);
                    printList(list1);
                    break;
               case 'p':
                    printList(list1);
                    break;
               case 'q':
                    flag = 0;
                    }
    }
	system("pause");
	return 0;
}
