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

void pop(list* list1,float val) {
	node* curr = list1->head;
	while ((curr->next != 0) && (curr->val != val)
int main() {
	list* list1 = (list*) malloc(sizeof(list));
	list1->head = getNewnode(2.7);
	list1->head->next=getNewnode(7.9);
	//getNewnod(2.3);
	//getNewnod(-0.1);
	printList(list1);
	return 0;
}
