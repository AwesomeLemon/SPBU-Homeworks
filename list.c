/* File with functions for singly linked list
		by Alexander Chebykin
*/
#include "list.h"
#include <stdlib.h>
#include <stdio.h>
node* getNewNode(vtype val) {
	node *res = (node*) malloc(sizeof(node));
	if (res) {
		res->val = val;
		res->next = 0;
	}
	else {
		printf("Error: Memory cannot be allocated. Exiting.");
		exit(0);
	}
	return res;
}

void printList(list* list1) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		while (curr) {
			//printf("%f ",curr->val);
			printf("%d ",curr->val);
			curr = curr->next;
		}
		printf("\n");
	}
	else printf("The list is empty.");
}

int removeValue(list* list1, vtype val) {
	if (list1->head) {
		node* curr = list1->head;
		if (curr->val == val) {
				pop(list1);
				return 1;
		}
		else { 
			while (curr->next) {
				  if (curr->next->val != val)
					  curr = curr->next;
				  else {
					   node* t = curr->next->next;
					   free(curr->next);
					   curr->next = t;
					   return 1;
				  }
			}
			printf("Such an element doesn't exist!");
			return 0;
		}
	}
	else {
		printf("You're trying to break my program! The list is empty!\n");
		return 0;
	}
}
/*void insert(node* el, vtype val) {
		node* newNode = getNewNode(val);
		if (newNode) {
			newNode->next=el->next;
			el->next = newNode;
		}
}*/

void pushFront (node **head, vtype val) {
    node *tmp = getNewNode(val);
    tmp->next = (*head);
    (*head) = tmp;
}

void pushBack(list* list1, vtype val) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		while (curr->next) {
			curr = curr->next;
		}
		curr->next = getNewNode(val);
	}
	else {
		pushFront(&(list1->head), val);
	}
}

void pop(list* list1) {
	if (list1->head) {
		node* t = list1->head->next;
		free(list1->head);
		list1->head = t;
	}
}
void clearExit(list* list1) {
	while (list1->head) {
		pop(list1);
	}
}

list* getNewList(void) {
	list* list1 = (list*) malloc(sizeof(list));
	list1->head = 0;
	return list1;
}

list* reverseList(list* list1) {
	int i;
	list* reverse = getNewList();
	for (i=0; i < list1->len; i++) {
		pushFront(&(reverse->head), list1->head->val);
		pop(list1);
	}
	reverse->len = list1->len;
	return reverse;
}
