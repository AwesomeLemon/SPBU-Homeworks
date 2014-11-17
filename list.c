/* File with functions for one-way list
		by Alexander Chebykin
*/
#include "list.h"
#include <stdlib.h>
#include <stdio.h>

node* getnewNode(float val) {
	node *res = (node*) malloc(sizeof(node));
	if (res) {
		res->val = val;
		res->next = 0;
	}
	return res;
}

void printList(list* list1) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		list1->len=0;
		while (1) {
			printf("%f ",curr->val);
			list1->len++;
			if (curr->next) curr = curr->next;
			else break;
		}
		printf("\n");
	}
}

int pop(list* list1,float val) {
	if (list1->head) {
		node* curr = list1->head;
		if (curr->val == val) {
				popFirst(list1);
				return 1;
		}
		else { 
			while (curr->next != 0) {
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
/*void insert(node* el, float val) {
		node* newNode = getnewNode(val);
		if (newNode) {
			newNode->next=el->next;
			el->next = newNode;
		}
}*/

void push (node **head, float val) {
    node *tmp = getnewNode(val);
    tmp->next = (*head);
    (*head) = tmp;
}

void popFirst(list* list1) {
	if (list1->head) {
		node* t = list1->head->next;
		free(list1->head);
		list1->head = t;
	}
}
void clearExit(list* list1) {
	while (list1->head) {
		popFirst(list1);
	}
}