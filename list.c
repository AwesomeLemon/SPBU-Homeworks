/* File with functions for singly linked list
		by Alexander Chebykin
*/
#include "list.h"
void getNewNode(vtype val, node** out) {
	*out = (node*) malloc(sizeof(node));
	if (*out) {
		(*out)->val = val;
		(*out)->next = 0;
	}
	else {
		printf("Error: Memory cannot be allocated. Exiting.");
		exit(0);
	}
}

void printList(list* list1) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		while (curr) {
			printf("%f ",curr->val);
			curr = curr->next;
		}
		printf("\n");
	}
	else printf("The list is empty.");
}

void removeValue(list* list1, vtype val) {
	if (list1->head) {
		node* curr = list1->head;
		if (curr->val == val) {
				pop(list1);
				return;
		}
		else { 
			while (curr->next) {
				  if (curr->next->val != val)
					  curr = curr->next;
				  else {
					   node* t = curr->next->next;
					   free(curr->next);
					   curr->next = t;
					   list1->len--;
					   return;
				  }
			}
			printf("Such an element doesn't exist!");
			return;
		}
	}
	else {
		printf("You're trying to break my program! The list is empty!\n");
		return;
	}
}

void pushFront (node **head, vtype val) {
    node *tmp;
	getNewNode(val, &tmp);
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
		getNewNode(val, &(curr->next));
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
			list1->len--;
	}
}
void clearExit(list* list1) {
	while (list1->len > 0) {
		pop(list1);
	}
	free(list1->head);
	free(list1);
}

void getNewList(list** list1) {
	*list1 = (list*) malloc(sizeof(list));
	if (!list1) {
		printf("Error: Memory cannot be allocated. Exiting.");
		exit(0);
	}
	(*list1)->head = 0;
	(*list1)->len = 0;
}

void reverseList(list** list1) {
	int i;
	node* curr = (*list1)->head;
	int len = (*list1)->len;
	for (i=0; i < len-1; i++) {
		node* temp = curr->next->next;
		pushFront(&((*list1)->head), curr->next->val);
		free(curr->next);
		curr->next = temp;
	}
}

void removeAfter(node** del) {
	if ((*del)->next) {
		node* temp = (*del)->next->next;
		free((*del)->next);
		(*del)->next = temp;
	}
}