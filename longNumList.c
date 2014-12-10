/* File with functions for singly linked list of long numbers
		by Alexander Chebykin
*/
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#include "longNumList.h"
#include <stdio.h>
void getNewNodeLong(valtype val, nodeLong** out) {
	*out = (nodeLong*) malloc(sizeof(nodeLong));
	if (*out) {
		(*out)->val = val;
		(*out)->next = 0;
	}
	else {
		printf("Error: Memory cannot be allocated. Exiting.");
		exit(0);
	}
}

void pushFrontLong(nodeLong **head, valtype val) {
    nodeLong *tmp;
	getNewNodeLong(val, &tmp);
    tmp->next = (*head);
    (*head) = tmp;
}

void popLong(listLong* list1) {
	if (list1->head) {
			nodeLong* t = list1->head->next;
			longNum_exit(&list1->head->val);
			list1->head = t;
			list1->len--;
	}
}
void clearExitLong(listLong* list1) {
	while (list1->len > 0) {
		popLong(list1);
	}
	free(list1->head);
	free(list1);
}

void getNewListLong(listLong** list1) {
	*list1 = (listLong*) malloc(sizeof(list));
	(*list1)->head = 0;
	(*list1)->len = 0;
}
