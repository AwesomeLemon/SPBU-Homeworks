/* File with functions for singly linked list of long numbers
		by Alexander Chebykin
*/
#include "longNumList.h"
void long_list_get_node(valtype val, nodeLong** out) {
	*out = (nodeLong*) malloc(sizeof(nodeLong));
	if (*out) {
		(*out)->val = val;
		(*out)->next = 0;
	}
	else {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
}

void long_list_push_front(nodeLong **head, valtype val) {
    nodeLong *tmp;
	long_list_get_node(val, &tmp);
    tmp->next = (*head);
    (*head) = tmp;
}

void long_list_pop(listLong* list1) {
	if (list1->head) {
			nodeLong* t = list1->head->next;
			longNum_exit(&list1->head->val);
			list1->head = t;
			list1->len--;
	}
}

void long_list_get_list(listLong** list1) {
	*list1 = (listLong*) malloc(sizeof(list));
	if (!list1) {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
	(*list1)->head = 0;
	(*list1)->len = 0;
}

void long_list_exit(listLong* list1) {
	while (list1->len > 0) {
		long_list_pop(list1);
	}
	free(list1->head);
	free(list1);
}