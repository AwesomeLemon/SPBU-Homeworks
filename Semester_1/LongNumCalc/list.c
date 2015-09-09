/* File with functions for singly linked list
		by Alexander Chebykin
*/
#include "list.h"
void list_get_node(vtype val, node** out) {
	*out = (node*) malloc(sizeof(node));
	if (*out) {
		(*out)->val = val;
		(*out)->next = 0;
	}
	else {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
}

void list_print(list* list1) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		while (curr) {
			printf("%d ",curr->val);
			curr = curr->next;
		}
		printf("\n");
	}
	else printf("The list is empty.");
}

void list_remove_value(list* list1, vtype val) {
	if (list1->head) {
		node* curr = list1->head;
		if (curr->val == val) {
				list_pop(list1);
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

void list_push_front(node **head, vtype val) {
    node *tmp;
	list_get_node(val, &tmp);
    tmp->next = (*head);
    (*head) = tmp;
}

void list_push_back(list* list1, vtype val) {
	if (list1->head) {
		node* curr;
		curr = list1->head;
		while (curr->next) {
			curr = curr->next;
		}
		list_get_node(val, &(curr->next));
	}
	else {
		list_push_front(&(list1->head), val);
	}
}

void list_pop(list* list1) {
	if (list1->head) {
			node* t = list1->head->next;
			free(list1->head);
			list1->head = t;
			list1->len--;
	}
}
void list_exit(list* list1) {
	while (list1->len > 0) {
		list_pop(list1);
	}
	free(list1->head);
	free(list1);
}

void list_get_list(list** list1) {
	*list1 = (list*) malloc(sizeof(list));
	if (!list1) {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
	(*list1)->head = 0;
	(*list1)->len = 0;
}

void list_reverse(list** list1) {
	int i;
	node* curr = (*list1)->head;
	int len = (*list1)->len;
	for (i=0; i < len-1; i++) {
		node* temp = curr->next->next;
		list_push_front(&((*list1)->head), curr->next->val);
		free(curr->next);
		curr->next = temp;
	}
}

void list_remove_after(node** del) {
	if ((*del)->next) {
		node* temp = (*del)->next->next;
		free((*del)->next);
		(*del)->next = temp;
	}
}