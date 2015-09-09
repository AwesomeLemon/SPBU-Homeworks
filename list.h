/* Header file with functions for singly linked list
		by Alexander Chebykin
*/
#include <stdlib.h>
#include <stdio.h>
#define vtype int
#define MAX_INT_LEN 10
typedef struct node {
	vtype val;
	struct node* next;
} node;

typedef struct list {
	struct node* head;
	int len;
} list;

void list_get_node(vtype, node**);
void list_print(list*);
void list_remove_value(list*, vtype);
void list_push_front(node **, vtype);
void list_push_back(list*, vtype);
void list_pop(list*);
void list_exit(list*);
void list_get_list(list**);
void list_reverse(list**);
void list_remove_after(node**);