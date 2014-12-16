/* Header file with functions for singly linked list
		by Alexander Chebykin
*/
#include <stdlib.h>
#include <stdio.h>
#define vtype float
#define MAX_INT_LEN 10
typedef struct node {
	vtype val;
	struct node* next;
} node;

typedef struct list {
	struct node* head;
	int len;
} list;

void getNewNode(vtype, node**);
void printList(list*);
void removeValue(list*, vtype);
void pushFront (node **, vtype);
void pushBack(list*, vtype);
void pop(list*);
void clearExit(list*);
void getNewList(list**);
void reverseList(list**);
void removeAfter(node**);