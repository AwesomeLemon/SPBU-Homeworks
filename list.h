/* Header file with functions for singly linked list
		by Alexander Chebykin
*/
#define vtype int
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
int removeValue(list*, vtype);
void pushFront (node **, vtype);
void pushBack(list*, vtype);
void pop(list*);
void clearExit(list*);
void getNewList(list**);
void reverseList(list**);
void removeAfter(node**);