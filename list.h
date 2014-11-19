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

node* getNewNode(vtype);
void printList(list*);
int removeValue(list*, vtype);
void pushFront (node **, vtype);
void pushBack(list*, vtype);
void pop(list*);
void clearExit(list*);
list* getNewList(void);
list* reverseList(list*);
