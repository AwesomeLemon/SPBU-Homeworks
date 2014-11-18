/* Header file with functions for one-way list
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

node* getnewNode(vtype);
void printList(list*);
int pop(list* ,vtype);
void push (node **, vtype);
void popFirst(list*);
void clearExit(list*);
list* getNewList(void);