/* Header file with functions for one-way list
		by Alexander Chebykin
*/

typedef struct node {
	float val;
	struct node* next;
} node;

typedef struct list {
	struct node* head;
	int len;
} list;

node* getnewNode(float);
void printList(list*);
int pop(list* ,float);
void push (node **, float);
void popFirst(list*);