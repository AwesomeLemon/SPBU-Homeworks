/* Header file with functions for singly linked list of long numbers
		by Alexander Chebykin
*/
#include "longMath.h"
#define valtype longNum
#define MAX_INT_LEN 10
typedef struct nodeLong {
	valtype val;
	struct nodeLong* next;
} nodeLong;

typedef struct listLong {
	struct nodeLong* head;
	int len;
} listLong;

void long_list_get_node(valtype, nodeLong**);
void long_list_push_front(nodeLong **, valtype);
void long_list_pop(listLong*);
void long_list_exit(listLong*);
void long_list_get_list(listLong**);