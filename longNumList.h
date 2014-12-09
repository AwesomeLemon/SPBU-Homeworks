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

void getNewNodeLong(valtype, nodeLong**);
void pushFrontLong(nodeLong **, valtype);
void popLong(listLong*);
void clearExitLong(listLong*);
void getNewListLong(listLong**);