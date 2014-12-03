/* Header file with functions for long numbers
          by Alexander Chebykin
*/
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#include "list.h"

typedef struct longNum {
	int sign;
	list* digits;
} longNum;

longNum longNum_add(longNum, longNum);
longNum longNum_sub(longNum, longNum);
longNum longNum_mul(longNum, longNum);
longNum longNum_div(longNum, longNum);
longNum longNum_neg(longNum);
void printLongNum(longNum);
int isLonger(longNum*, longNum*);
int isBigger(longNum, longNum);
longNum* longNum_scan(void);
