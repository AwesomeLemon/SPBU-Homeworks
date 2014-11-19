#include "list.h"

typedef struct longNum {
	int sign;
	list* digits;
} longNum;

longNum longNum_add(longNum, longNum);
longNum longNum_sub(longNum, longNum);
longNum longNum_neg(longNum);
void printLongNum(longNum);