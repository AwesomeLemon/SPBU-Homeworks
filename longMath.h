/* Header file with functions for long numbers
          by Alexander Chebykin
*/
#include "list.h"

typedef struct longNum {
	int sign;
	list* digits;
} longNum;

void longNum_add(longNum, longNum, longNum**);
void longNum_sub(longNum, longNum, longNum**);
void longNum_mul(longNum, longNum, longNum**);
void longNum_mul_kernel(longNum, longNum, longNum**);
void longNum_div(longNum, longNum, longNum**);
longNum longNum_neg(longNum);
void printLongNum(longNum);
int isLonger(longNum*, longNum*);
int isBigger(longNum, longNum);
void longNum_scan(longNum**);
void longNum_scan_no_sign(longNum**);
void longNum_exit(longNum*);