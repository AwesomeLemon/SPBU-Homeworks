#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#include <stdio.h>
#include <stdlib.h>
#include "longMath.h"

int main() {
	longNum* x;
	longNum* y;
	longNum* z;
	char sign;
	longNum_scan(&x);
	longNum_scan(&y);
	z = (longNum*) malloc(sizeof(longNum));
	getNewList(&(z->digits));
	printf("Enter an operation you require\n");
	scanf("%c", &sign);
	switch (sign) {
		case '+':
			longNum_add(*x, *y, &z);
			break;
		case '-':
			longNum_sub(*x, *y, &z);
			break;
		case '*':
			longNum_mul(*x, *y, &z);
			break;
		case '/':
			longNum_div(*x, *y, &z);
	}
	printLongNum(*z);
	longNum_exit(x);
	longNum_exit(y);
	longNum_exit(z);
	_CrtDumpMemoryLeaks();
	return 0;
}
