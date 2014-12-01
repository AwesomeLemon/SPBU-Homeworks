#include <stdio.h>
#include <stdlib.h>
#include "longMath.h"

int main() {
	longNum* x = (longNum*) malloc(sizeof(longNum));
	longNum* y = (longNum*) malloc(sizeof(longNum));
	longNum z;
	int i;
	char sign;
	x->digits = getNewList();
	y->digits = getNewList();
	x->digits->len = 0;
	y->digits->len = 0;
	x = longNum_scan();
	y = longNum_scan();
	printf("Enter an operation you require\n");
	scanf("%c", &sign);
	switch (sign) {
		case '+':
			z = longNum_add(*x, *y);
			break;
		case '-':
			z = longNum_sub(*x, *y);
			break;
		case '*':
			z = longNum_mul(*x, *y);
			break;
		case '/':
			z = longNum_div(*x, *y);
	}
	printLongNum(z);
	system("pause");
	free(x);
	free(y);
	return 0;
}
