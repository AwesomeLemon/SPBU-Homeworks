#include <stdio.h>
#include <stdlib.h>
#include "longMath.h"

int main() {
	longNum* x = (longNum*) malloc(sizeof(longNum));
	longNum* y = (longNum*) malloc(sizeof(longNum));
	longNum z;
	int i;
	int t;
	char flag=' ';
	char sign;
	x->digits = getNewList();
	y->digits = getNewList();
	x->digits->len = 0;
	y->digits->len = 0;

	printf("Enter first number by digits, separating them by space.\nSign (0 for positive and 1 for negative) goes first.\n");
	scanf("%d",&(x->sign));
	while (flag==' ') {
		x->digits->len++;
		scanf("%d%c",&t,&flag);
		pushFront(&(x->digits->head),t);
	}
	flag = ' ';

	printf("Enter second number by digits, separating them by space.\nSign (0 for positive and 1 for negative) goes first.\n");
	scanf("%d",&(y->sign));
	while (flag==' ') {
		y->digits->len++;
		scanf("%d%c",&t,&flag);
		pushFront(&(y->digits->head),t);
	}

	printf("Enter an operation you require\n");
	scanf("%c",&sign);
	switch (sign) {
		case '+':
			z = longNum_add(*x,*y);
			break;
		case '-':
			z = longNum_sub(*x,*y);
	}
	printLongNum(z);
	return 0;
}