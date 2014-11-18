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
	x->sign=1;
	x->digits = getNewList();
	y->sign=1;
	y->digits = getNewList();
	printf("Enter first number by digits, separating them by space. Sign (0 for positive and 1 for negative) goes first.\n");
	scanf("%d",&(x->sign));
	while (flag==' ') {
		scanf("%d%c",&t,&flag);
		push(&(x->digits->head),t);
	}
	flag = ' ';
	printf("Enter second number by digits, separating them by space. Sign (0 for positive and 1 for negative) goes first.\n");
	scanf("%d",&(y->sign));
	while (flag==' ') {
		scanf("%d%c",&t,&flag);
		push(&(y->digits->head),t);
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
	printList(z.digits);
	system("pause");
	return 0;
}