/* File with functions for long numbers
          by Alexander Chebykin
*/
#include "longMath.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
longNum longNum_neg(longNum x) {
	x.sign = 1 - x.sign;
	return x;
}

longNum longNum_sub(longNum x, longNum y) {
	return longNum_add(x, longNum_neg(y));
}

longNum longNum_add(longNum x, longNum y) {
	longNum z;
	node* curr1;
	node* curr2;
	z.digits = getNewList();
	curr1 = x.digits->head;
	curr2 = y.digits->head;
	if (x.sign == y.sign) {
		int overflow = 0;
		z.sign = x.sign;
		while (curr1 && curr2) {
			int temp;
			temp = curr1->val + curr2->val + overflow;
			pushFront(&(z.digits->head), temp % 10);
			overflow = temp / 10;
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			pushFront(&(z.digits->head), (curr1->val + overflow) % 10);
			overflow = (curr1->val + overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			pushFront(&(z.digits->head), (curr2->val+overflow) % 10);
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			pushFront(&(z.digits->head), overflow);
		}
	}
	else {
		int bigger = isBigger(x, y);
		if (bigger == 2) { //For equal numbers. To avoid final answer of '-0'
			z.sign = 0;
			pushFront(&(z.digits->head), 0);
			return z;
		}
		else {
			curr1 = x.digits->head;
			curr2 = y.digits->head;

			if (bigger) {
				z.sign = x.sign;
				while (curr1 && curr2) {
					int temp = 0;
					if (curr1->val > curr2->val) {
						temp = curr1->val-curr2->val;
					}
					else 
						if (curr1->val < curr2->val) {
							temp = curr1->val-curr2->val + 10;
							curr1->next->val--;
						}
						pushFront(&(z.digits->head), temp);
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr1) {
					if (curr1->val >= 0) pushFront(&(z.digits->head), curr1->val);
					else {
						pushFront(&(z.digits->head), curr1->val + 10);
						curr1->next->val--;
					}
					curr1 = curr1->next;
				}
			}
			else {
				z.sign = y.sign;
				while (curr1 && curr2) {
					int temp = 0;
					if (curr2->val > curr1->val) {
						temp = curr2->val-curr1->val;
					}
					else 
						if (curr2->val < curr1->val) {
							temp = curr2->val - curr1->val + 10;
							curr2->next->val--;
						}
						pushFront(&(z.digits->head),temp);
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr2) {
					pushFront(&(z.digits->head), curr2->val);
					curr2 = curr2->next;
				}
			}
		}
		return z;
	}
}

void printLongNum(longNum x) {
	if (x.sign) printf("-");
	if (x.digits->head) {
		node* curr;
		int flag = 1; //to avoid printing leading zeroes
		curr = x.digits->head;
		while (curr) {
			//printf("%f ",curr->val);
			if (!(flag && !(curr->val))) {
				printf("%d",curr->val);
				flag = 0;
			}
			curr = curr->next;
		}
		if (flag) printf("0");
		printf("\n");
	}
	else printf("The list is empty.");
}

int isBigger(longNum x,longNum y) {
		int bigger = 2; //For equal numbers. To avoid final answer of '-0'
		node* curr1 = x.digits->head;
		node* curr2 = y.digits->head;
		if (x.digits->len > y.digits->len) bigger = 1;
		else if (y.digits->len > x.digits->len) bigger = 0;
		else {
			while (curr1 && curr2) {
				if (curr1->val < curr2->val) {
					bigger=0;
					break;
				}
				else 
					if (curr1->val > curr2->val) {
						bigger=1;
						break;
					}
					else {
						curr1 = curr1->next;
						curr2 = curr2->next;
					}
			}
		}
		return bigger;
}

longNum* longNum_scan(void) {
	char c;
	longNum* x = (longNum*) malloc(sizeof(longNum));
	x->digits = getNewList();
	x->digits->len = 0;
	printf("Enter a number\n");
	scanf("%c", &c);
	if (c=='-') x->sign = 1;
	else {
		x->sign = 0;
		x->digits->len++;
		pushFront(&(x->digits->head), atoi(&c));
	}
	scanf("%c",&c);
	while (c != '\n') {
		x->digits->len++;
		pushFront(&(x->digits->head), atoi(&c));
		scanf("%c",&c);
	}
	return x;
}
