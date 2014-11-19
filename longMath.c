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
			pushFront(&(z.digits->head), temp%10);
			overflow = temp / 10;
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			pushFront(&(z.digits->head),(curr1->val + overflow) % 10);
			overflow = (curr1->val + overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			pushFront(&(z.digits->head),(curr2->val+overflow)% 10);
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			pushFront(&(z.digits->head), overflow);
		}
	}
	else {
		int bigger;
		if (x.digits->len > y.digits->len) bigger = 1;
		else if (y.digits->len > x.digits->len) bigger=0;
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

		if (bigger) {
			z.sign = x.sign;
			while (curr1 && curr2) {
				int temp=0;
				if (curr1->val > curr2->val) {
					temp = curr1->val-curr2->val;
				}
				else 
					if (curr1->val < curr2->val) {
						temp = curr1->val-curr2->val + 10;
						curr1->next->val--;
					}
					pushFront(&(z.digits->head),temp);
					curr1 = curr1->next;
					curr2 = curr2->next;
			}
			while (curr1) {
				if (curr1->val >= 0) pushFront(&(z.digits->head),curr1->val);
				else {
					pushFront(&(z.digits->head),curr1->val+10);
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
				pushFront(&(z.digits->head),curr2->val);
				curr2 = curr2->next;
			}
		}
	}
	return z;
}

void printLongNum(longNum x) {
	if (x.sign) printf("-");
	else printf("+");
	printList(x.digits);
}