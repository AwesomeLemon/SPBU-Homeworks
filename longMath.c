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
	int overflow;
	z.digits = getNewList();
	curr1 = x.digits->head;
	curr2 = y.digits->head;
	overflow = 0;
	if (x.sign == y.sign) {
		while (curr1 && curr2) {
			int temp;
			temp = curr1->val+curr2->val+overflow;
			pushBack(z.digits,temp%10);
			overflow = temp /10;
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			pushBack(z.digits,(curr1->val+overflow)% 10);
			overflow = (curr1->val+overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			pushBack(z.digits,(curr2->val+overflow)% 10);
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			pushBack(z.digits,overflow);
		}
	}
	else {
		int bigger=0;
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

		x.digits = reverseList(x.digits);
		y.digits = reverseList(y.digits);

		curr1 = x.digits->head;
		curr2 = y.digits->head;
		if (bigger) {
			z.sign = x.sign;
			while (curr1 && curr2) {
				int temp;
				if (curr1->val > curr2->val) {
					temp = curr1->val-curr2->val;
				}
				else 
					if (curr1->val < curr2->val) {
						temp = curr1->val-curr2->val + 10;
						curr1->next->val--;
					}
					else {
						curr1 = curr1->next;
						curr2 = curr2->next;
						continue;
					}
				pushFront(&(z.digits->head),temp);
				curr1 = curr1->next;
				curr2 = curr2->next;
			}
			while (curr1) {
				pushFront(&(z.digits->head),curr1->val);
				curr1 = curr1->next;
			}
		}
		else {
			z.sign = y.sign;
			while (curr1 && curr2) {
				int temp;
				if (curr2->val > curr1->val) {
					temp = curr2->val-curr1->val;
				}
				else 
					if (curr2->val < curr1->val) {
						temp = curr2->val - curr1->val + 10;
						curr2->next->val--;
					}
					else {
						curr1 = curr1->next;
						curr2 = curr2->next;
						continue;
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