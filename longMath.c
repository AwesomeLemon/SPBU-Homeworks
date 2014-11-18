#include "longMath.h"
#include <stdlib.h>
#include <stdio.h>

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
		z.sign = x.sign;
		while (curr1 && curr2) {
			int temp = curr1->val+curr2->val+overflow;
			push(&(z.digits->head),temp%10);
			overflow = temp /10;
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			push(&(z.digits->head),(curr1->val+overflow)% 10);
			overflow = (curr1->val+overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			push(&(z.digits->head),(curr2->val+overflow)% 10);
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			push(&(z.digits->head),overflow);
		}
		return z;
	}
	/*else {
		int bigger;
		while (curr1 && curr2) {
			if (curr1->val < curr2->val) {*/
}