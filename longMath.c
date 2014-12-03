/* File with functions for long numbers
          by Alexander Chebykin
*/
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#include "longMath.h"
#include <stdio.h>
#include <math.h>
longNum longNum_neg(longNum x) {
	x.sign = 1 - x.sign;
	return x;
}

longNum longNum_sub(longNum x, longNum y) {
	return longNum_add(x, longNum_neg(y));
}
//Little-endian
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
			z.digits->len++;
			overflow = temp / 10;
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			pushFront(&(z.digits->head), (curr1->val + overflow) % 10);
			z.digits->len++;
			overflow = (curr1->val + overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			pushFront(&(z.digits->head), (curr2->val+overflow) % 10);
			z.digits->len++;
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			pushFront(&(z.digits->head), overflow);
			z.digits->len++;
		}
	}
	else {
		int bigger = isLonger(&x, &y);
		if (bigger == 2) { //For equal numbers. To avoid final answer of '-0'
			z.sign = 0;
			pushFront(&(z.digits->head), 0);
			z.digits->len++;
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
						z.digits->len++;
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr1) {
					if (curr1->val >= 0) {
						pushFront(&(z.digits->head), curr1->val);
						z.digits->len++;
					}
					else {
						pushFront(&(z.digits->head), curr1->val + 10);
						z.digits->len++;
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
						z.digits->len++;
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr2) {
					pushFront(&(z.digits->head), curr2->val);
					z.digits->len++;
					curr2 = curr2->next;
				}
			}
		}
		
	}
	z.digits = reverseList(z.digits);
	return z;
}
//Big-endian
void printLongNum(longNum x) {
	if (x.sign) printf("-");
	if (x.digits->head) {
		node* curr;
		int flag = 1; //to avoid printing leading zeroes
		x.digits = reverseList(x.digits);
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
//Big-endian
int isLonger(longNum* x,longNum* y) {//SIGNS ARE NOT CONSIDERED!!!
		int bigger = 2; //For equal numbers. To avoid final answer of '-0' in addition and subtraction
		node* curr1;
		node* curr2;
		if (x->digits->len > y->digits->len) bigger = 1;
		else if (y->digits->len > x->digits->len) bigger = 0;
		else {
			x->digits = reverseList(x->digits);
			y->digits = reverseList(y->digits);
			curr1 = x->digits->head;
			curr2 = y->digits->head;
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
			x->digits = reverseList(x->digits);
			y->digits = reverseList(y->digits);
		}
		return bigger;
}
//Little-endian
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
//Little-endian
longNum longNum_mul(longNum x, longNum y) {
	node* curr1;
	node* curr2;
	longNum res;
	int tempsign;
	int overflow = 0;
	curr1 = x.digits->head;
	curr2 = y.digits->head;
	res.digits = getNewList();
	pushFront(&(res.digits->head), 0);
	res.digits->len++;
	res.sign = 0;
	if (x.sign == y.sign) {
		tempsign = 0;
		x.sign = 0;
		y.sign = 0;
	}
	else {
		tempsign = 1;
		x.sign = 0;
		y.sign = 0;
	}
	if(isLonger(&x,&y)) {
		int i = 0;
		int j;
		int len = y.digits->len;
		for (i; i < len; i++) {
			longNum t;
			int curMul = y.digits->head->val;
			t.digits = getNewList();
			t.sign = 0;
			t.digits->len = 0;
			while (curr1) {
				int temp;
				temp = curr1->val * curMul + overflow;
				pushFront(&(t.digits->head), temp % 10);
				t.digits->len++;
				overflow = temp / 10;
				curr1 = curr1->next;
			}
			if (overflow) {
				pushFront(&(t.digits->head), overflow);
				t.digits->len++;
			}
			for (j = i; j > 0; j--) {
				pushBack(t.digits, 0);
				t.digits->len++;
			}
			t.digits = reverseList(t.digits);
			res = longNum_add(res, t);
			pop(y.digits);
			curr1 = x.digits->head;
		}
	}
	else {
		int i = 0;
		int j;
		int len = x.digits->len;
		for (i; i < len; i++) {
			longNum t;
			int curMul = x.digits->head->val;
			t.digits = getNewList();
			t.sign = 0;
			t.digits->len = 0;
			while (curr2) {
				int temp;
				temp = curr2->val * curMul + overflow;
				pushFront(&(t.digits->head), temp % 10);
				t.digits->len++;
				overflow = temp / 10;
				curr2 = curr2->next;
			}
			if (overflow) {
				pushFront(&(t.digits->head), overflow);
				t.digits->len++;
			}
			for (j = i; j > 0; j--) {
				pushBack(t.digits, 0);
				t.digits->len++;
			}
			t.digits = reverseList(t.digits);
			res = longNum_add(res, t);
			pop(x.digits);
			curr2 = y.digits->head;
		}
	}
//	res.digits = reverseList(res.digits);
	res.sign = tempsign;
	return res;
}
//Big-endian
int isBigger(longNum x,longNum y) {
		int bigger = 2; //For equal numbers. To avoid final answer of '-0' in addition and subtraction
		x.digits = reverseList(x.digits);
		y.digits = reverseList(y.digits);
		if (!(x.sign || y.sign)) { //both are positive
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
		}
		else {
			if(!x.sign && y.sign) bigger = 1;
			else 
				if (x.sign && !y.sign) bigger = 0;
				else {
					x.sign = 0;
					y.sign = 0;
					bigger = 1 - isBigger(x,y);
				}
		}
		return bigger;
}
//Big-endian
longNum longNum_div(longNum x, longNum y) {
	int tempsign;
	longNum res;
	longNum t;
	node* curr1;
	node* curr2;
	res.sign = 0;
	res.digits = getNewList();
	t.sign = 0;
	t.digits = getNewList();
	if (!isLonger(&x,&y)) {
		pushBack(res.digits, 0);
		res.digits->len++;
		return res;
	}
	if (x.sign == y.sign) {
		tempsign = 0;
		x.sign = 0;
		y.sign = 0;
	}
	else {
		tempsign = 1;
		x.sign = 0;
		y.sign = 0;
	}
	x.digits = reverseList(x.digits);

	//check if y == 0
	y.digits = reverseList(y.digits);
	if (!(y.digits->head->val)) {
		printf("DIVISION BY ZERO");
		exit(0);
	}
	y.digits = reverseList(y.digits);

	curr1 = x.digits->head;
	curr2 = y.digits->head;
	while (curr1) {
		pushFront(&(t.digits->head), curr1->val);
		t.digits->len++;
		if (isLonger(&t,&y)) {
			int count = 0;
			while (isLonger(&t,&y)) {
				t = longNum_sub(t,y);
				t.digits = reverseList(t.digits);
				while ((t.digits->head->val == 0) && (t.digits->len != 1)){
					pop(t.digits);
				}
				t.digits = reverseList(t.digits);
				count++;
			}
			pushFront(&(res.digits->head), count);
			res.digits->len++;
		}
		curr1 = curr1->next;
	}
	return res;
}