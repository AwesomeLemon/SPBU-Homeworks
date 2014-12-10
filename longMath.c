/* File with mathematical operations for long numbers
          by Alexander Chebykin
*/
#include <stdlib.h>
#include "longMath.h"
#include <stdio.h>

longNum longNum_neg(longNum x) {
	x.sign = 1 - x.sign;
	return x;
}

void longNum_sub(longNum x, longNum y, longNum** z) {
	longNum_add(x, longNum_neg(y), z);
}

void longNum_add(longNum x, longNum y, longNum** z) {
	node* curr1;
	node* curr2;
	int flag = 0;
	node* del;
	curr1 = x.digits->head;
	curr2 = y.digits->head;
	if(((*z)->digits->len)) {
		// If len != 0 then we are probably trying to write (x+y) into (x). That's why we are cleaning it AFTER calculations are done.
		flag = 1;
	}
	if (x.sign == y.sign) {
		node* del = curr1;
		int overflow = 0;
		(*z)->sign = x.sign;
		while (curr1 && curr2) {
			int temp;
			temp = curr1->val + curr2->val + overflow;
			pushFront(&((*z)->digits->head), temp % 10);
			(*z)->digits->len++;
			overflow = temp / 10;
			if (flag == 1) {
				del = (*z)->digits->head;
				flag = 2;
			}
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			pushFront(&((*z)->digits->head), (curr1->val + overflow) % 10);
			(*z)->digits->len++;
			overflow = (curr1->val + overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			pushFront(&((*z)->digits->head), (curr2->val+overflow) % 10);
			(*z)->digits->len++;
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			pushFront(&((*z)->digits->head), overflow);
			(*z)->digits->len++;
		}
		if (flag) {// At this poit flag is either 0 or 2
			while (del->next) {
				removeAfter(&del);
				(*z)->digits->len--;
			}
		}
	}
	else {
		int bigger = isLonger(&x, &y);
		if (bigger == 2) { //For equal numbers. To avoid final answer of '-0'
			(*z)->sign = 0;
			pushFront(&((*z)->digits->head), 0);
			(*z)->digits->len++;
			if (flag == 1) {
				del = (*z)->digits->head;
				flag = 2;
			}
		}
		else {
			curr1 = x.digits->head;
			curr2 = y.digits->head;

			if (bigger) {
				(*z)->sign = x.sign;
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
						pushFront(&((*z)->digits->head), temp);
						(*z)->digits->len++;
						if (flag == 1) {
							del = (*z)->digits->head;
							flag = 2;
						}
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr1) {
					if (curr1->val >= 0) {
						pushFront(&((*z)->digits->head), curr1->val);
						(*z)->digits->len++;
					}
					else {
						pushFront(&((*z)->digits->head), curr1->val + 10);
						(*z)->digits->len++;
						curr1->next->val--;
					}
					curr1 = curr1->next;
				}
			}
			else {
				(*z)->sign = y.sign;
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
						pushFront(&((*z)->digits->head),temp);
						(*z)->digits->len++;
						if (flag == 1) {
							del = (*z)->digits->head;
							flag = 2;
						}
						curr1 = curr1->next;
						curr2 = curr2->next;
				}
				while (curr2) {
					pushFront(&((*z)->digits->head), curr2->val);
					(*z)->digits->len++;
					curr2 = curr2->next;
				}
			}
		}
		if (flag) {
			while (del->next) {
				removeAfter(&del);
				(*z)->digits->len--;
			}
		}
	}	
	reverseList(&((*z)->digits));
}

void printLongNum(longNum x) {
	if (x.sign) printf("-");
	if (x.digits->head) {
		node* curr;
		int flag = 1; //to avoid printing leading zeroes
		reverseList(&(x.digits));
		curr = x.digits->head;
		while (curr) {
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

int isLonger(longNum* x,longNum* y) {//SIGNS ARE NOT CONSIDERED!!!
		int bigger = 2; //For equal numbers. To avoid final answer of '-0' in addition and subtraction
		node* curr1;
		node* curr2;
		if (x->digits->len > y->digits->len) bigger = 1;
		else if (y->digits->len > x->digits->len) bigger = 0;
			else {
				reverseList(&(x->digits));
				reverseList(&(y->digits));
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
				reverseList(&(x->digits));
				reverseList(&(y->digits));
			}
		return bigger;
}

void longNum_scan(longNum** x) {
	char c;
	*x = (longNum*) malloc(sizeof(longNum));
	getNewList(&((*x)->digits));
	(*x)->digits->len = 0;
	printf("Enter a number\n");
	scanf("%c", &c);
	if (c=='-') (*x)->sign = 1;
	else {
		(*x)->sign = 0;
		(*x)->digits->len++;
		pushFront(&((*x)->digits->head), atoi(&c));
	}
	scanf("%c",&c);
	while (c != '\n') {
		(*x)->digits->len++;
		pushFront(&((*x)->digits->head), atoi(&c));
		scanf("%c",&c);
	}
}

void longNum_mul(longNum x, longNum y, longNum** res) {
	node* curr1;
	node* curr2;
	int tempsign;
	int overflow = 0;
	(*res)->sign = 0;
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
		curr1 = x.digits->head;
		curr2 = y.digits->head;
		for (i; i < len; i++) {
			longNum t;
			int curMul = y.digits->head->val;
			getNewList(&(t.digits));
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
			reverseList(&(t.digits));
			longNum_add(**res, t, res);
			pop(y.digits);
			clearExit(t.digits);
			curr1 = x.digits->head;
			overflow = 0;
		}
	}
	else {
		int i = 0;
		int j;
		int len = x.digits->len;
		curr1 = x.digits->head;
		curr2 = y.digits->head;
		for (i; i < len; i++) {
			longNum t;
			int curMul = x.digits->head->val;
			getNewList(&(t.digits));
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
			reverseList(&(t.digits));
			longNum_add(**res, t, res);
			pop(x.digits);
			clearExit(t.digits);
			curr2 = y.digits->head;
			overflow = 0;
		}
	}
	(*res)->sign = tempsign;
}

/*int isBigger(longNum x,longNum y) {
		int bigger = 2; //For equal numbers. To avoid final answer of '-0' in addition and subtraction
		reverseList(&(x.digits));
		reverseList(&(y.digits));
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
*/
void longNum_div(longNum x, longNum y, longNum** z) {
	int tempsign;
	longNum res;
	longNum* t = (longNum*) malloc(sizeof(longNum));
	node* curr1;
	node* curr2;
	int flag = 0;
	(*z)->sign = 0;
	t->sign = 0;
	getNewList(&(t->digits));
	if (!isLonger(&x,&y)) {
		pushBack((*z)->digits, 0);
		(*z)->digits->len++;
		longNum_exit(t);
		return;
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
	reverseList(&(x.digits));

	//check if y == 0
	reverseList(&(y.digits));
	if (!(y.digits->head->val)) {
		printf("DIVISION BY ZERO");
		exit(0);
	}
	reverseList(&(y.digits));

	curr1 = x.digits->head;
	curr2 = y.digits->head;
	while (curr1) {
		pushFront(&(t->digits->head), curr1->val);
		if ((t->digits->len == 1) && (!t->digits->head->next->val)) { //if t used to be == 0 on previous iteration, this 0 should be deleted now. Otherwise after pushing mistake will occcur: for example, if we push "3" t will equal "30"
			removeAfter(&(t->digits->head));
			t->digits->len--;
		}
		t->digits->len++;
		if (isLonger(t,&y)) {
			int count = 0;
			flag = 1;
			while (isLonger(t,&y)) {
				longNum_sub(*t, y, &t);
				reverseList(&(t->digits));
				while ((t->digits->head->val == 0) && (t->digits->len != 1)){
					pop(t->digits);
				}
				reverseList(&(t->digits));
				count++;
			}
			pushFront(&((*z)->digits->head), count);
			(*z)->digits->len++;
		}
		else {
			if (flag) {
				pushFront(&((*z)->digits->head), 0);
				(*z)->digits->len++;
			}
		}
		curr1 = curr1->next;
	}

	//
	//longNum* test = (longNum*) malloc(sizeof(longNum));
	//if (z+1)*y < x togda dobvlyam 0 k z;
	(*z)->sign = tempsign;
	longNum_exit(t);
}

void longNum_exit(longNum* x) {
	clearExit(x->digits);
	free(x);
}

char longNum_scan_no_sign(longNum** x) {
	char c;
	*x = (longNum*) malloc(sizeof(longNum));
	getNewList(&((*x)->digits));
	(*x)->digits->len = 0;
	scanf("%c", &c);
	while ((c <= '9') && (c >= '0')) {
		(*x)->digits->len++;
		pushFront(&((*x)->digits->head), atoi(&c));
		scanf("%c",&c);
	}
	return c;// so we'll not lose last scanned symbol
}