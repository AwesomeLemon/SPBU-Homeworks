/* File with mathematical operations for long numbers
          by Alexander Chebykin
*/
//P.S. There are A LOT of list reversions. It is so so that every function would take and give "little-endian" long numbers (in head of the list lies the most low-oreder digit)
//			e.g. if long Num==123 then list looks like 3->2->1
#include "longMath.h"
#define SUM_VARIABLE_NOT_EMPTY 1
#define SUM_VARIABLE_EMPTY 0
#define GARBAGE_BEGINNING_SAVED 2
#define LONG_NUM_EQUAL 2
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
	int flag = SUM_VARIABLE_EMPTY;
	node* del;
	curr1 = x.digits->head;
	curr2 = y.digits->head;
	if(((*z)->digits->len)) {
		// If len != 0 then we are probably trying to write (x+y) into (x). That's why we are cleaning it AFTER calculations are done.
		flag = SUM_VARIABLE_NOT_EMPTY;
	}
	if (x.sign == y.sign) {
		int overflow = 0;
		del = curr1;
		(*z)->sign = x.sign;
		while (curr1 && curr2) {
			int temp;
			temp = curr1->val + curr2->val + overflow;
			list_push_front(&((*z)->digits->head), temp % 10);
			(*z)->digits->len++;
			overflow = temp / 10;
			if (flag == SUM_VARIABLE_NOT_EMPTY) {
				del = (*z)->digits->head;
				flag = GARBAGE_BEGINNING_SAVED;
			}
			curr1 = curr1->next;
			curr2 = curr2->next;
		}
		while (curr1) {
			list_push_front(&((*z)->digits->head), (curr1->val + overflow) % 10);
			(*z)->digits->len++;
			overflow = (curr1->val + overflow) / 10;
			curr1 = curr1->next;
		}
		while (curr2) {
			list_push_front(&((*z)->digits->head), (curr2->val+overflow) % 10);
			(*z)->digits->len++;
			overflow = (curr2->val+overflow) / 10;
			curr2 = curr2->next;
		}
		if (overflow) {
			list_push_front(&((*z)->digits->head), overflow);
			(*z)->digits->len++;
		}
	}
	else {
		int bigger = longNum_is_first_longer(&x, &y);
		if (bigger == LONG_NUM_EQUAL) { //For equal numbers. To avoid final answer of '-0'
			(*z)->sign = 0;
			list_push_front(&((*z)->digits->head), 0);
			(*z)->digits->len++;
			if (flag == SUM_VARIABLE_NOT_EMPTY) {
				del = (*z)->digits->head;
				flag = GARBAGE_BEGINNING_SAVED;
			}
		}
		else {
			curr1 = x.digits->head;
			curr2 = y.digits->head;

			if (bigger) {
				(*z)->sign = x.sign;
				while (curr1 && curr2) {
					int temp = 0;
					temp = curr1->val - curr2->val;
					if (curr1->val < curr2->val) {
						temp += 10;
						curr1->next->val--;
					}
					list_push_front(&((*z)->digits->head), temp);
					(*z)->digits->len++;
					if (flag == SUM_VARIABLE_NOT_EMPTY) {
						del = (*z)->digits->head;
						flag = GARBAGE_BEGINNING_SAVED;
					}
					curr1 = curr1->next;
					curr2 = curr2->next;
				}
				while (curr1) {
					if (curr1->val >= 0) {
						list_push_front(&((*z)->digits->head), curr1->val);
						(*z)->digits->len++;
					}
					else {
						list_push_front(&((*z)->digits->head), curr1->val + 10);
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
					temp = curr2->val - curr1->val;
					if (curr2->val < curr1->val) {
						temp += 10;
						curr2->next->val--;
					}
					list_push_front(&((*z)->digits->head), temp);
					(*z)->digits->len++;
					if (flag == SUM_VARIABLE_NOT_EMPTY) {
						del = (*z)->digits->head;
						flag = GARBAGE_BEGINNING_SAVED;
					}
					curr1 = curr1->next;
					curr2 = curr2->next;
				}
				while (curr2) {
					if (curr2->val >= 0) {
						list_push_front(&((*z)->digits->head), curr2->val);
						(*z)->digits->len++;
					}
					else {
						list_push_front(&((*z)->digits->head), curr2->val + 10);
						(*z)->digits->len++;
						curr2->next->val--;
					}
					curr2 = curr2->next;
				}
			}
		}
	}
	if (flag == GARBAGE_BEGINNING_SAVED) {
		while (del->next) {
			list_remove_after(&del);
			(*z)->digits->len--;
		}
	}	
	list_reverse(&((*z)->digits));
}

void longNum_print(longNum x) {
	if (x.sign) printf("-");
	if (x.digits->head) {
		node* curr;
		int flag = 1; //to avoid printing leading zeroes
		list_reverse(&(x.digits));
		curr = x.digits->head;
		while (curr) {
			if (!(flag && !(curr->val))) {
				printf("%d",curr->val);
				flag = 0;
			}
			curr = curr->next;
		}
		if (flag) printf("0");
		list_reverse(&(x.digits));
	}
	else printf("The list is empty.");
}

int longNum_is_first_longer(longNum* x,longNum* y) {//SIGNS ARE NOT CONSIDERED!!!
		int bigger = LONG_NUM_EQUAL; //For equal numbers. To avoid final answer of '-0' in addition and subtraction
		node* curr1;
		node* curr2;
		if (x->digits->len > y->digits->len) bigger = 1;
		else {
			if (y->digits->len > x->digits->len) bigger = 0;
			else {
				list_reverse(&(x->digits));
				list_reverse(&(y->digits));
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
				list_reverse(&(x->digits));
				list_reverse(&(y->digits));
			}
		}
		return bigger;
}

void longNum_scan(longNum** x) {
	char c;
	*x = (longNum*) malloc(sizeof(longNum));
	if (!x) {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
	list_get_list(&((*x)->digits));
	(*x)->digits->len = 0;
	printf("Enter a number\n");
	scanf("%c", &c);
	if (c=='-') (*x)->sign = 1;
	else {
		(*x)->sign = 0;
		(*x)->digits->len++;
		list_push_front(&((*x)->digits->head), atoi(&c));
	}
	scanf("%c",&c);
	while (c != '\n') {
		(*x)->digits->len++;
		list_push_front(&((*x)->digits->head), atoi(&c));
		scanf("%c",&c);
	}
}

void longNum_mul(longNum x, longNum y, longNum** res) {
	int tempsign;
	int overflow = 0;
	(*res)->sign = 0;
	if (x.sign == y.sign) {
		tempsign = 0;
	}
	else {
		tempsign = 1;
	}
	x.sign = 0;
	y.sign = 0;

	if(longNum_is_first_longer(&x,&y)) {
		longNum_mul_kernel(x,y,res);
	}
	else {
		longNum_mul_kernel(y, x, res);
	}
	(*res)->sign = tempsign;
}

void longNum_mul_kernel(longNum x, longNum y, longNum** res) {
	int i = 0;
	int j;
	int len = y.digits->len;
	node* curr1 = x.digits->head;
	node* curr2 = y.digits->head;
	int overflow = 0;
	for (i; i < len; i++) {
		longNum t;
		int curMul = y.digits->head->val;
		list_get_list(&(t.digits));
		t.sign = 0;
		t.digits->len = 0;
		while (curr1) {
			int temp;
			temp = curr1->val * curMul + overflow;
			list_push_front(&(t.digits->head), temp % 10);
			t.digits->len++;
			overflow = temp / 10;
			curr1 = curr1->next;
		}
		if (overflow) {
			list_push_front(&(t.digits->head), overflow);
			t.digits->len++;
		}
		for (j = i; j > 0; j--) {
			list_push_back(t.digits, 0);
			t.digits->len++;
		}
		list_reverse(&(t.digits));
		longNum_add(**res, t, res);
		list_pop(y.digits);
		list_exit(t.digits);
		curr1 = x.digits->head;
		overflow = 0;
	}
}

void longNum_div(longNum x, longNum y, longNum** z) {
	int tempsign;
	int flag_lead_zeroes = 0;
	longNum* t = (longNum*) malloc(sizeof(longNum));
	node* curr1;
	node* curr2;
	int flag_sign = 0;
	if (!t) {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
	(*z)->sign = 0;
	t->sign = 0;
	if (!longNum_is_first_longer(&x,&y)) {
		if (x.sign && !y.sign) {
			list_push_back((*z)->digits, 1);
			(*z)->sign = 1;
		}
		else {
			if (x.sign && y.sign) {
				list_push_back((*z)->digits, 1);
				(*z)->sign = 0;
			}
			else {
				list_push_back((*z)->digits, 0);
			}
		}
		(*z)->digits->len++;
		return;
	}
	list_get_list(&(t->digits));
	if (x.sign) flag_sign = 1;//for mathematically right division (e.g. -9 / 4 should equal -3, not -2)
	if (x.sign == y.sign) {
		tempsign = 0;
	}
	else {
		tempsign = 1;
	}
	x.sign = 0;
	y.sign = 0;

	list_reverse(&(x.digits));

	//check if y == 0
	list_reverse(&(y.digits));
	if (!(y.digits->head->val)) {// We don't go in here in the calculator(because we've already checked it before), but I'll leave it in case this will be used as a separate library
		printf("Division by zero\n");
		exit(1);
	}
	list_reverse(&(y.digits));

	curr1 = x.digits->head;
	curr2 = y.digits->head;
	while (curr1) {
		list_push_front(&(t->digits->head), curr1->val);
		if ((t->digits->len == 1) && (!t->digits->head->next->val)) { //if t used to be == 0 on previous iteration, this 0 should be deleted now. Otherwise after pushing mistake will occcur: for example, if we push "3" t will equal "30"
			list_remove_after(&(t->digits->head));
			t->digits->len--;
		}
		t->digits->len++;
		if (longNum_is_first_longer(t,&y)) {
			int count = 0;
			flag_lead_zeroes = 1;
			while (longNum_is_first_longer(t,&y)) {
				longNum_sub(*t, y, &t);
				list_reverse(&(t->digits));
				while ((t->digits->head->val == 0) && (t->digits->len != 1)){
					list_pop(t->digits);
				}
				list_reverse(&(t->digits));
				count++;
			}
			list_push_front(&((*z)->digits->head), count);
			(*z)->digits->len++;
		}
		else {
			if (flag_lead_zeroes) {
				list_push_front(&((*z)->digits->head), 0);
				(*z)->digits->len++;
			}
		}
		curr1 = curr1->next;
	}
	//for mathematically right division (e.g. -9 / 4 should equal -3, not -2)
	if (flag_sign && !(t->digits->len == 1 && t->digits->head->val == 0)) {
		longNum* one = (longNum*) malloc(sizeof(longNum));
		list_get_list(&one->digits);
		one->sign = 0;
		list_push_back(one->digits, 1);
		longNum_add(**z, *one, z);
		longNum_exit(one);
	}
	(*z)->sign = tempsign;
	longNum_exit(t);
}

void longNum_exit(longNum* x) {
	list_exit(x->digits);
	free(x);
}

int longNum_scan_no_sign(longNum** x) {
	char c;
	*x = (longNum*) malloc(sizeof(longNum));
	if (!x) {
		printf("Error: Memory cannot be allocated. Exiting.\n");
		exit(1);
	}
	list_get_list(&((*x)->digits));
	(*x)->digits->len = 0;
	scanf("%c", &c);
	while ((c <= '9') && (c >= '0')) {
		(*x)->digits->len++;
		list_push_front(&((*x)->digits->head), atoi(&c));
		if (scanf("%c",&c) == EOF) {
			return 0;//Correctly written number
		}
	}
	if (c != '\n') {
		return 1;//Incorrectly written number
	}
	return 0;//Correctly written number
}