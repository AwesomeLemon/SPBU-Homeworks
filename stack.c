#include "stack.h"

void stack_calculate(stack* stack1) {
	char temp = ' ';
	char temp2 = ' ';
	int a,b;
	while ((temp != '=') && (temp2 != '=')) {
		scanf("%c",&temp);
		if (temp == ' ') continue;
		if ((temp <= '9') && (temp >= '0')) {
			char c;
			longNum* x;
			c = longNum_scan_no_sign(&x);
			x->sign = 0;
			pushBack(x->digits, temp - '0');
			x->digits->len++;
			stack_push(&(stack1->head), *x);
			stack1->len++;
			temp = c;
			free(x);
		}
		else {
			scanf("%c", &temp2);
			if ((temp2 <= '9') && (temp2 >= '0') && (temp == '-')) {
				char c;
				longNum* x;
				temp = temp2;
				temp2 = ' ';
				c = longNum_scan_no_sign(&x);
				x->sign = 1;
				pushBack(x->digits, temp - '0');
				x->digits->len++;
				stack_push(&(stack1->head), *x);
				stack1->len++;
				temp = c;
				free(x);
			}
		}
		stack_true_calculte(stack1, temp);
		stack_true_calculte(stack1, temp2);
	}
}

void stack_true_calculte(stack* stack1, char sign) {
	longNum* t;
	switch (sign) {
		case '+':
			t = &(stack1->head->next->val);
			longNum_add(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			break;
		case '-':
			t = &(stack1->head->next->val);
			longNum_sub(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			break;
		case '*':
			t = (longNum*) malloc(sizeof(longNum));
			getNewList(&t->digits);
			longNum_mul(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			stack_pop(stack1);
			stack_push(&(stack1->head), *t);
			stack1->len++;
			free(t);
			break;
		case '/':
			if (!stack1->head->val.digits->head->val) {
				printf("DIVISION BY ZERO");
				stack_pop(stack1);
			}
			else {
				t = (longNum*) malloc(sizeof(longNum));
				getNewList(&t->digits);
				longNum_div(stack1->head->next->val, stack1->head->val, &t);
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head), *t);
				stack1->len++;
				free(t);
			}
	}
	//longNum_exit(t);
}