/* File with functions for stack calculator
		by Alexander Chebykin
*/
#include "stack.h"

void stack_start_to_calculate(stack* stack1) {
	char temp = ' ';
	char temp2 = ' ';
	char* address; // We only need it because C doesn't allow to take address of address in one operation (I, at least, cannot do &&(char) but I want to have char**)
	while ((temp != '=') && (temp2 != '=')) {
		temp2 = ' ';
		address = &temp2;
		temp = stack_scan_for_calc(stack1, &address);// there's nothing before temp, so we give address of a ' '
		if (temp == '-') {
			address = &temp;
			temp2 = stack_scan_for_calc(stack1, &address);
		}
		stack_calculate(stack1, temp);
		stack_calculate(stack1, temp2);
	}
}

void stack_calculate(stack* stack1, char sign) {
	longNum* t;
	switch (sign) {
		case '+':
			t = &(stack1->head->next->val);
			longNum_add(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			break;
		case '-':
			t = &(stack1->head->next->val);
			longNum_sub(stack1->head->next->val, stack1->head->val, &t);
			stack_pop(stack1);
			break;
		case '*':
			t = (longNum*) malloc(sizeof(longNum));
			if (!t) {
				printf("Error: Memory cannot be allocated. Exiting.");
				exit(0);
			}
			list_get_list(&t->digits);
			longNum_mul(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			stack_pop(stack1);
			stack_push(&(stack1->head), *t);
			stack1->len++;
			free(t);
			break;
		case '/':
			//Here we check if stack1->head->val == 0
			list_reverse(&(stack1->head->val.digits));
			if (!stack1->head->val.digits->head->val) {
				printf("DIVISION BY ZERO");
				exit(0);
			}
			
			else {
				list_reverse(&(stack1->head->val.digits));
				t = (longNum*) malloc(sizeof(longNum));
				if (!t) {
					printf("Error: Memory cannot be allocated. Exiting.");
					exit(0);
				}
				list_get_list(&t->digits);
				longNum_div(stack1->head->next->val, stack1->head->val, &t);
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head), *t);
				stack1->len++;
				free(t);
			}
	}
}

char stack_scan_for_calc(stack* stack1, char **previous) {
	char temp;
	scanf("%c",&temp);
	if ((temp <= '9') && (temp >= '0')) {
		longNum* x;
		longNum_scan_no_sign(&x);
		x->sign = 0;
		if (**previous == '-') {//if after '-' goes number, it means that number is negative and '-' should not be considered mathematical operation
			x->sign = 1;
			**previous = ' ';
		}
		list_push_back(x->digits, temp - '0');
		x->digits->len++;
		stack_push(&(stack1->head), *x);
		stack1->len++;
		free(x);
	}
	return temp;
}