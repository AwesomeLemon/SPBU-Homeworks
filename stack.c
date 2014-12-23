/* File with functions for stack calculator
		by Alexander Chebykin
*/
#include "stack.h"

void stack_start_to_calculate(stack* stack1) {
	char temp = '\n';
	char temp2 = '\n';
	char* address; 
	address = &temp2;
	temp = stack_scan_for_calc(stack1, &address);// there's nothing before temp, so we give address of a '\n'.
	if (temp == '-') {
		address = &temp;
		temp2 = stack_scan_for_calc(stack1, &address);//temp2 always equals '\n', so we shouldn't calculate it
	}
	stack_calculate(stack1, temp);
}

void stack_calculate(stack* stack1, char sign) {
	//we can enter the function with "*-+/"(then we calculate), "\n"(we check that it isn't an operation and continue) or unknown symbol(then we post error)
	longNum* t;
	switch (sign) {
		case '+':
			if (stack1->len < 2) {
				printf("Not enough arguments\n");
				stack_exit(stack1);
				exit(1);
			}
			t = &(stack1->head->next->val);
			longNum_add(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			break;
		case '-':
			if (stack1->len < 2) {
				printf("Not enough arguments\n");
				stack_exit(stack1);
				exit(1);
			}
			t = &(stack1->head->next->val);
			longNum_sub(stack1->head->val, stack1->head->next->val, &t);
			stack_pop(stack1);
			break;
		case '*':
			if (stack1->len < 2) {
				printf("Not enough arguments\n");
				stack_exit(stack1);
				exit(1);
			}
			t = (longNum*) malloc(sizeof(longNum));
			if (!t) {
				printf("Error: Memory cannot be allocated. Exiting.\n");
				exit(1);
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
			if (stack1->len < 2) {
				printf("Not enough arguments\n");
				stack_exit(stack1);
				exit(1);
			}
			//Here we check if stack1->head->val == 0
			list_reverse(&(stack1->head->next->val.digits));
			if (!stack1->head->next->val.digits->head->val) {
				printf("Division by zero\n");
				stack_exit(stack1);
				exit(1);
			}
			
			else {
				list_reverse(&(stack1->head->next->val.digits));
				t = (longNum*) malloc(sizeof(longNum));
				if (!t) {
					printf("Error: Memory cannot be allocated. Exiting.\n");
					exit(1);
				}
				list_get_list(&t->digits);
				longNum_div(stack1->head->val, stack1->head->next->val, &t);
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head), *t);
				stack1->len++;
				free(t);
			}
			break;
		case '=':
			if (stack1->len < 1) {
				printf("Not enough arguments\n");
				stack_exit(stack1);
				exit(1);
			}
			longNum_print(stack1->head->val);
			printf("\n");
			break;
		case '\n':
			break;
		default:
			printf("Unknown command\n");
			stack_exit(stack1);
			exit(1);
	}
}

char stack_scan_for_calc(stack* stack1, char **previous) {
	char temp;
	scanf("%c",&temp);
	if ((temp <= '9') && (temp >= '0')) {
		longNum* x;
		int flag;
		ungetc(temp, stdin);
		temp = '\n';
		flag = longNum_scan_no_sign(&x);
		if (flag) {
			printf("Unknown command\n");
			stack_exit(stack1);
			exit(1);
		}
		x->sign = 0;
		if (**previous == '-') {//if after '-' goes number, it means that number is negative and '-' should not be considered mathematical operation
			x->sign = 1;
			**previous = '\n';
		}
		stack_push(&(stack1->head), *x);
		stack1->len++;
		free(x);
	}
	return temp;
}

void stack_print_exit(stack* stack1) {
	printf("[");
	while (stack1->len > 0) {
		longNum_print(stack1->head->val);
		if(stack1->len != 1) printf("; ");
		stack_pop(stack1);
	}
	printf("]");
	free(stack1->head);
	free(stack1);
}