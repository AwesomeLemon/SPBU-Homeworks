#include "stack.h"

void stack_calculate(stack* stack1) {
	char temp = ' ';
	int a,b;
	while (temp != '=') {
		scanf("%c", &temp);
		if ((temp <= '9') && (temp >= '0')) {
			char s[MAX_INT_LEN] = "";
			int curr = 0;
			while ((temp <= '9') && (temp >= '0')) {
				s[curr] = temp;
				curr++;
				scanf("%c", &temp);
			}
			stack_push(&(stack1->head), atoi(s));
		}
		switch (temp) {
			case '+':
				a = stack1->head->val;
				b = stack1->head->next->val;
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head), a + b);
				break;
			case '-':
				a = stack1->head->val;
				b = stack1->head->next->val;
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head) ,b - a);
				break;
			case '*':
				a = stack1->head->val;
				b = stack1->head->next->val;
				stack_pop(stack1);
				stack_pop(stack1);
				stack_push(&(stack1->head) ,b * a);
				break;
			case '/':
				a = stack1->head->val;
				b = stack1->head->next->val;
				stack_pop(stack1);
				stack_pop(stack1);
				if (!a) {
					printf("DIVISION BY ZERO");
					exit(0);
				}
				else
				stack_push(&(stack1->head) ,b / a);
		}
	}
}