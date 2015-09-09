#include "stack.h"

void stack_calculate(stack* stack1) {
	char temp = ' ';
	char temp2 = ' ';
	int a,b;
	while ((temp != '=') && (temp2 != '=')) {
		scanf("%c",&temp);
		if (temp == ' ') continue;
		if ((temp <= '9') && (temp >= '0')){
			char s[MAX_INT_LEN] = "";
			int curr = 0;
			while ((temp <= '9') && (temp >= '0')) {
				s[curr] = temp;
				curr++;
				scanf("%c", &temp);
			}
			stack_push(&(stack1->head), atoi(s));
		}
		else {
			scanf("%c", &temp2);
			if ((temp2 <= '9') && (temp2 >= '0') && (temp == '-')) {
				char s[MAX_INT_LEN] = "";
				int curr = 0;
				temp = temp2;// we know that in temp lies '-', and it should not be considered a mathematic operation
				temp2 = ' ';
				while ((temp <= '9') && (temp >= '0')) {
					s[curr] = temp;
					curr++;
					scanf("%c", &temp);
				}
				stack_push(&(stack1->head), -atoi(s));
			}
		}
		stack_true_calculte(stack1, temp);
		stack_true_calculte(stack1, temp2);
	}
}

void stack_true_calculte(stack* stack1, char sign) {
	int a,b;
	switch (sign) {
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