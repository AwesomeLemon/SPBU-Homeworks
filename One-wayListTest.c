/* One-way list with basic operations
										by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
typedef struct node {
	float val;
	struct node* next;
} node;

typedef struct list {
	struct node* head;
	int len;
} list;

node* getnewNode(float val) {
	node *res = (node*) malloc(sizeof(node));
	if (res) {
		res->val = val;
		res->next = NULL;
	}
	return res;
}

void printList(list* list1) {
	node* curr;
	curr = list1->head;
	list1->len=0;
	while (curr) {
		printf("%f ",curr->val);
		curr = curr->next;
		list1->len++;
	}
	printf("\n");
}

int pop(list* list1,float val) {
	if (list1->head) {
		node* curr = list1->head;
		if (curr->val == val) {
				node* t = list1->head->next;
				free(list1->head);
				list1->head = t;   
				return 0;  
				}
		while (curr->next != 0) {
			  if (curr->next->val != val)
				  curr = curr->next;
			  else {
				   node* t = curr->next->next;
				   free(curr->next);
				   curr->next = t;
				   return 0;
				   }
			  }
		printf("Such an element doesn't exist!");
		return 1;
	}
	else 
		printf("You're trying to break my program! The list is empty!\n");
}
/*void insert(node* el, float val) {
		node* newNode = getnewNode(val);
		if (newNode) {
			newNode->next=el->next;
			el->next = newNode;
		}
}*/

void push (node **head, float val) {
    node *tmp = getnewNode(val);
    tmp->next = (*head);
    (*head) = tmp;
}
void sendAlot(int n) {
	freopen("in.txt","w+",stdout);
	int i=0;
	for (i; i<n; i++) printf("a %i.5\n",i);
	printf("p q");
	fclose(stdout);
}
int main() {
	list* list1 = (list*) malloc(sizeof(list));
    char c;
    float val;
    int flag=1;
	FILE* f;
	list1->head = getnewNode(0);
	pop(list1,0); //
	sendAlot(750000);
	freopen("in.txt","r",stdin);
	freopen("out.txt","w+",stdout);
    while (flag) {
        scanf("%c",&c);
        switch (c) {
               case 'a':
                    scanf("%f",&val);
					push(&(list1->head),val);
                    break;
               case 'r':
                    scanf("%f",&val);
                    pop(list1,val);
					list1->len--;
                    break;
               case 'p':
                    printList(list1);
                    break;
               case 'q':
                    flag = 0;
                    }
    }
	fclose(stdin);
	fclose(stdout);
	return 0;
}
