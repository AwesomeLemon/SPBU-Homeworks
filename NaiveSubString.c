/* Realization of naive algorithm for finding how many times substring ocurres in string
               by Alexander Chebykin
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* scanStr(void) {
	int len1;
	char* c;
	printf("Enter length of your string\n");
	scanf("%d", &len1);
	c = (char*) malloc (len1 * sizeof(char));
	printf("Enter your string\n");
	scanf("%s", c);
	return c;
}

int howManyOcurrences(char* c, char* d) {
	int len1, len2;
	int i, j;
	int count = 0;
	len1 = strlen(c);
	len2 = strlen(d);
	if (len1 >= len2) {
		for (i=0; i < len1; i++) {
			int flag = 1;
			for (j=0; j < len2; j++) 
				if (c[i + j] != d[j]) {
					flag = 0;
					continue;
				}
			if (flag) count++;
		}
		return count;
	}
	printf("ERROR: String is longer than substring");
	return -1;
}
int main() {
	char *c, *d;
	int res;
	c = scanStr();  //main string
	d = scanStr();  //substring to be found
	res = howManyOcurrences(c,d);
	if (res != -1) printf("Substring was found %d times\n", res);
	return 0;
}
