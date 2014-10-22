/* Bitwise Operations
    by Alexander Chebykin
*/


#include <stdio.h>
#include <stdlib.h>
double apow(int a, int n) {
    if (n >= 0) {
    	int res = 1;
    	while (n > 0) {
    		if (n & 1) res *= a;
    		a *= a;
    		n = n >> 1;
    	}
    	return (double) res;
    }
    else {
         return (1.0 / apow(a, -n));
    }
	
}

void from10to2(int a) {
	if (a) {
		from10to2(a >> 1);
		printf("%d",a & 1);
	}
}


int fitsbits(int x, int n) {
	int t=(1 << (n-1));
	return (!(x +  (~(((t + x) & ((1 << n) -1)) + (~ t + 1)) +1)   ));
}

int fsign(int x) {
	return ((x >> 31) | (!! x));
}
int main() {
	int res;

	/* //FSIGN
	int a;
	scanf("%d",&a);
	res = fsign(a);
	printf("%d", res);*/

	/* //FITSBITS
	int x, n;
	scanf("%d%d", &x, &n);
	res = fitsbits(x, n);
	printf("%d", res);*/

	/* //APOW
	int x, n;
	scanf("%d%d",&x, &n);
	double r = apow(x, n);
	printf("%llf", r);*/

	/* //FROM10TO2
	int a;
	scanf("%d", &a);
	from10to2(a);
	*/
	system("pause");
		return 0;
}
