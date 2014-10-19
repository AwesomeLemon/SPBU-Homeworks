#include <stdio.h>
#include <stdlib.h>
double apow(int a,int n) {
    if (n >= 0) {
    	int res=1;
    	while (n>0) {
    		if (n&1) res *=a;
    		a*=a;
    		n = n >>1;
    	}
    	return (double) res;
    }
    else {
         return (1.0/apow(a,-n));
    }
	
}
int fsign(int const * x) {
	return (((*x)>>31) | (!!(*x)));
}
int fitsbits(int const * x,int const * n) {
	int t=(1<<((*n)-1));
	return (!((*x)+  (~(((t + (*x)) & ((1 << (*n)) -1)) + (~t+1)) +1)   ));
}
void from10to2(int a) {
	if (a) {
		from10to2(a>>1);
		printf("%d",a&1);
	}
}
int main() {
	/*int a,n;
	scanf("%d%d",&a,&n);
	printf("%d",apow(a,n));
	*/
	
	int res;
	//printf("%d",fsign(x));
	//printf("%d",fitsbits(x,n));
	
	/* FSIGN
	int a;
	scanf("%d",&a);
	res=fsign(&a);
	printf("%d",res);*/

	/* //APOW
	int x,n;
	scanf("%d%d",&x,&n);
	double r=apow(x,n);
	printf("%llf",r);*/

	/* APOW
	int x,n;
	scanf("%d%d",&x,&n);
	res=apow(&x,&n);
	printf("%d",res);*/

	/* FROM10TO2
	int a;
	scanf("%d",&a);
	from10to2(a);
	*/
	system("pause");
		return 0;
}
