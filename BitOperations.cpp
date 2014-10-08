#include <iostream>
#include <stdlib.h>
using namespace std;

int apow(int a,int n) {
	int res=1;
	while (n>0) {
		if (n&1) res *=a;
		a*=a;
		n = n >>1;
	}
	return res;
}

void from10to2(int a) {
	if (a) {
		from10to2(a>>1);
		printf("%d",a&1);
	}
}


int fitsbits(int x,int n) {
	int t=(1<<(n-1));
	return (!(x+  (~(((t + x) & ((1 << n) -1)) + (~t+1)) +1)   ));
}

int fsign(int x) {
	return ((x>>31) | (!!x));
}

int main() {
    int a,n;
    cin >> a >> n;
    from10to2(a);
    system("pause");
	return 0;
}
