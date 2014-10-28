#include <stdio.h>
void print(int sign, int e, int mant) {
     if (e==0 && mant==0) printf ("0");
    	else 
             if (e==0 && mant!=0) printf("NaN");
             else 
                  if (e==255 && mant==0 && sign==0) printf("+INFINTY");
                  else 
                       if (e==255 && mant==0 && sign==1) printf("-INFINTY");
                       else {
                        	printf("(-1)^%d * 1,",sign);
                        	int i;
                        	//for (i=23; i > 0; i--) printf("%d",(mant & (1 << (i-1))) >> (i-1));
                        	printf("%d",mant);
                            printf(" * 2^%d",e-127);
                            }
}
void disassemble (int ival) {
     
     int sign = ((ival & (1 << 31)) >> 31) & 1;
     int e = (ival & (255 << 23)) >> 23;
     int mant = ival & ((1 << 23) -1 );
     print(sign,e,mant);
}
struct {
       union {
             int ival;
             float fval;
             } val;
} data;
int main () {
    
    /*
    float val=5.0;
    int ival = * ((int*) ((void*) &val));
	disassemble(val);
    */
	
	/*
	data.val.fval=3.0;
	int x=data.val.ival;
	disassemble(x);
	*/
	
   	return 0;
}
