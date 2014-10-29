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
                        	printf("(-1)^%d * 1,%d * 2^%d",sign,mant,e-127);
                        	//printf("%d",mant);
                           // printf(" * 2^%d",e-127);
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

struct {
       union {
             float fval;
			 struct {
				 unsigned int mant:23;
				 unsigned int e:8;
				 unsigned int sign:1;
			 } parts;
             } val;
} data1;
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
	
	
	data1.val.fval=3.0;
	print(data1.val.parts.sign,data1.val.parts.e,data1.val.parts.mant);

   	return 0;
}
