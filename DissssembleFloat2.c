/* 
   Disassemble Float to sign, e and mantissa
               by Alexander Chebykin
*/

#include <stdio.h>

void print(int sign, int e, float mant) {
     if (e==0 && mant==0) printf ("0");
    	else 
             if (e==0 && mant!=0) printf("NaN");
             else 
                  if (e==255 && mant==0 && sign==0) printf("+INFINTY");
                  else 
                       if (e==255 && mant==0 && sign==1) printf("-INFINTY");
                       else {
                        	printf("(-1)^%d * %f * 2^%d\n",sign,mant+1,e-127);            
                            }
}

void disassemble (int ival) {
     
     int sign = ((ival & (1 << 31)) >> 31) & 1;
     int e = (ival & (255 << 23)) >> 23;
     int mant1 = ival & ((1 << 23) -1 );
     float mant = ((float) mant1) /pow(2,23);
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
    float val=17.0;
    /*
    int ival = * ((int*) ((void*) &val));
	disassemble(ival);
	*/
	
	/*
	data.val.fval=val;
	int x=data.val.ival;
	disassemble(x);
	*/
	
	/*
	data1.val.fval=val;
	print(data1.val.parts.sign,data1.val.parts.e,((float) data1.val.parts.mant) /pow(2,23));
	*/
	
   	return 0;
}
