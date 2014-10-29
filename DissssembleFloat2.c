#include <stdio.h>
void from10to2(int a) {
	if (a) {
		from10to2(a >> 1);
		printf("%d",a & 1);
	}
}
void print(int sign, int e, float mant) {
     if (e==0 && mant==0) printf ("0");
    	else 
             if (e==0 && mant!=0) printf("NaN");
             else 
                  if (e==255 && mant==0 && sign==0) printf("+INFINTY");
                  else 
                       if (e==255 && mant==0 && sign==1) printf("-INFINTY");
                       else {
                        	//printf("(-1)^%d * 1,%d * 2^%d",sign,mant,e-127);
                        //	printf("%d",mant);
                           //  printf("%5d %24d %9d\n",sign,mant,e);
                        //	printf("%5x %24x %9x\n",sign,mant,e);
                        	printf("%5d  %24f ",sign,mant);
                        	/*int i;
                        	for (i=23; i > 0; i--) printf("%d",(mant & (1 << (i-1))) >> (i-1));
                        	printf("   ");
                        	int i=1<<8;
                        	while (i>0) {
                                  printf("%d", e & i);*/
                         
                         from10to2(e);
                           // printf(" * 2^%d",e-127);
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
    
    
   float val=3.0;
    int ival = * ((int*) ((void*) &val));
	disassemble(ival);
    printf("\n");
	
	/*
	data.val.fval=3.0;
	int x=data.val.ival;
	disassemble(x);
	    printf("\n");
	*/
	
	/*data1.val.fval=3.0;
	print(data1.val.parts.sign,data1.val.parts.e,data1.val.parts.mant);*/
    system("pause");
   	return 0;
}
