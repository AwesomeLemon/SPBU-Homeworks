#include <stdio.h>
int f1();
int f2() {
    printf("You, my friend, have been hacked.");
    exit(0);
}
int main() {
    int r=f1();
    printf("%d",r);
    system("pause");
    return 0;
}
int f1() {
    int data[3];
    int next;
    printf("%d\n",(int) &f2);
    scanf("%d",&next);
    int i=0;   
    while (next!=0) {
          data[i++]=next;
          scanf("%d",&next);
          }
    
    /* In this block of code we're finding return address, so we can replace it
    
    int pm=(int) &main;
    printf("%d\n\n",pm);
    int j;
    
    for (j=0; j<100; j++)
        printf("%d\n",data[j]);
    */
    
    //Why use scanf when there's an easier way? 
    //data[7]=(int) &f3;
    
    return 17;
}
