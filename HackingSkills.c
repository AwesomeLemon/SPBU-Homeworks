#include <stdio.h>
int f1();
int f3() {
    printf("You, my friend, have been hacked.");
    system("pause");
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
    printf("%d\n",(int) &f3);
    scanf("%d",&next);
    int i=0;   
    while (next!=0) {
          data[i++]=next;
          scanf("%d",&next);
          }
    /*int pm=(int) &main;
    printf("%d\n\n",pm);
    int j;
    
    for (j=0; j<100; j++)
        printf("%d\n",data[j]);*/
    
    //data[7]=(int) &f3;
    return 17;
}
