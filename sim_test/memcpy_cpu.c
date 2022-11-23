#include<stdio.h>
#include "rocc.h"
#include "aes16k.h"
#include "rocc.h"
#include "encoding.h"
#define SIZE 1024 * 16
unsigned long addr = randNum;
__attribute__((aligned(512))) char destNum[SIZE];

int main()
{
	printf("*************\n");
	long a,r1,r2,r3,res1,res2,res3;
	int i;
res1 =  rdcycle();
	memcpy(destNum, randNum,SIZE);
res2 =  rdcycle();
	printf("cycles %ld\n",res2 - res1);
	for(i=SIZE-64; i<SIZE; i++){
		printf("%x ",randNum[i]);
		if((i+1)%16 ==0 &&i !=0) printf("\t%d\n",1+i);	
	}
	for(i=SIZE-64; i<SIZE; i++){
		printf("%x ",destNum[i]);
		if((i+1)%16 ==0 &&i !=0) printf("\t%d\n",1+i);	
	}
    	return 0;
}
