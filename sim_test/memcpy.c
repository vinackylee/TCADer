#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "aes16k.h"
#define SIZE 1024 * 16
unsigned long addr = randNum;
__attribute__((aligned(512))) char destNum[SIZE];

int main()
{
	printf("*************\n");
	long a,r1,r2,r3,res1,res2,res3;	
	int i;
        for(i=SIZE-64; i<SIZE; i++){
	        destNum[i] = 7;
                printf("%x ",destNum[i]);
                if((i+1)%16 ==0 &&i !=0) printf("\t%d\n",1+i);
        }

	printf("**************************************************\n");
	YGJK_INS_RRR(res1, randNum, SIZE, 0);
        YGJK_INS_RRR(res1, randNum, 0, 0);
        YGJK_INS_RRR(res1, destNum, SIZE, 0);
	printf("1111111111111100000000000000\n");
	
	while(1){
          YGJK_TEST(res3,0,0,8);
	  YGJK_TEST(res2,0,0,7);
	  if(res2==10) break;
  	}
        YGJK_CTRL_FUNC(0);
	printf("cycles: %d\n",res3);
        YGJK_TEST(res3,0,0,6);
        printf("compute_only_cycles: %d\n",res3);
        YGJK_TEST(res3,0,0,9);
        printf("cycles_total: %d\n",res3);
	for(i=SIZE-64; i<SIZE; i++){
		printf("%x ",randNum[i]);
		if((i+1)%16 ==0 &&i !=0) printf("\t%d\n",1+i);	
	}
	printf("destNum addr %ud\n",destNum);
	for(i=SIZE-64; i<SIZE; i++){
		printf("%x ",destNum[i]);
		if((i+1)%16 ==0 &&i !=0) printf("\t%d\n",1+i);	
	}
    	return 0;
}
