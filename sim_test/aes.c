#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "aes16k.h"
#define SIZE 1024*8
unsigned long addr = randNum;
int main()
{
	long a,r1,r2,r3,res1,res2,res3;
	int i;
	unsigned long long key1 = 0x3CA10B2157F01916;
        unsigned long long key2 = 0x902E1380ACC107BD;
	printf("**************************************************\n");
        YGJK_INS_RRR( res1, randNum , SIZE, 0);
		YGJK_INS_RRR(res1, 0, 0, 0);
        YGJK_INS_RRR( res1, randNum , SIZE, 0);
        YGJK_INS_RRR( res1, 0, 0, 2);
        YGJK_INS_RRR( res1, key1, key2, 2);
//	printf("**************************************************\n");
 	for (i=0;i<5;i++) asm("nop");
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
        printf("**************************************************\n");
        YGJK_INS_RRR( res1, randNum , SIZE, 0);
	YGJK_INS_RRR(res1, 0, 0, 0);
        YGJK_INS_RRR( res1, randNum , SIZE, 0);
        YGJK_INS_RRR( res1, 0, 0, 2);
        YGJK_INS_RRR( res1, key1, key2, 2);
//        printf("**************************************************\n");
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

    	return 0;
}
