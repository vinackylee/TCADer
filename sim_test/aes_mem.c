#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "aes16k.h"
//#include "aes_128k.h"
//#define SIZE 1024 * 16
__attribute__((aligned(512))) char destNum[1024*16];
int main()
{
	long r1, r2, r3, res1, res2, res3,sum1=0,sum2=0,sum3=0;
	int c[SIZE];
	unsigned long long key1 = 0x3CA10B2157F01916;
        unsigned long long key2 = 0x902E1380ACC107BD;
	int i=0;
//	for(i=0;i<SIZE*16;i++){
//   	    a[i] = 2;
//	    b[i] = 2;
//	}
	memcpy(destNum, randNum,SIZE); 
	for(i=0;i<(1024*16)/SIZE;i++){
	r1 =  rdcycle();
//            printf("%d************************\n",i);
//	r1 =  rdcycle();
//            YGJK_INS_RRR( res1, randNum+SIZE*i , SIZE, 0);
//            YGJK_INS_RRR( res1, 0, 0, 0);
//            YGJK_INS_RRR( res1, randNum+SIZE*i , SIZE, 0);
//            YGJK_INS_RRR( res1, 0, 0, 2);
//            YGJK_INS_RRR( res1, key1, key2, 2);
//	    printf("%d************************\n",i);
          if(i<(1024*16)/SIZE-1){
        	memcpy(destNum+SIZE*(i+1), randNum+SIZE*(i+1),SIZE); 
          }
//	    printf("%d************************\n",i);
//	    while(1){
//            	YGJK_TEST(res1,0,0,8);
//		YGJK_TEST(res2,0,0,7);
//		printf("rd \n",res2);
//        	if(res2==10) break;
//            }
//	    YGJK_CTRL_FUNC(0);
//	    sum1 += res3;
//            printf("cycles: %d\n",res1);
//            YGJK_TEST(res2,0,0,6);
//            sum2 += res3;
//	    printf("compute_only_cycles: %d\n",res2);
//            YGJK_TEST(res3,0,0,9);
//            sum3 += res3;
//	    printf("cycles_total: %d\n",res3);
	res3 =  rdcycle();
	printf("rdcycles %ld\n",res3 - r1);
	}
//      printf("cycles: %d\n",sum1);
//	printf("compute_only_cycles: %d\n",sum2);
//	printf("cycles_total: %d\n",sum3);
	
//	YGJK_INS_RRR( res1, 0, 0, 3);
//	printf("sum: %d\n", res1);
	printf("size: %d*4B\n", ((1<<16)/SIZE));
	printf("for: %d\n", SIZE);
	printf("*****************************\n");
    	return 0;
}
