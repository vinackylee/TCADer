#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "mat16ka.h"
#include "mat16kb.h"
#define SIZE 1024 * 16

int main()
{
	long r1, r2, r3, res1, res2, res3,sum1=0,sum2=0,sum3=0;
	int c[SIZE];
	int i=0;
//	for(i=0;i<SIZE*16;i++){
//   	    a[i] = 2;
//	    b[i] = 2;
//	}
//r1 =  rdcycle();
	for(i=0;i<SIZE;i++){
		
	    printf("%d************************\n",i);
	    YGJK_INS_RRR( res1, a+i*((1<<16)/4/SIZE), ((1<<16)/SIZE), 0);
            YGJK_INS_RRR(res1, b+i*((1<<16)/4/SIZE), ((1<<16)/SIZE), 0);
            YGJK_INS_RRR(res1, b, 0, 0);
            YGJK_INS_RRR( res1, 0, 0, 2);
            while(1){
            	YGJK_TEST(res3,0,0,8);
		YGJK_TEST(res2,0,0,7);
        	if(res2==10) break;
            }
	    YGJK_CTRL_FUNC(0);

	    sum1 += res3;
            //printf("cycles: %d\n",res3);
            YGJK_TEST(res3,0,0,6);
            sum2 += res3;
	    //printf("compute_only_cycles: %d\n",res3);
            YGJK_TEST(res3,0,0,9);
            sum3 += res3;
	    //printf("cycles_total: %d\n",res3);
//            YGJK_INS_RRR(res2, b, 0, 3);
//	    c[i] = res2;

    	}
//r3 =  rdcycle();
//        printf("cycles: %ld\n",r3 - r1);
        printf("cycles: %ld\n",sum1);
	printf("compute_only_cycles: %ld\n",sum2);
	printf("cycles_total: %ld\n",sum3);
	
//	YGJK_INS_RRR( res1, 0, 0, 3);
//	printf("sum: %d\n", res1);
	printf("size: %d*4B\n", ((1<<16)/4/SIZE));
	printf("for: %d\n", SIZE);
	printf("*****************************\n");
    	return 0;
}
