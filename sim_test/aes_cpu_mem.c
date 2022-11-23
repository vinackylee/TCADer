#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "aes16k.h"
//#include "aes_128k.h"
//#define SIZE 1024 * 16
__attribute__((aligned(512))) long destNum[1024*16];
__attribute__((aligned(512))) long res1[1024*16/8];
int main()
{
	long r1, r2, r3, res2, res3,sum1=0,sum2=0,sum3=0;
	int c[SIZE];
	unsigned long long key1 = 0x3CA10B2157F01916;
        unsigned long long key2 = 0x902E1380ACC107BD;
	int i=0,j=0;
//	for(i=0;i<SIZE*16;i++){
//   	    a[i] = 2;
//	    b[i] = 2;
//	}
	memcpy(destNum, randNum,SIZE); 
	for(i=0;i<(1024*16)/SIZE;i++){
	r1 =  rdcycle();
	  for(j=0;j<128;j+=2){
	      YGJK_INS_RRR( res1[i*128+j], randNum[SIZE*i/8+j], randNum[SIZE*i/8+j+1], 0);
	  }
          if(i<(1024*16)/SIZE-1){
        	memcpy(destNum+SIZE*(i+1), randNum+SIZE*(i+1),SIZE); 
          }
	res3 =  rdcycle();
	printf("rdcycles %ld\n",res3 - r1);
	}
//      printf("cycles: %d\n",sum1);
//	printf("compute_only_cycles: %d\n",sum2);
	printf("cycles_total: %d\n",sum3);
	
//	YGJK_INS_RRR( res1, 0, 0, 3);
//	printf("sum: %d\n", res1);
	printf("size: %d*4B\n", ((1<<16)/SIZE));
	printf("for: %d\n", SIZE);
	printf("*****************************\n");
    	return 0;
}
