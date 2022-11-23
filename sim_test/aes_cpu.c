#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "aes_func.h"
#define SIZE 1024*16/8 
unsigned long addr = randNum;
int main()
{
	long a,r1,r2,r3,res1[SIZE],res2,res3;
	int i,j;
	unsigned long long key1 = 0x3CA10B2157F01916;
        unsigned long long key2 = 0x902E1380ACC107BD;

	printf("**************************************************\n");
	res2 =  rdcycle();
	for(i=0;i<SIZE;i+=2){
          YGJK_INS_RRR(res2, randNum[i], randNum[i+1], 0);
	}
	res3 =  rdcycle();
        printf("**************************************************\n");
	printf("cycles_total: %ld\n",res3 - res2);
//	for(i=0; i<SIZE/2; i++){
//              printf("%x \n",res1[i]);
//        }

    	return 0;
}
