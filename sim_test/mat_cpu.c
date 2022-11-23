#include<stdio.h>
#include "rocc.h"
#include "encoding.h"
#include "ygjk.h"
#include "mat16ka.h"
#include "mat16kb.h"
//#define SIZE 1024 * 16

int main()
{
	long r1, r2, r3, res1, res2=0, res3,sum1=0,sum2=0,sum3=0;
	int c[SIZE];
	int i=0,j=0;
	int t = (1<<16)/4/SIZE,tmp;
//	for(i=0;i<SIZE*16;i++){
//   	    a[i] = 2;
//	    b[i] = 2;
//	}
	
res1 =  rdcycle();
	for(i=0;i<SIZE;i++){
//	    res2 = 0;
	    tmp = i * t;
	    for(j=0;j<t;j++){
	    	res2 += a[tmp + j] * b[tmp+ j];
	    }
//	    c[i] = res2;
	}
res3 =  rdcycle();
	printf("cycles_total: %ld %ld %ld\n",res3 - res1,res1,res3);
	
//	YGJK_INS_RRR( res1, 0, 0, 3);
//	printf("sum: %d\n", res1);
	printf("size: %d*4B\n", t);
	printf("for: %d\n", SIZE);
	printf("*****************************\n");
    	return 0;
}
