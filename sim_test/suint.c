#include<stdio.h>
int main()
{
    printf("\n-----------------------------Try to Catch User Mode Interrupt-------------------------------\n");
    int ori_val=0x123;
    int write_val=6;
/*    asm volatile(
        "csrr %0, 0x304"
        :"=r"(ori_val)
        :
    );
    ori_val = 0x1e088;
    asm volatile(
        "csrw mstatus,%0"
        :
        :"r"(ori_val)
    );
    ori_val |= 0x01888;
    asm volatile(
        "csrw mie,%0"
        :
        :"r"(ori_val)
    );*/
    asm volatile(
        "csrw 0x040,%0"
        :
        :"r"(write_val)
    );
    asm volatile(
        "csrr %0, 0x040"
        :"=r"(ori_val)
        :
    );
	void* p=&&l1;
    write_val=1;
    printf("deleg after write:%x\n",ori_val);
    asm volatile(
        "csrw utvec,%0"
        :
        :"r"(p)
    );
    asm volatile(
        "csrw uie,%0"
        :
        :"r"(write_val)
    );
    ori_val = 1<<30;
	volatile int a=0;
    while(ori_val){ori_val--;a++;}
	printf("%d\n",a);
    asm("nop");
l1:
	printf("get user interrupt!!!\n");
    return 0;
}
