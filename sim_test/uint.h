#ifndef UINT_H
#define UINT_H
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include "ygjk.h"
#define UINT_DELEG  6
#define UINT_UIE    1

static void sigusr(int signo)
{
    printf("ygjk finished in other thread\n");
}

void task_start(){
    if(signal(SIGUSR1,sigusr)==SIG_ERR)
        printf("can't register signal handle\n");
}

//打开代理
void fence_init(){
    asm volatile(
        "csrw 0x040,%0"
        :
        :"r"(UINT_DELEG)
    );
}

//等待执行
unsigned long comu_fence(int t){
    void* p=&&L1;
    unsigned long end;
    asm volatile(
        "csrw utvec,%0"
        :
        :"r"(p)
    );
    asm volatile(
        "csrw uie,%0"
        :
        :"r"(UINT_UIE)
    );
    long rd;
    t*=1000;
    while(t--);
   asm volatile(
        "csrw uie,%0"
        :
        :"r"(0)
    );
    printf("ready to system mode\n");
//    int ans=pause(); 
//  if(ans==-1)
//      return;
    asm("nop");
L1:
    end = rdcycle();
    YGJK_INT_STOP();
    return end;
}

#endif
