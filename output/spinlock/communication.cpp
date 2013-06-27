#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <mutex>

#define N 300

typedef struct int2 {
  int fst,snd;
  int2() {
    fst = 0;
    snd = 0;
  }
  int2(int x, int y) {
    fst = x;
    snd = y;
  }
}int2;

typedef struct int3 {
  int fst,snd,thd;  
  int3() {
    fst = 0;
    snd = 0;
    thd = 0;
  }
  int3(int x, int y, int z) {
    fst = x;
    snd = y;
    thd = z;
  }
}int3;

typedef struct int4 {
  int fst,snd,thd,frth;
  int4() {
    fst = 0;
    snd = 0;
    thd = 0;
    frth = 0;
  }
  int4(int w, int x, int y, int z) {
    fst = w;
    snd = x;
    thd = y;
    frth = z;
  }
}int4;

int channel[N];
bool empty[N];
std::mutex wlock[N];

void setup() {
  for(int i=0; i<N ; i++) {
    empty[i] = true;
  }
}

void write(int port, int data) {
  printf("W%d: start\n", port);
  if(!empty[port]) { printf("%d: deadlock\n", port); exit(1); }

  printf("W%d: lock\n", port);
  wlock[port].lock();
  printf("W%d: write\n", port);
  channel[port] = data;
  empty[port] = false;

  while(!empty[port]) {}
  wlock[port].unlock();
  printf("W%d: unlock\n", port);
}

int read(int port) {
  printf("R%d: start\n", port);
  while(empty[port]) {}
  printf("R%d: data arrives\n", port);
  
  int data = channel[port];
  //printf("R%d : receive %d\n", port, data);
  empty[port] = true;
  printf("R%d: empty channel\n", port);

  return data;
}

int in() {
  int data;
  scanf("%d", &data);
  return data;
}

void out(int data) {
  printf("%d\n", data);
}
