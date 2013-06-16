#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <mutex>

#define N 300

typedef struct int2 {
  int fst,snd;
}int2;

typedef struct int3 {
  int fst,snd,thd;
}int3;

typedef struct int4 {
  int fst,snd,thd,frth;
}int4;

typedef struct int5 {
  int fst,snd,thd,frth,ffth;
}int5;

int channel[N];
bool empty[N];
std::mutex wlock[N];

void setup() {
  for(int i=0; i<N ; i++) {
    empty[i] = true;
  }
}

void write(int port, int data) {
  if(!empty[port]) { printf("%d: deadlock\n", port); exit(1); }

  wlock[port].lock();
  channel[port] = data;
  empty[port] = false;

  while(!empty[port]) {}
  wlock[port].unlock();
}

int read(int port) {
  while(empty[port]) {}
  
  int data = channel[port];
  //printf("R%d : receive %d\n", port, data);
  empty[port] = true;

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
