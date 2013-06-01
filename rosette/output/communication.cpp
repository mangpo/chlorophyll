#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <mutex>

#define N 300

int channel[N];
bool empty[N];
std::mutex lock[N];

void setup() {
  for(int i=0; i<N ; i++) {
    empty[i] = true;
  }
}

void write(int port, int data) {
  if(!empty[port]) { printf("%d: deadlock\n", port); exit(1); }

  lock[port].lock();
  channel[port] = data;
  empty[port] = false;
  lock[port].unlock();

  while(!empty[port]) {}
}

int read(int port) {
  while(empty[port]) {}

  lock[port].lock();
  int data = channel[port];
  printf("%d : receive %d\n", port, data);
  empty[port] = true;
  lock[port].unlock();

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
