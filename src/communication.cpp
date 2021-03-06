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

typedef struct long2 {
  long fst,snd;
  long2() {
    fst = 0;
    snd = 0;
  }
  long2(long x, long y) {
    fst = x;
    snd = y;
  }
}long2;

typedef struct long3 {
  long fst,snd,thd;  
  long3() {
    fst = 0;
    snd = 0;
    thd = 0;
  }
  long3(long x, long y, long z) {
    fst = x;
    snd = y;
    thd = z;
  }
}long3;

typedef struct long4 {
  long fst,snd,thd,frth;
  long4() {
    fst = 0;
    snd = 0;
    thd = 0;
    frth = 0;
  }
  long4(long w, long x, long y, long z) {
    fst = w;
    snd = x;
    thd = y;
    frth = z;
  }
}long4;

////////////////////////////////////////////////////////////////////

long neg_ext = -1 << 18;
long finitize(long x) {
  long ret = x & 0x3ffff;
  if(ret & 0x20000)
    return ret | neg_ext;
  else
    return ret;
}

long2 divmod(long x, long y) {
  long d, r;
  d = x/y;
  r = x%y;
  return long2(r,d);
}

long2 mult2(long x, long y) {
  long res, h, l;
  res = x*y;
  h = res >> 18;
  l = finitize(res); // res & 0x3ffff;
  return long2(h,l);
}

long2 rightrot(long x, long y) {
  long h, l;
  h = x >> y;
  l = finitize(x << (18 - y)); // (x << (18 - y)) & 0x3ffff;
  return long2(h,l);
}

////////////////////////////////////////////////////////////////////

long channel[N];
bool empty[N];
std::mutex wlock[N];

void setup() {
  for(int i=0; i<N ; i++) {
    empty[i] = true;
  }
}

void write(int port, long data) {
  if(!empty[port]) { printf("%d: deadlock\n", port); exit(1); }

  wlock[port].lock();
  channel[port] = data;
  empty[port] = false;

  while(!empty[port]) {}
  wlock[port].unlock();
}

long read(int port) {
  while(empty[port]) {}
  
  long data = channel[port];
  //printf("R%d : receive %d\n", port, data);
  empty[port] = true;

  return data;
}

long in() {
  long data;
  scanf("%li", &data);
  return data;
}

long in_fp(int k) {
  double data;
  scanf("%lf", &data);
  return finitize(data * (1 << (18-k)));
}

void out(long data) {
  printf("%li\n", data);
}

void out_fp(long data, int k) {
  printf("%lf\n", double(data)/(1 << (18-k)));
}
