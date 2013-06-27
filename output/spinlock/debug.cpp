#include "communication.cpp"

//----------------------- CORE 0(0,0) ------------------------
void *main_0(void *dummy) {
  int _tmp_0, _tmp1_0, _tmp0_0;
  int y_0[25];
  int z_0[25];
  for(int i_0 = 75; i_0 < 100; ++i_0) {
    _tmp0_0 = read(40);
    y_0[i_0] = _tmp0_0;
  }

  printf("core(0,0) - done init\n");

  for(int i_0 = 75; i_0 < 100; ++i_0) {
    _tmp1_0 = read(20);
    z_0[i_0] = (_tmp1_0+y_0[i_0]);
  }
  

  for(int i_0 = 75; i_0 < 100; ++i_0) {
    write(40,z_0[i_0]);
  }

  return NULL;
}


//----------------------- CORE 1(0,1) ------------------------
void *main_1(void *dummy) {
  int _tmp_1;
  for(int i_1 = 75; i_1 < 100; ++i_1) {
    write(20,read(1));
  }

  return NULL;
}


//----------------------- CORE 2(0,2) ------------------------

//----------------------- CORE 3(0,3) ------------------------

//----------------------- CORE 4(0,4) ------------------------

//----------------------- CORE 5(1,0) ------------------------
void *main_5(void *dummy) {
  int _tmp_5, _tmp0_5;
  int x_5[50];
  for(int i_5 = 25; i_5 < 75; ++i_5) {
    _tmp0_5 = read(45);
    x_5[i_5] = _tmp0_5;
  }
  
  printf("core(1,0) - done init\n");

  for(int i_5 = 25; i_5 < 50; ++i_5) {
    write(25,x_5[i_5]);
  }

  for(int i_5 = 50; i_5 < 75; ++i_5) {
    write(5,x_5[i_5]);
  }

  return NULL;
}


//----------------------- CORE 6(1,1) ------------------------
void *main_6(void *dummy) {
  int _tmp_6, _tmp1_6, _tmp0_6;
  int y_6[25];
  int z_6[25];
  for(int i_6 = 25; i_6 < 50; ++i_6) {
    _tmp0_6 = read(46);
    y_6[i_6] = _tmp0_6;
  }

  printf("core(1,1) - done init\n");

  for(int i_6 = 0; i_6 < 25; ++i_6) {
    write(26,read(6));
  }

  for(int i_6 = 25; i_6 < 50; ++i_6) {
    _tmp1_6 = read(25);
    z_6[i_6] = (_tmp1_6+y_6[i_6]);
  }

  for(int i_6 = 75; i_6 < 100; ++i_6) {
    write(1,read(6));
  }

  for(int i_6 = 25; i_6 < 50; ++i_6) {
    write(46,z_6[i_6]);
  }

  return NULL;
}


//----------------------- CORE 7(1,2) ------------------------
void *main_7(void *dummy) {
  int _tmp_7, _tmp1_7, _tmp0_7;
  int y_7[25];
  int z_7[25];
  for(int i_7 = 0; i_7 < 25; ++i_7) {
    _tmp0_7 = read(47);
    y_7[i_7] = _tmp0_7;
  }
  printf("core(1,2) - done init\n");


  for(int i_7 = 0; i_7 < 25; ++i_7) {
    _tmp1_7 = read(26);
    z_7[i_7] = (_tmp1_7+y_7[i_7]);
  }

  for(int i_7 = 0; i_7 < 25; ++i_7) {
    write(47,z_7[i_7]);
  }

  return NULL;
}


//----------------------- CORE 8(1,3) ------------------------

//----------------------- CORE 9(1,4) ------------------------

//----------------------- CORE 10(2,0) ------------------------
void *main_10(void *dummy) {
  int _tmp_10, _tmp1_10, _tmp0_10;
  int y_10[25];
  int z_10[25];
  for(int i_10 = 50; i_10 < 75; ++i_10) {
    _tmp0_10 = read(50);
    y_10[i_10] = _tmp0_10;
  }
  printf("core(2,0) - done init\n");

  for(int i_10 = 50; i_10 < 75; ++i_10) {
    _tmp1_10 = read(5);
    z_10[i_10] = (_tmp1_10+y_10[i_10]);
  }

  for(int i_10 = 50; i_10 < 75; ++i_10) {
    write(50,z_10[i_10]);
  }

  return NULL;
}


//----------------------- CORE 11(2,1) ------------------------
void *main_11(void *dummy) {
  int _tmp_11, _tmp1_11, _tmp0_11;
  int x_11[50];
  for(int i_11 = 0; i_11 < 25; ++i_11) {
    printf("i = %d\n", i_11);
    _tmp0_11 = read(51);
    x_11[i_11] = _tmp0_11;
  }

  for(int i_11 = 75; i_11 < 100; ++i_11) {
    _tmp1_11 = read(51);
    x_11[i_11-25] = _tmp1_11;
  }

  printf("core(2,1) - done init\n");
  for(int i_11 = 0; i_11 < 25; ++i_11) {
    write(6,x_11[i_11]);
  }

  for(int i_11 = 75; i_11 < 100; ++i_11) {
    write(6,x_11[i_11-25]);
  }

  return NULL;
}


//----------------------- CORE 12(2,2) ------------------------

//----------------------- CORE 13(2,3) ------------------------

//----------------------- CORE 14(2,4) ------------------------

//----------------------- CORE 15(3,0) ------------------------

//----------------------- CORE 16(3,1) ------------------------

//----------------------- CORE 17(3,2) ------------------------

//----------------------- CORE 18(3,3) ------------------------

//----------------------- CORE 19(3,4) ------------------------

//----------------------- CORE 20(4,0) ------------------------
int _temp2_20;
int _temp1_20;
int _temp0_20;
void *main_20(void *dummy) {
  int _tmp_20, _tmp3_20, _tmp2_20, _tmp1_20, _tmp0_20;
  printf("init x1\n");
  for(int i_20 = 0; i_20 < 25; ++i_20) {
    _temp0_20 = in();
    write(51,_temp0_20);
  }

  printf("init x2,x3\n");
  for(int i_20 = 25; i_20 < 75; ++i_20) {
    _temp0_20 = in();
    write(45,_temp0_20);
  }

  printf("init x4\n");
  for(int i_20 = 75; i_20 < 100; ++i_20) {
    _temp0_20 = in();
    write(51,_temp0_20);
  }

  printf("init y1\n");
  for(int i_20 = 0; i_20 < 25; ++i_20) {
    _temp1_20 = in();
    write(47,_temp1_20);
  }

  printf("init y2\n");
  for(int i_20 = 25; i_20 < 50; ++i_20) {
    _temp1_20 = in();
    write(46,_temp1_20);
  }

  printf("init y3\n");
  for(int i_20 = 50; i_20 < 75; ++i_20) {
    _temp1_20 = in();
    write(50,_temp1_20);
  }

  printf("init y4\n");
  for(int i_20 = 75; i_20 < 100; ++i_20) {
    _temp1_20 = in();
    write(40,_temp1_20);
  }

  printf("io - done init\n");
  for(int i_20 = 0; i_20 < 25; ++i_20) {
    _tmp0_20 = read(47);
    _temp2_20 = _tmp0_20;
    out(_temp2_20);
  }

  for(int i_20 = 25; i_20 < 50; ++i_20) {
    _tmp1_20 = read(46);
    _temp2_20 = _tmp1_20;
    out(_temp2_20);
  }

  for(int i_20 = 50; i_20 < 75; ++i_20) {
    _tmp2_20 = read(50);
    _temp2_20 = _tmp2_20;
    out(_temp2_20);
  }

  for(int i_20 = 75; i_20 < 100; ++i_20) {
    _tmp3_20 = read(40);
    _temp2_20 = _tmp3_20;
    out(_temp2_20);
  }

  return NULL;
}


int main() {
  pthread_t t_0, t_1, t_5, t_6, t_7, t_10, t_11, t_20;
  setup();
  pthread_create(&t_0, NULL, main_0, NULL);
  pthread_create(&t_1, NULL, main_1, NULL);
  pthread_create(&t_5, NULL, main_5, NULL);
  pthread_create(&t_6, NULL, main_6, NULL);
  pthread_create(&t_7, NULL, main_7, NULL);
  pthread_create(&t_10, NULL, main_10, NULL);
  pthread_create(&t_11, NULL, main_11, NULL);
  pthread_create(&t_20, NULL, main_20, NULL);
  pthread_join(t_0, NULL);
  pthread_join(t_1, NULL);
  pthread_join(t_5, NULL);
  pthread_join(t_6, NULL);
  pthread_join(t_7, NULL);
  pthread_join(t_10, NULL);
  pthread_join(t_11, NULL);
  pthread_join(t_20, NULL);
  return 0;
}
