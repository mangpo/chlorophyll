/**
define every operator to work with pair and vector (element-wise operation) as language primitive
**/

/***** PLACE *******
Place can be {0-9}+ (correspond to real physical node) or {a-zA-Z0-9}+ for abstract location place

Need to declare abstract location.
place p1, p2, ...;

Special location type.
@h, @!h, @any, @dist

place(x) is a way to get place that variable x resides.

Example:
int@1 x;
int@place(x) y; // y will be at the same place as x
*/

/***** DATA TYPE ******
int@1
pair<int,int>@(2,3)
vector<int>@{[0:N]=loc4}
vector<pair<int,int>>@{[0:10]=(loc5,loc6), [10:N]=(loc7,loc8)}
vector<vector<int>>@{[0:N]={[0:M]=loc9}}
pair<vector<int>,vector<int>>@({[0:N]=loc10},{[0:M]=loc11})
*/

/***** OPERATION TYPE *****
This is for primitive operator.
op@p1

Example:
*@p1 // multiply operator is at abstract core p1.
**/

/******** known vs int ***********
known - known statically: doesn't depend on input.
int(or unknown) - unknown statically: depends on input.

known and int allow optimization for array access. 
If the index of an array is known, the access pattern can be determined statically.
Hence, we can create total schedule for array access and store.
However, if index is int (unknown), we need request/respond protocol for array access and store.
*/

typedef tuple(int,int) myInt;
typedef tuple(known,known) _myInt;

_myInt[]@{[0:64]=(106,6)} k[64] = { (0xd76a,0xa478), ... };
known[]@{[0:64]=102} r[64] = { ... };
myInt[]@{[0:64]=(103,3)} message[16]; //streamed and stored in local mem.
myInt[]@{[0:4]=(104,4)} output[4];
myInt[]@{[0:4]=(104,4)} hash[4] = { (0x6745,0x2301), (0xefcd,0xab89), (0x98ba,0xdcfe), (0x1032,0x5476) };


myInt@(105,5) leftrotate(myInt& x, val& r);

myInt@(105,5) csum(myInt& x, myInt& y) {
  // sum of lower bit
  int@5   sum_l = x.second +@5 y.second;
  // sum of higher bit + carry bit from node 5
  int@105 sum_h = x.first +@105 y.first +@105 (sum_l >>@5 16@5);
  // only keep the 16 bits.
  sum_l = sum_l %@5 &&@5 0xffff@5;
  return (sum_h, sum_l);
}

myInt@(105,5) sumrotate(myInt& buffer, 
    _myInt& i, 
    _myInt& g, 
    myInt& b) {
  return csum(b, leftrotate(buffer +@h k[i] +@h message[g] , r[i.first]));
}

/****** @h *******
@h is at here. "Here" refer to "home" of the function it is in.
"Home" is defined to be the same as the return type of the function.

For example, @h for the function sumrotate is 105 for the first element
and 5 for the second element.

&& || ... are annotated with @h if we went everything on the same node
**/

/*
Element-wise < is independent from each other.

For example,
pair<int,int>@(100,200) a = (2,10);
pair<int,int>@(100,200) output;
if(a < (5,5)) {
  output = 0;
} else {
  output = 1;
}

The program trace of the two elements will be different.
output = (0,1)
*/

myInt@(104,4) inner(myInt& a, myInt& b, myInt& c, myInt& d, _myInt& i) {
  // i:@h@(103,3)  --> There are multiple copies of i's, so we need to specify which i we want.
  // <:@h@(103,3)  --> specify where < happens.
  // 16:@h@(103,3) --> specify where 16 is.
  // CAUTION: i:@(107,7) will cause error because there is no i at those locations.
  //          i:pair<int,int>@h is fine. We can fullly specify the complete type.
  //          i:int@h is bad because i is not int.
  //          i:@all is a syntactice sugar for @h@(106,6)@(102,2)@(103,3).

  _myInt@(103,3) g;
  parif (i <@h,(103,3) 16) {
    f = (b && c) || ((! b) && d);
    g = i;
  }
  else parif (i <@h,(103,3) 32) {
    f = (d && b) || ((! d) && c);
    g = (5*i +@(103,3) 1) %@(103,3) 16;
  }
  else parif (i <@h,(103,3) 48) {
    f = b ^ c ^ d;
    g = (3*i +@(103,3) 5) %@(103,3) 16;
  }
  else {
    f = c ^ (b || (! d));
    g = (7*i) %@(103,3) 16;
  }
  return sumrotate(a +@h f, i, g, b);
}

void@(104,4) md5() {
  for(_myInt@any t = 0; t < 16; t++) {
    myInt@h a = hash[0], b = hash[1], c = hash[2], d = hash[3];  
    
    // These multiple @'s indicate that we need this loop in these cores.
    // Also, since this is an inner loop, we need to replicate outter loop as well.
    for(_myInt@any i = 0; i < 64; i++) {
      myInt@h temp = d;
      d = c;
      c = b;
      b = inner(a, b, c, d, i);
      a = temp;
    }
    hash[0] += a;
    hash[1] += b;
    hash[2] += c;
    hash[3] += d;
  }
  output[0] = hash[0];
  output[1] = hash[1];
  output[2] = hash[2];
  output[3] = hash[3];
}

void@(103,3) prerun() {
  for(_myInt@h i = 0; i < 64; i++) {
    message[i] = readIO(h); // read from io pin. h is here.
  }
}

void main() {
  prerun();
  md5();
}
