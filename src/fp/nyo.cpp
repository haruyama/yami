#include <iostream>
#include <vector>
#include <cstdio>
#include <boost/foreach.hpp>
using namespace std;


static void print_fp(double d) {
  unsigned char* p = (unsigned char*)&d;

  for (int i = 0; i < sizeof(double); ++i) {
    printf("%02x ", *p);
    ++p;
  }
  printf("\n");
}

int main() {
  vector<double> v = {1e24, 1e25, 1e24*1e24, 1e25 * 1e25, 1e48, 1e50};

  BOOST_FOREACH(double d, v) {
    print_fp(d);
  }

  return 0;
}
