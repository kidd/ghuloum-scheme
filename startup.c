#include <stdio.h>
/* define all scheme constants */
#define bool_f               0x2F
#define bool_t               0x6F
#define null                 0b00111111 /* 91? */
#define fx_mask              0x03
#define fx_tag               0x00
#define fx_shift                2


typedef unsigned int ptr;

static void print_ptr(ptr x){
  ptr y;
  if((x & fx_mask) == fx_tag){
    printf("%d", ((int)x) >> fx_shift);
  } else if(x == bool_f){
    printf("#f");
  } else if(x == bool_t){
    printf("#t");
  } else if(x == null){
    printf("()");
  } else if((x & 0b0000000011111111) == 0b00001111){
    printf("#\\");
    y = (x >> 8);
    if(y == 0x09){
      printf("tab");
    } else if(y == 0x0D){
      printf("return");
    } else if(y == 0x0A){
      printf("newline");
    } else if(y == 0x20){
      printf("space");
    } else {
      printf("%c", y);
    }
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}


int main(int argc, char** argv){
  print_ptr(scheme_entry());
  return 0;
}
