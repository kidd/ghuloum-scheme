#include <stdio.h>
#include <sys/mman.h>
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

void error(char *msg){
  printf("%s", msg);
}

static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
		 PROT_READ | PROT_WRITE,
		 MAP_ANONYMOUS | MAP_PRIVATE,
		 0, 0);
  if (p == MAP_FAILED){ error("map failed"); }
  status = mprotect(p, page, PROT_NONE);
  if(status != 0){ error("mprotect error");}
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0){ error("mprotect2 error");}
  return (p + page);
}

static void deallocate_protected_space(char* p, int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0){ error("munmap error"); }
}

int main(int argc, char** argv){
  int stack_size = (16 * 4096); /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}


/* int main(int argc, char** argv){ */
/*   print_ptr(scheme_entry()); */
/*   return 0; */
/* } */
