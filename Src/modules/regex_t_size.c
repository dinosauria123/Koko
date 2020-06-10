//
// determines the size of the regex_t structure
//
#include <stdio.h>
#include <regex.h>

int main(void){
  printf("integer, parameter :: REGEX_T_SIZE = %d\n", (int)sizeof(regex_t));
}
