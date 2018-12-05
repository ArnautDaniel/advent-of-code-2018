#include <stdio.h>
#include <stdlib.h>
#define LIM_MIN 64 
#define MINUTES 61

void print_usage (){
  printf("usage: slepz *input.zz*\n");
  return;
}

void sga (int* gua, int id){
  *gua = id;
  return;
}

void sfa (int* fal, int val){
  sga(fal, val);
  return;
}

void print_data_row (int* memory){
  printf("\n\t---Guard %d's stats---\n", memory[0]);
  for (int i=1; i < MINUTES ; i++){
    printf("cell[%d]: %d\n", i, memory[i]);
  }
  printf("\t--------------------------------\n");
  return;
}

void pdm (int** memory, int lim){
  if (lim == 1){
    return;
  }
  if (memory[lim-1] == 0){
    pdm(memory, lim-1);
  }
  else {
    print_data_row(memory[lim-1]);
    pdm(memory, lim-1);
  }
  return;
}

void swa (int* wak, int val){
  sga(wak, val);
  return;
}

void ret (int* ret, int* gua){
  printf("Guard %d returned %d\n", *ret, *gua);
  return;
}

void initialize_cells (int* cell){
  for (int i=0; i < MINUTES; i++){
    *(cell+i) = 1;
  }
  return;
}

void initialize_ram(int** ram, int lim){
  for (int i=0; i <= lim; i++){
    ram[i] = (int*) malloc (sizeof(int) * MINUTES+1);
    initialize_cells(ram[i]);
  }
  return;
}

int main (int argc, char** argv) {
  if (argc != 2) {
    print_usage();
    exit(-1);
  }

  int gua, fal, wak, ret, hig, cpr, lim, rdr;
  lim = LIM_MIN;
  int** ram = (int**) malloc(sizeof(int) * LIM_MIN);
  initialize_ram(ram, lim);
  pdm(ram, lim);
  return 0;
}

  
  
