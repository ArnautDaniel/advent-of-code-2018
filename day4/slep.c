#include <stdio.h>
#include <stdlib.h>
#define LIM_MIN 64 
#define MINUTES 61

enum opcodes {op_sga, op_sfa, op_swa, op_ret, op_add, op_sum
	      , op_hig, op_cpg, op_cpe, op_cpl, op_low, op_hgn
	      , op_phr, op_mic, op_pdm};
  
void print_usage (){
  printf("usage: slepz *input.zz*\n");
  return;
}

void sga (int* gua, int id){
  *gua = id;
  return;
}

int guard_contained_in_ram(int** ram, int gua, int lim){
  for (int i = 0; i < lim; i++){
    if (ram[i][0] == gua){
      return 0;
    }
  }
  return 1;
}

void internal_set_cell_guard (int** ram, int gua, int* lim){
  int k = guard_contained_in_ram(ram, gua, *lim);
  if (k == 0){
    break;
  } else
    
  return;
}

void sfa (int* fal, int val){
  sga(fal, val);
  return;
}

void swa (int* wak, int val){
  sga(wak, val);
  return;
}

void print_cell (int* memory){
  printf("\n\t---Guard %d's stats---\n", memory[0]);
  for (int i=1; i+1 < MINUTES ; i++){
    printf("\t[%d:%d]", i, memory[i]);
  }
  printf("\n\t--------------------------------\n");
  return;
}

// opcode print data memory 0->lim
void pdm (int** memory, int lim){
  if (lim == 1){
    return;
  }
  if (memory[lim-1] == 0){
    pdm(memory, lim-1);
  }
  else {
    print_cell(memory[lim-1]);
    pdm(memory, lim-1);
  }
  return;
}



void f_ret (int* ret, int* gua){
  printf("Guard %d returned %d\n", *gua, *ret);
  return;
}

void add (int* cell, int n){
  cell[n] += 1;
}

void sum (int* cell, int* ret){
  int res = 0;
  for(int i=1; i < MINUTES; i++){
    res += *(cell+i);
  }
  *ret = res;
  return;
}

void f_hig (int** ram, int* gua, int* ret, int lim){
  int temp = 0;
  int temp_gua = 0;
  for (int i=0; i < lim; i++){
    temp = *ret;
    temp_gua = *gua;
    sum(ram[i], ret);
    if (*ret >= temp){
      *gua = ram[i][0];
    } else {
      *ret = temp;
      *gua = temp_gua;
    }
  }
  return;
}

void f_low (int** ram, int* gua, int* ret, int lim){
  int temp = 0;
  int temp_gua = 0;
  for (int i=0; i < lim; i++){
    temp = *ret;
    temp_gua = *gua;
    sum(ram[i], ret);
    if (*ret <= temp){
      *gua = ram[i][0];
    } else {
      *ret = temp;
      *gua = temp_gua;
    }
  }
  return;
}

void phr (int* hig, int* ret){
  *ret = *hig;
  return;
}

void mic (int* cell, int* fal, int* wak){
  if (*fal > *wak){
    return;
  }
  cell[*fal] += 1;
  *fal = *fal + 1;
  mic(cell, fal, wak);
  return;
}


void initialize_cell (int* cell){
  for (int i=0; i < MINUTES; i++){
    *(cell+i) = 0;
  }
  return;
}

void initialize_ram(int** ram, int lim){
  for (int i=0; i < lim; i++){
    ram[i] = (int*) malloc (sizeof(int) * MINUTES+1);
    initialize_cell (ram[i]);
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
  int** ram = (int**) malloc(sizeof(int*) * LIM_MIN+1);
  initialize_ram(ram, lim);
  initialize_cell (ram[0]);
  ret = 0;
  sfa(&fal, 1);
  swa(&wak, 50);
  sga(&gua, 2);
  internal_set_cell_guard(ram, gua, &lim);
  mic(ram, &fal, &wak, &gua);
  sga(&gua, 0);
  f_hig(ram, &gua, &ret, lim);
  f_ret(&ret, &gua);
  return 0;
}

  
  
