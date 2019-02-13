


//In C code, Pointers are required: also
void double_me(int* x) {
  // Doubles the value at the memory location pointed to by x
  *x = *x + *x;
  return *x;
}
