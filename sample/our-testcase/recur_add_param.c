// #include <stdio.h>


int add (int l, int r, int arr[]) {
  int mid = (l + r) / 2;

  if (r - l == 1) return arr[l];
  return add(l, mid, arr) + add(mid, r, arr);
}


int main () {
  int arr[10];
  int i;

  for (i = 0; i < 10; i = i + 1)
    arr[i] = i;

  // printf("%d\n", add(0, 10, arr));
  write(add(0, 1, arr));

  return 0;
}
