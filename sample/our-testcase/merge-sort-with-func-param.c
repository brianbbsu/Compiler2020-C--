int seed;
int rand() {
  seed = seed * 131 + 1;
  seed = seed - (seed / 100000) * 100000;
  return seed;
}

void MergeSort(int l, int r, int arr[]) {
  int mid = (l + r) / 2;
  int i = l;
  int j = mid;
  int k = l;
  int tmp[10000];
  if (r - l == 1) return;
  MergeSort(l, mid, arr);
  MergeSort(mid, r, arr);
  while (i < mid && j < r) {
    if (arr[i] < arr[j]) {
      tmp[k] = arr[i];
      i = i + 1;
    } else {
      tmp[k] = arr[j];
      j = j + 1;
    }
    k = k + 1;
  }
  while (i < mid) {
    tmp[k] = arr[i];
    i = i + 1;
    k = k + 1;
  }
  while (j < r) {
    tmp[k] = arr[j];
    j = j + 1;
    k = k + 1;
  }
  i = l;
  while (i < r) {
    arr[i] = tmp[i];
    i = i + 1;
  }
}

void DoMergeSort(int n) {
  int i;
  int arr[10000];
  for (i = 0; i < n; i = i + 1) {
    arr[i] = rand();
  }
  MergeSort(0, n, arr);
  for (i = 1; i < n; i = i + 1) {
    if (arr[i] < arr[i - 1]) {
      write("wrong\n");
      return;
    }
  }
  write("correct\n");
}

int main() {
  int n;
  for (n = 1; n < 10; n = n + 1) {
    seed = n;
    DoMergeSort(n);
  }
  while (n <= 10000) {
    seed = 123;
    DoMergeSort(n);
    n = n * 10;
  }
}
