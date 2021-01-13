float print_0 (float a) {
  write(a);
  return 0.;
}

int print_1 (float a) {
  write(a);
  return 1;
}

int main () {
  float a, b, c;
  a = 0.5;
  b = 0.3;
  c = 0.;

  if (a && print_1(a))
    print_1(b);

  if (c && print_1(c))
    print_1(a);

  if (c && b && print_1(a))
    print_1(c);

  if (c && print_0(b))
    print_0(a);

  if (c || print_1(a))
    print_1(a);

  if (print_1(b) || print_1(b))
    print_1(a);

  if (print_0(a) && print_1(c))
    print_1(c);

  if (print_0(c) || print_0(a))
    print_0(b);

  if (print_0(1) || c || a || print_1(a))
    print_1(b);

  if (print_1(1) && c || print_1(a) && b || print_0(c))
    print_1(a);

  return 0;
}
