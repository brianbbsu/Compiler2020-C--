int a, b;
int main() {
  a = read();
  b = read();
  if (a > b) write("larger");
  else if (a == b) write("equal");
  else write("smaller");
}
