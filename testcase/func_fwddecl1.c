int f(int a, int b, int c);

int main() {
    int x = f(2, 3, 4);
    write(x);
}

int f(int a, int b, int c) {
    return a + b + c;
}
