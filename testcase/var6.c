int main() {
    int a = 5;
    int b = 0;
    write(a); write("\n");
    write(b); write("\n");
    {
        int a = 10;
        int b = a;
        write(b); write("\n");
    }
    write(a); write("\n");
    write(b); write("\n");
    b = a;
    write(b); write("\n");
}
