int f() {
    return 5; 
}

int g() {
    return 2;
}

int main() {
    float a = f() / g();
    write(a); write("\n");
    write("Ok.");
}
