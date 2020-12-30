float f() {
    return 5.0; 
}

float g() {
    return 10.0;
}

int main() {
    int a = f() / g();
    write(a); write("\n");
    write("Ok.");
}
