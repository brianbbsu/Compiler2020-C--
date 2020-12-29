// print N level of star
int N = 100;

int main() {
    int i, j;
    for (i = 1; i <= 100; i = i + 1) {
        for (j = 1; j <= i; j = j + 1)
            write("*");
        write("\n");
    }
}
