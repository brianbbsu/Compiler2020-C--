// Play 'guess number game' for numbers in range 1 ~ N
int N = 1000;

int ask(int x, int target) {
    write("ask "); write(x); write(" "); write(target); write("\n");
    if (x < target) { 
        write("Too small\n");
        return -1;
    }
    else if (x > target) {
        write("Too big\n");
        return 1;
    }
    write("Correct~\n");
    return 0;
}

void game(int target) {
    int l = 1, r = N; // [l, r]
    int found = 0;
    write("Target = "); write(target);
    write("\n");
    write(l); write(" "); write(r); write("\n");
    while (found == 0) {
        int h = (l + r) / 2;
        int res;
        write(l); write(" "); write(r); write("\n");
        write(h); write("\n");
        res = ask(h, target);
        write("res = "); write(res); write("\n");
        if (res == -1) l = h + 1;
        else if (res == 1) r = h - 1;
        else {
            write("Found: ");
            write(h);
            write("\n");
            found = 1;
        }
    }
    write("Done!");
}

int main() {
    int i;
    for (i = 1; i <= N; i = i + 1) {
        write("For: "); write(i); write("\n");
        game(i);
    }
    write("Finish!");
}
