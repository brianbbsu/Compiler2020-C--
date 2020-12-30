int k;


void Try() {
    if (k < 10) {
        write(k);
        write("\n");
        k = k + 1;
        Try();
    }
}

int main() {
    k = 0;
    Try();
    return 0;
}
