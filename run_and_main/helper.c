#include <stdio.h>

void _write_int(int val) {
    printf("%d", val);
}

void _write_float(float val) {
    printf("%f", val);
}

void _write_string(const char *val) {
    printf("%s", val);
}

int read() {
    int val;
    scanf("%d", &val);
    return val;
}

float _fread() {
    float val;
    scanf("%f", &val);
    return val;
}
