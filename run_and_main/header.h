void _write_int(int val);

void _write_float(float val);

void _write_string(const char *val);

int read();

float _fread();

// https://stackoverflow.com/a/37261235
#define write(X) _Generic((X),                  \
                    int: _write_int,            \
                    float: _write_float,        \
                    char*: _write_string,       \
                    default: _write_int         \
                 )(X)

// Prevent conflict with fread
#define fread _fread
