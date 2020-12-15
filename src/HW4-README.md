# HW4 Semantic Analysis

### Bonus Features

##### forward declaration of function

example:
```cpp
int f2 (int);

int f1 (int x) {
    if (x <= 0) return 0;
    return f2(x - 1);
}

int f2 (int x) {
    if (x <= 0) return 1;
    return f1(x - 1);
}
```

##### enumeration

example:
```cpp
enum color {RED, BLUE, GREEN};

enum color c1, c2 = GREEN;

typedef enum {
    STUDENT,
    TEACHER,
    PROFESSOR = 2,
    JANITOR,
    PRESIDENT = JANITOR + TEACHER,
} person_t;
```
