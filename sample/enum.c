enum color {RED, BLUE, GREEN};
typedef enum color color_t;

typedef enum {
    STUDENT,
    TEACHER,
    PROFESSOR = 2,
    JANITOR,
    PRESIDENT = JANITOR + TEACHER,
} person_t;

int main () {
    enum color c1 = RED;
    color_t c2;
    person_t me = STUDENT;
    enum {ADD, SUB} op;

    c2 = GREEN;
    op = ADD;
    c1 = 1.2;

    return 0;
}
