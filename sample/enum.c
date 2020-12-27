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

    write(c1);
    write("\n");
    write(c2);
    write("\n");
    write(me);
    write("\n");
    write(op);
    write("\n");

    write(STUDENT);
    write("\n");
    write(TEACHER);
    write("\n");
    write(PROFESSOR);
    write("\n");
    write(JANITOR);
    write("\n");
    write(PRESIDENT);
    write("\n");


    return 0;
}
