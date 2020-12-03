TARGET = parser
OBJECT = parser.cc parser.o lexer.cc lexer.o print.o
OUTPUT = parser.output parser.hh

CC = g++ -g -std=c++17 -Wall -Wextra -Wno-unused-result
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS = -lfl 

parser: parser.o lexer.o print.o
	$(CC) -o $@ $^ $(LIBS)

%.o: %.cc
	$(CC) -c -o $@ $<

lexer.cc: lexer.ll parser.hh header.hh
	$(LEX) -o $@ $<

parser.cc: parser.yy header.hh
	$(YACC) $(YACCFLAG) -o $@ $<

parser.hh: parser.cc

clean:
	rm -f $(TARGET) $(OBJECT) $(OUTPUT)

