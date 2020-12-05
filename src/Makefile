TARGET = parser
OBJECT = parser.cc parser.o lexer.cc lexer.o print.o symbolTable.o AST_Graph.gv
OUTPUT = parser.output parser.hh

CC = g++ -g -std=c++17 -Wall -Wextra -Wno-unused-result
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS = -lfl 

parser: parser.o lexer.o print.o symbolTable.o
	$(CC) -o $@ $^ $(LIBS)

%.o: %.cc
	$(CC) -c -o $@ $<

lexer.cc: lexer.l parser.hh header.hh
	$(LEX) -o $@ $<

parser.cc: parser.yy header.hh
	$(YACC) $(YACCFLAG) -o $@ $<

parser.hh: parser.cc

print.o: print.hh
symbolTable.o: symbolTable.hh

clean:
	rm -f $(TARGET) $(OBJECT) $(OUTPUT)

