TARGET = parser
OBJECT = parser.cc parser.o lexer.cc lexer.o print.o symbolTable.o semanticAnalysis.o
OUTPUT = parser.output parser.hh AST_Graph.gv

CC = g++ -g -std=c++17 -O2 -Wall -Wextra -Wno-unused-result
LEX = flex
YACC = bison -v
YACCFLAG = -d
LIBS =

parser: parser.o lexer.o print.o symbolTable.o semanticAnalysis.o
	$(CC) -o $@ $^ $(LIBS)

%.o: %.cc
	$(CC) -c -o $@ $<

lexer.cc: lexer.l parser.hh header.hh
	$(LEX) -o $@ $<

parser.cc: parser.yy header.hh
	$(YACC) $(YACCFLAG) -o $@ $<

parser.hh: parser.cc

print.o: print.hh header.hh
symbolTable.o: symbolTable.hh header.hh
semanticAnalysis.o: semanticAnalysis.hh symbolTable.hh header.hh

clean:
	rm -f $(TARGET) $(OBJECT) $(OUTPUT)

