%{
#include <string>
#include <cstring>

#include "header.hh"
#include "parser.hh"

int lineno = 1;

# define YY_DECL \
  yy::parser::symbol_type yylex ()

%}

%option noyywrap nounput

letter          [A-Za-z]
digit           [0-9]
kwInt           "int"
kwFloat         "float"
kwVoid          "void"
kwIf            "if"
kwElse          "else"
kwWhile         "while"
kwFor	        "for"
kwTypedef       "typedef"
kwReturn        "return"
ID              ({letter})({letter}|{digit}|"_")*
op_assign       "="
op_or           "||"
op_and          "&&"
op_not          "!"
op_eq           "=="
op_ne           "!="
op_lt           "<"
op_gt           ">"
op_le           "<="
op_ge           ">="
op_plus         "+"
op_minus        "-"
op_times        "*"
op_divide       "/"
int_constant    {digit}+
flt_constant    (({digit}*\.{digit}+|{digit}+\.)([eE][+-]?{digit}+)?)|({digit}+[eE][+-]?{digit}+)
s-const         \"([^"\n])*\"
comment	        "/*"(([^*])|([*]+[^/*]))*("*")+"/"
ws              [ \t]+
newline         "\n"
mk_lparen       "("
mk_rparen       ")"
mk_lbrace       "{"
mk_rbrace       "}"
mk_lsqbrace     "["
mk_rsqbrace     "]"
mk_comma        ","
mk_semicolon    ";"
mk_dot          "."
error            .

%%

{ws}            ;   /* do nothing with whitespace */
{comment}	    {
                    int i=0;
                    while (yytext[i]!='\0') {
                        if (yytext[i]=='\n')
                            lineno++;
                        i++;
                    }
                }	
{kwInt}         return yy::parser::make_INT();
{kwFloat}       return yy::parser::make_FLOAT();
{kwVoid}		return yy::parser::make_VOID();
{kwIf}          return yy::parser::make_IF();
{kwElse}        return yy::parser::make_ELSE();
{kwWhile}       return yy::parser::make_WHILE();
{kwFor}      	return yy::parser::make_FOR();
{kwTypedef}     return yy::parser::make_TYPEDEF();
{kwReturn}      return yy::parser::make_RETURN();
{ID}			return yy::parser::make_ID(std::string(yytext));
{op_assign}     return yy::parser::make_OP_ASSIGN();
{op_and}        return yy::parser::make_OP_AND();
{op_or}         return yy::parser::make_OP_OR();
{op_not}        return yy::parser::make_OP_NOT();
{op_eq}         return yy::parser::make_OP_EQ();
{op_ne}         return yy::parser::make_OP_NE();
{op_lt}         return yy::parser::make_OP_LT();
{op_gt}         return yy::parser::make_OP_GT();
{op_le}         return yy::parser::make_OP_LE();
{op_ge}         return yy::parser::make_OP_GE();
{op_plus}       return yy::parser::make_OP_PLUS();
{op_minus}      return yy::parser::make_OP_MINUS();
{op_times}      return yy::parser::make_OP_TIMES();
{op_divide}     return yy::parser::make_OP_DIVIDE();
{int_constant}  {
                    Const p;
                    p.const_type = INTEGERC;
                    p.value.emplace<int>(atoi(yytext));
                    return yy::parser::make_CONST(p);
                }
{flt_constant}  {
                    Const p;
                    p.const_type = FLOATC;
                    p.value.emplace<double>(atof(yytext));
                    return yy::parser::make_CONST(p);
                }
{s-const}       {
                    Const p;
                    p.const_type = STRINGC;
                    p.value.emplace<std::string>(yytext);
                    return yy::parser::make_CONST(p);
                }
{mk_lparen}     return yy::parser::make_MK_LPAREN();
{mk_rparen}     return yy::parser::make_MK_RPAREN();
{mk_lbrace}     return yy::parser::make_MK_LBRACE();
{mk_rbrace}     return yy::parser::make_MK_RBRACE();
{mk_lsqbrace}   return yy::parser::make_MK_LB();
{mk_rsqbrace}   return yy::parser::make_MK_RB();
{mk_comma}      return yy::parser::make_MK_COMMA();
{mk_semicolon}  return yy::parser::make_MK_SEMICOLON();
{mk_dot}		return yy::parser::make_MK_DOT();
{newline}       lineno += 1;
{error}         return yy::parser::make_ERROR();

<<EOF>>         return yy::parser::make_END_OF_FILE();

%%
