/* ===== 2020 Definition Section ===== */
%skeleton "lalr1.cc"
%require "3.5.1"
%defines

%code requires {
  #include <cstdarg>
  #include <utility>
  #include <vector>

  #include "header.hh"
  #include "print.hh"
  #include "semanticAnalysis.hh"
}

%code {
  yy::parser::symbol_type yylex();
  AST *prog;

  static inline AST *makeChild(AST * parent, std::vector<AST *> & children) {
    parent->children = std::move(children);
    return parent;
  }

  static inline AST *makeChild(AST * parent, AST * child) {
    if (child == nullptr) return parent;
    parent->children.push_back(child);
    return parent;
  }

  static AST *makeFamily(AST * parent, int childrenCount, ...) {
    va_list childrenList;
    va_start(childrenList, childrenCount);
    int index;
    for (index = 0; index < childrenCount; ++index) {
      AST *child = va_arg(childrenList, AST *);
      parent->children.push_back(child);
    }
    va_end(childrenList);
    return parent;
  }

  static inline AST *makeIDNode(std::string idName, IDENTIFIER_KIND idKind) {
    AST *node = new AST(IDENTIFIER_NODE);
    IdentifierSemanticValue value;
    value.identifierName = idName;
    value.kind = idKind;
    value.isEnumerator = false;
    node->semanticValue = value;
    return node;
  }
  static inline AST *makeStmtNode(STMT_KIND stmtKind) {
    AST *node = new AST(STMT_NODE);
    STMTSemanticValue value;
    value.kind = stmtKind;
    node->semanticValue = value;
    return node;
  }
  static inline AST *makeDeclNode(DECL_KIND declKind) {
    AST *node = new AST(DECLARATION_NODE);
    DECLSemanticValue value;
    value.kind = declKind;
    node->semanticValue = value;
    return node;
  }
  static inline AST *makeExprNode(EXPR_KIND exprKind, int operationEnumValue) {
    AST *node = new AST(EXPR_NODE);
    EXPRSemanticValue value;
    value.isConstEval = 0;
    value.kind = exprKind;
    if (exprKind == BINARY_OPERATION)
      value.op = (BINARY_OPERATOR)operationEnumValue;
    else if (exprKind == UNARY_OPERATION)
      value.op = (UNARY_OPERATOR)operationEnumValue;
    else
      throw "Unknown exprKind in makeExprNode";
    node->semanticValue = value;
    return node;
  }
}

%define parse.trace
%define parse.assert
%define api.value.type variant
%define api.token.constructor
%define api.token.prefix {TOK_}


%token
  VOID "void"
  INT "int"
  FLOAT "float"
  ENUM "enum"
  IF "if"
  ELSE "else"
  WHILE "while"
  FOR "for"
  TYPEDEF "typedef"
  OP_ASSIGN "="
  OP_OR "|"
  OP_AND "&"
  OP_NOT "!"
  OP_EQ "=="
  OP_NE "!="
  OP_GT ">"
  OP_LT "<"
  OP_GE ">="
  OP_LE "<="
  OP_PLUS "+"
  OP_MINUS "-"
  OP_TIMES "*"
  OP_DIVIDE "/"
  MK_LB "["
  MK_RB "]"
  MK_LPAREN "("
  MK_RPAREN ")"
  MK_LBRACE "{"
  MK_RBRACE "}"
  MK_COMMA ","
  MK_SEMICOLON ";"
  MK_DOT "."
  END_OF_FILE 0 "end of file"
  RETURN "return"
  ERROR
;

%token <std::string>ID "identifier"
%token <Const>CONST "constant"

%nterm <AST*> program func_def non_global_block func_body type param expr_null expr
%nterm <AST*> func_decl type_decl var_decl decl cexpr relop_expr init_id
%nterm <AST*> stmt test assign_expr_list relop_expr_list var_ref assign_expr rel_op enum_type
%nterm <AST*> relop_term relop_factor add_op mul_op term factor unary_op enum_type_def enum_type_ref

%nterm <std::vector<AST*>> global_block decl_list stmt_list param_list type_list
%nterm <std::vector<AST*>> func_head_with_param_name func_head_without_param func_head_with_only_type
%nterm <std::vector<AST*>> dim_fn enum_def_list id_list init_id_list dim_decl nonempty_assign_expr_list
%nterm <std::vector<AST*>> nonempty_relop_expr_list dim_list

%start program

%%

/* ==== Grammar Section ==== */

program : global_block
            {
              $$ = makeChild(new AST(PROGRAM_NODE), $1);
              prog = $$;
            }
        ;

global_block  : global_block decl_list func_def
                  {
                    AST *decl_list = makeChild(new AST(VARIABLE_DECL_LIST_NODE), $2);
                    $$ = std::move($1);
                    $$.insert($$.end(), {decl_list, $3});
                  }
              | global_block func_def
                  {
                    $$ = std::move($1);
                    $$.push_back($2);
                  }
              | %empty {}
              ;

non_global_block  : decl_list stmt_list
                      {
                        $$ = new AST(BLOCK_NODE);
                        makeFamily($$, 2, makeChild(new AST(VARIABLE_DECL_LIST_NODE), $1),
                                          makeChild(new AST(STMT_LIST_NODE), $2));
                      }
                  | decl_list
                      {
                        $$ = new AST(BLOCK_NODE);
                        makeChild($$, makeChild(new AST(VARIABLE_DECL_LIST_NODE), $1));
                      }
                  | stmt_list
                      {
                        $$ = new AST(BLOCK_NODE);
                        makeChild($$, makeChild(new AST(STMT_LIST_NODE), $1));
                      }
                  | %empty { $$ = new AST(BLOCK_NODE); }
                  ;

func_def  : func_head_with_param_name func_body
              {
                $$ = makeDeclNode(FUNCTION_DECL);
                makeChild($$, $1);
                makeChild($$, $2);
              }
          | func_head_without_param func_body
              {
                $$ = makeDeclNode(FUNCTION_DECL);
                makeChild($$, $1);
                makeChild($$, $2);
              }
          ;

func_head_with_param_name : type ID MK_LPAREN param_list MK_RPAREN
                              {
                                AST *parameterList = new AST(PARAM_LIST_NODE);
                                makeChild(parameterList, $4);
                                $$.push_back($1);
                                $$.push_back(makeIDNode($2, NORMAL_ID));
                                $$.push_back(parameterList);
                              }
                          ;

func_head_with_only_type  : type ID MK_LPAREN type_list MK_RPAREN
                              {
                                AST *parameterList = new AST(PARAM_LIST_NODE);
                                makeChild(parameterList, $4);
                                $$.push_back($1);
                                $$.push_back(makeIDNode($2, NORMAL_ID));
                                $$.push_back(parameterList);
                              }
                          ;

func_head_without_param : type ID MK_LPAREN MK_RPAREN
                            {
                              AST *emptyParameterList = new AST(PARAM_LIST_NODE);
                              $$.push_back($1);
                              $$.push_back(makeIDNode($2, NORMAL_ID));
                              $$.push_back(emptyParameterList);
                            }
                        ;

func_body : MK_LBRACE non_global_block MK_RBRACE { $$ = $2; }
          ;

param_list  : param_list MK_COMMA param
                {
                  $$ = std::move($1);
                  $$.push_back($3);
                }
            | param { $$.push_back($1); }
            ;

param : type ID
          {
            $$ = makeDeclNode(FUNCTION_PARAMETER_DECL);
            makeFamily($$, 2, $1, makeIDNode($2, NORMAL_ID));
          }
      | type ID dim_fn
          {
            $$ = makeDeclNode(FUNCTION_PARAMETER_DECL);
            makeFamily($$, 2, $1, makeChild(makeIDNode($2, ARRAY_ID), $3));
          }
      ;

type_list : type_list MK_COMMA type
              {
                $$ = std::move($1);
                $$.push_back($3);
              }
          | type { $$.push_back($1); }
          ;

dim_fn  : MK_LB expr_null MK_RB { $$.push_back($2); }
        | dim_fn MK_LB expr MK_RB
            {
              $$ = std::move($1);
              $$.push_back($3);
            }
		    ;

expr_null : expr
          | %empty { $$ = new AST(NUL_NODE); }
          ;

decl_list : decl_list decl
              {
                $$ = std::move($1);
                $$.push_back($2);
              }
          | decl { $$.push_back($1); }
          ;

decl  : type_decl
      | var_decl
      | func_decl
      ;

type_decl : TYPEDEF type id_list MK_SEMICOLON
              {
                /* TODO: resolve insert front */
                $$ = makeDeclNode(TYPE_DECL);
                makeChild($$, $3);
                $$->children.insert($$->children.begin(), $2);
              }
          | enum_type_def MK_SEMICOLON
              {
                $$ = makeDeclNode(ENUM_DECL);
                makeChild($$, $1);
              }
          ;

var_decl  : type init_id_list MK_SEMICOLON
              {
                /* TODO: resolve insert front */
                $$ = makeDeclNode(VARIABLE_DECL);
                makeChild($$, $2);
                $$->children.insert($$->children.begin(), $1);
              }
          ;

func_decl : func_head_with_param_name MK_SEMICOLON
              {
                $$ = makeDeclNode(FUNCTION_DECL);
                makeChild($$, $1);
              }
          | func_head_with_only_type MK_SEMICOLON
              {
                $$ = makeDeclNode(FUNCTION_DECL);
                makeChild($$, $1);
              }
          | func_head_without_param MK_SEMICOLON
              {
                $$ = makeDeclNode(FUNCTION_DECL);
                makeChild($$, $1);
              }
          ;

type  : INT { $$ = makeIDNode("int", NORMAL_ID); }
      | FLOAT { $$ = makeIDNode("float", NORMAL_ID); }
      | VOID { $$ = makeIDNode("void", NORMAL_ID); }
      | enum_type
      | ID { $$ = makeIDNode($1, NORMAL_ID); }
      ;

enum_type : enum_type_ref
          | enum_type_def
          ;

enum_type_ref : ENUM ID
                  {
                    $$ = new AST(ENUM_NODE);
                    makeChild($$, makeIDNode($2, NORMAL_ID));
                  }
              ;

enum_type_def : ENUM ID MK_LBRACE enum_def_list MK_RBRACE
                  {
                    $$ = new AST(ENUM_NODE);
                    makeChild($$, $4);
                    $$->children.insert($$->children.begin(), makeIDNode($2, NORMAL_ID));
                  }
              | ENUM ID MK_LBRACE enum_def_list MK_COMMA MK_RBRACE
                  {
                    $$ = new AST(ENUM_NODE);
                    makeChild($$, $4);
                    $$->children.insert($$->children.begin(), makeIDNode($2, NORMAL_ID));
                  }
              | ENUM MK_LBRACE enum_def_list MK_RBRACE
                  {
                    $$ = new AST(ENUM_NODE);
                    makeChild($$, $3);
                    $$->children.insert($$->children.begin(), new AST(NUL_NODE));
                  }
              | ENUM MK_LBRACE enum_def_list MK_COMMA MK_RBRACE
                  {
                    $$ = new AST(ENUM_NODE);
                    makeChild($$, $3);
                    $$->children.insert($$->children.begin(), new AST(NUL_NODE));
                  }
              ;

enum_def_list : ID
                  {
                    $$.push_back(makeIDNode($1, NORMAL_ID));
                  }
              | ID OP_ASSIGN cexpr
                  {
                    $$.push_back(makeChild(makeIDNode($1, WITH_INIT_ID), $3));
                  }
              | enum_def_list MK_COMMA ID
                  {
                    $$ = std::move($1);
                    $$.push_back(makeIDNode($3, NORMAL_ID));
                  }
              | enum_def_list MK_COMMA ID OP_ASSIGN cexpr
                  {
                    $$ = std::move($1);
                    $$.push_back(makeChild(makeIDNode($3, WITH_INIT_ID), $5));
                  }
              ;

id_list : ID { $$.push_back(makeIDNode($1, NORMAL_ID)); }
        | ID dim_decl
            {
                $$.push_back(makeChild(makeIDNode($1, ARRAY_ID), $2));
            }
        | id_list MK_COMMA ID
            {
                $$ = std::move($1);
                $$.push_back(makeIDNode($3, NORMAL_ID));
            }
        | id_list MK_COMMA ID dim_decl
            {
                $$ = std::move($1);
                $$.push_back(makeChild(makeIDNode($3, ARRAY_ID), $4));
            }
        ;

dim_decl	: MK_LB cexpr MK_RB { $$.push_back($2); }
          | dim_decl MK_LB cexpr MK_RB
              {
                $$ = std::move($1);
                $$.push_back($3);
              }
          ;

cexpr : relop_expr
      ;

init_id_list  : init_id { $$.push_back($1); }
              | init_id_list MK_COMMA init_id
                  {
                    $$ = std::move($1);
                    $$.push_back($3);
                  }
              ;

init_id : ID { $$ = makeIDNode($1, NORMAL_ID); }
        | ID dim_decl
            {
              $$ = makeChild(makeIDNode($1, ARRAY_ID), $2);
            }
        | ID OP_ASSIGN relop_expr
            {
              $$ = makeChild(makeIDNode($1, WITH_INIT_ID), $3);
            }
        ;

stmt_list : stmt_list stmt
              {
                $$ = std::move($1);
                $$.push_back($2);
              }
          | stmt { $$.push_back($1); }
          ;

stmt  : MK_LBRACE non_global_block MK_RBRACE {$$ = $2; }
      | MK_SEMICOLON {$$ = new AST(NUL_NODE);}
      | WHILE MK_LPAREN test MK_RPAREN stmt
          {
            $$ = makeStmtNode(WHILE_STMT);
            $$ = makeFamily($$, 2, $3, $5);
          }
      | FOR MK_LPAREN assign_expr_list MK_SEMICOLON relop_expr_list MK_SEMICOLON assign_expr_list MK_RPAREN stmt
          {
            $$ = makeStmtNode(FOR_STMT);
            $$ = makeFamily($$, 4, $3, $5, $7, $9);
          }
      | var_ref OP_ASSIGN relop_expr MK_SEMICOLON
          {
            $$ = makeStmtNode(ASSIGN_STMT);
            $$ = makeFamily($$, 2, $1, $3);
          }
      | IF MK_LPAREN test MK_RPAREN stmt
          {
            $$ = makeStmtNode(IF_STMT);
            $$ = makeFamily($$, 3, $3, $5, new AST(NUL_NODE));
          }
      | IF MK_LPAREN test MK_RPAREN stmt ELSE stmt
          {
            $$ = makeStmtNode(IF_STMT);
            $$ = makeFamily($$, 3, $3, $5, $7);
          }
      | ID MK_LPAREN relop_expr_list MK_RPAREN MK_SEMICOLON
          {
            $$ = makeStmtNode(FUNCTION_CALL_STMT);
            $$ = makeFamily($$, 2, makeIDNode($1, NORMAL_ID), $3);
          }
      | RETURN MK_SEMICOLON
          {
            $$ = makeStmtNode(RETURN_STMT);
            $$ = makeChild($$, new AST(NUL_NODE));
          }
      | RETURN relop_expr MK_SEMICOLON
          {
            $$ = makeStmtNode(RETURN_STMT);
            $$ = makeChild($$, $2);
          }
      ;

assign_expr_list  : nonempty_assign_expr_list
                      {
                        $$ = new AST(NONEMPTY_ASSIGN_EXPR_LIST_NODE);
                        makeChild($$, $1);
                      }
                  | %empty { $$ = new AST(NUL_NODE); }
                  ;

nonempty_assign_expr_list : nonempty_assign_expr_list MK_COMMA assign_expr
                              {
                                $$ = std::move($1);
                                $$.push_back($3);
                              }
                          | assign_expr { $$.push_back($1); }
                          ;

test  : assign_expr
      ;

assign_expr : ID OP_ASSIGN relop_expr
                {
                  $$ = makeStmtNode(ASSIGN_STMT);
                  makeFamily($$, 2, makeIDNode($1, NORMAL_ID), $3);
                }
            | relop_expr
		        ;


relop_expr	: relop_term
            | relop_expr OP_OR relop_term
                {
                  $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_OR);
                  makeFamily($$, 2, $1, $3);
                }
            ;

relop_term	: relop_factor
            | relop_term OP_AND relop_factor
                {
                  $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_AND);
                  makeFamily($$, 2, $1, $3);
                }
            ;

relop_factor  : expr
              | expr rel_op expr
                  {
                    $$ = makeFamily($2, 2, $1, $3);
                  }
              ;

rel_op  : OP_EQ { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_EQ); }
        | OP_GE { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_GE); }
        | OP_LE { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_LE); }
        | OP_NE { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_NE); }
        | OP_GT { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_GT); }
        | OP_LT { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_LT); }
        ;


relop_expr_list : nonempty_relop_expr_list
                    {
                      $$ = new AST(NONEMPTY_RELOP_EXPR_LIST_NODE);
                      makeChild($$, $1);
                    }
                | %empty { $$ = new AST(NUL_NODE); }
                ;

nonempty_relop_expr_list  : nonempty_relop_expr_list MK_COMMA relop_expr
                              {
                                $$ = std::move($1);
                                $$.push_back($3);
                              }
                          | relop_expr { $$.push_back($1); }
                          ;

expr  : expr add_op term
          {
            $$ = makeFamily($2, 2, $1, $3);
          }
      | term
      ;

add_op  : OP_PLUS { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_ADD); }
        | OP_MINUS { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_SUB); }
        ;

term  : term mul_op factor
          {
            $$ = makeFamily($2, 2, $1, $3);
          }
      | factor
      ;

mul_op  : OP_TIMES { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_MUL); }
        | OP_DIVIDE { $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_DIV); }
        ;

factor  : MK_LPAREN relop_expr MK_RPAREN { $$ = $2; }
        | unary_op MK_LPAREN relop_expr MK_RPAREN
            {
              $$ = makeChild($1, $3);
            }
        | CONST
            {
              $$ = new AST(CONST_VALUE_NODE);
              $$->semanticValue = $1;
            }
        | unary_op CONST
            {
              AST *val = new AST(CONST_VALUE_NODE);
              val->semanticValue = $2;
              $$ = makeChild($1, val);
            }
        | ID MK_LPAREN relop_expr_list MK_RPAREN
            {
              $$ = makeStmtNode(FUNCTION_CALL_STMT);
              makeFamily($$, 2, makeIDNode($1, NORMAL_ID), $3);
            }
        | unary_op ID MK_LPAREN relop_expr_list MK_RPAREN
            {
              AST *func_call = makeStmtNode(FUNCTION_CALL_STMT);
              makeFamily(func_call, 2, makeIDNode($2, NORMAL_ID), $4);
              $$ = makeChild($1, func_call);
            }
        | var_ref
        | unary_op var_ref
            {
              $$ = makeChild($1, $2);
            }
        ;

var_ref : ID { $$ = makeIDNode($1, NORMAL_ID); }
        | ID dim_list { $$ = makeChild(makeIDNode($1, ARRAY_ID), $2); }
        ;


dim_list  : dim_list MK_LB expr MK_RB
              {
                $$ = std::move($1);
                $$.push_back($3);
              }
          | MK_LB expr MK_RB { $$.push_back($2); }
		      ;

unary_op  : OP_PLUS { $$ = makeExprNode(UNARY_OPERATION, UNARY_OP_POSITIVE); }
          | OP_MINUS { $$ = makeExprNode(UNARY_OPERATION, UNARY_OP_NEGATIVE); }
          | OP_NOT { $$ = makeExprNode(UNARY_OPERATION, UNARY_OP_LOGICAL_NEGATION); }
		      ;

%%

extern char *yytext;

namespace yy {
  void parser::error (const std::string &msg) {
    std::cerr << "Line #" << lineno << ": " << msg << ", next token: " << yytext << std::endl;
    exit(1);
  }
} // namespace yy

int main(int argc, char *argv[]) {
  if (argc > 1) freopen(argv[1], "r", stdin);
  yy::parser parse;
  // parse.set_debug_level(1);
  parse();
  assert(prog != nullptr);
  ASTPrinter printer(prog);
  printer.print();
  SemanticAnalysis semanticAnalysis(prog);
  semanticAnalysis.runAnalysis();
  if (!semanticAnalysis.anySemanticError)
    std::cout << "Parsing completed. No errors found.\n";
  else
    return 1;
  return 0;
}
