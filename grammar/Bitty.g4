grammar Bitty;
options { language=Cpp; }

program: statement* EOF;

statement
    : blockStmt         # StmtBlock
    | varDeclStmt       # StmtVarDecl
    | ifStmt            # StmtIf
    | whileStmt         # StmtWhile
    | funcDeclStmt      # StmtFuncDecl
    | printStmt         # StmtPrint   
    | returnStmt SEMI   # StmtReturn
    | expression SEMI   # StmtExpression
    ;

printStmt: PRINT expression SEMI; 

blockStmt: LBRACE statement* RBRACE;

varDeclStmt: LET ID (ASSIGN expression)? SEMI;

ifStmt: IF LPAREN expression RPAREN statement (ELSE statement)?;

whileStmt: WHILE LPAREN expression RPAREN statement;

funcDeclStmt: FN ID LPAREN paramList? RPAREN blockStmt;

returnStmt: RETURN expression?;

expression
    : primary # ExprPrimary
    | expression LPAREN exprList? RPAREN # ExprCall
    | op=(ADD | SUB | NOT) expression # ExprUnary
    | left=expression op=(MUL | DIV) right=expression # ExprMulDiv
    | left=expression op=(ADD | SUB) right=expression # ExprAddSub
    | left=expression op=(LT | GT | LTE | GTE) right=expression # ExprCompare
    | left=expression op=(EQ | NEQ) right=expression # ExprEquality
    | left=expression op=AND right=expression          # ExprLogicalAnd
    | left=expression op=OR  right=expression          # ExprLogicalOr
    | <assoc=right> lhs=ID ASSIGN rhs=expression # ExprAssign
    ;

exprList: expression (COMMA expression)*;
paramList: ID (COMMA ID)*;

primary
    : LPAREN expression RPAREN
    | literal
    | ID
    ;

literal
    : NUMBER
    | STRING
    | TRUE
    | FALSE
    | NIL
    ;

LINE_COMMENT
  : '//' ~[\r\n]* -> skip
  ;
BLOCK_COMMENT
  : '/*' .*? '*/' -> skip
  ;

LET: 'let';
IF: 'if';
ELSE: 'else';
WHILE: 'while';
RETURN: 'return';
FN: 'fn';
PRINT: 'print';
TRUE: 'true';
FALSE: 'false';
NIL: 'nil';

ID: [a-zA-Z_] [a-zA-Z_0-9]*;

NUMBER: [0-9]+ ('.' [0-9]+)?;
STRING: '"' ( '\\' . | ~["\\\r\n] )* '"';

ASSIGN: '=';
SEMI:   ';';
COMMA:  ',';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';

ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';
AND: '&&';
OR:  '||';
NOT: '!';

EQ:  '==';
NEQ: '!=';
LT:  '<';
GT:  '>';
LTE: '<=';
GTE: '>=';

WS: [ \t\r\n]+ -> skip;
