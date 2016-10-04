(*  COMP 321 Homework 3:  Lexer and parser for a fragment of C.
*
*   ml-ulex specification.
*   
*   N. Danner
*   Fall 2016
*)

(*  The name of the generated structure must be CPPLexer.
*)
%name CPPLexer ;

%let digit = [0-9] ;
%let num={digit}+ ;
%let alpha=[a-zA-Z] ;
%let id={alpha}+ ;
%let ws=[\n\t\ ] ;

(*  You might want to add some definitions, but these should be useful. *)
%defs (
  structure T = CPPGrmTokens
  type lex_result = T.token
  fun eof() = T.EOF
  exception lex_error of string
) ;


"+"               =>	( T.PLUS ) ;
"*"               =>	( T.TIMES ) ;
";"				  =>	( T.SEMICOLON ) ;
{num}             =>	( T.NUM ( valOf (Int.fromString yytext)) ) ;
{id}              =>	( T.ID yytext ) ;
{ws}              =>	( skip() ) ;
.                 =>	( raise Fail ( "Unexpected character: " ^ yytext )) ;



