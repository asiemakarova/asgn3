(*  COMP 321 Homework 3:  Lexer and parser for a fragment of C.
*
*   Abstract syntax definitions.
*   
*   N. Danner
*   Fall 2016
*)

structure Ast =
struct

  (*  The type of expressions.
  *   
  *   You must have this type in your solution.  Do not use the ENone
  *   constructor; it is here solely so that this skeleton file compiles.
  *)
  datatype exp = EInt of int
               | EString of string
               | EPlus of exp*exp
               | ETimes of exp*exp
(*               | EFunc of exp list
*)

  (*  The type of programs.
  *   
  *   You must have this type in your solution.  Do not use the PNone
  *   constructor; it is here solely so that this skeleton file compiles.
  That means defining
the Ast.pgm type. I suggest making Ast.pgm a list of definitions, where a definition is
defined by some datatype definition that ought to include some sort of constructor like
DDecl for a variable declaration.
  *)


  type id = string

  type decl = id*exp

  datatype defn = DDecl of decl

  datatype pgm = Pgm of defn list

  (*  ********
  *   'a -> string conversion functions.
  *)

  (*  expToString e = a string representation of e.
  *   
  *   This function is used by the driver program to print the result of
  *   parsing an expression.
  *)
  fun expToString (e : exp) : string =
        case e of
         EPlus(e0, e1) => 
           "EPlus(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ")"
       | EString(x) => 
           "EString(" ^ x ^ ")"    
       | ETimes(e0, e1) => 
           "ETimes(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ")"
       | EInt(n) =>
           "EInt(" ^ (Int.toString n) ^ ")"


  (*  programToString p = a string representation of p.
  *   
  *   This function is used by the driver program to print the result of
  *   parsing a program.
  *)

  fun programToString ( p : pgm ) : string = "hello"

end