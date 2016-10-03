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
  datatype exp = ENum of int
               | EId of string
               | EPlus of exp*exp
               | ETimes of exp*exp


  (*  The type of programs.
  *   
  *   You must have this type in your solution.  Do not use the PNone
  *   constructor; it is here solely so that this skeleton file compiles.
  *)
  (*datatype program = Func of fun
                    | Comment of 
                    | EOLComment of 
                    | BlockComment of string
                    | PreproDir of string
                    | LongProgram of program+
*)
  datatype program = PNone

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
       | EId(x) => 
           "EId(" ^ x ^ ")"    
       | ETimes(e0, e1) => 
           "ETimes(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ")"
       | ENum(n) =>
           "ENum(" ^ (Int.toString n) ^ ")"



  (*  programToString p = a string representation of p.
  *   
  *   This function is used by the driver program to print the result of
  *   parsing a program.
  *)
 (*) fun programToString (p : program) : string =
    case p of
      EOLComment(p) => "EOLComment(" ^ p ^ ")"
    | BlockComment(p) => "BlockComment(" ^ p ^ ")"
    | PreproDir(p) => "PreproDir(" ^ p ^ ")"
    | Func(p) => "Func(" 
    | LongProgram(p) => "LongProgram(" ^ programToString p ^ ")"
*)
  fun programToString (p : program) : string =
    ""
end