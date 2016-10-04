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
  (*datatype program = Func of fun
                    | Comment of 
                    | EOLComment of 
                    | BlockComment of string
                    | PreproDir of string int x = 7;
                    | LongProgram of program+
*)

  datatype cid = CId of string 
  datatype ctype = CString of string
                |  CInt of int

  datatype defn = DDecl of ctype*cid (*where is id defined*)
  datatype program = Pgm of defn list

 
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
  fun programToString (p : program) : string =
    case p of
        [] => "Pgm()"
        | (a,b)::ys => case (a, b) of 
          (CString(s),CId(i)) => "Pgm(" ^ s ^ " " ^ i ^ ", " ^ (programToString ys)
          | (CInt(n),CId(i)) => "Pgm(" ^ (Int.toString n) ^ " " ^ i ^ ", " ^ (programToString ys)



end