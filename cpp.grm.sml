structure 
CPPGrmTokens = struct

    datatype token = EOF
      | SEMICOLON
      | TIMES
      | PLUS
      | STRING of string
      | ID of string
      | NUM of int

    val allToks = [EOF, SEMICOLON, TIMES, PLUS]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (SEMICOLON) => "SEMICOLON"
  | (TIMES) => "TIMES"
  | (PLUS) => "PLUS"
  | (STRING(_)) => "STRING"
  | (ID(_)) => "ID"
  | (NUM(_)) => "NUM"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (SEMICOLON) => false
  | (TIMES) => false
  | (PLUS) => false
  | (STRING(_)) => false
  | (ID(_)) => false
  | (NUM(_)) => false
(* end case *))

  fun isEOF EOF = true
    | isEOF _ = false

end

functor CPPGrmParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
CPPGrmTokens
    structure UserCode =
      struct


fun ctype_PROD_1_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.CInt NUM )
fun defn_PROD_1_ACT (ID, SEMICOLON, ctype, ID_SPAN : (Lex.pos * Lex.pos), SEMICOLON_SPAN : (Lex.pos * Lex.pos), ctype_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.DDecl ( ctype, ( Ast.CId ID ) ) )
fun pgm_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Pgm SR )
fun exp_PROD_1_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.ENum NUM)
fun exp_PROD_2_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.EId STRING)
fun exp_PROD_3_ACT (PLUS, exp1, exp2, PLUS_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME Ast.EPlus)
fun exp_PROD_4_ACT (exp1, exp2, TIMES, exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME Ast.ETimes)
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) = 
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in 
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) = 
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMICOLON strm = (case (lex(strm))
 of (Tok.SEMICOLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))

val (ctype_NT, exp_NT) = 
let
fun exp_NT (strm) = let
      fun exp_PROD_1 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.exp_PROD_2_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_3 (strm) = let
            val (exp1_RES, exp1_SPAN, strm') = exp_NT(strm)
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm')
            val (exp2_RES, exp2_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(exp1_SPAN), #2(exp2_SPAN))
            in
              (UserCode.exp_PROD_3_ACT (PLUS_RES, exp1_RES, exp2_RES, PLUS_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_4 (strm) = let
            val (exp1_RES, exp1_SPAN, strm') = exp_NT(strm)
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm')
            val (exp2_RES, exp2_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(exp1_SPAN), #2(exp2_SPAN))
            in
              (UserCode.exp_PROD_4_ACT (exp1_RES, exp2_RES, TIMES_RES, exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STRING(_), _, strm') => exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun ctype_NT (strm) = let
      val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
      val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
      in
        (UserCode.ctype_PROD_1_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (ctype_NT, exp_NT)
end
val ctype_NT =  fn s => unwrap (Err.launch (eh, lexFn, ctype_NT , true) s)
val exp_NT =  fn s => unwrap (Err.launch (eh, lexFn, exp_NT , false) s)

in (ctype_NT, exp_NT) end
  in
fun parse lexFn  s = let val (ctype_NT, exp_NT) = mk lexFn in ctype_NT s end

fun parseexp lexFn  s = let val (ctype_NT, exp_NT) = mk lexFn in exp_NT s end

  end

end
