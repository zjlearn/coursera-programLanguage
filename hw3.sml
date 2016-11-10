(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(strlist)=
  List.filter( Char.isUpper o String.sub, strlist )

fun longest_string1(strlist)=
  List.foldl( fn (x,y)=>if String.size(x)>= String.size(y) then x else y ,"", strlist )

fun longest_string2(strlist)=
   List.foldl( fn (x,y)=>if String.size(x)> String.size(y) then x else y ,"", strlist )

(* (int*int->bool)-> string list-> string    *)
fun longest_string_helper f= fun strlist => List.foldl( f o String.size, "", strlist)
fun longest_string3(strlist)=longest_string_helper(fun (x, y)=> x >= y)
fun longest_string4(strlist)=longest_string_helper(fun (x, y)=> x>y )


fun longest_capitalize(strlist)=foldl(fun (x,y )=> if Char.isUpper(String.sub(y,0) andalso String.size(y)> String.size(x) then y else x)  , "", strlist)


fun rev_string(str)=String.implode  List.rev o String.explode

(* ('a -> 'b option ) -> 'a list -> 'b *)
fun first_answer f= fun strlist => case strlist of
				       [] => raise NoAnswer
				    | s: xs => case f(s) of SOME then s else f(xs)
(*  ('a-> 'b list option ) -> 'a list->  'b list option *)
fun all_ansmers()
