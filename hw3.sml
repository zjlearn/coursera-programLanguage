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
  List.filter (fn x=> Char.isUpper(String.sub(x,0))) strlist



fun longest_string1(strlist)=
  List.foldl (fn(x,Acc)=>if String.size(x)> String.size(Acc) then x else Acc)  "" strlist 

fun longest_string2(strlist)=
   List.foldl (fn (x,Acc)=>if String.size(x)>= String.size(Acc) then x else Acc) "" strlist



(* (int*int->bool)-> string list-> string    *)
fun longest_string_helper f strlist= List.foldl  (fn  (y,Acc)=>if f(String.size(y), String.size(Acc)) then y else Acc) "" strlist

val longest_string3 = longest_string_helper op> 
val longest_string4 = longest_string_helper op>= 




val  longest_capitalized=longest_string1 o only_capitals


val  rev_string= String.implode o rev o String.explode


(* ('a -> 'b option ) -> 'a list -> 'b *)
fun first_answer f strlist =
  case strlist of
      [] => raise NoAnswer
    | s::xs => case f(s) of SOME(t) => t
			  | _  => first_answer f xs


(*  ('a-> 'b list option ) -> 'a list->  'b list option *)
fun all_answers f alist =  
  let fun helper(alist, Acc)=
	case alist of 
	    [] => SOME Acc
	  | x::xs' => case f(x) of NONE => NONE
				 | SOME(e) => helper(xs', e@Acc)
  in
      helper(alist, [])
  end 


(*  use g to define the function  *)
val  count_wildcards  =g (fn () =>1) (fn _ =>0) 

val  count_wild_and_variable_lengths = g (fn ()=>1) String.size

fun count_some_var(str, p)= g (fn () =>0) (fn x=> if(str=x) then 1 else 0) p


(*" 10  pattern -> bool "*)
fun check_pat p = 
  let 
      fun strUsed pat=
	case pat of Variable s => [s]
		| TupleP plist => List.foldl (fn (x,Acc)=>strUsed(x)@Acc) [] plist 
		| ConstructorP (str,pat)=> strUsed(pat)
		| _ =>[] 
      fun hasRepeats(strlist)=
	case strlist of [] => true
		      | x:: xs =>  (not (List.exists (fn t=> t=x) xs)) andalso hasRepeats(xs)
  in
      hasRepeats(strUsed(p))  
  end




(*11 alu* pattern -> (string * valu) list option *)
fun match(t) = 
  case t of (_ , Wildcard) => SOME ([])
	   |( v, Variable s) => SOME([(s,v)])
	   | (Unit,UnitP ) =>SOME([])
	   | (Const x, ConstP s) =>if x=s then SOME([]) else NONE
	   | ( Tuple vs ,TupleP ps ) => if length vs = length ps 
					then  all_answers  match  (ListPair.zip(vs,ps))
					else NONE
	   | (Constructor (s2,v), ConstructorP(s1,p)) => if s1=s2 then  match(v, p) else NONE
	   | _ => NONE         



(* valu=>  pattern => (string * valu) list option  *)
fun first_match  value patlst  =
  SOME (first_answer (fn x=> match(value,  x))  patlst)
			   handle NoAnswer => NONE



