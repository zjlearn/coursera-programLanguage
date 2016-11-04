(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* put your solutions for problem 1 here *)
(*problem1 to get the exception item in the   *)
fun  all_except_option(str:string, strlist: string list)= 
  let fun helper(strlist: string list, isSame : bool, Acc: string list )= 
(* => (bool, Acc)*)
      case strlist of 
	  []=>(isSame , Acc)
	    | head:: tail  => if same_string(head, str) 
			      then helper(tail, true, Acc)
			      else helper(tail, isSame, head :: Acc)
      val result = helper(strlist, false, [])
  in 
    case result of 
	(false, _ )=> NONE
      | (true, re) =>SOME(re) 
  end

val test10 = all_except_option("string", ["string"])
val test11= all_except_option("string", ["string","test"])
val test12= all_except_option("string", ["test"])		     

(*  this function *)
fun get_substitutions1(strlistlist: string list list , s: string)=
  case strlistlist of
      [] => []

fun get_substitutions2(strlistlist: string list list, s:string )=
  55



fun similar_names(strlistlist: string list list , full_name : {first:string, middle:string, last:string} )=
  45
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
