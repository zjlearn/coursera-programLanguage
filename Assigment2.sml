(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* put your solutions for problem 1 here *)
(*problem1 to get the exception item in the list *)
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

(*  this function return  the list that contain all string except s0*)
fun get_substitutions1(strlistlist: string list list , s: string)=
  case strlistlist of
      [] => []
    | head:: tail => 
      let 
	  val t= all_except_option(s, head);
      in
	 case t of SOME(te) =>  te@get_substitutions1(tail,s)
		| _  =>  get_substitutions1(tail,s)
      end


val test20 = get_substitutions1([["fred","fredrick"],["betty"],["fred"]],"fred");
 

(*tail recursive version of get_substitution  *)
fun get_substitutions2(strlistlist: string list list, s:string )=
  let 
      fun helper(strlistlist: string list list, Acc: string list)=
	case strlistlist of [] => Acc
      | head::tail  => 
	let 
	    val t= all_except_option(s, head)
	in
	    case t of SOME(te) => helper(tail, te@Acc)  
		   | NONE => helper(tail, Acc)
	end
  in
      helper(strlistlist, [])
  end


val test31 = get_substitutions2([["fred","fredrick"],["betty"],["fred"]],"fred")

(* through substitution to get similar names *)
fun similar_names(strlistlist: string list list , full_name : {first:string, middle:string, last:string} )=
  let 
      fun helper(subname: string list, middleName: string, lastName: string, Acc: {first:string, middle:string, last:string} list)=
	case subname of [] => Acc
		      | head :: tail  => helper(tail, middleName, lastName, {first=head, middle=middleName, last=lastName}::Acc)

      val partResult=
	  case full_name of {first=firstName, middle = middleName, last=lastName} => helper( get_substitutions2(strlistlist, firstName), middleName, lastName, [])
   
  in
       full_name::partResult
  end


val test41= similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})




(*-------------------the second problem for a  solitaire card game*)



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* return the color of card*)
fun card_color(s:suit, r:rank )=
  case s  of 
      Clubs => Black
   |  Spades => Black
   | _ => Red 

val test51= card_color(Clubs,Num 2);


(* card value*)
fun card_value(s:suit, r:rank)=
  case r of 
      Num n => n
   | Ace  => 11
   | _  => 10

 val test6 = card_value (Clubs, Num 2) = 2

(* remove card*)
fun remove_card(cs: card list, c: card, e)=
  let 
      fun helper(  cs : card list,hasSame: bool, Acc: card list)=
	case cs of 
	    [] => if hasSame then Acc else raise e
	 | head :: tail  => if hasSame=false  andalso head=c then  helper(tail, true,Acc )  else helper(tail, hasSame, head::Acc) 
in 
    helper(cs, false, [])
end

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
									    


fun all_same_color(cs : card list)= (*card list => bool*)
  case cs of [] => true  (*no element in the list*)
	  |  c1::c2::tail=> 
	     if card_color(c1)=card_color(c2) then all_same_color(c2::tail) else false
	     | _  => true  (*one element in the list *) 



val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true



fun sum_cards(cs: card list)=
 let 
     fun sum(cs : card list , Acc: int)=
       case cs of [] => Acc
		     | c::tail => sum(tail, card_value(c)+Acc)
 in
     sum(cs, 0)
 end


val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4


fun score(cs :card list, goal: int)=
  let 
      val sum= sum_cards(cs);
      val pre_sum = if sum > goal then (sum-goal)*(sum-goal)*(sum-goal) else goal -sum
  in
    if all_same_color(cs)  then pre_sum div 2
    else
	pre_sum
  end

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

fun officiate(cs :card list, movelist: move list, goal: int  )=
  let fun helper(held_cards: card list, movelist: move list, remain: card list )=
	case movelist of [] => score(held_cards, goal)
			 | (Discard c)::tail => helper( remove_card(held_cards, c, IllegalMove), tail, remain)
			   | (Draw)::tail =>  
			     case remain of [] =>  score(held_cards,goal)
						| first:: others =>
						  if sum_cards(first::held_cards) > goal then score(first::held_cards, goal) else
					helper(first::held_cards, tail, others) 
  in
      helper([], movelist, cs)
  end

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)

val test14=score([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
