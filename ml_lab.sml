(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Bryan Plant
* bryanplant@gmail.com
*
***************************************************************)

(* fixes problem where output is truncated with a '#' *)
Control.Print.printDepth := 1024;

(* warmup function *)
fun f [] = [] (* a *)
  | f (x::xs) = (x + 1) :: (f xs) (* b *)

(* datatype with two different types: Set and Empty,
   Set is of type 'element * 'element set *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* returns whether element e is part of the set *)
fun isMember e Empty = false
  | isMember e (Set (x,xs)) = e = x orelse isMember e xs;

fun isSet Empty = true
  | isSet (Set (x,xs)) = if (isMember x xs) then false
                          else isSet xs;

(* converts a given list into a set without duplicates *)
fun list2Set [] = Empty
  | list2Set (x::xs) =
      let fun helper [] = Empty (* builds a set with duplicates *)
            | helper (x::xs) = Set(x, helper (xs))
      in
        (* if x is later in the set don't include x in the set and recurse *)
        if isMember x (helper(xs)) then list2Set(xs)
                      (* otherwise include x and recurse*)
                       else Set(x, list2Set(xs))
      end

(* returns the set representing the mathematical union of 2 sets *)
fun union Empty set2 = set2
  | union (Set (x,xs)) set2 = if isMember x set2 then union xs set2
                              else union xs (Set (x, set2));

(* returns the set representing the mathematical intersection of 2 sets *)
fun intersect Empty set2 = Empty
  | intersect (Set (x,xs)) set2 = if isMember x set2 then Set (x, (intersect xs set2))
                                  else intersect xs set2;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));

(* exit SML *)
OS.Process.exit(OS.Process.success);
