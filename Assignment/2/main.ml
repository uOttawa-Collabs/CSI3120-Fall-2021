(*** CSI 3120 Assignment 2 ***)
(*** Jake Wang ***)
(*** * ***)
(*** OCaml version 4.11.1 ***)


(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)
let exp1a : string = "Each element of the list should be of type (string * int * char)."
let prob1a : (string * int * char) list = [("7", 8, '9')];;


(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)
let exp1b : string = "The type should be a tuple, whose first element is a list of strings and second element is a list of integers."
let prob1b : (string list * int list) = (["apples";"bananas";"carrots"],[3;2;1]);;


(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)
let exp1c : string = "Operator (::) requires the head to be a list element, rather than a list. In this case, the head should be a list of string, rather than a list of list of string."
let prob1c : string list list = ["2"; "b"; "or"; "not"; "2b"] :: [["that is"; "the"]; ["question"]]


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)
let prob2a : (int * ((string * float) option list)) list = [(0, ([Some ("0", 0.0)]))]


(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option
let prob2b : string * pet list option = ("0", Some [("0", "0", Some 0)])


(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)
let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in
    f [(false, "0")]

(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false
 *)
let rec text_filter (xs: char list) (ys: char list): bool =
    match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (xsHead::xsTail, ysHead::ysTail) ->
        if (xsHead == ysHead) then
            text_filter xsTail ysTail
        else
            text_filter xs ysTail

(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)
let rec text_filter (xs: 'a list) (ys: 'a list): bool =
    match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (xsHead::xsTail, ysHead::ysTail) ->
        if (xsHead == ysHead) then
            text_filter xsTail ysTail
        else
            text_filter xs ysTail

(*************)
(* PROBLEM 4 *)
(*************)
(* Write a function that converts an integer into a natural number if
   one exists.  Use an option type because not all integer inputs can
   be converted. *)
type nat = Zero | Succ of nat

let natural_of_int (i: int): nat option =
    if i < 0 then
        None
    else
        let rec recursion (i: int) (accumulator: nat): nat =
            match i with
            | 0 -> accumulator
            | _ -> recursion (i - 1) (Succ accumulator)
        in
            Some (recursion i Zero)
