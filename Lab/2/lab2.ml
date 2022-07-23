(* 
  Agenda:
   * tuples and lists
   * options
   * higher order functions

  Note that questions 2, 8, and 9 are optional.
*)

(* An employee is a tuple of name, age, and boolean indicating marriage status *)
type employee = string * int * bool

(* 1. Write a function that takes an employee, and prints out the information in some readable form. *)
let print_employee_info ((name, age, is_married): employee) =
    print_string ("Name:\t\t" ^ name ^ "\n");
    print_string ("Age:\t\t" ^ (string_of_int age) ^ "\n");
    print_string ("Married:\t" ^ (string_of_bool is_married) ^ "\n");;

(* 2. Reimplement the OCaml standard functions List.length and List.rev
   for lists of strings.
   This question is optional, but is good practice for the next one. *)
let length (l: string list): int =
    let rec recursion (l: string list) (depth: int): int =
        match l with
        | [] -> depth
        | _::tail -> recursion tail (depth + 1)
    in
        recursion l 0;;

let rev (l: string list): string list =
    let rec recursion (l: string list) (accumulator: string list): string list =
        match l with
        | [] -> accumulator
        | head::tail -> recursion tail (head::accumulator)
    in
        recursion l [];;


(* 3. Remove the kth element from a list. Assume indexing from 0 *)
(* example: rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * results in: [["to" ; "be" ; "not" ; "to" ; "be"] *)
let rmk (k: int) (l: string list) : string list =
    let rec recursion (l: string list) (depth: int): string list =
        if depth < k then
            match l with
            | [] -> l
            | head::tail -> head::(recursion tail (depth + 1))
        else
            match l with
            | [] -> []
            | _::tail -> tail
    in
        recursion l 0;;


(* 4. Write a function that returns the final element of an integer list,
   if it exists, and None otherwise *)
let final (l: int list) : int option =
    match l with
    | [] -> None
    | _ ->
        let rec recursion (l: int list): int option =
            match l with
            | head::[] -> Some head
            | _::tail -> recursion tail
            | _ -> None
        in
            recursion l

(* 5. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. Do 
 * the same for the larger of two int options.*)
let min_option (x: int option) (y: int option): int option =
    match (x, y) with
    | (None, None) -> None
    | (None, y) -> y
    | (x, None) -> x
    | (Some x, Some y) ->
        if x < y then
            Some x
        else
            Some y;;
    
let max_option (x: int option) (y: int option): int option =
    if x > y then    (* Some _ is always bigger than None *)
        x
    else
        y;;

(* 6. Write a function that returns the integer buried in the argument
 * or None otherwise *)  
let get_option (x: int option option option option): int option =
    match x with 
    | None                  -> None
    | Some None             -> None
    | Some (Some None)      -> None
    | Some (Some (Some a))  -> a


(* 7. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)
let and_option (x: bool option) (y: bool option): bool option = 
    match (x, y) with
    | (None, None) -> None
    | (None, y) -> y
    | (x, None) -> x
    | (Some x, Some y) -> Some (x && y);;

let or_option (x: bool option) (y: bool option): bool option = 
    match (x, y) with
    | (None, None) -> None
    | (None, y) -> y
    | (x, None) -> x
    | (Some x, Some y) -> Some (x || y);;

(* What's the pattern? How can we factor out similar code? *)
(*
 * The detection of x or y being None is duplicated, which we could factor out.
 * We could design a general-purpose function for x and y, which detects if x or y
 * being None, as well as accepting an extra function for actual operation.
 *
 * See Question 8 and 9.
 *)

(**************)
(* Note: Questions 8 and 9 are optional.  We have not yet covered all
   of this material in class, but you still may be able to do them,
   especially if you have read ahead in the class notes. *)
(**************)
                                 
(* 8. Optional:
 * Write a higher-order function for binary operations on options.
 * If both arguments are present, then apply the operation.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
let calc_option (f: 'a -> 'a -> 'a) (x: 'a option) (y: 'a option): 'a option =
    match (x, y) with
    | (None, None) -> None
    | (None, y) -> y
    | (x, None) -> x
    | (Some x, Some y) -> Some (f x y);;

(* What is the type of the calc_option function? *)
(* calc_option: ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option *)

(* 9. Optional:
 * Now rewrite the following functions using the above higher-order function
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other.
 * Do the same for the larger of two int options. *)
let min_option2 (x: int option) (y: int option) : int option = 
    calc_option min x y;;

let max_option2 (x: int option) (y: int option) : int option = 
    calc_option max x y;;
