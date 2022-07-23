(* If you complete Questions 1-5 of lab3.ml, continue with the
   questions in this file for more practice. All of these are optional. *)

let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl);;

(* 6. Write a new version of and_list and or_list from Question 1,
   but this time, use reduce as defined above. *)
let and_list' (lst: bool list) : bool =
    reduce (&&) true lst
           
let or_list' (lst: bool list) : bool = 
    reduce (||) false lst

(* 7. Implement length in terms of reduce.  length lst returns the
   length of lst. length [] = 0. *)        
let length (lst: int list) : int =
    reduce (fun total e -> 1) 0 lst

  
(* 8. Redo Question 3 using reduce.  I.e., write a function that
   returns the max of a list, or None if the list is empty. *)
let max_of_list' (lst: int list): int option =
    match lst with
    | [] -> None
    | head::tail -> Some (reduce max head lst)
               

(* 9. Write a function that returns both the min and max of a list, or
   None if the list is empty. You can use reduce, but you don't have
   to. *)
let bounds (lst:int list): (int option * int option) =
    match lst with
    | [] -> (None, None)
    | head::tail -> (Some (reduce min head lst), Some (reduce max head lst))
