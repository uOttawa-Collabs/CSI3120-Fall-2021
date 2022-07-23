(*** CSI 3120 Assignment 3 ***)
(*** Jake Wang ***)
(*** * ***)
(*** OCaml version 4.11.1 ***)


(*************)
(* PROBLEM 1 *)
(*************)

(* Problem 1a: Below is a definition of the datatype for propositional
   logic from the Data Types tutorial, with connectives for
   conjunction, (the operator written /\), disjunction (the OR
   operator written \/), and logical implication (written ->).  *)

type prop = string

type form =
  | True
  | False
  | Prop of prop 
  | And of form * form
  | Or of form * form
  | Imp of form * form

(* Write a function "count_atoms" that takes a "form" as input and
   counts the number of atoms.  The atoms include "True", "False", and
   "Prop".  For example, the formula below contains 6 atoms, which
   include 2 occurrences of p, 1 occurrence of q, 2 occurrences of r,
   and 1 occurrence of False.  (Note that every occurrence of an atom,
   even if it is repeated, counts as 1 atom.)

   ((p \/ q) /\ (False \/ p \/ r)) -> r *)

let rec count_atoms (f: form): int =
    match f with
    | And(f1, f2)
    | Or(f1, f2)
    | Imp(f1, f2) -> count_atoms f1 + count_atoms f2
    | _ -> 1

(* Problem 1b: Consider the new types form' and env below *)

type form' =
  | True'
  | False'
  | Prop' of prop * bool
  | And' of form' * form'
  | Or' of form' * form'
  | Imp' of form' * form'

type env = (prop * bool) list

(* The type form' is like form, except that the constructor for
   propsitional variables includes an additional boolean argument to
   indicate the truth value of the proposition. The type env is a list
   of propositional variables and their truth values.  Write a function
   "get_env" that takes a proposition and returns an "env", which simply
   extracts the information from the propositional variables in a
   formula and returns this information in the form of a list.  For
   example, if the input is the formula

  (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)

  the output should be a list containing 5 pairs. Do not remove duplicates
  and don't worry about inconsistencies (the same propositional variable
  paired with both "true" and "false").
 *)

let rec get_env (f: form'): env =
    match f with
    | Prop'(p, b) -> [(p, b)]
    | And'(f1, f2)
    | Or'(f1, f2)
    | Imp'(f1, f2) -> (get_env f1) @ (get_env f2)
    | _ -> []



(* Problem 1c: Write a function "simplify_env" that takes an "env" as
   input and returns an option type.  The result should be None if the
   input is inconsistent (i.e., there is at least one propositional
   variable that is paired with both "true" and "false").  Otherwise
   the function should return the environment with duplicates removed.
   For example, consider the 2 formulas below:

   (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)
   (((p,true) \/ (q,false)) /\ (False \/ (p,true) \/ (r,true))) -> (r,true)

   The function should return None for the first one because of the
   occurrences of (p,true) and (p,false).  For the second one, the
   function should return a list of length 3 (as the argument to
   Some), where the elements include (p,true), (q,false), and
   (r,true).  Hint: you will likely need some helper functions.
   You can choose to define them as local functions inside the main
   function, but you do not have to.  *)

let simplify_env (e: env): env option =
    let rec recursion (e: env) (accumulator: env): env option =
        match e with
        | [] -> Some accumulator
        | (headProp, headValue)::tail ->
            let rec isConsistent (e: env) ((p, v): prop * bool): int =
                match e with
                | [] -> 1    (* True *)
                | (headProp, headValue)::tail ->
                    if (p = headProp) then
                        if (v = headValue) then
                            2    (* Ignore *)
                        else
                            0    (* False *)
                    else
                        isConsistent tail (p, v)
            in
                let result = (isConsistent accumulator (headProp, headValue)) in
                    match result with
                    | 1 -> recursion tail ((headProp, headValue)::accumulator)
                    | 2 -> recursion tail accumulator
                    | _ -> None
    in
        recursion e []


(*************)
(* PROBLEM 2 *)
(*************)

(* Below is a signature of a module for a functional version of a
   queue data structure where all elements of the queue are strings. A
   queue is a First-In-First-Out (FIFO) data structure. In a FIFO data
   structure, the first element added to the queue will be the first
   one to be removed. This is equivalent to the requirement that once
   a new element is added, all elements that were added before have to
   be removed before the new element can be removed. By reading the
   types and the comments, you will see the differences between
   stacks, as studied in class, and queues. *)

module type StringQueue =
sig
    (* t is a queue whose elements have type string. *)
    type t

    (* The empty queue. *)
    val empty: unit -> t

    (* Whether a queue is empty. *)
    val is_empty: t -> bool

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    val enqueue: string -> t -> t

    (* [peek q] is [Some x], where [x] is the element at the front of the queue,
       or [None] if the queue is empty. *)
    val peek: t -> string option

    (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
       of [q] except the front of [q], or [None] if [q] is empty. *)
    val dequeue: t -> t option
end

(* Problem 2a: Implement a queue (define a module called
   ReverseListStringQueue containing a structure whose type is
   StringQueue).  Represent your queues as lists, whose elements are
   in reverse order, which means that the first element of the list is
   the last one entered into the queue, and the last element of the
   list is the front of the queue.  In other words, represent a queue
   as a list, where the list [sn; ...; s2; s1] represents the queue with
   [s1] at its front, followed by [s2], ..., followed by [sn]. You are
   of course allowed to implement helper functions inside the structure
   that do not appear in the signature above. *)

module ReverseListStringQueue: StringQueue =
struct
    type t = string list
    
    let empty (): t = []
    
    let is_empty (q: t): bool =
        q = []
    
    let enqueue (s: string) (q: t): t =
        s::q
    
    let rec peek (q: t): string option =
        match q with
        | [] -> None
        | head::[] -> Some head
        | _::tail -> peek tail
    
    let dequeue (q: t): t option =
        match q with
        | [] -> None
        | _::[] -> Some []
        | _ -> 
            let rec recursion (q: t): t =
                match q with
                | head::[] -> []
                | head::tail ->
                    head::(recursion tail)
                | _ -> []
            in
                Some (recursion q)
end

let q0 = ReverseListStringQueue.empty ()
let q1 = ReverseListStringQueue.enqueue "hello" q0
let q2 = ReverseListStringQueue.enqueue "bonjour" q1
let  i = ReverseListStringQueue.peek q2
let  j = let rest = ReverseListStringQueue.dequeue q2 in
         match rest with
         | Some q -> ReverseListStringQueue.peek q
         | None -> None



(* Problem 2b: Modify the StringQueue signature above so that it is
   polymorphic in the sense that it can be used to create queues of
   elements of any type, not just strings. (This should be relatively
   easy.) *)

module type Queue =
sig
    type 'a queue

    val empty: unit -> 'a queue

    val is_empty: 'a queue -> bool

    val enqueue: 'a -> 'a queue -> 'a queue
    
    val peek: 'a queue -> 'a option

    val dequeue: 'a queue -> 'a queue option
end

    
(* Problem 2c: Modify your solution to Question 1a by
   introducing a new structure called ReverseListQueue with type
   Queue so that it can be used to create queues of elements of
   any type. (This should also be relatively easy.) *)

module ReverseListQueue: Queue =
struct
    type 'a queue = 'a list
    
    let empty (): 'a queue = []
    
    let is_empty (q: 'a queue): bool =
        q = []
    
    let enqueue (e: 'a) (q: 'a queue): 'a queue =
        e::q
    
    let rec peek (q: 'a queue): 'a option =
        match q with
        | [] -> None
        | head::[] -> Some head
        | _::tail -> peek tail
    
    let dequeue (q: 'a queue): 'a queue option =
        match q with
        | [] -> None
        | _::[] -> Some []
        | _ -> 
            let rec recursion (q: 'a queue): 'a queue =
                match q with
                | head::[] -> []
                | head::tail ->
                    head::(recursion tail)
                | _ -> []
            in
                Some (recursion q)
end


let q3 = ReverseListQueue.empty ()
let q4 = ReverseListQueue.enqueue "hello" q3
let q5 = ReverseListQueue.enqueue "bonjour" q4
let i1 = ReverseListQueue.peek q5
let j1 = let rest = ReverseListQueue.dequeue q5 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None
let q6 = ReverseListQueue.empty ()
let q7 = ReverseListQueue.enqueue (3,"three") q6
let q8 = ReverseListQueue.enqueue (4,"four") q7
let i2 = ReverseListQueue.peek q8
let j2 = let rest = ReverseListQueue.dequeue q8 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None


(* Problem 2d: Fill in all the "..." in the modified version below
   of the StringQueue signature above so that it is a signature
   for an imperative version of a string queue called
   ImpStringQueue. *)

module type ImpStringQueue =
sig
    (* t is a queue whose elements have type string. *)
    type t

    (* Create an empty queue. *)
    val empty: unit -> t

    (* Whether a queue is empty. *)
    val is_empty: t -> bool

    (* [enqueue x q] modifies [q] by adding [x] to the end. *)
    val enqueue: string -> t -> unit

    (* [dequeue q] returns [Some x], where [x] is the element at the
       front of the queue, or [None] if the queue is empty.  It also
       modifies q by removing the front element, if there is one;
       otherwise no modifications are done.  *)
    val dequeue: t -> string option
end


(* Problem 2e: Modify your solution to Question 1a (using reversed
   lists as before) by introducing a new structure called
   ImpListStringQueue with type ImpStringQueue so that it
   implements an imperative queue. *)
                     
module ImpListStringQueue: ImpStringQueue =
struct
    type t = (string list) ref
    
    let empty (): t =
        ref []
        
    let is_empty (q: t): bool =
        !q = []
        
    let enqueue (s: string) (q: t): unit =
        q := s::!q
        
    let dequeue (q: t): string option =
        match !q with
        | [] -> None
        | head::[] ->
            q := [];
            Some head
        | _ ->
            let rec recursion q (h: string ref) =
                match q with
                | head::[] ->
                    h := head;
                    []
                | head::tail ->
                    head::(recursion tail h)
                | _ -> []
            in
                let h: string ref = ref "" in
                    q := (recursion !q h);
                    Some !h
end


let q' = ImpListStringQueue.empty ()
let _  = ImpListStringQueue.enqueue "hello" q'
let _  = ImpListStringQueue.enqueue "bonjour" q'
let  i = ImpListStringQueue.dequeue q'
let  j = ImpListStringQueue.dequeue q'
let  k = ImpListStringQueue.dequeue q'
