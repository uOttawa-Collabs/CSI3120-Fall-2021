(* A condensed version of the signature in range.mli.    Your first step is to study the contents of range.mli. *)
module type RANGE =
sig
    type t
    type e
    val singleton : e -> t
    val range : e -> e -> t
    val sadd : t -> e -> t
    val smult : t -> e -> t
    val bridge : t -> t -> t
    val size : t -> int
    val contains : t -> e -> bool
    val rless : t -> t -> bool option
end

(* An implementation of the RANGE datatype with int as range type and
    pairs representing a range *)
module LoHiPairRange : RANGE with type e = int =
struct
    type e = int
    type t = e * e
    let singleton (i:e) : t = (i,i)
    let range (i:e) (j:e) : t = ((min i j), (max i j))
    let sadd (x:t) (i:e) : t = let (lo,hi) = x in (lo+i,hi+i)
    let smult (x:t) (i:e) : t =
        let (lo, hi) = x in
        if i >= 0 then (lo*i,hi*i)
        else (hi*i,lo*i)
    let bridge (x:t) (y:t) : t =
        let (lx, hx) = x in
        let (ly, hy) = y in
        ((min lx ly), (max hx hy))
    let size (x:t) : int =
        let (lo,hi) = x in
        hi - lo - (-1)
    let contains (x:t) (i:e) : bool =
        let (lo,hi) = x in
        (lo <= i) && (i <= hi)
    let rless (x:t) (y:t) : bool option =
        let (lx, hx) = x in
        let (ly, hy) = y in
        if hx < ly then Some true
        else if hy < lx then Some false
        else None
end

(* Exercise 1: Complete the new implementation of RANGE in the
        ListRange module below.    The part that is already implemented
        should give you enough information to implement the rest.    Add
        some test code to test your implementation. *)
        
(* An implementation of the RANGE datatype with int as range type and
    lists representing a range *)
module ListRange : RANGE with type e = int =
struct
    type e = int
    type t = e list

    (* auxiliary functions *)
    let minmax (l:t) : (e*e) option =
        let rec max (t:t) (e:e) : e =
            match t with
            | [] -> e
            | h::r -> max r h
        in
            match l with
            | [] -> None
            | h::r -> Some (h, (max r h))
    let rec build (i:e) (j:e) : e list =
        if i = j then [j]
        else i :: build (i+1) j
    
    let singleton (i:e) : t = [i]
    let range (i:e) (j:e) : t = build (min i j) (max i j)
    
    (* Exercise 1: Replace all the code below with correct implementations of the operations. *)
    let rec getRangeMax (r: t): e =
        match r with
        | [] -> raise (Invalid_argument "getRangeMax: internal list should not be empty")
        | head::[] -> head
        | _::tail -> getRangeMax tail
    
    let rec sadd (x: t) (i: e): t =
        match x with
        | [] -> []
        | head::tail -> (head + i)::(sadd tail i)
    let smult (x: t) (i: e): t =
        match x with
        | [] -> []
        | head::tail ->
            if i > 0 then
                build (head * i) ((getRangeMax tail) * i)
            else if i = 0 then
                []
            else
                build ((getRangeMax tail) * i) (head * i)
    let bridge (x: t) (y: t): t =
        match (x, y) with
        | (headX::tailX, headY::tailY) ->
            let newRangeLow =
                min headX headY
            in
                let newRangeHigh =
                    max (getRangeMax tailX) (getRangeMax tailY)
                in
                    build newRangeLow newRangeHigh
        | (_::_, []) -> x
        | ([], _::_) -> y
        | _ -> []
    let size (x: t): int =
        let rec recursion (r: t) (accumulator: int): int =
            match r with
            | [] -> accumulator
            | _::tail -> recursion tail (accumulator + 1)
        in
            recursion x 0
    let rec contains (x: t) (i: e): bool =
        match x with
        | [] -> false
        | head::tail ->
            if head = i then
                true
            else
                contains tail i
    let rless (x: t) (y: t) : bool option =
        match (x, y) with
        | (headX::tailX, headY::tailY) ->
            if (getRangeMax tailX) < headY then
                Some true
            else if (getRangeMax tailY) < headX then
                Some false
            else
                None
        | _ -> None
end

(* Exercise 1: Add some test code to test your new implementation. *)
let r = ListRange.range (-8) 7
let s = ListRange.range 8 16

let r' = ListRange.sadd r (-1)

let r'' = ListRange.smult r 3
let r'' = ListRange.smult r 0
let r'' = ListRange.smult r (-3)

let r''' = ListRange.bridge r s
let r''' = ListRange.bridge s r
let r''' = ListRange.bridge r r'
let r''' = ListRange.bridge r' r
let r''' = ListRange.bridge r r''

let z = ListRange.size r''

let b = ListRange.contains r 24
let b = ListRange.contains r'' 24

let b' = ListRange.rless s r
let b' = ListRange.rless r s
let b' = ListRange.rless r' r
let b' = ListRange.rless r r'

(* Exercise 2: Design an imperative version of RANGE.    Do so by
    copying range.mli here and changing the types as necessary.    And
    then copy the implementation of LoHiPairRange and modify the code
    as necessary.    All the operations should remain the same as in the
    functional version.    The singleton and range operations should each
    create a new range.    The sadd and smult operations should modify
    existing ranges. Consider the design choices and design your own
    version of bridge. *)

module type IMPERATIVE_RANGE =
sig
    type t
    type e
    
    val singleton : e -> t
    val range : e -> e -> t
    val sadd : t -> e -> unit
    val smult : t -> e -> unit
    val bridge : t -> t -> t
    val size : t -> int
    val contains : t -> e -> bool
    val rless : t -> t -> bool option
end

(* module ImperativeLoHiPairRange: IMPERATIVE_RANGE with type e = int = *)
module ImperativeLoHiPairRange = 
struct
    type e = int
    type t = (e ref) * (e ref)
    
    let singleton (i: e): t =
        (ref i, ref i)
    let range (i: e) (j: e): t =
        (ref (min i j), ref (max i j))
        
    let sadd ((low, high): t) (i: e): unit =
        low := !low + i;
        high := !high + i
        
    let smult ((low, high): t) (i: e): unit =
        if i >= 0 then begin
            low := !low * i;
            high := !high * i
        end else begin
            low := !high * i;
            high := !low * i
        end
    
    let bridge ((lowX, highX): t) ((lowY, highY): t): t =
        ((min lowX lowY), (max highX highY))
    
    let size ((low, high): t): int =
        !high - !low + 1
    
    let contains ((low, high): t) (i: e): bool =
        (!low <= i) && (i <= !high)
    
    let rless ((lowX, highX): t) ((lowY, highY): t): bool option =
        if !highX < !lowY then
            Some true
        else if !highY < !lowX then
            Some false
        else
            None
end
