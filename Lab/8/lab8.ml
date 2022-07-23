(* Object-Oriented Programming in OCaml (Real World OCaml Chapter 12):
   Representing Grammars as Classes and Expressions as Objects *)

(* We can represent expressions given by the grammar:

   e ::= num | e + e

   by using objects from a class called "expression".  We begin with
   an abstract class (using the keyword "virtual" in OCaml) called
   "expression".  Although this class has no instances, it lists the
   operations common to all the different kinds of expressions.  These
   operations include a predicate "is_atomic" indicating whether there
   are subexpressions or not, operations to retrieve the left and
   right subexpressions (if the expression is not atomic), and a
   method computing the value of the expression.
 *)

class virtual expression =
object
    method virtual is_atomic: bool
    method virtual value: int
end

class virtual unary_expression =
object
    inherit expression as super
    method virtual sub: expression option
end

class virtual binary_expression =
object
    inherit expression as super
    method virtual left_sub: expression option
    method virtual right_sub: expression option
end

class virtual ternary_expression =
object
    inherit expression as super
    method virtual left_sub: expression option
    method virtual mid_sub: expression option
    method virtual right_sub: expression option
end

(* Because the grammar has two cases, we have two subclasses of
   "expression", one for numbers, and one for sums.
 *)

class number_exp (n: int) =
object
    inherit expression as super
    val mutable number_val = n
    method is_atomic = true
    method value = number_val
end               

class sum_exp (e1: expression) (e2: expression) =
object
    inherit binary_expression as super
    val mutable left_exp = e1
    val mutable right_exp = e2
    method is_atomic = false
    method left_sub = Some left_exp
    method right_sub = Some right_exp
    method value = left_exp#value + right_exp#value
end

(* QUESTION 1. Product Class and Method Calls *)
(* 1(a). Extend this class hierarchy by writing a "prod_exp" class to
   represent product expressions of the form:

   e ::= ... | e * e
 *)

class prod_exp (e1: expression) (e2: expression) = 
object
    inherit binary_expression as super
    val mutable left_exp = e1
    val mutable right_exp = e2
    method is_atomic = false
    method left_sub = Some left_exp
    method right_sub = Some right_exp
    method value = left_exp#value * right_exp#value
end


(* 1(b). Create objects representing the following expressions:
   - An expression "a" representing the number 3
   - An expression "b" representing the number 0
   - An expression "c" representing the number 5
   - An expression "d" representing the sum of "a" and "b"
   - An expression "e" representing the product of "d" and "c"
   Then send the message "value" to "e".
   Note that "e" represents the expression (3+0)*5.

   To answer 1(b), uncomment this code and fill it in:
*)
let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp (d :> expression) c
let _ = e#value

                                              
(* QUESTION 2. Unary Expressions *)
(* Extend the class hierarchy further by writing a "square_exp".
   The expression below written e^2 means "e squared":

   e ::= ... | e^2

   Changes will be required to the "expression" interface, so you will
   need to reimplement all the classes from above with these changes.
   Try to make as few changes as possible to the program. *)

class square_exp (e: expression) = 
object
    inherit unary_expression as super
    val mutable e = e
    method is_atomic = false
    method sub = Some e
    method value = e#value * e#value
end


(* QUESTION 3. Ternary Expressions and More Method Calls *)
(* 3(a). Extend this class heirarchy by writing a "cond_exp" class to
   represent conditionals of the form

   e ::= ... | e?e:e

   In a conditional expression a?b:c, evaluate "a" and if the value is
   not 0, then return the value of "b".  If the value of "a" is 0,
   then return the value of "c".

   Again, try to make as few changes as possible to the program.  If
   necessary, redesign the class hierarchy you created for Question 2
   so that it handles both unary and ternary expressions.
 *)

class cond_exp (e1: expression) (e2: expression) (e3: expression) = 
object
    inherit ternary_expression as super
    val mutable left_exp = e1
    val mutable mid_exp = e2
    val mutable right_exp = e3
    method is_atomic = false
    method left_sub = Some left_exp
    method mid_sub = Some mid_exp
    method right_sub = Some right_exp
    method value =
        if left_exp#value = 0 then
            right_exp#value
        else
            mid_exp#value
end

(* 3(b). Re-create all the objects a,b,c,d,e above and create new
   objects:
   - An expression "f" representing the square of "c"
   - An expression "g" representing the conditional b?e:f
   Then send the message "value" to "g".
 *)
let f = new square_exp c
let g = new cond_exp b (e :> expression) (f :> expression)
let _ = g#value
 
(* 3(c) Enter the following expressions (for a,b,c,d,e,f,g) into OCaml
   so that you can see what is printed by the OCaml interpreter.  In
   each case, the type of the expression will be printed. Note that
   they are not all the same.

   Then enter the expression defining the value of "e_list".  Note
   that this is a list containing elements of type "expression", which
   is a different type than the ones printed out for a,b,c,d,e,f,g.
   Explain why these elements are all allowed to have more than one
   type in OCaml.

   To answer 3(c), uncomment this code and execute it.
 *)
let _ = a
let _ = b
let _ = c
let _ = d
let _ = e
let _ = f
let _ = g
let e_list : expression list = [a;b;c;(d :> expression);(e :> expression);(f:> expression);(g:> expression)]

(*
    Since their type is derived from the same base class, we can treat them as base class objects with type coercion.
    Thus the list of base class objects can store objects that are derived from the base class.
*)

(* QUESTION 4. Redesign the entire hierarchy again, so that it
   includes a new operation that takes one argument (x:int) and
   modifies an expression object so that all of its leaves are
   incremented by the value of x.  (The leaves of an expression are
   all the subexpressions belonging to the "number_exp" class.)  This
   operation should not return a new instance of an "expression".  It
   should modify the instances that it is applied to.

   Re-create all the objects a,b,c,d,e,f,g again.  Then send the
   message "value" to "g".  Then apply the new operation with any
   argument value greater than 0.  Then send the message "value" to
   "g". The new value should be different than the original one.
   Verify that your implementation gives the expected new value.  *)

class virtual expression =
object
    method virtual is_atomic: bool
    method virtual value: int
    
    method virtual increment: int -> unit
end

class virtual unary_expression =
object
    inherit expression as super
    method virtual sub: expression option
end

class virtual binary_expression =
object
    inherit expression as super
    method virtual left_sub: expression option
    method virtual right_sub: expression option
end

class virtual ternary_expression =
object
    inherit expression as super
    method virtual left_sub: expression option
    method virtual mid_sub: expression option
    method virtual right_sub: expression option
end

class number_exp (n: int) =
object
    inherit expression as super
    val mutable number_val = n
    method is_atomic = true
    method value = number_val
    
    method increment x =
        number_val <- number_val + x
end               

class sum_exp (e1: expression) (e2: expression) =
object
    inherit binary_expression as super
    val mutable left_exp = e1
    val mutable right_exp = e2
    method is_atomic = false
    method left_sub = Some left_exp
    method right_sub = Some right_exp
    method value = left_exp#value + right_exp#value
    
    method increment (x: int) =
        left_exp#increment x;
        right_exp#increment x
end

class prod_exp (e1: expression) (e2: expression) = 
object
    inherit binary_expression as super
    val mutable left_exp = e1
    val mutable right_exp = e2
    method is_atomic = false
    method left_sub = Some left_exp
    method right_sub = Some right_exp
    method value = left_exp#value * right_exp#value
    
    method increment (x: int) =
        left_exp#increment x;
        right_exp#increment x
end

class square_exp (e: expression) = 
object
    inherit unary_expression as super
    val mutable e = e
    method is_atomic = false
    method sub = Some e
    method value = e#value * e#value
    
    method increment (x: int) =
        e#increment x
end

class cond_exp (e1: expression) (e2: expression) (e3: expression) = 
object
    inherit ternary_expression as super
    val mutable left_exp = e1
    val mutable mid_exp = e2
    val mutable right_exp = e3
    method is_atomic = false
    method left_sub = Some left_exp
    method mid_sub = Some mid_exp
    method right_sub = Some right_exp
    method value =
        if left_exp#value = 0 then
            right_exp#value
        else
            mid_exp#value
            
    method increment (x: int) =
        left_exp#increment x;
        mid_exp#increment x;
        right_exp#increment x
end

(*
    Since OCaml may use passing by reference,
    we need to copy objects that are used multiple times manually.
    Otherwise, leaf objects may be incremented multiple times.
*)

let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a (Oo.copy b)
let e = new prod_exp (d :> expression) (Oo.copy c)
let f = new square_exp (Oo.copy c)
let g = new cond_exp (Oo.copy b) (e :> expression) (f :> expression)
let _ = g#value
let _ = g#increment 1
let _ = g#value
