(* These exercises are suggested for programming practice and
   acclimation to "thinking functionally".  You can try each one out
   by interacting with the OCaml interpreter, but then once you have
   the answer, store it in this file.  You can later load the whole
   file using "#use".  *)

(* 1a. Make it so that that x equals 42, by adding 22 to 20 *)
let x = 22 + 20

(* 1b. Make it so that x1 equals 42.0, by casting x. *)
(* Hint: Look in the OCaml documentation in the Pervasives chapter:
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
   and search for "float". *)
let x1 = float_of_int x


(* 1c. Write a function that takes a string, and appends
 * " is my favourite programming language." to the end of it. *)
let favourizer (arg:string) = arg ^ " is my favourite programming language."


(* call your favourizer, creating a complete string *)
let myfavourite:string = favourizer "C"


(* 1d. Write a function that takes a number and returns
 * the difference between that number and 42.
 * Eg, if 'num' is 50, the result should be 8.
 * If 'num' is 30, the result should be -12 *)
let diff_42 num = num - 42


(* 1e. One more simple arithmetic example...
 * Write a function that returns the volume of a cylinder
 * with radius r, height h. (volume is pi * r^2 * h) *)
let pi = 4.0 *. atan 1.0
let volume_cylinder (r:float) (h:float) : float = pi *. r *. r *. h


(* 1f. Determine if an integer is even. Again use the Pervasives library. *)
let even (x: int) : bool =
    if x land 1 == 0 then
        true
    else
        false


(* 1g. Write odd in terms of even *)
let odd (x: int) : bool = not (even x)

(* 1h. In addition to the Pervasives, OCaml comes pre-packaged with a
   standard library, that includes a lot of other utility functions that
   you don't have to write yourself.
  * For instance, check out the String module *
   (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html) * *
   Now... write a function that takes a String, and returns whether *
   or not that String is more than 10 characters long. *)
let gt_10_chars str : bool = (String.length str) > 10


(* 2. Compare the following two boolean values.  Why the difference? *)
let why1 = (1.0 = 1.0)
let why2 = (1.0 == 1.0)
let because = "\
    According to the documentation of the 'Pervasives' library:\
    Function '=' tests for structural equality, which indicates that it only compares the contents of two operands.\
    Function '==' tests for physical equality, which indicates that it compares memory addresses of the two operands.\
    
    Since type 'float' is an immutable type, the two literals '1.0' in expression '(1.0 == 1.0)' are located in\
    two different locations in computer memory. Therefore, expression '(1.0 == 1.0)' returns 'false'.\
    \
    However, the two literals '1.0' both represent the value of the real number 1.0, thus expression '(1.0 = 1.0)'\
    returns 'true'.
    "

(* Moral of the story : Don't use == unless you really know what
 * you're doing
 *)


(* 3. Compute the GCD for two integers using Euclid's recursion
 * https://en.wikipedia.org/wiki/Euclidean_algorithm *)
let rec gcd (x : int) (y : int) : int = 
    if y == 0 then
        x
    else
        gcd y (x mod y)


(* 4. Compute the McCarthy 91 function as shown in 
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 *)
let rec mccarthy (x : int) : int = 
    if x > 100 then
        x - 10
    else
        mccarthy (mccarthy (x + 11))


(* 5. Optional advanced: Compute the square root of x using Heron of Alexandria's
 * algorithm (circa 100 AD).  x must be greater than 1.0.

 * We start with an initial (poor) guess that the square root is 1.0.
 * Let's call our current guess g.  We'll maintain the invariant that
 * g^2 is less than x and therefore that g is less than the square root
 * of x.

 * Notice that if g is less than the square root of x then x/g is slightly
 * greater than the square root of x.  The real square root is then between
 * g and x/g.

 * To compute a slightly better guess than g, we can take the average of
 * g and x/g:

     g + x/g
     -------
        2

 * We can keep improving our guess by averaging again and again.  Stop
 * the process when you get pretty close.  For this function, when the
 * difference between g and x/g is less than 0.001, compute one final
 * average and return it

 *  More on this method here:

 * http://www.mathpages.com/home/kmath190.htm

 * More on Heron of Alexandria here:

 * http://en.wikipedia.org/wiki/Hero_of_Alexandria
*)
let squareRoot (x : float) : float = 
    assert (x >= 1.0);
    let rec squareRootRecursion (x: float) (g: float): float =
        if (abs_float (g -. x /. g)) > 0.001 then
            squareRootRecursion x ((g +. x /. g) /. 2.0)
        else
            g
    in
        squareRootRecursion x 1.0
