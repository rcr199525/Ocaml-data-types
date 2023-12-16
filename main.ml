(* The num data type is composed of three variants:

Intv of int: Represents an integer value.
Floatv of float: Represents a floating-point value.
Stringv of string: Represents a string which is guaranteed to be either a valid integer or a valid floating-point number.
Objective:

Define an infix operator +++ that sums 2 num values and returns a result of the num type, adhering to the following rules:

Intv + Intv: Sum as integers, return as Intv.
Floatv + Floatv: Sum as floats, return as Floatv.
Intv + Floatv or Floatv + Intv: Convert the Intv to a float, sum as floats, and return as Floatv.
Stringv + any num type:
Convert the Stringv to its numeric representation (Intv or Floatv).
Sum according to the rules above.
Stringv + Stringv:
Convert both Stringv values to their numeric representations and sum the 2 values
Return the result as Stringv, converting the numeric result back to string format.
Note: The conversion from Stringv should account for the possibility that the string could represent either an integer or a float. 
The problem guarantees that the string will always be a valid representation of one of these two. *)

type num =
  | Intv of int
  | Floatv of float
  | Stringv of string;;

let string_to_num st =
  try Intv (int_of_string st)
  with Failure _ ->
    Floatv (float_of_string st);;

let rec (+++) x y =
  match x, y with
  | Intv iv, Intv jv -> Intv (iv + jv) | Floatv f1, Floatv f2 -> Floatv (f1 +. f2) | Stringv s1, Stringv s2 -> (string_to_num s1) +++ (string_to_num s2)
  | Intv i1, Floatv f2 -> Floatv ((float_of_int i1) +. f2) | Floatv f1, Intv i2 -> Floatv (f1 +. (float_of_int i2)) | Stringv s1, _ -> (string_to_num s1) +++ y
  | _, Stringv s2 -> x +++ (string_to_num s2);;

