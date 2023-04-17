

(* x^2 - 3x + 7 *)
(* val p = Poly (Variable "x", [Term (0, 7), Term (1, ~3), Term (2, 1)]) *)
datatype term_t = Term of int * int
datatype variable_t = Variable of string
datatype poly_t = Poly of variable_t * term_t list
exception VariableMismatch

fun variable (Poly (v, _)) = v
fun term_list (Poly (_, l)) = l
fun expon (Term (e, _)) = e
fun coeff (Term (_, c)) = c


fun add_terms (l1, l2) =
  (* your code here *)

  
  
  (* hint: consider implement a helper function (not required) *)
  (*   adjoin_term = fn : term_t * term_t list -> term_t list *)
  
  

fun add_poly (Poly (x, l1), Poly (y, l2)) =
  if x = y then 
    Poly (x, add_terms (l1, l2))
  else
    raise VariableMismatch 



(* x^2 - 3x + 7 *)
val p = Poly (Variable "x", [Term (0, 7), Term (1, ~3), Term (2, 1)])
(* x *)
val p0 = Poly (Variable "x", [Term (1, 1)])
(* x + 1 *)
val p1 = Poly (Variable "x", [Term (0, 1), Term (1, 1)])
(* x - 1 *)
val p2 = Poly (Variable "x", [Term (0, ~1), Term (1, 1)])
val x = Variable "x"

val add_poly_test_1 = add_poly (p1, p)
  = Poly (Variable "x", [Term (0, 8), Term (1, ~2), Term (2, 1)])

(* x * x = x^2 *)
val mul_poly_test_1 = mul_poly (p0, p0)
  = Poly (Variable "x",[Term (2,1)])

(* x(x + 1) = x^2 + x *)
val mul_poly_test_2 = mul_poly (p0, p1)
  = Poly (Variable "x",[Term (1,1),Term (2,1)])

(* (x + 1)(x - 1) = x^2 -1 *)
val mul_poly_test_3 = mul_poly (p1, p2)
  = Poly (Variable "x",[Term (0,~1),Term (2,1)])

(* (x - 1)(x^2 - 3x + 7)(x + 1) = x^4 - 3x^3 + 6x^2 + 3x - 7 *)
val mul_poly_test_4 = mul_poly (p2, mul_poly (p, p1)) 
  = Poly (Variable "x", [Term (0, ~7), Term (1, 3), Term (2, 6), Term (3, ~3), Term (4, 1)])

val diff_poly_test_1 = diff_poly (p0, x)
  = Poly (Variable "x", [Term (0, 1)])

val diff_poly_test_2 = diff_poly (p2, x)
  = Poly (Variable "x", [Term (0, 1)])

val diff_poly_test_3 = diff_poly (mul_poly (p2, mul_poly (p, p1)), x)
 = Poly (Variable "x", [Term (0, 3), Term (1, 12), Term (2, ~9), Term (3, 4)])
  
