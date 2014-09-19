(* PART 1: EXPRESSION TREES *)
type 'a exprTree = 
  | Val of 'a
  | Unop of ('a -> 'a) * 'a exprTree 
  | Binop of ('a -> 'a -> 'a) * 'a exprTree * 'a exprTree 

(*returns the number of function app in an exprTree*)
let rec count_ops (et : 'a exprTree) : int = 
match et with 
  Val x -> 0
  |Unop (op,x)->1+ count_ops x
  |Binop (op,x1,x2) -> 1+ count_ops x1 + count_ops x2
  

(*returns an exprTree representing the execution of fact n (as defined in the
  write-up for exercise 5)*)
let rec make_fact_tree (n : int) : int exprTree =
  if n< 0 then raise (Failure "input should not be negative")
  else if n= 0 then Val 1
  (*else if n= 2 then Binop (( * ), Val 2, Val 1) *)
  else Binop (( * ), Val n , make_fact_tree (n-1))

  
(*computes the expression represented by [et]*)
let rec eval (et : 'a exprTree) : 'a =
  match et with
  Val x -> x
  |Unop (op,x)->  op (eval x)
  |Binop (op,x1,x2) -> op  (eval x1) (eval x2) 

(*PART 2:FOLDING*)
(*returns the product of all elements in the list lst. returns 1.0 if lst is empty*)
let product (lst : float list) : float =
  List.fold_left ( *. ) 1.0 lst 

(*returns the concatenation of all elemtents in the string list lst. if empty returns empty string
uses fold left*)
let concat_left (lst : string list) : string = 
  List.fold_left ( ^ ) "" lst

(*concatenates all elements of the string list lst. if empty returns tye empty string. uses fol_right *)
let concat_right (lst : string list) : string = 
  List.fold_right ( ^ ) lst ""

(*returns a tuple of first value of tuple n+1 and appended result of function f on fst n and x to the snd value in tule n*)
let foo f n x =
((fst n)+1, (snd n)@ [f (fst n) x])

(*applies function f applied to the index as the first and each element of the list as the second args*)
let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  snd (List.fold_left (foo f) (0,[]) lst ) 

(*appends a dot and a space to a string of a integer x*)
let foo2 x =
 (^)(string_of_int (x+1) ^ ". ")

(*appends a dot a number and a space to every element in lst*)
let outline (lst: string list) : string list =
   mapi_lst foo2 lst

(*helper used to keep track of values in scan_right *)
let foo3 f n x =
 (f (fst n) x , (snd n) @ [f (fst n) x] )

(*helper used to keep track of values while folding in scan_left*)
let foo4 f x n =
 (f x (fst n), (snd n) @ [f x (fst n)] )

(*it returns a list of every value taken by the accumulator in a fold; uses fold_left *)
let scan_right (f: 'a -> 'b -> 'a)  (lst: 'b list) (acc: 'a): 'a list =
  snd (List.fold_right (foo4 f) lst (acc,[acc]))
 (*it returns a list of every value taken by the accumulator in a fold; uses fold right*)     
let scan_left (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  snd (List.fold_left (foo3 f) (acc,[acc]) lst)

(* requires: n >= 1 
   returns: the list [1;2;...;n] *)
let countup (n:int) : int list =
  (* tail-recursive helper function for countup:  
       accumulate the answer in l, 
       starting from n and working down *)
  let rec countup' i l =
    if i<=0 then l
    else countup' (i-1) (i::l)
  in countup' n []

(*returns a list of factorials of all numbers less than integer n
assumes n >= 1*)
let fact_list (n: int) : int list =
  let lst =countup n in
  match scan_left ( * ) 1 lst with 
  h::t -> t
  |_-> []



(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

(*prints out the values in a vector; takes in a unit value y and the vector x *)
let show_vec y x = 
let c= List.map string_of_int x in
let lst= List.map ((^)" ") c in
print_string ((List.fold_left (^) " " lst)^ "\n")

(*Pronts out the contenst of a matrx*)
let show (m : matrix) : unit = 
  List.fold_left show_vec () m 
 
(*appends a list containing x to a lisy y*)  
let foo y x=
 (@) y [x] 

(*adds a list containing x to a list of lits y *)
let help1 x y=
[x]::y

(*creates lists of individual elements of list x *)
let help2 x=
List.fold_right help1 x []

(* inserts a the column vector c to the end of matrix m*)
let insert_col (m : matrix) (c : vector) : matrix = 
  match m with []-> help2 c 
  |_->if List.length m <> List.length c then 
      raise (MatrixFailure "incompatible matrices")
      else List.map2 foo m c

(*creates lists of individual elements of list x *)
let help2 x=
List.fold_right help1 x []

(*adds each element of a vector y to evh vector in matrix x*)
let help3 x y =
List.map2 (fun x y -> x @ [y]) x y

(*returns the transpose of matrix m*)
let transpose (m : matrix) : matrix = 
match m with []-> []
|h::t -> List.fold_left help3 (help2 h) t

(*adds two vectors m1 and m2 if they are of equal length*)
let add_vec m1 m2 =
 if List.length m1 != List.length m2 
 then raise (MatrixFailure "incompatible vectors")
 else List.map2 (+) m1 m2 
 
(*adds two matrices if they are of equal legth*)
let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
 if m1=[] then m2
 else if m2=[] then m1
 else if List.length m1 != List.length m2 
 then raise (MatrixFailure "incompatible matrices")
 else List.map2 add_vec m1 m2

(*multiplies scalar x by vector y*)
let scal_mult y x=
 List.map (( * ) x) y
(*multiplies vectors x and y*)
let vec_mult x y=
if List.length x <> List.length y then raise (MatrixFailure "incompatible vectors")
else (List.map (scal_mult y) x)

(*computes the multiplication of matrices m1 and m2*)
let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  let m3= transpose m1 in
  if List.length m3 <> List.length m2 then raise (MatrixFailure "incompatible matrices ")
  else List.fold_left add_matrices [] (List.map2 vec_mult m3 m2)

(* PART 4: PATTERN MATCHING *)

(*type definitions: **********************************************************)
type pat =
  | WCPat (*short for "wildcard pattern"; equivalent to an underscore*)
  | VarPat of string
  | UnitPat
  | ConstPat of int
  | TuplePat of pat list
  | StructorPat of string * (pat option) (*Short for "constructor pattern"*)

type value = 
  | ConstVal of int
  | UnitVal
  | TupleVal of value list
  | StructorVal of string * (value option)

type bindings = (string * value) list option

(*1. z applies a function f1 to wild cards and f2 to variable pattrens in pattern p *)

let rec z f1 f2 p =
  let r = z f1 f2 in
    match p with
    | WCPat -> f1 ()
    | VarPat x -> f2 x
    | TuplePat ps -> List.fold_left (fun acc e -> (r e) + acc) 0 ps
    | StructorPat (_,Some p) -> r p
    | _ -> 0

(*counts the number of wildcards that occur in a pattern*)
let count_wcs (p: pat) : int = 
  z (fun x -> 1) (fun y -> 0) p

(*counts the number of wildcards in a pattern, and adds that quantity to the sum
of the lengths of the variable names used in the pattern*)
let count_wcs_and_var_lengths (p: pat) : int = 
 z (fun x -> 1) (fun y -> String.length y) p 


(*counts how oftern a variable occurs in a pattern*)
let count_var (var_name: string) (p: pat) : int = 
  z (fun x -> 0) (fun y -> if y = var_name then 1 else 0) p

                       
(*2. *************************************************************************)

(*produces a list of all variable names that occur in pattern p*)
let rec extract_names (p: pat) : string list = 
 match p with 
 |VarPat x-> [x]
 |TuplePat ps -> List.fold_left (fun acc e-> extract_names e @ acc) [] ps
 |StructorPat (_,Some p) -> extract_names p
 |_->[]

(*checks whether duplicates occur in list of values *)
let has_dups (l: 'a list) : bool = 
 let x = List.sort compare l in
 fst (List.fold_left (fun acc e-> (fst acc || snd acc=e, e)) (false, "") x)

                                                         
(*deetermines whther all variable pattern in a pattern p have unique names*) 
let all_vars_unique (p: pat) : bool = 
  match has_dups (extract_names p) with
  |true -> false 
  |false -> true 

(*3. **************************************************************************)
(* returns the value from an option of a value*)
let no_opt x=
 match x with None-> failwith "this really shouldn't be happening tho"
 |Some x-> x

(*checks if a lst contains none*)
let contains_none lst =
 let is_none x= match x with None->true
 |Some x-> false in
 let l=List.map (is_none) lst in
 List.fold_left (||) false l

(*the function f is applied to every element in list l and adds the result to an option of a list
however, if any application of f returns none, then none is returned*)
let rec all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
 let lst =List.map f l in
 if contains_none lst  then None
 else Some (List.fold_left (@) [] (List.map no_opt lst))

(*4. *************************************************************************)
(*converts lists l1 and l2 to a list of tuples of their respective elements*)
let lists_to_tuplist l1 l2 =
List.map2 (fun x y->(x,y)) l1 l2

(*matches value v with pattern p*)
let rec match_pat ((v:value) ,(p:pat)) : bindings =
  match p with 
  |WCPat->Some []
  |VarPat x->Some [(x,v)] 
  |UnitPat-> (match v with 
            |UnitVal-> Some []
            |_ -> None)
  |ConstPat x->(match v with
            |ConstVal x -> Some []
            |_-> None)
  |TuplePat pats -> (match v with
            |TupleVal vals -> if List.length vals != List.length pats
             then None
             else all_answers match_pat (lists_to_tuplist vals pats) 
            |_->None)
  |StructorPat (s, p_opt)-> match v with
            |StructorVal (s, v_opt)-> if s!=s then None 
                                      else (match p_opt with 
                                      |None-> (match v_opt with 
                                              |None-> Some []
                                              |Some v-> None)
                                      |Some p-> (match v_opt with
                                              |None-> None
                                              |Some v->match_pat (v,p)))
            |_-> None 


(*5. *************************************************************************)
exception NoAnswer
(* applies the function argument to elements of the list argument until that function 
returns Some v—in which case, first_answer will return v. If first_answer never encounters an element 
that pro- duces Some v, it should raise the exception NoAnswer.*)
let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  match l with 
  []-> raise (NoAnswer)
  |h::t-> (match f h with
          Some v-> v 
          |None-> first_answer f t)


(*6. *************************************************************************)
(*checks whether a value matches any of the patterns in the list argument. If so, return Some b, where b is the list 
of bindings produced by the first pattern that matches. Otherwise, returns None*)

let match_pats ((v: value), (ps: pat list)) : bindings =
 let tuplefy x p_list=
 List.map (fun y->(x,y)) p_list in
 try Some (first_answer match_pat (tuplefy v ps)) with
 NoAnswer->None
