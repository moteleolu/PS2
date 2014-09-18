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

let product (lst : float list) : float =
  List.fold_left ( *. ) 1.0 lst 

let concat_left (lst : string list) : string = 
  List.fold_left ( ^ ) "" lst

let concat_right (lst : string list) : string = 
  List.fold_right ( ^ ) lst ""

let foo f n x =
((fst n)+1, (snd n)@ [f (fst n) x])

let mapi_lst (f: (int -> 'a -> 'b)) (lst: 'a list) : 'b list =
  snd (List.fold_left (foo f) (0,[]) lst ) 

let foo2 x =
 (^)(string_of_int (x+1) ^ ".")

let outline (lst: string list) : string list =
   mapi_lst foo2 lst

let foo3 f n x =
 (f (fst n) x , (snd n) @ [f (fst n) x] )

let foo4 f x n =
 (f x (fst n), (snd n) @ [f x (fst n)] )

let scan_right (f: 'a -> 'b -> 'a) (acc: 'a) (lst: 'b list) : 'a list =
  snd (List.fold_right (foo4 f) lst (acc,[acc]))
      
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

let fact_list (n: int) : int list =
  let lst =countup n in
  match scan_left ( * ) 1 lst with 
  h::t -> t
  |_-> []



(* PART 3: MATRICES *)

type vector = int list
type matrix = vector list

exception MatrixFailure of string

let show_vec y x = 
let c= List.map string_of_int x in
let lst= List.map ((^)" ") c in
print_string ((List.fold_left (^) " " lst)^ "/")

let show (m : matrix) : unit = 
  List.fold_left show_vec () m 
  
let foo y x=
 (@) y [x] 

let insert_col (m : matrix) (c : vector) : matrix = 
  if List.length m < List.length c then 
   raise (MatrixFailure "column is too long")
  else List.map2 foo m c

let help1 x y=
[x]::y

let help2 x=
List.fold_right help1 x []

let help3 x y =
List.map2 (fun x y -> x @ [y]) x y

let transpose (m : matrix) : matrix = 
match m with []-> []
|h::t -> List.fold_left help3 (help2 h) t

let add_vec m1 m2 =
 if List.length m1 != List.length m2 
 then raise (MatrixFailure "incompatible vectors")
 else List.map2 (+) m1 m2 
 

let add_matrices (m1 : matrix) (m2 : matrix) : matrix = 
 if m1=[] then m2
 else if m2=[] then m1
 else if List.length m1 != List.length m2 
 then raise (MatrixFailure "incompatible matrices")
 else List.map2 add_vec m1 m2

let scal_mult y x=
 List.map (( * ) x) y

let vec_mult x y=
 (List.map (scal_mult y) x)

let multiply_matrices (m1 : matrix) (m2 : matrix) : matrix = 
  List.fold_left add_matrices [] (List.map2 vec_mult (transpose m1) m2)

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

(*1. *************************************************************************)

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
  z (fun x -> 0) (fun y -> 1) p

                       
(*2. *************************************************************************)

let rec extract_names (p: pat) : string list = 
 match p with 
 |VarPat x-> [x]
 |TuplePat ps -> List.fold_left (fun acc e-> extract_names e @ acc) [] ps
 |StructorPat (_,Some p) -> extract_names p
 |_->[]


let has_dups (l: 'a list) : bool = 
 let x = List.sort compare l in
 fst (List.fold_left (fun acc e-> (fst acc || snd acc=e, e)) (false, "") x)

                                                         

let all_vars_unique (p: pat) : bool = 
  match has_dups (extract_names p) with
  |true -> false 
  |false -> true 

(*3. **************************************************************************)
let no_opt x=
 match x with None-> failwith "this really shouldn't be happening tho"
 |Some x-> x

let contains_none lst =
 let is_none x= match x with None->true
 |Some x-> false in
 let l=List.map (is_none) lst in
 List.fold_left (||) false l

let rec all_answers (f: 'a -> 'b list option) (l: 'a list) : 'b list option =
 let lst =List.map f l in
 if contains_none lst  then None
 else Some (List.fold_left (@) [] (List.map no_opt lst))

(*4. *************************************************************************)
let lists_to_tuplist l1 l2 =
List.map2 (fun x y->(x,y)) l1 l2

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

let rec first_answer (f: 'a -> 'b option) (l: 'a list) =
  match l with 
  []-> raise (NoAnswer)
  |h::t-> (match f h with
          Some v-> v 
          |None-> first_answer f t)


(*6. *************************************************************************)

let match_pats ((v: value), (ps: pat list)) : bindings =
 let tuplefy x p_list=
 List.map (fun y->(x,y)) p_list in
 try Some (first_answer match_pat (tuplefy v ps)) with
 NoAnswer->None
