open Assertions
open Ps2 

(*Problem 1*)
TEST_UNIT "problem_1_test"=
let p= count_ops (Binop ((+), Val 2, Val 3)) in
assert_true (p =1)
let q= count_ops (Binop ((+), Val 3, Unop((~-), Binop ((/), Val 5, Val 2)))) in
assert_true ( q =3)
(*excercise 2*)
let x= make_fact_tree 3 in
assert_true ((eval x) = 6)
(*excercise 3*)
let e1= eval (Unop ((~-), Val 5)) in
assert_true (e1 = ~-5)
let e2 = eval (make_fact_tree 5) in
assert_true (e2 = 120) 


(*Problem 2*)
TEST_UNIT "problem_2_test" =
let e1 = product [777.5; 4.] in
assert_true (e1 = 3110.0)
let e2 = product [] in
assert_true (e2 =1.0)
let e3 = concat_left ["cs"; "3110"] in
let e4 = concat_right ["cs"; "3110"] in
assert_true (e3 = e4)
let e5= mapi_lst (+) [3; 0; -1; -3] in
assert_true ( e5 = [3; 1; 1; 0] )
let e6 = outline ["point 1"; "point 2"; "point 3"] in
let e7 = ["1. point 1"; "2. point 2"; "3. point 3"] in
assert_true (e6 = e7 )
let e8= scan_right (^) ["zar"; "doz"] "swag" in
assert_true ( e8 = ["swag"; "dozswag"; "zardozswag"])
let e9= scan_left (^) "swag" ["zar"; "doz"] in
assert_true (e9 = ["swag"; "swagzar"; "swagzardoz"])
let e10 = fact_list 1 in
assert_true (e10 = [1])
let e11 = fact_list 7 in
assert_true (e11 = [1; 2; 6; 24; 120; 720; 5040] )

(*Problem 3*)
TEST_UNIT "problem3_test" =
let m = [[1;2;3];[42;41;40]] in
let m1= insert_col m [6;7] in
assert_true (m1 = [[1;2;3;6];[42;41;40;7]])
let m = [[1;2;3];[42;41;40]] in
let m2 = transpose m in
assert_true (m2 = [[1;42];[2;41];[3;40]])
let m1 =[[1;2;3];[4;5;6]] and m2 = [[42;42;42];[43;43;43]] in
assert_true (add_matrices m1 m2 = [[43;44;45];[47;48;49]])
let m1 = [[1;2;3];[4;5;6]] and m2 = [[7;8];[9;10];[11;12]] in
assert_true ( (multiply_matrices m1 m2)= [[58;64];[139;154]])

(*Problem 4*)
TEST_UNIT "problem4_test"=
assert_true ((count_wcs (TuplePat [WCPat; WCPat; VarPat "zardoz"])) = 2);
assert_true ((count_wcs_and_var_lengths (TuplePat [WCPat; WCPat; VarPat "zardoz"])) = 8);
assert_true ((count_var  "zardoz" (TuplePat [WCPat; WCPat; VarPat "zardoz"])) = 1);
assert_true (all_vars_unique (VarPat "yay"));
assert_false (all_vars_unique (TuplePat [VarPat "yay"; VarPat "yay"; WCPat]));

let p1 = WCPat in
let p2 = TuplePat([VarPat("xy");UnitPat]) in
let p3 = match_pat (UnitVal,p1) in
let p4 = match_pat (TupleVal [StructorVal("Some",(Some (ConstVal(5))));UnitVal],p2) in
let p5 = match_pat (TupleVal [StructorVal("Some",(Some (ConstVal(5))));ConstVal(1)],p2) in
let _ = assert_true (p3 = Some []) in
let _ = assert_true (p4 = Some [("xy", StructorVal ("Some", Some (ConstVal 5)))]) in
let _ = assert_true (p5 = None) in 
();
assert_true ( (first_answer (fun x-> if x=2 then Some 3 else None) [2;3;34;4]) = 3);
let p6 = match_pats ((UnitVal),[VarPat "yay"; ConstPat 3; WCPat]) in
assert_true (p6 = Some [("yay", UnitVal)])
