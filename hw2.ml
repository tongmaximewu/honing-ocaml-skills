(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([A;A;A;A;G;G;G;G], [(4,A);(4,G)]); 
  ([A; A; A; A; G; G; A; T; T; T; C; T; C], [(4, A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]);
  ([],[]);
  ([T],[(1,T)]);

]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list = 
  let rec aux counter acc = function
    |[]->acc 
    |[x]->(1+counter,x) ::acc
    |x::(y::_ as rem)-> 
        if x=y then aux (1+counter) acc rem else aux 0 ((counter+1,x)::acc)rem
  in let rec rev = function
      |[]->[]
      |x::xs -> rev xs @[x]
  in rev (aux 0 [] l)


        
          
(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([(4, A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], [A; A; A; A; G; G; A; T; T; T; C; T; C]);
  ([],[]);
  ([(1,T)],[T]);
]

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec replicate n x =
    if n=0 then [] else x::replicate(n-1)x
        
  in match l with
  |[]->[]
  |(counter, nucleobase) :: rem->(replicate counter nucleobase )@
                                 (decompress rem)
                                     
(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  (FLOAT 1.0, 1.0);
  (PLUS (FLOAT 1.0, FLOAT 1.0), 2.0);
  (MINUS (FLOAT 2.0, FLOAT 1.0), 1.0);
  (MULT (FLOAT 1.0, FLOAT 2.0), 2.0);
  (DIV (FLOAT 1.0, FLOAT 1.0), 1.0);
  (SIN (FLOAT 0.0), 0.0);
  (COS (FLOAT 0.0), 1.0);
  (EXP (FLOAT 1.0), exp 1.0);
]

(* TODO: Implement eval. *)
let rec eval e =
  match e with 
  |FLOAT f->f 
  |PLUS (e1,e2)-> eval e1 +. eval e2
  |MINUS (e1,e2)-> eval e1 -. eval e2
  |MULT (e1,e2)-> eval e1 *. eval e2
  |DIV (e1,e2)-> eval e1 /. eval e2
  |SIN e-> sin(eval e)
  |COS e-> cos(eval e)
  |EXP e-> exp(eval e)

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (FLOAT 1.0, [Float 1.0]);
  (PLUS (FLOAT 1.0, FLOAT 1.0), [Float 1.0; Float 1.0; Plus]);
  (MINUS ( FLOAT 2.0, FLOAT 1.0), [Float 2.0;Float 1.0;Minus]);
  (MULT ( FLOAT 2.0, FLOAT 1.0), [Float 2.0;Float 1.0;Mult]);
  (DIV ( FLOAT 2.0, FLOAT 1.0), [Float 2.0;Float 1.0;Div]);
  (SIN (FLOAT 0.0), [Float 0.0;Sin]);
  (COS (FLOAT 0.0), [Float 0.0;Cos]);
  (EXP (FLOAT 1.0), [Float 1.0;Exp]); 
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
  match e with
  |FLOAT f->[Float f]
  |PLUS (e1, e2)-> to_instr e1 @ to_instr e2 @ [Plus]
  |MINUS (e1, e2)-> to_instr e1 @ to_instr e2 @ [Minus]
  |MULT (e1, e2)-> to_instr e1 @ to_instr e2 @ [Mult]
  |DIV (e1, e2)-> to_instr e1 @ to_instr e2 @ [Div]
  |SIN e -> to_instr e @ [Sin] 
  |COS e -> to_instr e @ [Cos] 
  |EXP e -> to_instr e @ [Exp] 

(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  (Float 1.0, [], Some [1.0]);
  (Minus, [3.0; 5.0], Some [2.0]);

]


(* TODO: Implement to_instr. *)               
let instr i s = 
  match i,s with
  |Float f,_->Some (f::s)
  |Plus, a :: b :: rem -> Some ((b+. a)::rem)
  |Minus, a :: b :: rem -> Some ((b-. a)::rem)
  |Mult, a :: b :: rem -> Some ((b*. a)::rem)
  |Div, a :: b :: rem -> Some ((b/. a)::rem)
  |Sin, a:: rem -> Some (sin a ::rem)
  |Cos, a:: rem -> Some (cos a ::rem)
  |Exp, a:: rem -> Some (exp a ::rem)
  |_,_ -> None 
(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 1.0; Float 1.0; Plus], Some 2.0);
  ([Float 2.0; Float 1.0; Minus], Some 1.0);
  ([Float 1.0; Float 2.0; Mult], Some 2.0);
  ([Float 2.0; Float 2.0; Div], Some 1.0);
  ([Float 0.0; Sin], Some 0.0);
  ([Float 1.0; Cos], Some (cos 1.0));
  ([Float 1.0; Exp], Some (exp 1.0));
]

(* TODO: Implement prog. *)
let prog instrs = 
  let rec eval instrs stack = match instrs with
    |[] ->(match stack with
        |[ans]->Some ans
        |_ -> None)
    | i :: rem ->
        match instr i stack with
        |Some newStack -> eval rem newStack
        |None -> None 
  in eval instrs[]