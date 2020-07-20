(*Consider the representation of "pre-terms" using the following data type definition

type term = V of variable | Node of symbol * (term list);;

Choose suitable type representations for types variable and symbol.
*)

(*In analogy to natural language, where a noun phrase refers to an object and a whole sentence
 refers to a fact, in mathematical logic, a term denotes a mathematical object and a 
 formula denotes a mathematical fact.
 A first-order term is recursively constructed from constant symbols, variables and function symbol
 
 An expression formed by applying a predicate symbol to an appropriate number of terms is called an atomic formula, 
 which evaluates to true or false in bivalent logics, given an interpretation.*)

 (* constant symbol e.g. 1, 0
 func symbol e.g.  + , * etc
 variable   e.g.   x , y

 atomic formula  e.g.  (x+1)*(x+1) >= 0  , here >= is a 'predicate symbol' and the LHS and RHS are 'term's *)

(*************************************************************************************************************)
 
type symbol = ConstSymbol of int| FuncSymbol of char;;

type term = Variable of char | Node of symbol * (term list);;  

(* example of terms:   x , y, (+,[x,y]), (-, [(+,[x,y]), y]) *)


let rec validSymbol sym = match sym with
        | ConstSymbol x -> true
        | FuncSymbol x -> true;;

let rec symbolExists elem lst = match lst with
        | [] -> false
        | (hd_sym, hd_ar)::tl -> elem = hd_sym || symbolExists elem tl;;

let rec dupSymbolExists lst =match lst with
        | [] -> false
        | (hd_sym, hd_ar)::tl -> (symbolExists hd_sym tl) || dupSymbolExists tl;;

(*sig_set is a list of tuples: (func_ / constant_)symbol*int  *)
(*Returns true if signature is valid*)
let rec check_sig sig_set = match sig_set with
                  | [] -> true   (*trivial case, empty signature*)
                  | (sym, ar)::tl ->     (validSymbol sym) 
                                      && (((symbolExists sym tl) || (dupSymbolExists tl)) = false)
                                      && (ar >= 0) ;;

let one = ConstSymbol 1;;                                  
let zero = ConstSymbol 0;;
let oR = FuncSymbol '|' ;;
let aND = FuncSymbol '&' ;;
let nOT = FuncSymbol '~' ;;

let x = Variable 'x' ;;
let y = Variable 'y' ;;


let test_sig = [(zero, 0);(one, 0);(nOT, 1);(oR,2);(aND,2)];;

let rec arity sym signature = match signature with
      | [] -> -1
      | (hd_sym, hd_ar)::tl -> if (sym = hd_sym) then hd_ar else (arity sym tl);;



let rec wfterm trm = match trm with
              | Variable v -> true
              | Node (sym, trm_lst) ->  (validSymbol sym) &&
                                        (List.length trm_lst = (arity sym test_sig))  &&
                                        (let rec wflist l =  match l with
                                                | [] -> true
                                                | hd::tl -> (wfterm hd) && (wflist tl)
                                          in wflist trm_lst
                                         )  ;;
(* test exmaple *)
let trm = Node (aND, [Node (oR, [x;Node (nOT, [y])]); y]);;
let trm2 = Node (aND, [Node (oR, [x;z]); y]);;


let rec ht t = match t with 
            | Variable v -> 0
            | Node (ConstSymbol x , l) -> 0
            | Node (aND, [t1;t2]) -> 1 + (max (ht t1) (ht t2)) 
            | Node (oR, [t1;t2]) -> 1 + (max (ht t1) (ht t2))
            | Node (nOT, [t1]) -> 1 + (ht t1)
            ;;  

let rec size t = match t with
            | Variable v -> 1
            | Node (ConstSymbol x, l) -> 1
            | Node (aND, [t1;t2]) -> 1 + (size t1) + (size t2) 
            | Node (oR, [t1;t2]) -> 1 + (size t1) + (size t2) 
            | Node (nOT, [t1]) -> 1 + (size t1)
            ;;  

let rec fiter_uniq l = match l with
            | [] -> l
            | hd::tl -> if (List.memq hd tl) then (filter_uniq tl) else hd::(filter_uniq tl)
            ;;

let rec vars t = 
      let res = match t with
            | Variable v -> [v]
            | Node (sym, [t1;t2]) -> List.append (vars t1) (vars t2)
            | Node (sym, [t1]) -> vars t1
      in
      filter_uniq res ;;



                              

(* substitutions *)

(* {x |-> t}   -- set  .. replace variable x by term t
subst = list of (x,t) pairs i.e. type (Variable, term) pairs *)

let test_subst = [(y, Node (zero, []))];;
let test_subst2 = [(y, z); (x,z)];;


let rec subst_var v s = match s with
            | [] -> Variable v
            | (Variable v1, Variable t1)::tl -> if v = v1 then Variable t1 else (subst_var v tl) 
            | (Variable v1, Node (s1, l))::tl -> if v = v1 then Node (s1, l) else (subst_var v tl);; 

            
let rec subst t s = match t with
            | Variable v -> subst_var v s
            | Node (aND, [t1;t2]) -> Node (aND, [(subst t1 s);(subst t2 s)])
            | Node (oR, [t1;t2]) -> Node (oR, [(subst t1 s);(subst t2 s)])
            | Node (nOT, [t1]) -> Node (nOT, [(subst t1 s)])
            | _ -> t ;;

let rec append_tuple s (x1,t1) = match s with
            | [] -> [(x1,t1)]
            | (x2,t2)::tl -> if x1 = x2 then [] else (append_tuple tl (x1,t1));;

let rec append_uniq s1 s2 = match s2 with
            | [] -> []
            | hd::tl -> List.append (append_tuple s1 hd) (append_uniq s1 tl)

let rec compose s1 s2 = let temp = 
      (match s1 with
            | [] -> []
            | (v, t)::tl -> (v, (subst t s2))::(compose tl s2)  
      )
      in append_uniq temp s2;;

exception NOT_UNIFIABLE;;

let rec occurs v l = match l with   (* l is term-list *)
            | [] -> false
            | (Node (sym, l1))::tl -> (occurs v l1) || (occurs v tl)
            | (Variable v1)::tl -> if v1 = v then true else (occurs v tl)  ;;

let rec mgu t1 t2 = match (t1, t2) with
            | (Variable v1, Variable v2) -> if v1 = v2 then [] else [(Variable v1, Variable v2)]
            | (Variable v1, Node (sym, l)) -> if (occurs v1 l) then raise NOT_UNIFIABLE 
                                              else [(Variable v1, Node (sym,l))]
            | (Node (sym, l), Variable v2) -> if (occurs v2 l) then raise NOT_UNIFIABLE 
                                              else [(Variable v2, Node (sym,l))]
            | (Node (sym1, l1), Node (sym2, l2)) -> if (sym1 = sym2) then
                                                      match (l1,l2) with
                                                            |([tr1], [tr2]) -> mgu tr1 tr2
                                                            |([tr1;tr2], [tr3;tr4]) -> List.append (mgu tr1 tr3) (mgu tr2 tr4)
                                                    else raise NOT_UNIFIABLE
            ;;



            



(**********************************************************************************************************)



