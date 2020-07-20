type symbol = Id of string | Var of string | Term of symbol*(symbol list);;
type clause = Clause of symbol*(symbol list);;
exception NOT_UNIFIABLE ;;


(* Clause (Term, [term_list])
Term (identifier_symbol, [symbol_list])
symbol ->   identifier i.e. predicate | variable | term itself *)

let goal = [Term (Id "male", Id "arjun")];;   (*  the term male(arjun)  *)

(* let rec print_list x = match x with
        | [] -> Printf.printf "END\n"
        | (Clause ( Term ( Id a,b),y))::tl -> Printf.printf "%s \n" a; print_list tl;; *)

                                     
let rec read_query () = let q = read_line () in
        match q with 
              | "" -> []
              | x -> x::(read_query ());; 

let extract s = match String.split_on_char '"' s with
      | _ :: s :: _ -> Some s | _ -> None

      male(sumit,fg) :- fat(jals, ad), fhj(C,kjd).

      let rec build_query str = match String.split_on_char ':' str  with
            | [x;y] -> (make_term (String.sub x 0 ((String.length x) - 1)::()

            | [x] -> [(make_term (String.sub x 0 ((String.length x) - 1)]
            | _ -> None

(* returns lst[offset:] *)
let rec advance_by offset lst = 
      if (offset <= 0) then lst else 
      match lst with 
            | [] -> []
            | hd::tl -> advance_by (offset -1) tl
            ;;
      
(* returns lst[1:]*)    
let rec delete_front lst = match lst with
            | [] -> []
            | hd::tl -> tl
            ;;

let rec subst_var v s = match s with
            | [] -> Var v
            | (Var v1, Var t1)::tl -> if v = v1 then Var t1 else (subst_var v tl) 
            | (Var v1, Id sym)::tl -> if v = v1 then Id sym else (subst var v tl)
            | (Var v1, Term (p , l))::tl -> if v = v1 then Term (p,l) else (subst_var v tl);; 

let rec subst trm_lst s = match trm_lst with
            | [] -> []
            | (Var v)::tl -> (subst_var v s)::(subst tl s)
            | (Term (p, l))::tl -> (Term (p, (subst l s)))::(subst tl s)
            ;; 

let rec occurs v l = match l with   (* l is term-list *)
            | [] -> false
            | (Var v1)::tl -> if v1 = v then true else (occurs v tl)
            | (Id sym)::tl -> (occurs v tl)
            | (Term (pred, l1))::tl -> (occurs v l1) || (occurs v tl)
              ;;

let rec mgu l1 l2 = match (l1, l2) with
      | ([], []) -> []
      | ((Id pred1, sym_list1)::tl1, (Id pred2, sym_list2)::tl2) -> (*if cur elem is itself a  term*)
            if (pred1 = pred2) then
                  List.append (mgu sym_list1 sym_list2) (mgu tl1 tl2)                  
            else raise NOT_UNIFIABLE
      | ((Id sym1)::tl1, (Id sym2)::tl2) -> if (sym1 = sym2) then [(mgu tl1 tl2)]
                                             else raise NOT_UNIFIABLE
      | ((Var v1)::tl1, (Var v2)::tl2) ->  (Var v1,Var v2)::(mgu tl1 tl2)
      | ((Var v1)::tl1, (Id sym2)::tl2) -> (Var v1, Id sym2)::(mgu tl1 tl2)
      | ((Id sym1)::tl1, (Var v2)::tl2) -> (Var v2, Id sym1)::(mgu tl1 tl2)
      | ((Var v1)::tl1, (Term (p2, l2))::tl2) -> if (occurs v1 l2) then raise NOT_UNIFIABLE
                                                 else (Var v1, Term (p2, l2))::(mgu tl1 tl2)
      | ((Term (p1, l1))::tl1, (Var v2)::tl2) -> if (occurs v2 l1) then raise NOT_UNIFIABLE
                                                 else (Var v2, Term (p1, l1))::(mgu tl1 tl2)
      | _  -> raise NOT_UNIFIABLE
      ;;
      

let rec find_mgu claus_lst index = match claus_lst with
      | [] -> ([], index)
      | (hd_trm , tl_trms_lst)::tl_clauses -> try (mgu [hd_trm] [hd]) with
                                             | NOT_UNIFIABLE ->  find_mgu tl_clauses (index + 1)
                                             | _ -> ((mgu [hd_trm] [hd]), index)
                                    
        (* goal is a term list *)
let rec resolve_query kb_clause_list goal prev_goals_list tree_level offset_list =  
      match (goal, cur_tree_level) with

      |([], x) -> if (x = 0) && (prev_goals_list = [])then true
                  else resolve_query (advance_by ((List.nth offset_list 0) + 1) kb_clause_list)
                                    (List.nth prev_goals_list 0)
                                    (delete_front prev_goals_list)
                                    (tree_level -1)
                                    (delete_front offset_list)

      |(hd::tl, x) ->   let (s,temp_offset) = find_mgu kb_clause_list in
                        if (s = []) && (tree_level = 0) then false 
                        else if (s = []) && (tree_level <> 0) 
                        then resolve_query (advance_by ((List.nth offset_list 0) + 1) kb_clause_list)
                                          (List.nth prev_goals_list 0)
                                          (delete_front prev_goals_list)
                                          (tree_level -1)
                                          (delete_front offset_list)                   
                        
                        else  resolve_query kb_clause_list  
                                            (subst (append_front tl_trms_lst (delete_front goal)) s) 
                                            goal
                                            (tree_level + 1)
                                            (List.append [temp_offset] offset_list)
                        ;;       
 