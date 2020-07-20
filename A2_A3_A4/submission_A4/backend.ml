(*
Note:
  A.
  Str module also needs to be linked
  So compile as: ocamlopt -o backend str.cmxa backend.ml

  B.
  In the functions defined:
  index is a integer tuple (a,b)
  range is a tuple of integer tuples ((a,b), (c,d))
  op is a string
  c is a float

  C.
  General Steps:
  1. Clean all existing lexer or parser dependencies:  
             make clean_lexer clean_parser
  2. Create lexer and parser
             make lexer parser
  3. Create the tokens file (which has the name of the input code file suffixed with "-lex")
             ./lexer code_file
  4. Parse (and run) the tokenized file (giving token file and sheet csv as inputs)
             ./parser code_file-lex data_file.csv
*)

exception RangeOutOfBounds;;
exception InvalidEntries;;
exception IndexOutOfBounds;;
exception DivisionByZero;;
exception IncompatibleRangeSizes;;
exception ReadingDone;;

(*Define the max number of rows and columns to have in matrix, i.e. the max num of rows and columns it expects in the sheet*)

let max_rows = 40;;
let max_cols = 18 ;;

(**Helper functions*)

(**Creates a matrix (float array array) with random values b/w 0. and 10. *)
let rec create_matrix r c = let arr = Array.make_matrix r c 0.0 in
      for x = 0 to r-1 do
        for y =0 to c-1 do
          let temp = Random.float 10.0 in
          if temp < 2.0 then arr.(x).(y) <- nan
          else arr.(x).(y) <- temp
        done	
      done; 
      arr;;

(* Uncomment it to create random matrix  with 15% probability for a cell to be empty *)
(* let sheet = create_matrix max_rows max_cols;; *)
let sheet = Array.make_matrix max_rows max_cols nan;; 

(*check for nan*)
let rec is_nan x = compare x nan = 0;;

(* Checks if the index is valid i.e. lies within the limit (0, max_col) (0, max_rows)*)
let rec within_limits (i,j) = if ((i >= 0) && (i < max_rows) && (j >= 0) && (j < max_rows)) 
                              then true 
                              else false;;

let rec num_of_string str = 
  let idregex = Str.regexp "[0-9]+" in
  if (Str.string_match idregex str 0) then float_of_string(str)
  else nan;;

(* For every input 'line' read from input csv, fills the corresponding row of 'sheet' with the 
read numbers*)
let rec fill_row line row = 
  let str_arr = Str.split (Str.regexp ",") line in
  let l = List.length str_arr in
  for col = 0 to max_cols -1 do
    if col < l then  sheet.(row).(col) <- num_of_string (List.nth str_arr col)      
    else sheet.(row).(col) <- nan
  done;
  ;;

(*Reads the csv file in the ref matrix (float array array) 'sheet' , upto max_rows and max_cols 
only;  If the input csv does not have enough rows or columns as max_rows or max_cols then it
fills the reamining values with 'nan'*)

let read_csv file =
  let in_ch = open_in file in 
  try
    for row = 0 to max_rows-1 do
      let line = input_line in_ch in 
      fill_row line row
    done;
  with End_of_file ->  
    close_in in_ch
  ;;
  


(* Writes 'sheet' matrix to the csv file. Make provision to write nan values as empty cells *)
let write_to_csv out_file= 
    let oc = open_out out_file in
    for i=0 to max_rows-1 do
      for j=0 to max_cols-2 do
        (* If sheet.(i).(j) is not nan then only fill it*)
        if (is_nan sheet.(i).(j)) then  Printf.fprintf oc ","
        else Printf.fprintf oc "%f," sheet.(i).(j)
      done;
      if (is_nan sheet.(i).(max_cols-1)) then  Printf.fprintf oc ",\n"
      else Printf.fprintf oc "%f\n" sheet.(i).(max_cols-1);
    done;
    close_out oc;;


(****************************************MAX**********************************************************)

(*find the max of column 'col' b/w range rows 'up' and 'dn' *)
let rec s_col_range_max mat col up dn = 
      let c = ref  min_float in
              for x = up to dn do
                if (is_nan  mat.(x).(col)) then raise InvalidEntries
                else if mat.(x).(col) > !c then c := mat.(x).(col)
              done;
              !c ;;
(*find the max of row 'row' b/w range cols 'left' and 'right' *)
let rec s_row_range_max mat row left right = 
  let c = ref  min_float in
          for x = left to right do
            if (is_nan  mat.(row).(x)) then raise InvalidEntries
            else if mat.(row).(x) > !c then c := mat.(row).(x)
          done;
          !c ;;
(*find the max in range with top left (r1,c1) and bottom right (r2,c2) *)
let rec s_range_max mat r1 c1 r2 c2 = 
  let c = ref  min_float in
          for x = r1 to r2 do
            let temp = s_row_range_max mat x c1 c2  in
            if temp > !c then c := temp
          done;
          !c ;;

(* Implement row_max now. If index is out of bounds then raise exception*)
let rec full_max mat range index = match range with 
    | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                                match index with
                                (i,j) ->  if within_limits (i,j) then 
                                              mat.(i).(j) <- (s_range_max mat r1 c1 r2 c2)
                                          else raise IndexOutOfBounds                                       
                            else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec row_max mat range index = match range with 
      | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                                  match index with
                                  (i,j) ->  for x = i to i + (r2 - r1) do
                                              if within_limits (x,j) then                                                 
                                                mat.(x).(j) <- (s_row_range_max mat (r1 + x - i) c1 c2)
                                              else raise IndexOutOfBounds   
                                            done                                    
                              else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec col_max mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                            match index with
                            (i,j) ->  for y = j to j + (c2 - c1) do
                                        if within_limits (i,y) then                                                 
                                          mat.(i).(y) <- (s_col_range_max mat (c1 + y -j) r1 r2)
                                        else raise IndexOutOfBounds   
                                      done                                    
                        else raise  RangeOutOfBounds;;


(****************************************MIN**********************************************************)



(*find the max of column 'col' b/w range rows 'up' and 'dn' *)
let rec s_col_range_min mat col up dn = 
  let c = ref  max_float in
          for x = up to dn do
            if (is_nan  mat.(x).(col)) then raise InvalidEntries
            else if mat.(x).(col) < !c then c := mat.(x).(col)
          done;
          !c ;;
(*find the max of row 'row' b/w range cols 'left' and 'right' *)
let rec s_row_range_min mat row left right = 
let c = ref  max_float in
      for x = left to right do
        if (is_nan  mat.(row).(x)) then raise InvalidEntries
        else if mat.(row).(x) < !c then c := mat.(row).(x)
      done;
      !c ;;
(*find the max in range with top left (r1,c1) and bottom right (r2,c2) *)
let rec s_range_min mat r1 c1 r2 c2 = 
let c = ref  max_float in
      for x = r1 to r2 do
        let temp = s_row_range_min mat x c1 c2  in
        if temp < !c then c := temp
      done;
      !c ;;

(* Implement row_max now. If index is out of bounds then raise exception*)
let rec full_min mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                            match index with
                            (i,j) ->  if within_limits (i,j) then 
                                          mat.(i).(j) <- (s_range_min mat r1 c1 r2 c2)
                                      else raise IndexOutOfBounds                                       
                        else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec row_min mat range index = match range with 
  | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                              match index with
                              (i,j) ->  for x = i to i + (r2 - r1) do
                                          if within_limits (x,j) then                                                 
                                            mat.(x).(j) <- (s_row_range_min mat (r1 + x - i) c1 c2)
                                          else raise IndexOutOfBounds   
                                        done                                    
                          else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec col_min mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                        match index with
                        (i,j) ->  for y = j to j + (c2 - c1) do
                                    if within_limits (i,y) then                                                 
                                      mat.(i).(y) <- (s_col_range_min mat (c1 + y -j) r1 r2)
                                    else raise IndexOutOfBounds   
                                  done                                    
                    else raise  RangeOutOfBounds;;


(****************************************SUM**********************************************************)

(* find the max of column 'col' b/w range rows 'up' and 'dn' *)
let rec s_col_range_sum mat col up dn = 
  let c = ref  0. in
          for x = up to dn do
            if (is_nan  mat.(x).(col)) then raise InvalidEntries
            else  c := !c +. mat.(x).(col)
          done;
          !c ;;
(*find the max of row 'row' b/w range cols 'left' and 'right' *)
let rec s_row_range_sum mat row left right = 
let c = ref  0. in
      for x = left to right do
        if (is_nan  mat.(row).(x)) then raise InvalidEntries
        else  c := !c +. mat.(row).(x)
      done;
      !c ;;
(*find the max in range with top left (r1,c1) and bottom right (r2,c2) *)
let rec s_range_sum mat r1 c1 r2 c2 = 
let c = ref  0. in
      for x = r1 to r2 do
        let temp = s_row_range_sum mat x c1 c2  in
        c := !c +. temp
      done;
      !c ;;

(* Implement row_max now. If index is out of bounds then raise exception*)
let rec full_sum mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                            match index with
                            (i,j) ->  if within_limits (i,j) then 
                                          mat.(i).(j) <- (s_range_sum mat r1 c1 r2 c2)
                                      else raise IndexOutOfBounds                                       
                        else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec row_sum mat range index = match range with 
  | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                              match index with
                              (i,j) ->  for x = i to i + (r2 - r1) do
                                          if within_limits (x,j) then                                                 
                                            mat.(x).(j) <- (s_row_range_sum mat (r1 + x - i) c1 c2)
                                          else raise IndexOutOfBounds   
                                        done                                    
                          else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec col_sum mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                        match index with
                        (i,j) ->  for y = j to j + (c2 - c1) do
                                    if within_limits (i,y) then                                                 
                                      mat.(i).(y) <- (s_col_range_sum mat (c1 + y -j) r1 r2)
                                    else raise IndexOutOfBounds   
                                  done                                    
                    else raise  RangeOutOfBounds;;


(****************************************AVG**********************************************************)

(* find the max of column 'col' b/w range rows 'up' and 'dn' *)
let rec s_col_range_avg mat col up dn = 
  let c = ref  0. in
          for x = up to dn do
            if (is_nan  mat.(x).(col)) then raise InvalidEntries
            else  c := !c +. mat.(x).(col)
          done;
          (!c /. float(dn - up + 1)) ;;
(*find the max of row 'row' b/w range cols 'left' and 'right' *)
let rec s_row_range_avg mat row left right = 
let c = ref  0. in
      for x = left to right do
        if (is_nan  mat.(row).(x)) then raise InvalidEntries
        else  c := !c +. mat.(row).(x)
      done;
      (!c /. float(right - left + 1)) ;;
(*find the max in range with top left (r1,c1) and bottom right (r2,c2) *)
let rec s_range_avg mat r1 c1 r2 c2 = 
let c = ref  0. in
      for x = r1 to r2 do
        let temp = s_row_range_sum mat x c1 c2  in
        c := !c +. temp
      done;
      (!c /. float( (r2 - r1 + 1) * (c2 - c1 + 1) )) ;;

(* Implement row_max now. If index is out of bounds then raise exception*)
let rec full_avg mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                            match index with
                            (i,j) ->  if within_limits (i,j) then 
                                          mat.(i).(j) <- (s_range_avg mat r1 c1 r2 c2)
                                      else raise IndexOutOfBounds                                       
                        else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec row_avg mat range index = match range with 
  | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                              match index with
                              (i,j) ->  for x = i to i + (r2 - r1) do
                                          if within_limits (x,j) then                                                 
                                            mat.(x).(j) <- (s_row_range_avg mat (r1 + x - i) c1 c2)
                                          else raise IndexOutOfBounds   
                                        done                                    
                          else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec col_avg mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                        match index with
                        (i,j) ->  for y = j to j + (c2 - c1) do
                                    if within_limits (i,y) then                                                 
                                      mat.(i).(y) <- (s_col_range_avg mat (c1 + y -j) r1 r2)
                                    else raise IndexOutOfBounds   
                                  done                                    
                    else raise  RangeOutOfBounds;;

(****************************************COUNT**********************************************************)

(* find the max of column 'col' b/w range rows 'up' and 'dn' *)
let rec s_col_range_count mat col up dn = 
  let c = ref  0. in
          for x = up to dn do
            if (is_nan  mat.(x).(col)) then c := !c +. 1.0
          done;
          !c     ;;
(*find the max of row 'row' b/w range cols 'left' and 'right' *)
let rec s_row_range_count mat row left right = 
  let c = ref  0. in
        for x = left to right do
          if (is_nan  mat.(row).(x)) then c := !c +. 1.0
        done;
        !c     ;;
(*find the max in range with top left (r1,c1) and bottom right (r2,c2) *)
let rec s_range_count mat r1 c1 r2 c2 = 
let c = ref  0. in
      for x = r1 to r2 do
        let temp = s_row_range_count mat x c1 c2  in
        c := !c +. temp
      done;
      !c ;;

(* Implement row_max now. If index is out of bounds then raise exception*)
let rec full_count mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                            match index with
                            (i,j) ->  if within_limits (i,j) then 
                                          mat.(i).(j) <- (s_range_count mat r1 c1 r2 c2)
                                      else raise IndexOutOfBounds                                       
                        else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec row_count mat range index = match range with 
  | ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                              match index with
                              (i,j) ->  for x = i to i + (r2 - r1) do
                                          if within_limits (x,j) then                                                 
                                            mat.(x).(j) <- (s_row_range_count mat (r1 + x - i) c1 c2)
                                          else raise IndexOutOfBounds   
                                        done                                    
                          else raise  RangeOutOfBounds;;

(*fills the result of every 'row' of 'range' into the column starting at 'index'*)
let rec col_count mat range index = match range with 
| ((r1,c1),(r2,c2)) ->  if ((within_limits (r1,c1)) && (within_limits (r2,c2))) then
                        match index with
                        (i,j) ->  for y = j to j + (c2 - c1) do
                                    if within_limits (i,y) then                                                 
                                      mat.(i).(y) <- (s_col_range_count mat (c1 + y -j) r1 r2)
                                    else raise IndexOutOfBounds   
                                  done                                    
                    else raise  RangeOutOfBounds;;


let rec eval_unaryR index op range = match op with
                | "MAX" -> full_max sheet range index            
                | "ROWMAX" -> row_max sheet range index
                | "COLMAX" -> col_max sheet range index
                | "MIN" -> full_min sheet range index
                | "ROWMIN" -> row_min sheet range index
                | "COLMIN" -> col_min sheet range index
                | "SUM" -> full_sum sheet range index
                | "ROWSUM" -> row_sum sheet range index
                | "COLSUM" -> col_sum sheet range index 
                | "AVG" -> full_avg sheet range index
                | "ROWAVG" -> row_avg sheet range index
                | "COLAVG" -> col_avg sheet range index
                | "COUNT"  -> full_count sheet range index
                | "ROWCOUNT" -> row_count sheet range index
                | "COLCOUNT" -> col_count sheet range index
                ;;

(****************************************BINARYOPS**********************************************************)

(*Applies func on the supplied range and returns the modified matrix of range size*)
let rec apply_on_range func c mat r1 c1 r2 c2 = 
        let temp_mat = Array.make_matrix (r2-r1+1) (c2-c1+1) 0.0 in
        for x = r1 to r2 do
          for y = c1 to c2 do
            if (is_nan mat.(x).(y)) then raise InvalidEntries
            else temp_mat.(x-r1).(y-c1)  <-  func c mat.(x).(y)
          done
        done;
        temp_mat;;
        
let add x y = x +. y;;

let rec add_const mat range c index = match (range, index) with
        | (((r1,c1),(r2,c2)), (i,j)) -> if ( (within_limits (r1,c1)) && 
                                             (within_limits (r2,c2)) &&
                                             (within_limits (i,j)) &&
                                             (within_limits ((i+r2 - r1), (j + c2 -c1)))) 
                                        then let temp = apply_on_range add c mat r1 c1 r2 c2 in
                                          for x = 0 to r2 - r1 do
                                            for y = 0 to c2 - c1 do
                                              mat.(i + x).(j + y) <- temp.(x).(y)
                                            done
                                          done                                                                
                                        else raise  RangeOutOfBounds;;

(*sbtract x from y:  since func c mat.(x).(y) defined*)                                       
let subt x y =  y -. x;;    
let rec subt_const mat range c index = match (range, index) with
        | (((r1,c1),(r2,c2)), (i,j)) -> if ( (within_limits (r1,c1)) && 
                                             (within_limits (r2,c2)) &&
                                             (within_limits (i,j)) &&
                                             (within_limits ((i+r2 - r1), (j + c2 -c1)))) 
                                        then let temp = apply_on_range subt c mat r1 c1 r2 c2 in
                                          for x = 0 to r2 - r1 do
                                            for y = 0 to c2 - c1 do
                                              mat.(i + x).(j + y) <- temp.(x).(y)
                                            done
                                          done                                                                
                                        else raise  RangeOutOfBounds;;     
                                        
let mult x y =  y *. x;;    
let rec mult_const mat range c index = match (range, index) with
        | (((r1,c1),(r2,c2)), (i,j)) -> if ( (within_limits (r1,c1)) && 
                                              (within_limits (r2,c2)) &&
                                              (within_limits (i,j)) &&
                                              (within_limits ((i+r2 - r1), (j + c2 -c1)))) 
                                        then let temp = apply_on_range mult c mat r1 c1 r2 c2 in
                                          for x = 0 to r2 - r1 do
                                            for y = 0 to c2 - c1 do
                                              mat.(i + x).(j + y) <- temp.(x).(y)
                                            done
                                          done                                                                
                                        else raise  RangeOutOfBounds;;     

let div x y =  if x = 0.0 then raise DivisionByZero
               else y /. x ;;   

let rec div_const mat range c index = match (range, index) with
      | (((r1,c1),(r2,c2)), (i,j)) -> if ( (within_limits (r1,c1)) && 
                                            (within_limits (r2,c2)) &&
                                            (within_limits (i,j)) &&
                                            (within_limits ((i+r2 - r1), (j + c2 -c1)))) 
                                      then let temp = apply_on_range div c mat r1 c1 r2 c2 in
                                        for x = 0 to r2 - r1 do
                                          for y = 0 to c2 - c1 do
                                            mat.(i + x).(j + y) <- temp.(x).(y)
                                          done
                                        done                                                                
                                      else raise  RangeOutOfBounds;;     


let rec eval_binaryCR index op c range = match op with
                | "ADD" -> add_const sheet range c index
                | "SUBT" -> subt_const sheet range c index
                | "MULT" -> mult_const sheet range c index
                | "DIV" -> div_const sheet range c index 
                ;;

let rec eval_binaryIR index op index_c range  = match index_c with
                (x,y) -> if (within_limits (x,y))
                         then if (is_nan sheet.(x).(y)) then raise InvalidEntries
                              else let temp = sheet.(x).(y) in
                                    eval_binaryCR index op temp range
                         else raise IndexOutOfBounds ;;

(****************************************BINARYOPS_RANGE**********************************************************)

(*Helper*)
let rec operate_on_ranges func mat r1 c1 r2 c2 rr1 cc1 rr2 cc2 = 
    let temp_mat = Array.make_matrix (r2-r1+1) (c2-c1+1) 0.0 in
    for x = 0 to r2 - r1 do
      for y = 0 to c2 - c1 do
        if ((is_nan mat.(r1 + x).(c1 + y)) || (is_nan mat.(rr1 + x).(cc1 + y))) then raise InvalidEntries
        else temp_mat.(x).(y)  <-  func mat.(rr1 + x).(cc1 + y)  mat.(r1 + x).(c1 + y)
      done
    done;
    temp_mat;;

(*Checks ranges for compatibility and writes their sum to block starting at index*)
let rec add_range mat range1 range2 index = match (range1, index, range2) with
    | (((r1,c1),(r2,c2)), (i,j), ((rr1, cc1), (rr2,cc2))) -> 
                    if ( ((r2 - r1) = (rr2 - rr1)) && ((c2 - c1) = (cc2 - cc1))) 
                    then  if ( (within_limits (r1,c1)) && 
                              (within_limits (r2,c2)) &&
                              (within_limits (i,j)) &&
                              (within_limits ((i+r2 - r1), (j + c2 -c1))) &&
                              (within_limits (rr1,cc1)) && 
                              (within_limits (rr2,cc2))) 
                          then let temp = operate_on_ranges add mat r1 c1 r2 c2 rr1 cc1 rr2 cc2 in
                            for x = 0 to r2 - r1 do
                              for y = 0 to c2 - c1 do
                                mat.(i + x).(j + y) <- temp.(x).(y)
                              done
                            done                                                                
                          else raise  RangeOutOfBounds
                    else raise IncompatibleRangeSizes;;

(*subtracts range2 from range1 *)
let rec subt_range mat range1 range2 index = match (range1, index, range2) with
        | (((r1,c1),(r2,c2)), (i,j), ((rr1, cc1), (rr2,cc2))) -> 
                  if ( ((r2 - r1) = (rr2 - rr1)) && ((c2 - c1) = (cc2 - cc1))) 
                  then  if ( (within_limits (r1,c1)) && 
                            (within_limits (r2,c2)) &&
                            (within_limits (i,j)) &&
                            (within_limits ((i+r2 - r1), (j + c2 -c1))) &&
                            (within_limits (rr1,cc1)) && 
                            (within_limits (rr2,cc2))) 
                        then let temp = operate_on_ranges subt mat r1 c1 r2 c2 rr1 cc1 rr2 cc2 in
                          for x = 0 to r2 - r1 do
                            for y = 0 to c2 - c1 do
                              mat.(i + x).(j + y) <- temp.(x).(y)
                            done
                          done                                                                
                        else raise  RangeOutOfBounds
                  else raise IncompatibleRangeSizes;;

let rec mult_range mat range1 range2 index = match (range1, index, range2) with
        | (((r1,c1),(r2,c2)), (i,j), ((rr1, cc1), (rr2,cc2))) -> 
                  if ( ((r2 - r1) = (rr2 - rr1)) && ((c2 - c1) = (cc2 - cc1))) 
                  then  if ( (within_limits (r1,c1)) && 
                            (within_limits (r2,c2)) &&
                            (within_limits (i,j)) &&
                            (within_limits ((i+r2 - r1), (j + c2 -c1))) &&
                            (within_limits (rr1,cc1)) && 
                            (within_limits (rr2,cc2))) 
                        then let temp = operate_on_ranges mult mat r1 c1 r2 c2 rr1 cc1 rr2 cc2 in
                          for x = 0 to r2 - r1 do
                            for y = 0 to c2 - c1 do
                              mat.(i + x).(j + y) <- temp.(x).(y)
                            done
                          done                                                                
                        else raise  RangeOutOfBounds
                  else raise IncompatibleRangeSizes;;

let rec div_range mat range1 range2 index = match (range1, index, range2) with
        | (((r1,c1),(r2,c2)), (i,j), ((rr1, cc1), (rr2,cc2))) -> 
                  if ( ((r2 - r1) = (rr2 - rr1)) && ((c2 - c1) = (cc2 - cc1))) 
                  then  if ( (within_limits (r1,c1)) && 
                            (within_limits (r2,c2)) &&
                            (within_limits (i,j)) &&
                            (within_limits ((i+r2 - r1), (j + c2 -c1))) &&
                            (within_limits (rr1,cc1)) && 
                            (within_limits (rr2,cc2))) 
                        then let temp = operate_on_ranges div mat r1 c1 r2 c2 rr1 cc1 rr2 cc2 in
                          for x = 0 to r2 - r1 do
                            for y = 0 to c2 - c1 do
                              mat.(i + x).(j + y) <- temp.(x).(y)
                            done
                          done                                                                
                        else raise  RangeOutOfBounds
                  else raise IncompatibleRangeSizes;;

  
let rec eval_binaryRR index op range1 range2 = match op with
                | "ADD" -> add_range sheet range1 range2 index
                | "SUBT" -> subt_range sheet range1 range2 index
                | "MULT" -> mult_range sheet range1 range2 index
                | "DIV" -> div_range sheet range1 range2 index
                ;;
                         

(*******************************************END*********************************************************)
