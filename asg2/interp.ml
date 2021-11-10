(* Id: interp.ml,v 1.19 2021-04-20 20:19:30-07 - - $ *)

open Absyn
open Tables
open Printf


let want_dump = ref false

let source_filename = ref ""

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    (*Accessing the Unary table*)
    (*Ref Used from Scheme: ((symbol? expr) 
    (hash-ref *var-table* expr NAN)) *)
    | Unary (oper, expr) -> let value = eval_expr expr  
    (*Used hashexample.ml: let value = Hashtbl.find variable_table var*)
                                in Hashtbl.find
                                    Tables.unary_fn_table oper value 
    (*Note: Oper = operation*)
    | Binary (oper, expr1, expr2) -> let val1 = eval_expr expr1 in
                                     let val2 = eval_expr expr2 
                in Hashtbl.find Tables.binary_fn_table oper val1 val2

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> 
               Array.get (Hashtbl.find Tables.array_table ident)
                  (Float.to_int (eval_expr expr))
    | Variable ident -> 
               Hashtbl.find Tables.variable_table ident
    (*| Arrayref (ident, expr) -> eval_STUB "eval_memref Arrayref"
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0*)

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

(*and eval_bool (expr : Absyn.expr) : float = match expr with*)



let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
       | _, _, None -> interpret continue
       | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim (ident, expr) continue
    (*interp_STUB "Dim (ident, expr)" continue *)
    | Let (memref, expr) -> interp_let (memref, expr) continue 
    (*interp_STUB "Let (memref, expr)" continue*)
    | Goto label ->  interp_goto label continue    
    (*Calls on Goto Function now *)
    | If (expr, label) -> interp_if (expr, label) continue
    (*interp_STUB "If (expr, label)" *)
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; 
    print_newline ());
    interpret continue

and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =  
        try  let number = Etc.read_number ()
             in match memref with
             | Variable ident -> (Hashtbl.add  (*Fix WARNING HERE *)
               Tables.variable_table ident number)
             | _ -> print_string "OWO"
             (*| Arrayref (ident, x) -> (Hashtbl.add
               Tables.array_table (ident, x) number)*) 
               (*add inputs into array?*)   
        with End_of_file -> 
             (print_string "End_of_file"; (Hashtbl.replace 
                 Tables.variable_table "eof" 1.);
                 print_newline ())
    in List.iter input_number memref_list;
    interpret continue

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

(*Untested*)
and interp_dim (ident, expr)
                 (continue: Absyn.program) =
    Hashtbl.add Tables.array_table ident
       (Array.make (Float.to_int (eval_expr expr)) 0.0);
    interpret continue

(*Variable Case Works but need to check if ArrayRef*)
and interp_let (memref, expr)
                 (continue : Absyn.program) = match memref with
    | Variable ident -> (Hashtbl.add
        Tables.variable_table ident (eval_expr expr);
        (*printf "HELLO";
        print_newline();*)
            (interpret continue))
    | Arrayref (ident, x) ->
        try let id = Hashtbl.find Tables.array_table ident
        (*set a n x modifies float array "a" in place, 
        replacing element number "n" with "x" *)
                            in (Array.set id (*a - location in array*)
                               (Float.to_int (eval_expr x)) 
                               (*n - current value*)
                               (eval_expr expr); (*x - new value*)
                               (interpret continue))
        with Not_found -> (exit 1);

(* Only Jump, no continue*)
and interp_goto label (continue : Absyn.program) =
    (*Attempts to check if the LABEL exists *)
    try let target = Hashtbl.find Tables.label_table label
        in interpret target  
    with Not_found -> (exit 1); 
    (*Note: Not_Found is an Exception Type Error*)

(*expr formats as (oper, expr1, expr2) =>  EX:  "= i 10" *)
(*Label is for the Goto Function *)
and interp_if (expr, label) (continue : Absyn.program) = 
    match expr with
    |Relexpr  (oper, expr1, expr2) ->

        (*Get the bool value established*)
        (*What if the OPER isnt valid?*)
        try let bin = Hashtbl.find Tables.bool_fn_table oper in
            (*If oper, expr1, expr2 exists & true
            then jump to said label 
            else do not jump*)
            if bin (eval_expr expr1) (eval_expr expr2) 
            (*Checks if "label" exists in the label-table
            else end program *)
            then (try let jump = Hashtbl.find Tables.label_table label
                        in interpret jump
                    (*IF LABEL DNE, End Program *)
                    with Not_found -> exit(1))  
            else interpret continue
        with Not_found -> exit(1);
    (*DEFAULT*)
    | _ -> exit(1) 
    (*Underscore (_) is a wildcard that will match anything *)


let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program;
     if !want_dump then Tables.dump_label_table ())
