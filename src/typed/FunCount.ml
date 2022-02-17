open Base
open Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Utils

type input = Tast_iterator.iterator

let lint_id = "fun_count"
let group = LINT.Style
let level = LINT.Warn
let lint_source = LINT.FPCourse

let describe_itself () =
  describe_as_clippy_json
    lint_id
    ~group
    ~level
    ~docs:
      {|
### What it does?

Checks what in file count of declarated functions is less than 30


### Why it is important?

IDUNNO. Thus Spoke Zarathustra... Vitaly Bragilevsky said what many functions in file is bad. 
|}
;;

(* let msg ppf s = Caml.Format.fprintf ppf "%s\n%!" s

let report filename ~loc e =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg e

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:loc.loc_start.pos_lnum
        msg
        e
    ;;
  end
  in
  (module M : LINT.REPORTER)
;; *)
let msg ppf s = Caml.Format.fprintf ppf "%s\n%!" s

let report ~filename ~loc e =
  let module M = struct
    let txt ppf () = Utils.Report.txt ~filename ~loc ppf msg e

    let rdjsonl ppf () =
      RDJsonl.pp
        ppf
        ~filename:(Config.recover_filepath loc.loc_start.pos_fname)
        ~line:0
        msg
        e
    ;;
  end
  in
  (module M : LINT.REPORTER)
;;

let run {Compile_common.source_file; _} fallback =
  let open Tast_iterator in
  { fallback with
  structure = 
      (fun self str ->
        let open Parsetree in

        let item_list = str.str_items in
        
        let is_item_has_function (str_item : Typedtree.structure_item) = 
          let is_function (expr : Typedtree.expression) : bool = match expr.exp_type.desc with 
          | Tarrow _ -> true
          | _ -> false
          in
          let is_binding_has_function (binding : Typedtree.value_binding) : bool = is_function binding.vb_expr in

          let bindings = match str_item.str_desc with
          | Tstr_value (_, bindings) -> bindings
          | Tstr_eval _ -> []
          | _ -> []
          in
          List.exists bindings ~f:is_binding_has_function
        in
        let count_of_fuctions = List.fold_left ~f:(fun acc item -> acc + if is_item_has_function item then 1 else 0) ~init:0 item_list in
        let filename = List.fold_left ~f:(fun acc item -> 
          let x = item.str_loc.Location.loc_start.Lexing.pos_fname in
          if x == acc || acc == "XD ;3" then x else ""
          ) ~init:"XD ;3" item_list in

        if count_of_fuctions > 100  then (
        let first_item = List.hd @@ List.filter ~f:is_item_has_function item_list in
        Option.iter first_item ~f:(fun first_item ->
          let loc = first_item.str_loc in
          CollectedLints.add ~loc (report ~filename ~loc ("This file contains " ^ string_of_int count_of_fuctions ^ " but less 100 expected!")))
        );
        

        fallback.structure self str)
  }
;;

