(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes
open Cmdliner

(** Commands

    EXTRACTION:

    get FIELD
    field-list
    field-items
    get-section SECTION

    EDITION:

    add FIELD str
    remove FIELD
    replace FIELD str
    append FIELD str
    prepend FIELD str
    map FIELD cmd
    filter FIELD cmd
    replace-item FIELD str str
    add-replace-item FIELD str str

    OPTIONS:

    --file -f
    --inplace -i
    --normalise -n
*)

type section = string
type field = string list
type shell_command = string

let string_cut c s =
  try
    let i = String.index s c in
    String.sub s 0 i, Some (String.sub s (i+1) (String.length s - i - 1))
  with Not_found ->
    s, None

let rec string_split c s =
  match string_cut c s with
  | s, None -> [s]
  | s, Some s' -> s :: string_split c s'

let field_of_string s = string_split '.' s
let string_of_field f = String.concat "." f
let section_of_string s = s
let string_of_section sec = sec
let shell_command_of_string s = s
let string_of_shell_command c = c

type command =
  (* extraction commands *)
  | Get of field
  | Field_list
  | Field_items of field
  | Get_section of section
  (* edition commands *)
  | Add of field * value
  | Remove of field
  | Replace of field * value
  | Append of field * value
  | Prepend of field * value
  | Map of field * shell_command
  | Filter of field * shell_command
  | Replace_item of field * value * value
  | Add_replace_item of field * value * value
  | Remove_item of field * value

let command_of_string s =
  let value_of_strings ss =
    let s = String.concat " " ss in
    try OpamParser.value_of_string s
    with Parsing.Parse_error ->
      Printf.ksprintf failwith "Syntax error in value in %S" s
  in
  let check_empty r =
    if r <> [] then
      Printf.kprintf failwith "Invalid extra argument %S"
        (String.concat " " r)
  in
  match string_split ' ' s with
  | "get" :: f :: r -> check_empty r;
    Get (field_of_string f)
  | "field-list" :: r -> check_empty r;
    Field_list
  | "field-items" :: f :: r -> check_empty r;
    Field_items (field_of_string f)
  | "get-section" :: sec :: r -> check_empty r;
    Get_section (section_of_string sec)
  | "add" :: f :: v ->
    Add (field_of_string f, value_of_strings v)
  | "remove" :: s :: r -> check_empty r;
    Remove (field_of_string s)
  | "replace" :: s :: v ->
    Replace (field_of_string s, value_of_strings v)
  | "append" :: s :: v ->
    Append (field_of_string s, value_of_strings v)
  | "prepend":: s :: v ->
    Prepend (field_of_string s, value_of_strings v)
  | "map" :: s :: cmd ->
    Map (field_of_string s, shell_command_of_string (String.concat " " cmd))
  | "filter" :: s :: cmd ->
    Filter (field_of_string s, shell_command_of_string (String.concat " " cmd))
  | "replace-item" :: s :: vs ->
    begin match value_of_strings ("[" :: vs @ ["]"]) with
      | List (_, [v1; v2]) -> Replace_item (field_of_string s, v1, v2)
      | _ ->
        failwith "replace-item expects 3 arguments: field name, expression to \
                  replace, and replacement"
    end
  | "add-replace-item" :: s :: vs ->
    begin match value_of_strings ("[" :: vs @ ["]"]) with
      | List (_, [v1; v2]) -> Add_replace_item (field_of_string s, v1, v2)
      | _ ->
        failwith "replace-item expects 3 arguments: field name, expression to \
                  replace, and replacement"
    end
  | "remove-item" :: s :: v ->
    Remove_item (field_of_string s, value_of_strings v)
  | _ ->
    Printf.ksprintf failwith "Invalid command: %S" s

let string_of_command =
  let (!) f = fun () -> f in
  function
  | Get f ->
    Printf.sprintf "get %a" !string_of_field f
  | Field_list ->
    "field-list"
  | Field_items f ->
    Printf.sprintf "field-items %a" !string_of_field f
  | Get_section sec ->
    Printf.sprintf "get-section %a" !string_of_section sec
  | Add (f, v) ->
    Printf.sprintf "add %a %a" !string_of_field f !OpamPrinter.value v
  | Remove f ->
    Printf.sprintf "remove %a" !string_of_field f
  | Replace (f, v) ->
    Printf.sprintf "replace %a %a" !string_of_field f !OpamPrinter.value v
  | Append (f, v) ->
    Printf.sprintf "append %a %a" !string_of_field f !OpamPrinter.value v
  | Prepend (f, v) ->
    Printf.sprintf "prepend %a %a" !string_of_field f !OpamPrinter.value v
  | Map (f, cmd) ->
    Printf.sprintf "map %a %a" !string_of_field f !string_of_shell_command cmd
  | Filter (f, cmd) ->
    Printf.sprintf "filter %a %a" !string_of_field f
      !string_of_shell_command cmd
  | Replace_item (f, v1, v2) ->
    Printf.sprintf "replace-item %a %a %a" !string_of_field f
      !OpamPrinter.value v1
      !OpamPrinter.value v2
  | Add_replace_item (f, v1, v2) ->
    Printf.sprintf "add-replace-item %a %a %a" !string_of_field f
      !OpamPrinter.value v1
      !OpamPrinter.value v2
  | Remove_item (f, v) ->
    Printf.sprintf "remove-item %a %a" !string_of_field f !OpamPrinter.value v

let is_extraction_command = function
  | Get _ | Field_list | Field_items _ | Get_section _ -> true
  | _ -> false

let is_edition_command c = not (is_extraction_command c)

let arg_files =
  let doc =
    "File to process. If unspecified, stdin is used. Can be repeated."
  in
  Arg.(value & opt_all file [] & info ~docv:"FILE" ~doc ["file"; "f"])

let arg_inplace =
  let doc =
    "When editing files, overwrite them rather than print the result to \
     standard output."
  in
  Arg.(value & flag & info ~doc ["inplace"; "i"])

(*
let arg_normalise =
  let doc =
    "Output the file normalised, without preserving any comments or \
     formatting."
  in
  Arg.(value & flag & info ~doc ["normalise"; "n"])
*)

let arg_commands =
  let doc = "Specify the commands to process" in
  let cmd =
    Arg.conv ~docv:"COMMAND"
      ((fun s -> try Ok (command_of_string s) with Failure m -> Error (`Msg m)),
       (fun fmt cmd -> Format.pp_print_string fmt (string_of_command cmd)))
  in
  Arg.(value & pos_all cmd [] & info ~docv:"COMMAND" ~doc [])

let rec get_field fld = function
  | [] -> raise Not_found
  | Variable (_, f, v) :: _ when f = fld -> v
  | _ :: r -> get_field fld r

let rec get_section sec = function
  | [] -> raise Not_found
  | Section (_, {section_kind; section_items}) :: _ when section_kind = sec ->
    section_items
  | _ :: r -> get_section sec r

let rec get_path path items =
  match path with
  | [] -> failwith "Invalid empty field specification"
  | [fld] -> get_field fld items
  | sec::path -> get_path path (get_section sec items)

let pos_null = "", -1, -1

let rec map_field ?absent f fld = function
  | Variable (pos, name, v) :: r when name = fld ->
    (match f v with
     | None -> r
     | Some v -> Variable (pos, name, v) :: r)
  | x :: r -> x :: map_field ?absent f fld r
  | [] -> match absent with
    | None -> []
    | Some v -> [Variable (pos_null, fld, v ())]

let rec map_section ?absent f sec = function
  | Section (pos, ({section_kind; _} as s)) :: r when section_kind = sec ->
    (match f s.section_items with
     | None -> r
     | Some section_items -> Section (pos, {s with section_items}) :: r)
  | x :: r -> x :: map_section ?absent f sec r
  | [] -> match absent with
    | None -> []
    | Some v ->
      let s = {
        section_kind = sec;
        section_name = None;
        section_items = v ();
      } in
      [Section (pos_null, s)]

let rec map_path ?absent f path items =
  match path with
  | [] -> failwith "Invalid empty field specification"
  | [fld] -> map_field ?absent f fld items
  | sec::path ->
    let abs = match absent with
      | None -> None
      | Some a -> Some (fun () -> map_path ~absent:a f path [])
    in
    map_section ?absent:abs
      (fun it -> Some (map_path ?absent f path it))
      sec items

let get_list = function
  | List (_, l) -> l
  | elt -> [elt]

let list its = List (pos_null, its)

let exec_command f cmd =
  let msg =
    match cmd with
    | Get path -> Some (OpamPrinter.value (get_path path f.file_contents))
    | Field_list -> assert false
    | Field_items path -> assert false
    | Get_section sec -> assert false
    | _ -> None
  in
  (match msg with Some m -> print_endline m | None -> ());
  let contents = f.file_contents in
  let contents =
    match cmd with
    | Get _ | Field_list | Field_items _ | Get_section _ -> contents
    | Add (path, v) ->
      map_path ~absent:(fun () -> v)
        (fun _ -> Printf.ksprintf failwith "Field %s exists already"
            (string_of_field path))
        path contents
    | Remove path ->
      map_path (fun _ -> None) path contents
    | Replace (path, v) ->
      map_path (fun _ -> Some v) path contents
    | Append (path, v) ->
      map_path ~absent:(fun () -> list [v])
        (fun x -> Some (list (get_list x @ [v])))
        path contents
    | Prepend (path, v) ->
      map_path ~absent:(fun () -> list [v])
        (fun x -> Some (list (v :: get_list x)))
        path contents
    | Map (path, cmd) -> assert false
    | Filter (path, cmd) -> assert false
    | Replace_item (path, v1, v2) ->
      let rec repl = function
        | [] -> []
        | x::r when x = v1 -> v2 :: r
        | x::r -> x :: repl r
      in
      map_path (fun x -> Some (list (repl (get_list x))))
        path contents
    | Add_replace_item (path, v1, v2) ->
      let rec repl = function
        | [] -> [v2]
        | x::r when x = v1 -> v2 :: r
        | x::r -> x :: repl r
      in
      map_path (fun x -> Some (list (repl (get_list x))))
        path contents
    | Remove_item (path, v) ->
      let rec rem = function
        | [] -> []
        | x::r when x = v -> r
        | x::r -> x :: rem r
      in
      map_path (fun x -> Some (list (rem (get_list x))))
        path contents
  in
  {f with file_contents = contents}

let run files inplace (*normalise*) commands =
  if files = [] then
    try
      let f = OpamParser.channel stdin "/dev/stdin" in
      let f = List.fold_left exec_command f commands in
      if commands = [] || List.exists is_edition_command commands then
        print_endline (OpamPrinter.opamfile f)
    with e ->
      Printf.eprintf "Error on input from stdin: %s\n"
        (Printexc.to_string e);
      exit 10
  else
  let ok =
    List.fold_left (fun ok file ->
        try
          let f = OpamParser.file file in
          let f = List.fold_left exec_command f commands in
          let s = OpamPrinter.opamfile f in (* This normalises *)
          if inplace then
            let oc = open_out file in
            output_string oc s;
            output_string stdout "\n";
            close_out oc;
            ok
          else
            (output_string stdout s;
             output_string stdout "\n";
             flush stdout;
             ok)
        with e ->
          Printf.eprintf "Error on file %s: %s\n" file
            (Printexc.to_string e);
          false
      ) true files
  in
  if not ok then exit 10

let cmd =
  Term.(pure run $ arg_files $ arg_inplace $ (*arg_normalise $*) arg_commands)

let man = [
  `S "ARGUMENTS";
  `S "COMMANDS";
  `P "A list of commands, each as one argument, can be specified on the \
      command-line.";
  `P "$(b,extraction commands) always print their results to stdout:";
  `P "get FIELD";
  `P "field-list";
  `P "field-items";
  `P "get-section SECTION";
  `P "$(b,edition commands) modify the contents, and write to stdout, or back \
      to the original file if $(b,--inplace) was specified:";
  `P "add FIELD str";
  `P "remove FIELD";
  `P "replace FIELD str";
  `P "append FIELD str";
  `P "prepend FIELD str";
  `P "map FIELD cmd";
  `P "filter FIELD cmd";
  `P "replace-item FIELD str str";
  `P "add-replace-item FIELD str str";
]

let main_cmd_info =
  Term.info "opam-ed" ~version:"0.1"
    ~doc:"A command-line editor for the opam file format"
    ~man

let () =
  let r = Term.eval (cmd, main_cmd_info) in
  Term.exit r
