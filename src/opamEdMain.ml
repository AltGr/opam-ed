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

type path = string list
type shell_command = string

let fatal_exn = function
  | Sys.Break as e -> raise e
  | _ -> ()

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

let path_of_string s = string_split '.' s
let string_of_path f = String.concat "." f
let shell_command_of_string s = s
let string_of_shell_command c = c

type command =
  (* extraction commands *)
  | Get of path
  | Field_list
  | Field_items of path
  | Get_section of path
  (* edition commands *)
  | Add of path * value
  | Remove of path
  | Replace of path * value
  | Add_replace of path * value
  | Append of path * value
  | Prepend of path * value
  | Map of path * shell_command
  | Filter of path * shell_command
  | Replace_item of path * value * value
  | Add_replace_item of path * value * value
  | Remove_item of path * value

let command_of_string s =
  let value_of_strings ss =
    let s = String.concat " " ss in
    try OpamParser.value_from_string s "<none>"
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
    Get (path_of_string f)
  | "field-list" :: r -> check_empty r;
    Field_list
  | "field-items" :: f :: r -> check_empty r;
    Field_items (path_of_string f)
  | "get-section" :: sec :: r -> check_empty r;
    Get_section (path_of_string sec)
  | "add" :: f :: v ->
    Add (path_of_string f, value_of_strings v)
  | "remove" :: s :: r -> check_empty r;
    Remove (path_of_string s)
  | "replace" :: s :: v ->
    Replace (path_of_string s, value_of_strings v)
  | "add-replace" :: s :: v ->
    Add_replace (path_of_string s, value_of_strings v)
  | "append" :: s :: v ->
    Append (path_of_string s, value_of_strings v)
  | "prepend":: s :: v ->
    Prepend (path_of_string s, value_of_strings v)
  | "map" :: s :: cmd ->
    Map (path_of_string s, shell_command_of_string (String.concat " " cmd))
  | "filter" :: s :: cmd ->
    Filter (path_of_string s, shell_command_of_string (String.concat " " cmd))
  | "replace-item" :: s :: vs ->
    begin match value_of_strings ("[" :: vs @ ["]"]) with
      | List (_, [v1; v2]) -> Replace_item (path_of_string s, v1, v2)
      | _ ->
        failwith "replace-item expects 3 arguments: field name, expression to \
                  replace, and replacement"
    end
  | "add-replace-item" :: s :: vs ->
    begin match value_of_strings ("[" :: vs @ ["]"]) with
      | List (_, [v1; v2]) -> Add_replace_item (path_of_string s, v1, v2)
      | _ ->
        failwith "replace-item expects 3 arguments: field name, expression to \
                  replace, and replacement"
    end
  | "remove-item" :: s :: v ->
    Remove_item (path_of_string s, value_of_strings v)
  | _ ->
    Printf.ksprintf failwith "Invalid command: %S" s

let string_of_command =
  let (!) f = fun () -> f in
  function
  | Get f ->
    Printf.sprintf "get %a" !string_of_path f
  | Field_list ->
    "field-list"
  | Field_items f ->
    Printf.sprintf "field-items %a" !string_of_path f
  | Get_section sec ->
    Printf.sprintf "get-section %a" !string_of_path sec
  | Add (f, v) ->
    Printf.sprintf "add %a %a" !string_of_path f !OpamPrinter.value v
  | Remove f ->
    Printf.sprintf "remove %a" !string_of_path f
  | Replace (f, v) ->
    Printf.sprintf "replace %a %a" !string_of_path f !OpamPrinter.value v
  | Add_replace (f, v) ->
    Printf.sprintf "add-replace %a %a" !string_of_path f !OpamPrinter.value v
  | Append (f, v) ->
    Printf.sprintf "append %a %a" !string_of_path f !OpamPrinter.value v
  | Prepend (f, v) ->
    Printf.sprintf "prepend %a %a" !string_of_path f !OpamPrinter.value v
  | Map (f, cmd) ->
    Printf.sprintf "map %a %a" !string_of_path f !string_of_shell_command cmd
  | Filter (f, cmd) ->
    Printf.sprintf "filter %a %a" !string_of_path f
      !string_of_shell_command cmd
  | Replace_item (f, v1, v2) ->
    Printf.sprintf "replace-item %a %a %a" !string_of_path f
      !OpamPrinter.value v1
      !OpamPrinter.value v2
  | Add_replace_item (f, v1, v2) ->
    Printf.sprintf "add-replace-item %a %a %a" !string_of_path f
      !OpamPrinter.value v1
      !OpamPrinter.value v2
  | Remove_item (f, v) ->
    Printf.sprintf "remove-item %a %a" !string_of_path f !OpamPrinter.value v

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

let arg_normalise =
  Arg.(value & vflag `Preserve [
      `Preserve, info ["preserve"] ~doc:
        "Preserve the formatting of unchanged parts of the file.";
      `Reformat, info ["reformat"] ~doc:
        "Reformat the file contents when printing. This discards comments.";
      `Canonical, info ["canonical"] ~doc:
        "Output the file in canonical format (ordered fields, minimal \
         formatting), for reproducible signatures."
    ])

let arg_commands =
  let doc = "Specify the commands to process" in
  let cmd =
    Arg.conv ~docv:"COMMAND"
      ((fun s -> try Ok (command_of_string s) with Failure m -> Error (`Msg m)),
       (fun fmt cmd -> Format.pp_print_string fmt (string_of_command cmd)))
  in
  Arg.(value & pos_all cmd [] & info ~docv:"COMMAND" ~doc [])

let string_of_channel ic =
  let b = Buffer.create 4096 in
  try while true do Buffer.add_channel b ic 4096 done; assert false
  with End_of_file -> Buffer.contents b

let shell_command cmd text =
  let ic, oc as process = Unix.open_process cmd in
  output_string oc text;
  close_out oc;
  let s = string_of_channel ic in
  match Unix.close_process process with
  | Unix.WEXITED 0 -> Some s
  | Unix.WSIGNALED i when i = Sys.sigint -> raise Sys.Break
  | _ -> None

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

let list ?(pos=pos_null) = function
  | [] -> None
  | its -> Some (List (pos, its))

let map_list f = function
  | List (pos, l) -> list ~pos (f l)
  | elt ->
    match f [elt] with
    | [e] -> Some e
    | l -> list l

let singleton x = List (pos_null, [x])

let rec value_equals v1 v2 = match v1, v2 with
  | Bool (_, b1), Bool (_, b2) -> b1 = b2
  | Int (_, i1), Int (_, i2) -> i1 = i2
  | String (_, s1), String (_, s2) -> s1 = s2
  | Relop (_, r1, va1, vb1), Relop (_, r2, va2, vb2) ->
    r1 = r2 && value_equals va1 va2 && value_equals vb1 vb2
  | Prefix_relop (_, r1, v1), Prefix_relop (_, r2, v2) ->
    r1 = r2 && value_equals v1 v2
  | Logop (_, l1, va1, vb1), Logop (_, l2, va2, vb2) ->
    l1 = l2 && value_equals va1 va2 && value_equals vb1 vb2
  | Pfxop (_, p1, v1), Pfxop (_, p2, v2) ->
    p1 = p2 && value_equals v1 v2
  | Ident (_, s1), Ident (_, s2) ->
    s1 = s2
  | List (_, vl1), List (_, vl2) ->
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Group (_, vl1), Group (_, vl2) ->
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Option (_, v1, vl1), Option (_, v2, vl2) ->
    value_equals v1 v2 &&
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Env_binding (_, v1, op1, vx1), Env_binding (_, v2, op2, vx2) ->
    op1 = op2 && value_equals v1 v2 && value_equals vx1 vx2
  | _ -> false

let exec_command f cmd =
  let contents = f.file_contents in
  let msg =
    match cmd with
    | Get path -> Some (OpamPrinter.value (get_path path contents))
    | Field_list ->
      let rec list_fields pfx = function
        | Section (_, {section_kind; section_items; _}) :: r ->
          list_fields (pfx^section_kind^".") section_items @
          list_fields pfx r
        | Variable (_, fld, _) :: r ->
          (pfx ^ fld) :: list_fields pfx r
        | [] -> []
      in
      let flds = list_fields "" contents in
      Some (String.concat "\n" flds)
    | Field_items path ->
      let l = get_list (get_path path contents) in
      let items = List.map OpamPrinter.value l in
      Some (String.concat "\n" items)
    | Get_section path ->
      let rec get contents = function
        | [] -> contents
        | sec :: path -> get (get_section sec contents) path
      in
      Some (OpamPrinter.items (get contents path))
    | _ -> None
  in
  (match msg with Some m -> print_endline m | None -> ());
  let contents =
    match cmd with
    | Get _ | Field_list | Field_items _ | Get_section _ -> contents
    | Add (path, v) ->
      map_path ~absent:(fun () -> v)
        (fun _ -> Printf.ksprintf failwith "Field %s exists already"
            (string_of_path path))
        path contents
    | Remove path ->
      map_path (fun _ -> None) path contents
    | Replace (path, v) ->
      map_path (fun _ -> Some v) path contents
    | Add_replace (path, v) ->
      map_path ~absent:(fun () -> v)
        (fun _ -> Some v) path contents
    | Append (path, v) ->
      map_path ~absent:(fun () -> singleton v)
        (map_list (fun l -> l @ [v]))
        path contents
    | Prepend (path, v) ->
      map_path ~absent:(fun () -> singleton v)
        (map_list (fun l -> v :: l))
        path contents
    | Map (path, cmd) ->
      map_path
        (map_list @@ List.map @@ fun v ->
         let s = OpamPrinter.value v in
         match shell_command cmd s with
         | None ->
           Printf.eprintf "Error while running command %S on %S\n" cmd s;
           v
         | Some s ->
           try OpamParser.value_from_string s f.file_name
           with Parsing.Parse_error ->
             Printf.eprintf
               "Error: command %S returned %S, which doesn't parse\n"
               cmd s;
             v)
        path contents
    | Filter (path, cmd) ->
      map_path
        (map_list @@ List.filter @@ fun v ->
         shell_command cmd (OpamPrinter.value v) <> None)
        path contents
    | Replace_item (path, v1, v2) ->
      let rec repl = function
        | [] -> []
        | x::r when value_equals x v1 -> v2 :: r
        | x::r -> x :: repl r
      in
      map_path (map_list repl) path contents
    | Add_replace_item (path, v1, v2) ->
      let rec repl = function
        | [] -> [v2]
        | x::r when value_equals x v1 -> v2 :: r
        | x::r -> x :: repl r
      in
      map_path (map_list repl) path contents
    | Remove_item (path, v) ->
      let rec rem = function
        | [] -> []
        | x::r when value_equals x v -> r
        | x::r -> x :: rem r
      in
      map_path (map_list rem) path contents
  in
  {f with file_contents = contents}

let run files inplace normalise commands =
  let print txt orig f =
    match normalise with
    | `Preserve -> OpamPrinter.Preserved.items txt orig.file_contents f.file_contents
    | `Reformat -> OpamPrinter.opamfile f ^ "\n"
    | `Canonical -> OpamPrinter.Normalise.opamfile f
  in
  let needs_reprint =
    commands = [] || List.exists is_edition_command commands ||
    inplace && normalise <> `Preserve
  in
  if files = [] then
    try
      let txt = try string_of_channel stdin with Sys_error _ -> "" in
      let orig = OpamParser.string txt "/dev/stdin" in
      let f = List.fold_left exec_command orig commands in
      if needs_reprint then print_string (print txt orig f)
    with e ->
      fatal_exn e;
      Printf.eprintf "Error on input from stdin: %s\n"
        (Printexc.to_string e);
      exit 10
  else
  let ok =
    List.fold_left (fun ok file ->
        try
          let txt =
            let ic = open_in file in
            try
              let s = string_of_channel ic in
              close_in ic; s
            with e -> close_in ic; raise e
          in
          let orig = OpamParser.string txt file in
          let f = List.fold_left exec_command orig commands in
          if not needs_reprint then ok else
          let s = print txt orig f in
          if inplace then
            let oc = open_out file in
            output_string oc s;
            close_out oc;
            ok
          else
            (output_string stdout s;
             flush stdout;
             ok)
        with e ->
          fatal_exn e;
          Printf.eprintf "Error on file %s: %s\n" file
            (Printexc.to_string e);
          false
      ) true files
  in
  if not ok then exit 10

let cmd =
  Term.(pure run $ arg_files $ arg_inplace $ arg_normalise $ arg_commands)

let man = [
  `S "ARGUMENTS";
  `S "COMMANDS";
  `P "A list of commands, each as one argument, can be specified on the \
      command-line. $(i,FIELD) and $(i,SECTION) arguments can be specified as \
      paths separated with dots for accessing in-section elements (e.g. \
      $(i,section.field)).";
  `P "$(b,extraction commands) always print their results to stdout, and don't \
      modify the file:";
  `I ("$(b,get) $(i,FIELD)",
      "Print out the value of the named $(i,FIELD).");
  `I ("$(b,field-list)",
      "List the field names present in the input.");
  `I ("$(b,field-items) $(i,FIELD)",
      "Print out the items of $(i,FIELD), understood as a list, separated by \
       newlines.");
  `I ("$(b,get-section) $(i,SECTION)",
      "Extract and print the contents of the given $(i,SECTION).");
  `P "$(b,edition commands) modify the contents, and write to stdout, or back \
      to the original file if $(b,--inplace) was specified:";
  `I ("$(b,add) $(i,FIELD) $(i,value)",
      "Add the given $(i,FIELD), with the given contents, to the file, if it \
       didn't exist already");
  `I ("$(b,remove) $(i,FIELD)",
      "Remove the given $(i,FIELD) from the file, if present.");
  `I ("$(b,replace) $(i,FIELD) $(i,value)",
      "Replace the contents of the given $(i,FIELD), if found, by the given \
       value.");
  `I ("$(b,add-replace) $(i,FIELD) $(i,value)",
      "Replace the contents of the given $(i,FIELD) by the given value, adding \
       the field if not present already. This is equivalent to the sequence \
       'remove $(i,FIELD)' 'add $(i,FIELD) $(i,value)'");
  `I ("$(b,append) $(i,FIELD) $(i,value)",
      "Append the given value to the given $(i,FIELD), treated as a list. The \
       field is created as a singleton if it didn't exist");
  `I ("$(b,prepend) $(i,FIELD) $(i,value)",
      "Prepend the given value to the given $(i,FIELD), treated as a list. The \
       field is created as a singleton if it didn't exist");
  `I ("$(b,map) $(i,FIELD) $(i,command)",
      "Run the given shell command with each member of $(i,FIELD), treated as \
       a list, as input, and replace it by the output of the command.");
  `I ("$(b,filter) $(i,FIELD) $(i,cmd)",
      "Run the given shell command with each member of $(i,FIELD), treated as \
       a list, as input, and remove any member for which the command doesn't \
       return 0.");
  `I ("$(b,replace-item) $(i,FIELD) $(i,value) $(i,replacement)",
      "Replace the first item of the contents of $(i,FIELD), treated as a \
       list, that is equal to $(i,value), with $(i,replacement). Nothing \
       happens if $(i,value) is not a member of the $(i,FIELD).");
  `I ("$(b,add-replace-item) $(i,FIELD) $(i,value) $(i,replacement)",
      "Replace the first item of the contents of $(i,FIELD), treated as a \
       list, that is equal to $(i,value), with $(i,replacement). \
       $(i,replacement) is appended to $(i,FIELD) if $(i,value) was not \
       found.");
  `I ("$(b,remove-item) $(i,FIELD) $(i,value)",
      "Remove the first item of the contents of $(i,FIELD), treated as a \
       list, that is equal to $(i,value), if any.");
]

let main_cmd_info =
  Term.info "opam-ed" ~version:"0.4"
    ~doc:"A command-line editor for the opam file format"
    ~man

let () =
  Sys.catch_break true;
  try
    let r = Term.eval (cmd, main_cmd_info) in
    Term.exit r
  with Sys.Break ->
    prerr_endline "Interrupted";
    exit 130
