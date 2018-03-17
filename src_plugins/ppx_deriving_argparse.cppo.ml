open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "argparse"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_help attrs =
  Ppx_deriving.(attrs |> attr ~deriver "help" |> Arg.(get_attr ~deriver string))

let attr_fun attrs =
  Ppx_deriving.(attrs |> attr ~deriver "fun" |> Arg.(get_attr ~deriver expr))

let sig_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let typ0 = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ_parse = [%type: [%t typ0] -> string array -> [%t typ0] * string array] in
  let typ_perr = [%type: [%t typ0] -> unit] in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl)) typ_parse);
  Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`PrefixSuffix (deriver, "perr")) type_decl)) typ_perr)]

let string_replace c_in c_out str =
    String.init (String.length str)
        (fun i -> match str.[i] with
         | c when c == c_in -> c_out
         | c -> c)

let perr_msg { pld_name = { txt = name; loc }; pld_type; pld_attributes } =
  let simple x =
      [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out [%e str (x ^ "%!")]] in
  let format x name0 name msg =
      [%expr Ppx_deriving_runtime.prerr_string
            [%e str (Printf.sprintf "  %s %s\t: %s {" name0 (String.uppercase_ascii name) msg)];
            [%e x] Ppx_deriving_runtime.Format.err_formatter [%e Exp.ident (lid name)]; 
            Ppx_deriving_runtime.prerr_endline "}"] in
  let rec aux = function
      | [%type: int]         -> simple "%d"
      | [%type: int32]
      | [%type: Int32.t]     -> simple "%ldl"
      | [%type: int64]
      | [%type: Int64.t]     -> simple "%LdL"
      | [%type: nativeint]
      | [%type: Nativeint.t] -> simple "%ndn"
      | [%type: float]       -> simple "%F"
      | [%type: bool]        -> simple "%B"
      | [%type: char]        -> simple "%C"
      | [%type: string]
      | [%type: String.t]    -> simple "%S"
      | [%type: [%t? typ] list]  ->
              let f = [%expr fun out v ->
                  Ppx_deriving_runtime.Format.pp_print_string out "[";
                  ignore (List.fold_left (fun sep v ->
                      if sep then Ppx_deriving_runtime.Format.fprintf out "; ";
                        [%e aux typ] out v; true) false v);
                  Ppx_deriving_runtime.Format.pp_print_string out "]"] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out [%e str "%a%!"] [%e f]]
      | [%type: [%t? typ] array] ->
              let f = [%expr fun out v ->
                  Ppx_deriving_runtime.Format.pp_print_string out "[|";
                  ignore (Array.fold_left (fun sep v ->
                      if sep then Ppx_deriving_runtime.Format.fprintf out ", ";
                        [%e aux typ] out v; true) false v);
                  Ppx_deriving_runtime.Format.pp_print_string out "|]"] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out [%e str "%a%!"] [%e f]]
      | [%type: [%t? typ] option] ->
            let f = [%expr fun out v ->
                match v with
                    | None -> Ppx_deriving_runtime.Format.pp_print_string out "None"
                    | Some v -> (Ppx_deriving_runtime.Format.pp_print_string out "Some ";
                                 [%e aux typ] out v;)] in
        [%expr fun out -> Ppx_deriving_runtime.Format.fprintf out [%e str "%a%!"] [%e f]]
      | _ -> assert false in
    format (aux pld_type)

let get_parse_fun { pld_name = { txt = name; loc }; pld_type; pld_attributes } =
    let wrap f =
        [%expr fun v -> try [%e f] v with _ -> raise (Invalid_argument v)] in
    let rec aux = function
        | [%type: int]         -> wrap [%expr Ppx_deriving_runtime.int_of_string]
        | [%type: int32]
        | [%type: Int32.t]     -> wrap [%expr Ppx_deriving_runtime.Int32.of_string]
        | [%type: int64]
        | [%type: Int64.t]     -> wrap [%expr Ppx_deriving_runtime.Int64.of_string]
        | [%type: nativeint]
        | [%type: Nativeint.t] -> wrap [%expr Ppx_deriving_runtime.Nativeint.of_string]
        | [%type: float]       -> wrap [%expr Ppx_deriving_runtime.float_of_string]
        | [%type: bool]        -> wrap [%expr Ppx_deriving_runtime.bool_of_string]
        | [%type: char]        -> wrap [%expr fun s -> if Ppx_deriving_runtime.String.length s = 1
                                            then s.[0] else raise Invalid_argument s]
        | [%type: string]
        | [%type: String.t]    -> wrap [%expr fun s -> s]
        | [%type: [%t? typ] list]  ->
                let parse_comma_sep = [%expr fun v ->
                        let v = Ppx_deriving_runtime.String.split_on_char ',' v in
                        Ppx_deriving_runtime.List.map (fun v -> [%e aux typ] (Ppx_deriving_runtime.String.trim v)) v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] array] ->
                let parse_comma_sep = [%expr fun v ->
                        let v = Ppx_deriving_runtime.String.split_on_char ',' v in
                        let v = Ppx_deriving_runtime.List.map (fun v -> [%e aux typ] (Ppx_deriving_runtime.String.trim v)) v in
                        Ppx_deriving_runtime.Array.of_list v] in
                wrap parse_comma_sep
        | [%type: [%t? typ] option] -> wrap [%expr fun v -> Some ([%e aux typ] v)]
        | _ -> assert false in
    aux pld_type

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  (* let arg_default = pvar "default" in *)
  let msg0 = [%expr Ppx_deriving_runtime.prerr_endline "Options:"] in
  let perrfun_name = Ppx_deriving.mangle_type_decl (`PrefixSuffix (deriver, "perr")) type_decl in
  let argparse_name = Ppx_deriving.mangle_type_decl (`Prefix deriver) type_decl in
  let quoter = Ppx_deriving.create_quoter () in
  let error_cases = 
        [Exp.case [%pat? arg :: rest] ~guard:[%expr is_option arg]
            [%expr Ppx_deriving_runtime.Printf.eprintf "PARSE ERROR: Option without required argument: \"%s\"\n" arg;
            [%e evar perrfun_name] default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? arg :: rest] ~guard:[%expr arg.[0] = '-']
            [%expr Ppx_deriving_runtime.Printf.eprintf "PARSE ERROR: Invalid option: \"%s\"\n" arg;
            [%e evar perrfun_name] default; Ppx_deriving_runtime.exit 2];
        Exp.case [%pat? rest] [%expr (cfg, rest)]] in
  let init_cases =
        [Exp.case (pnil ()) [%expr (cfg, [])];
        Exp.case [%pat? "--" :: rest] [%expr (cfg, rest)]] in
  match type_decl.ptype_kind with
  | Ptype_record labels ->
    let creator, cases, options =
        List.fold_right (fun ({ pld_name = { txt = name; loc }; pld_type; pld_attributes } as fld) (msgs, cases, options) ->
          let option = "-" ^ (string_replace '_' '-' name) in
          let msg0 = match attr_help pld_attributes with
              | Some msg -> msg
              | None -> "no message" in
          let msg = perr_msg fld option name msg0 in
          let parse_fun = get_parse_fun fld in
          let expr = Exp.record [lid name, [%expr [%e parse_fun] i]] (Some [%expr cfg]) in
          let expr = [%expr aux [%e expr] rest] in
          let case = Exp.case [%pat? [%p pstr option] :: i :: rest] ~guard:[%expr not (is_option i)] expr in
          msg :: msgs, case :: cases, str option :: options) labels ([], error_cases, []) in
    let cases = init_cases @ cases in
    let fields = List.map (fun { pld_name = { txt }} -> (txt, pvar txt)) labels in
    let creator = Exp.fun_ Label.nolabel None (precord fields) (sequence (msg0 :: creator)) in
    let argparse0 = [%expr fun cfg args -> try [%e Exp.match_ (evar "args") cases]
        with Invalid_argument s ->
            Ppx_deriving_runtime.Printf.eprintf
                "PARSE ERROR: Invalid argument for keyword option \"%s\": \"%s\"\n"
                (Ppx_deriving_runtime.List.hd args) s; Ppx_deriving_runtime.exit 2] in
    let argparse = [%expr fun default args ->
            let is_option o = Ppx_deriving_runtime.List.mem o [%e list options] in
            let rec aux = [%e argparse0] in
            let cfg, rest = aux default (Ppx_deriving_runtime.List.tl (Ppx_deriving_runtime.Array.to_list args)) in
            cfg, Ppx_deriving_runtime.Array.of_list rest] in
    [Vb.mk (pvar perrfun_name) (Ppx_deriving.sanitize ~quoter creator);
     Vb.mk (pvar argparse_name) (Ppx_deriving.sanitize ~quoter argparse)]
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
