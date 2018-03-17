
type activation = Relu | Sigmoid | Tanh | Silu | Softmax

let parse_activation = function 
    "relu" -> Relu | "sigmoid" -> Sigmoid | "tanh" -> Tanh | "silu" -> Silu | "softmax" -> Softmax
    | s -> raise (Invalid_argument s)

let show_activation = function
    | Relu -> "relu"
    | Sigmoid -> "sigmoid"
    | Tanh -> "tanh"
    | Silu -> "silu"
    | Softmax -> "softmax"

type t = {
    src_vocab_size : int   [@help "the source vocabulary size"];
    tgt_vocab_size : int   [@help "the target vocabulary size"];
    num_units      : int   [@help "the number of units"];
    nheads         : int   [@help "the number of multi-head attention"];
    nlayers        : int   [@help "the number of layers"];
    use_dropout    : bool  [@help "true if use dropout"];
    dropout_rate   : float option [@help "dropout rate"];
    test : int list option [@help "this is an test argument"];
    test2 : int option list;
    activation : activation [@help "activation function in feed forward layers"]
                        [@print show_activation] [@parse parse_activation];
    activation2 : activation [@help "activation function in feed forward layers"]
                        [@parse parse_activation];
} [@@deriving argparse]

let perr_config c =
    Printf.eprintf "Options:\n";
  Printf.eprintf "  -src-vocab-size SRC_VOCAB_SIZE\t: %s [%i]\n" "the source vocabulary size" c.src_vocab_size;
  Printf.eprintf "  -tgt-vocab-size TGT_VOCAB_SIZE\t: %s [%i]\n" "the target vocabulary size" c.tgt_vocab_size;
  Printf.eprintf "  -num-units NUM_UNITS\t: %s [%i]\n" "the number of units" c.num_units;
  Printf.eprintf "  -nheads NHEADS\t: %s [%i]\n" "the number of multi-head attention" c.nheads;
  Printf.eprintf "  -nlayers NLAYERS\t: %s [%i]\n" "the number of layers" c.nlayers;
  Printf.eprintf "  -use-dropout\t: %s [%B]\n" "true if use dropout" c.use_dropout
  (* Printf.eprintf "  -dropout-rate DROPOUT_RATE\t: %s [%f]\n" "dropout rate" c.dropout_rate *)


let default = {
    src_vocab_size = 0;
    tgt_vocab_size = 0;
    num_units = 512;
    nheads = 8;
    nlayers = 6;
    use_dropout = true;
    dropout_rate = Some 0.1;
    test = Some [1;2;3];
    test2 = [Some 1;Some 2;Some 3; None];
    activation = Relu;
    activation2 = Relu;
}

let parse default argv = 
    let options = ["-src-vocab-size"; "-tgt-vocab-size"; "-num-units"; "-nheads"; "-nlayers"; "-use-dropout"; "-dropout-rate"] in
    let is_option o = List.mem o options in
    let rec aux cfg args = try (match args with
        | [] -> (cfg, [])
        | "--" :: rest -> (cfg, rest)
        | "-src-vocab-size" :: i :: rest when not (is_option i)
            -> aux {cfg with src_vocab_size = (fun v -> try int_of_string v with _ -> raise (Invalid_argument v)) i} rest
        | "-tgt-vocab-size" :: i :: rest when not (is_option i)
            -> aux {cfg with tgt_vocab_size = (fun v -> try int_of_string v with _ -> raise (Invalid_argument v)) i} rest
        | "-num-units" :: i :: rest when not (is_option i)
            -> aux {cfg with num_units = (fun v -> try int_of_string v with _ -> raise (Invalid_argument v)) i} rest
        | "-nheads" :: i :: rest when not (is_option i)
            -> aux {cfg with nheads = (fun v -> try int_of_string v with _ -> raise (Invalid_argument v)) i} rest
        | "-nlayers" :: i :: rest when not (is_option i)
            -> aux {cfg with nlayers = (fun v -> try int_of_string v with _ -> raise (Invalid_argument v)) i} rest
        | "-use-dropout" :: rest
            -> aux {cfg with use_dropout = false} rest
        | "-dropout-rate" :: i :: rest when not (is_option i)
            -> aux {cfg with dropout_rate = (fun v -> try Some (float_of_string v) with _ -> raise (Invalid_argument v)) i} rest
        | arg :: rest when is_option arg ->
            Printf.eprintf "PARSE ERROR: Option without required argument: \"%s\"\n" arg;
            perr_config default; exit 2
        | arg :: rest when arg.[0] = '-' ->
            Printf.eprintf "PARSE ERROR: Invalid option: \"%s\"\n" arg;
            perr_config default; exit 2
        | rest -> (cfg, rest))
    with Invalid_argument s ->
        Printf.eprintf "PARSE ERROR: Invalid argument for keyword option \"%s\": \"%s\"\n" (List.hd args) s; exit 2 in
    let cfg, rest = aux default (List.tl (Array.to_list argv)) in
    cfg, Array.of_list rest

let () =
    let cfg, rest = argparse default Sys.argv in
    argparse_perr cfg;
    Array.iter print_endline rest

