open Microcaml

let usage = "microcaml <src> [-scoping s] [-call c]"
let src = ref ""
let anon_cnt = ref 0
let scoping = ref Interpreter.Static
let call_method = ref Interpreter.ByValue

let parse_scoping = function
  | "static" -> Interpreter.Static
  | "dynamic" -> Interpreter.Dynamic
  | _ -> failwith ""

let parse_call_method = function
  | "value" -> Interpreter.ByValue
  | "name" -> Interpreter.ByName
  | _ -> failwith ""

let speclist =
  [
    ("src", Arg.String (fun str -> src := str), "Source file");
    ( "-s",
      Arg.Symbol
        ([ "static"; "dynamic" ], fun str -> scoping := parse_scoping str),
      " Scoping method" );
    ( "-c",
      Arg.Symbol
        ([ "value"; "name" ], fun str -> call_method := parse_call_method str),
      " Call method" );
  ]

let () =
  Arg.parse speclist
    (fun str ->
      src := str;
      anon_cnt := !anon_cnt + 1)
    usage;
  if !src = "" || !anon_cnt <> 1 then (
    Arg.usage speclist usage;
    exit 1)
  else ()
;;

try
  let channel = Core.In_channel.create !src in
  let lexbuf = Lexing.from_channel channel in
  let ast = Parser.start Lexer.read lexbuf in
  ignore (Interpreter.start ast { call = !call_method; scoping = !scoping });
  Core.In_channel.close channel
with
| Lexer.LexicalError msg -> prerr_endline ("[SyntaxError] " ^ msg)
| Parser.Error -> prerr_endline "[ParserError]"
| Interpreter.TypeError msg -> prerr_endline ("[TypeError] " ^ msg)
| Interpreter.RuntimeError msg -> prerr_endline ("[RuntimeError] " ^ msg)
