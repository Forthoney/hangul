let lines instrm =
  let rec next () =
    try
      let line = input_line instrm in
      Seq.Cons (line, next)
    with End_of_file -> Nil
  in
  next

let input_files = ref []
let ignore = ref true
let filter = ref ""
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("--ignore", Arg.Set ignore, "Ignore non-Hangul characters.");
    ( "--strict",
      Arg.Clear ignore,
      "Fail when a non-Hangul character is detected. Set to false by default" );
    ( "--only",
      Arg.Set_string filter,
      "Only output [choseong | jungseong | jongseong]" );
  ]

let () = Arg.parse speclist anon_fun "hangul [FLAGS...] [<FILE>] ..."

let () =
  let instrm =
    match !input_files with
    | [] -> stdin
    | [ "-" ] -> stdin
    | [ f ] -> open_in f
    | _ -> failwith "Multiple target files"
  in
  let f =
    match !filter with
    | "" -> Hangul.Decompose.decompose ~ignore:!ignore
    | "choseong" -> Hangul.Decompose.extract_choseong ~ignore:!ignore
    | "jungseong" -> Hangul.Decompose.extract_jungseong ~ignore:!ignore
    | "jongseong" -> Hangul.Decompose.extract_jongseong ~ignore:!ignore
    | otherwise -> failwith ("Invalid filter: " ^ otherwise)
  in
  lines instrm |> Seq.map f |> Seq.iter print_endline
