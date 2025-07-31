let lines instrm =
  let rec next () =
    try
      let line = input_line instrm in
      Seq.Cons (line, next)
    with End_of_file -> Nil
  in
  next

let () =
  let instrm =
    match Sys.argv with
    | [| _; "-" |] | [| _ |] -> stdin
    | [| _; file |] -> open_in file
    | _ -> failwith "Unrecognized"
  in
  lines instrm
  |> Seq.map (Hangul.Decompose.decompose ~ignore:true)
  |> Seq.iter print_endline
