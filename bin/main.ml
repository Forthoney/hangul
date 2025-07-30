let lines instrm =
  let rec next () =
    try
      let line = input_line instrm in
      Seq.Cons (line, next)
    with End_of_file -> Nil
  in
  next

let extract_choseong s =
  let s = Uunf_string.normalize_utf_8 `NFD s in
  let n = String.length s in
  let buf = Buffer.create (n * Hangul.Jamo.byte_size) in
  let rec loop loc =
    if loc >= n then Buffer.contents buf
    else
      let converted =
        String.get_utf_8_uchar s loc
        |> Uchar.utf_decode_uchar |> Hangul.Jamo.of_uchar
      in
      let () =
        match converted with
        | Some (Cho c) ->
            Buffer.add_utf_8_uchar buf (Hangul.Jamo.Choseong.to_compat c)
        | _ -> ()
      in
      loop (loc + Hangul.Jamo.byte_size)
  in
  loop 0

let () =
  let instrm =
    match Sys.argv with
    | [| _; "-" |] | [| _ |] -> stdin
    | [| _; file |] -> open_in file
    | _ -> failwith "Unrecognized"
  in
  lines instrm |> Seq.map extract_choseong |> Seq.iter print_endline
