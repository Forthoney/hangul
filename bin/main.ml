let extract_choseong s =
  let s = Uunf_string.normalize_utf_8 `NFD s in
  let n = String.length s in
  let rec loop buf loc =
    if loc >= n then Buffer.contents buf
    else
      let () =
        match
          String.get_utf_8_uchar s loc
          |> Uchar.utf_decode_uchar |> Hangul.Jamo.of_uchar
        with
        | Some (Cho c) ->
            Buffer.add_utf_8_uchar buf (Hangul.Jamo.Choseong.to_compat c)
        | _ -> ()
      in
      loop buf (loc + Hangul.Jamo.byte_size)
  in
  loop (Buffer.create (n * Hangul.Jamo.byte_size)) 0

let () = read_line () |> extract_choseong |> print_endline
