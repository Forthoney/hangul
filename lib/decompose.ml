let bufferize seq s =
  let s = Uunf_string.normalize_utf_8 `NFD s in
  let buf = String.length s |> Buffer.create in
  let () = Seq.iter (Buffer.add_utf_8_uchar buf) (seq s) in
  Buffer.contents buf

let decompose_aux ~ignore s =
  let len = String.length s in
  let rec loop loc () =
    if loc >= len then Seq.Nil
    else
      String.get_utf_8_uchar s loc
      |> Uchar.utf_decode_uchar |> Jamo.of_uchar |> unwrap loc
  and unwrap loc converted =
    let next = loc + Jamo.byte_size in
    if ignore then
      match converted with
      | Some c -> Seq.Cons (c, loop next)
      | None -> loop next ()
    else Seq.Cons (Option.get converted, loop next)
  in
  loop 0

let decompose ~ignore =
  Fun.compose (Seq.map Jamo.to_compat) (decompose_aux ~ignore) |> bufferize

let extract_aux ~ignore f =
  Fun.compose (Seq.filter_map f) (decompose_aux ~ignore) |> bufferize

let extract_choseong =
  extract_aux (function
    | Jamo.Cho c -> Some (Jamo.Choseong.to_compat c)
    | _ -> None)

let extract_jungseong =
  extract_aux (function
    | Jamo.Jung c -> Some (Jamo.Jungseong.to_compat c)
    | _ -> None)

let extract_jongseong =
  extract_aux (function
    | Jamo.Jong c -> Some (Jamo.Jongseong.to_compat c)
    | _ -> None)
