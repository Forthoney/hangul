let decompose_aux ~ignore s =
  let s = Uunf_string.normalize_utf_8 `NFD s in
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

let decompose ~ignore s =
  let len = String.length s in
  (* Worst case scenario, a syllable with cho-, jung-, jongseong will undergo a 3x size blowup.
     Not sure if this much allocation is needed though *)
  let buf = Buffer.create (len * Jamo.byte_size) in
  let () =
    Seq.iter
      (Fun.compose (Buffer.add_utf_8_uchar buf) Jamo.to_compat)
      (decompose_aux ~ignore s)
  in
  Buffer.contents buf

let extract_aux ~ignore f =
  Fun.compose (Seq.filter_map f) (decompose_aux ~ignore)

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
