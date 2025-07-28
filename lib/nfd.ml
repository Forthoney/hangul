let get_code s =
  String.get_utf_8_uchar s 0 |> Uchar.utf_decode_uchar |> Uchar.to_int

let start_choseong = get_code "ᄀ"

(* let start_jungseong = get_code "ᅡ" *)
(* let start_jongseong = get_code "ᆨ" *)
let end_choseong = get_code "ᄒ"
