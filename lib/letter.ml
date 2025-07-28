let start =
  String.get_utf_8_uchar "가" 0 |> Uchar.utf_decode_uchar |> Uchar.to_int

let fin = String.get_utf_8_uchar "힣" 0 |> Uchar.utf_decode_uchar |> Uchar.to_int

type t = { choseong : int; jungseong : int; jongseong : int option }

let of_uchar s =
  let ord = Uchar.to_int s in
  if ord >= start && ord <= fin then
    let hangul_offset = ord - start in
    let jongseong_idx = hangul_offset mod 28 in
    let jungseong_idx = (hangul_offset - jongseong_idx) / 28 mod 21 in
    let choseong_idx = hangul_offset - (jongseong_idx / 28 / 21) in
    Some
      {
        choseong = choseong_idx;
        jungseong = jungseong_idx;
        jongseong =
          (if jongseong_idx = 0 then None else Some (jongseong_idx - 1));
      }
  else None
