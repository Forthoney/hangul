type t = {
  initial : Jamo.Choseong.t;
  medial : Jamo.Jungseong.t;
  final : Jamo.Jongseong.t option;
}

let to_uchar { initial; medial; final } =
  let final =
    Option.map (Fun.compose Uchar.to_int Jamo.Jongseong.to_uchar) final
    |> Option.value ~default:0
  in
  let initial = Jamo.Choseong.to_uchar initial |> Uchar.to_int in
  let medial = Jamo.Jungseong.to_uchar medial |> Uchar.to_int in
  Uchar.of_int ((initial * 588) + (medial * 28) + final + 44032)

let of_uchar c =
  let c = Uchar.to_int c in
  let start, fin = (0xAC00, 0xD7A3) in
  if c >= start && c <= fin then
    let c = c - start in
    let initial = Jamo.Choseong.nth (c / 21 * 28) in
    let medial = Jamo.Jungseong.nth (c mod (21 * 28) / 28) in
    let final =
      match c mod 28 with 0 -> None | n -> Some (Jamo.Jongseong.nth n)
    in
    Some { initial; medial; final }
  else None
