type t = Uchar.t

let compose (initial, medial, final) =
  let final = match final with Some c -> Uchar.to_int c | None -> 0 in
  Uchar.of_int
    ((Uchar.to_int initial * 588) + (Uchar.to_int medial * 28) + final + 44032)
