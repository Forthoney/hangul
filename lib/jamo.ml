let byte_size = 3

module type CompatTable = sig
  val tbl : string
  val start : Uchar.t
  val fin : Uchar.t
end

module type Cluster = sig
  type t

  val of_uchar : Uchar.t -> t option
  val to_compat : t -> Uchar.t
end

module Make (Compat : CompatTable) : Cluster = struct
  type t = Uchar.t

  let of_uchar c = if c >= Compat.start && c <= Compat.fin then Some c else None

  let to_compat c =
    let idx = Uchar.to_int c - Uchar.to_int Compat.start in
    String.get_utf_8_uchar Compat.tbl (idx * byte_size)
    |> Uchar.utf_decode_uchar
end

module ChoseongTbl : CompatTable = struct
  let tbl = "ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ"
  let start = Uchar.of_int 0x1100
  let fin = Uchar.of_int 0x1112
end

module JungseongTbl : CompatTable = struct
  let tbl = "ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ"
  let start = Uchar.of_int 0x1161
  let fin = Uchar.of_int 0x1176
end

module JongseongTbl : CompatTable = struct
  let tbl = "ㄱㄲㄳㄴㄵㄶㄷㄹㄺㄻㄼㄽㄾㄿㅀㅁㅂㅄㅅㅆㅇㅈㅊㅋㅌㅍㅎ"

  let start = Uchar.of_int 0x11A8
  let fin = Uchar.of_int 0x11C2
end

module Choseong = Make (ChoseongTbl)
module Jungseong = Make (JungseongTbl)
module Jongseong = Make (JongseongTbl)

type t = Cho of Choseong.t | Jung of Jungseong.t | Jong of Jongseong.t

let of_uchar c =
  match Choseong.of_uchar c with
  | Some c -> Some (Cho c)
  | None -> (
      match Jungseong.of_uchar c with
      | Some c -> Some (Jung c)
      | None -> (
          match Jongseong.of_uchar c with
          | Some c -> Some (Jong c)
          | None -> None))

let to_compat = function
  | Cho c -> Choseong.to_compat c
  | Jung c -> Jungseong.to_compat c
  | Jong c -> Jongseong.to_compat c
