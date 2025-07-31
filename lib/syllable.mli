type t = {
  initial : Jamo.Choseong.t;
  medial : Jamo.Jungseong.t;
  final : Jamo.Jongseong.t option;
}

val to_uchar : t -> Uchar.t
val of_uchar : Uchar.t -> t option
