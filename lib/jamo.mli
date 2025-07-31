val byte_size : int

(* Jamo subcluster *)
module type Cluster = sig
  type t

  (* try to create a member of this cluster from a Uchar *)
  val of_uchar : Uchar.t -> t option
  val to_uchar : t -> Uchar.t
  val nth : int -> t

  (* convert into an equivalent Hangul Compatibility Uchar *)
  val to_compat : t -> Uchar.t
end

module Choseong : Cluster
module Jungseong : Cluster
module Jongseong : Cluster

type t = Cho of Choseong.t | Jung of Jungseong.t | Jong of Jongseong.t

val of_uchar : Uchar.t -> t option
val to_uchar : t -> Uchar.t
val to_compat : t -> Uchar.t
