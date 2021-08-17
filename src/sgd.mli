open Owl

(** minimize a differentiable function using ADAM
    @param eta learning rate (default=0.002)
    *)
val min
  :  ?eta:[ `constant of float | `of_iter of int -> float ]
  -> ?lb:Mat.mat
  -> ?ub:Mat.mat
  -> stop:(int -> 'a -> bool)
  -> (Mat.mat -> Mat.mat -> 'a)
  -> Mat.mat
  -> 'a
