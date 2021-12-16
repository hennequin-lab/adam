open Owl

(** minimize a differentiable function using ADAM
    @param eta learning rate (default=0.002)
    @param epsilon (default=10E-8)
    @param beta1 (default=0.9)
    @param beta2 (default=0.999) *)
val min
  :  ?eta:[ `constant of float | `of_iter of int -> float ]
  -> ?epsilon:float
  -> ?beta1:float
  -> ?beta2:float
  -> ?lb:Mat.mat
  -> ?ub:Mat.mat
  -> ?clip:float
  -> stop:(int -> 'a -> bool)
  -> (int -> Mat.mat -> Mat.mat -> 'a)
  -> Mat.mat
  -> 'a

module Sgd : module type of Sgd
