open Base
open Owl

let min
    ?(eta = `constant 0.001)
    ?(epsilon = 1E-8)
    ?(beta1 = 0.9)
    ?(beta2 = 0.999)
    ?lb
    ?ub
    ?clip
    ~stop
    f_df
    x
  =
  let n = Mat.numel x in
  let g = Mat.zeros 1 n in
  let g2 = Mat.zeros 1 n in
  let m = Mat.zeros 1 n in
  let v = Mat.zeros 1 n in
  let mhat = Mat.zeros 1 n in
  let vhat = Mat.zeros 1 n in
  let f_df =
    match clip with
    | None -> f_df
    | Some alpha ->
      fun x g ->
        let cost = f_df x g in
        let g_norm = Mat.l2norm' g in
        if Float.(g_norm > alpha)
        then (
          Stdio.printf "CLIPPING GRADIENT\n%!";
          Mat.mul_scalar_ ~out:g g Float.(alpha / g_norm));
        cost
  in
  let axpy ~alpha x y =
    let n = Mat.numel x in
    let x = Bigarray.array1_of_genarray (Mat.flatten x) in
    let y = Bigarray.array1_of_genarray (Mat.flatten y) in
    Owl_cblas_basic.axpy n alpha x 1 y 1
  in
  let rec iterate t cost beta1_t beta2_t =
    let eta =
      match eta with
      | `constant eta -> eta
      | `of_iter f -> f t
    in
    Mat.sqr_ ~out:g2 g;
    (* update m *)
    Mat.mul_scalar_ ~out:m m beta1;
    axpy ~alpha:(1. -. beta1) g m;
    (* update v *)
    Mat.mul_scalar_ ~out:v v beta2;
    axpy ~alpha:(1. -. beta2) g2 v;
    (* bias correction factors *)
    Mat.fill mhat 0.;
    axpy ~alpha:Float.(1. / (1. - beta1_t)) m mhat;
    Mat.fill vhat 0.;
    axpy ~alpha:Float.(1. / (1. - beta2_t)) v vhat;
    (* compute update *)
    Mat.sqrt_ vhat;
    Mat.add_scalar_ vhat epsilon;
    Mat.div_ ~out:mhat mhat vhat;
    axpy ~alpha:(-.eta) mhat x;
    (* clip at upper and lower bounds *)
    (match lb with
    | None -> ()
    | Some lb ->
      for i = 0 to n - 1 do
        Mat.(set x 0 i Float.(max (get lb 0 i) (get x 0 i)))
      done);
    (match ub with
    | None -> ()
    | Some ub ->
      for i = 0 to n - 1 do
        Mat.(set x 0 i Float.(min (get ub 0 i) (get x 0 i)))
      done);
    if not (stop t cost)
    then (
      let cost = f_df x g in
      iterate (t + 1) cost Float.(beta1 * beta1_t) Float.(beta2 * beta2_t))
    else cost
  in
  iterate 1 (f_df x g) beta1 beta2


module Sgd = Sgd
