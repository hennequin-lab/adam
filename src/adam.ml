open Base
open Owl

let min
    ?(eta = 0.002)
    ?(epsilon = 10E-8)
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
  let tmp1 = Mat.zeros 1 n in
  let tmp2 = Mat.zeros 1 n in
  let f_df =
    match clip with
    | None -> f_df
    | Some alpha ->
      fun x g ->
        let cost = f_df x g in
        let g_norm = Maths.sqrt (Mat.l2norm_sqr' g) in
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
  let rec iterate t cost =
    Mat.sqr_ ~out:g2 g;
    (* update m *)
    Mat.mul_scalar_ ~out:m m beta1;
    axpy ~alpha:(1. -. beta1) g m;
    (* update v *)
    Mat.mul_scalar_ ~out:v v beta2;
    axpy ~alpha:(1. -. beta2) g2 v;
    (* bias correction factors *)
    Mat.fill mhat 0.;
    axpy ~alpha:Float.(1. / (1. - int_pow beta1 t)) m mhat;
    Mat.fill vhat 0.;
    axpy ~alpha:Float.(1. / (1. - int_pow beta2 t)) v vhat;
    (* compute update *)
    Mat.sqrt_ ~out:tmp1 vhat;
    Mat.add_scalar_ ~out:tmp2 tmp1 epsilon;
    Mat.div_ ~out:tmp1 mhat tmp2;
    axpy ~alpha:(-.eta) tmp1 x;
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
      iterate (t + 1) cost)
    else cost
  in
  iterate 1 (f_df x g)
