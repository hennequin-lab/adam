open Base
open Owl

let min ?(eta = `constant 0.001) ?lb ?ub ~stop f_df x =
  let n = Mat.numel x in
  let g = Mat.zeros 1 n in
  let axpy ~alpha x y =
    let n = Mat.numel x in
    let x = Bigarray.array1_of_genarray (Mat.flatten x) in
    let y = Bigarray.array1_of_genarray (Mat.flatten y) in
    Owl_cblas_basic.axpy n alpha x 1 y 1
  in
  let rec iterate t cost =
    let eta =
      match eta with
      | `constant eta -> eta
      | `of_iter f -> f t
    in
    axpy ~alpha:(-.eta) g x;
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
