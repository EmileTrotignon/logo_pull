open Gg
open Vg

let line_width = 0.01

let uniform_angles ?(offset = 0.) n =
  let li = List.init n Fun.id in
  List.map
    (fun i ->
      let n = float_of_int n and i = float_of_int i in
      offset +. (Float.two_pi *. i /. n) )
    li

let push s v = s := v :: !s

let pop s =
  match !s with
  | [] ->
      failwith "popping empty stack"
  | v :: s' ->
      s := s' ;
      v

let segment p1 p2 = P.(empty |> sub p1 |> line p2)

let inter_rays ~angle ~n ~factor ~size ~radius p =
  let outside_radius = radius +. size in
  let n' = n * factor in
  let current_angle =
    if factor = 1 then angle
    else
      let n' = float_of_int n' and factor = float_of_int factor in
      -.((Float.two_pi /. (n' *. factor)) -. angle)
  in
  let outside_angles = uniform_angles ~offset:current_angle (n * factor) in
  let inside_angles = uniform_angles ~offset:angle n in
  let outside_points =
    List.map (fun d -> V2.of_polar (V2.v outside_radius d)) outside_angles
  in
  let inside_points =
    List.map (fun d -> V2.of_polar (V2.v radius d)) inside_angles
  in
  let groups =
    let outside_points = ref outside_points in
    List.map
      (fun inside_point ->
        let points = ref [] in
        for _ = 1 to factor do
          push points (pop outside_points)
        done ;
        let points = !points in
        (inside_point, points) )
      inside_points
  in
  let lines =
    List.concat
    @@ List.map
         (fun (p_in, ps_out) ->
           List.map (fun p_out -> segment p_in p_out) ps_out )
         groups
  in
  let paths = lines in
  let path = P.(List.fold_left append p paths) in
  (path, current_angle)

type floor = {factor: int; size: float}

let floors =
  [ {factor= 1; size= 0.2}
  ; {factor= 2; size= 0.1}
  ; {factor= 2; size= 0.1}
  ; {factor= 2; size= 0.1}
  ; {factor= 2; size= 0.1} ]

let path, _angle, _n, _size =
  List.fold_left
    (fun (path, angle, n, radius) {factor; size} ->
      let path, angle = inter_rays ~angle ~n ~factor ~size ~radius path in
      let n = n * factor in
      let radius = radius +. size in
      (path, angle, n, radius) )
    (P.empty, 0., 8, 0.3) floors

let chip size n_broche size_broche path =
  let path =
    path
    |> P.rect (Box2.v (V2.v (-.(size /. 2.)) (-.(size /. 2.))) (V2.v size size))
  in
  let broches = List.init n_broche Fun.id in
  let broches =
    let left_side y = V2.v (-.size /. 2.) y
    and up_side x = V2.v x (size /. 2.)
    and right_side y = V2.v (size /. 2.) y
    and down_side x = V2.v x (-.size /. 2.) in
    let index i =
      let i = float_of_int i in
      let n = float_of_int n_broche in
      (i *. size /. n) -. (size /. 2.) +. (size /. (2. *. n))
    in
    let broches_left =
      List.map
        (fun i ->
          let start = left_side (index i) in
          let end_ = V2.(start + v (-.size_broche) 0.) in
          (start, end_) )
        broches
    in
    let broches_up =
      List.map
        (fun i ->
          let start = up_side (index i) in
          let end_ = V2.(start + v 0. size_broche) in
          (start, end_) )
        broches
    in
    let broches_right =
      List.map
        (fun i ->
          let start = right_side (index i) in
          let end_ = V2.(start + v size_broche 0.) in
          (start, end_) )
        broches
    in
    let broches_down =
      List.map
        (fun i ->
          let start = down_side (index i) in
          let end_ = V2.(start + v 0. (-.size_broche)) in
          (start, end_) )
        broches
    in
    broches_left @ broches_up @ broches_right @ broches_down
  in
  let broches_lines = List.map (fun (p1, p2) -> segment p1 p2) broches in
  let path = List.fold_left P.append path broches_lines in
  path

let path_chip = P.empty |> chip 0.421 7 0.1

let area = `O {P.o with P.width= line_width}

let area_round = `O {P.o with P.width= line_width; cap= `Round}

let axial_image =
  I.axial
    [(-1., Color.blue); (1.0, Color.red)]
    (P2.v (-1.0) (-1.0)) (P2.v 1.0 1.0)

let white_image = I.const Color.white

let image = I.cut ~area:area_round path white_image

let image = I.blend image (I.cut ~area path_chip white_image)

let image = I.blend image axial_image

let svg_of_usquare i =
  let size = Size2.v 200. 200. in
  let view = Box2.v (V2.v (-1.) (-1.)) (V2.v 2. 2.) in
  try
    let oc = open_out "./output.svg" in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try
      ignore (Vgr.render r (`Image (size, view, i))) ;
      ignore (Vgr.render r `End) ;
      close_out oc
    with e -> close_out oc ; raise e
  with Sys_error e -> prerr_endline e

let () = svg_of_usquare image

type my_adt = TI of int | TF of float

let f_safe = function TI i -> i | TF f -> int_of_float f

type my_tags = TI | TF

let f_unsafe tag n =
  match tag with
  | TI ->
      (Obj.magic n : int)
  | TF ->
      int_of_float (Obj.magic n : float)

type _ my_gadt = TI : int my_gadt | TF : float my_gadt

let f_gadt : type t. t my_gadt -> t -> int =
 fun tag n -> match tag with TI -> n | TF -> int_of_float n
