open! Core
open! Lwt.Syntax
open! Lwt.Infix
open! Plplot

let plot ~filename ~f ~x_lower ~x_upper ~y_lower ~y_upper ~x_label ~y_label ~title =
  let n = 100 in
  let delta = (x_upper -. x_lower) /. (float n) in
  let xs = Array.init n ~f:(fun x -> x_lower +. (float x *. delta)) in
  let ys = Array.init n ~f:(fun x -> f (x_lower +. (float x *. delta))) in
  plscol0 1 0 0 0;
  plscolbg 255 255 255;
  plsdev "svg";
  plsfnam filename;
  plinit ();
  plenv x_lower x_upper y_lower y_upper 0 0;
  pllab x_label y_label title;
  plline xs ys;
  plend ()

let plot_mesh ~filename ~f ~x_lower ~x_upper ~y_lower ~y_upper ~x_label ~y_label ~z_label =
  let n = 20 in
  let x_delta = (x_upper -. x_lower) /. (float n) in
  let y_delta = (y_upper -. y_lower) /. (float n) in
  let xs = Array.init n ~f:(fun x -> x_lower +. (float x *. x_delta)) in
  let ys = Array.init n ~f:(fun x -> y_lower +. (float x *. y_delta)) in
  let zs = Array.make_matrix ~dimx:n ~dimy:n 0.0 in
  let () =
    for x = 0 to n - 1 do
      for y = 0 to n - 1 do
        zs.(x).(y) <- f
          (x_lower +. (float x *. x_delta))
          (y_lower +. (float y *. y_delta))
      done
    done
  in
  let z_upper, z_lower = plMinMax2dGrid zs in
  plscol0 1 0 0 0;
  plscolbg 255 255 255;
  plsdev "svg";
  plsfnam filename;
  plinit ();
  pladv 0;
  plvpor 0.0 1.0 0.0 0.9;
  plwind (-1.0) 1.0 (-1.0) 1.5;
  let x_rotation_angle = 45.0 in
  let z_rotation_angle = -25.0 in
  plw3d 1.0 1.0 1.2 x_lower x_upper y_lower y_upper z_lower z_upper x_rotation_angle z_rotation_angle;
  plbox3
    "bnstu" x_label 0.0 0
    "bnstu" y_label 0.0 0
    "bcdmnstuv" z_label 0.0 0;
  plmesh xs ys zs [PL_DRAW_LINEXY; PL_MAG_COLOR];
  plend ()
  

let plot_contour ~filename ~f ~x_lower ~x_upper ~y_lower ~y_upper ~levels ~x_label ~y_label ~title =
  let n = 20 in
  let x_delta = (x_upper -. x_lower) /. (float n) in
  let y_delta = (y_upper -. y_lower) /. (float n) in
  let _xs = Array.init n ~f:(fun x -> x_lower +. (float x *. x_delta)) in
  let _ys = Array.init n ~f:(fun x -> y_lower +. (float x *. y_delta)) in
  let zs = Array.make_matrix ~dimx:n ~dimy:n 0.0 in
  let () =
    for x = 0 to n - 1 do
      for y = 0 to n - 1 do
        zs.(x).(y) <- f
          (x_lower +. (float x *. x_delta))
          (y_lower +. (float y *. y_delta))
      done
    done
  in
  plscol0 1 0 0 0;
  plscolbg 255 255 255;
  plsdev "svg";
  plsfnam filename;
  plinit ();
  pl_setcontlabelformat 4 3;
  pl_setcontlabelparam 0.006 0.3 0.1 1;
  plenv x_lower x_upper y_lower y_upper 0 0;
  plaxes 0.0 0.0 "g" 0.0 0 "g" 0.0 0;
  pllab x_label y_label title;
  plset_pltr pltr0;
  plcont zs 1 n 1 n levels;
  plunset_pltr ();
  plend ()
