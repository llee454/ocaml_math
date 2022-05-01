open! Core
open! Lwt.Syntax
open! Lwt.Infix
open! Gsl

let main =
  Lwt_main.run @@
    let* () = Lwt_io.printlf "log1p: %f" (ocaml_log1p 0.4) in
    let Linear_fit.{c0; c1} = Linear_fit.f [|0.0; 1.0; 2.0|] [|5.0; 10.0; 20.0|] in 
    let* () = Lwt_io.printlf "linear regression: %f + %f x" c0 c1 in
    let Integrate.{ out; err; _ } = Integrate.f (fun x -> Float.log1p x) 0.1 0.5 in
    let* () = Lwt_io.printlf "integration: result %f err %f" out err in
    Lwt.return_unit
