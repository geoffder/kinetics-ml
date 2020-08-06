open Base
open Owl
open Owl_plplot

(* Locals *)
open Graph_builds
open Model

let open_prob m =
  Map.keys m
  |> List.filter ~f:(fun s -> String.contains s '*')
  |> List.map ~f:(Map.find_exn m)
  |> List.reduce_exn ~f:Mat.( + )

let mat_of_1d_list =
  Fn.compose (fun a -> Mat.of_array a (Array.length a) 1) List.to_array

let set_plot_defaults h =
  Plot.set_foreground_color h 0 0 0;
  Plot.set_background_color h 255 255 255;
  Plot.set_font_size h 10.;
  Plot.set_pen_size h 3.

let test_plot () =
  let rig =
    Diffusion.ach_2D 0.
    |> Rig.build @@ Alpha7.make ~on_multi:1. ~desens_div:1. () in
  let data = rig |> Rig.run |> Map.map ~f:mat_of_1d_list in
  (* Time begins as one dt in, so add t0. *)
  let time = rig.time |> List.cons 0. |> mat_of_1d_list in
  let h = Plot.create "alpha7_r0.png" in
  set_plot_defaults h;
  Plot.set_title h "Alpha7, radius = 0";
  Plot.set_xlabel h "Time (ms)";
  Plot.set_ylabel h "Probability";
  Plot.(plot ~h
          ~spec:[ RGB (255,0,0);  LineStyle 1 ]
          time (open_prob data));
  Plot.output h
