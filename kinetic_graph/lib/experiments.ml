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

(* TODO: It works, so abstract and break up. *)
let test_plot () =
  let rig_prox =
    Diffusion.ach_2D 0.
    |> Rig.build @@ Alpha7.make ~on_multi:1. ~desens_div:4. () in
  let rig_dist =
    Diffusion.(ach_2D (1.1 *^. -.6.))
    |> Rig.build @@ Alpha7.make ~on_multi:1. ~desens_div:4. () in
  let data_prox = rig_prox |> Rig.run |> Map.map ~f:mat_of_1d_list in
  let data_dist = rig_dist |> Rig.run |> Map.map ~f:mat_of_1d_list in
  (* Time begins as one dt in, so add t0. *)
  let time = rig_prox.time |> List.cons 0. |> mat_of_1d_list in
  let h = Plot.create "alpha6_prox_distal.png" in
  set_plot_defaults h;
  Plot.set_title h "Alpha6";
  Plot.set_xlabel h "Time (ms)";
  Plot.set_ylabel h "Probability";
  Plot.(plot ~h
          ~spec:[ RGB (255, 0, 0);  LineStyle 1 ]
          time (open_prob data_prox));
  Plot.(plot ~h
          ~spec:[ RGB (0, 0, 255);  LineStyle 1 ]
          time (open_prob data_dist));
  Plot.legend_on h [| "Proximal"; "Distal" |];
  Plot.output h
