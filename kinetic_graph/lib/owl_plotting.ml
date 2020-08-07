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

let run_to_matrix = Rig.run |> Fn.compose Map.map ~f:mat_of_1d_list

let set_plot_defaults h =
  Plot.set_foreground_color h 0 0 0;
  Plot.set_background_color h 255 255 255;
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.

let prox_vs_distal pth title diff builder =
  let rig_prox : Rig.t = 0. |> diff |> builder in
  let rig_dist = Diffusion.(1.1 *^. -.6. |> diff) |> builder in
  let time = rig_prox.time |> List.cons 0. |> mat_of_1d_list in
  let prox = rig_prox |> run_to_matrix |> open_prob in
  let dist = rig_dist |> run_to_matrix |> open_prob in
  let h = Plot.create pth in
  set_plot_defaults h;
  Plot.set_title h title;
  Plot.set_xlabel h "Time (ms)";
  Plot.set_ylabel h "Probability";
  Plot.(plot ~h
          ~spec:[ RGB (255, 0, 0);  LineStyle 1 ]
          time prox);
  Plot.(plot ~h
          ~spec:[ RGB (0, 0, 255);  LineStyle 1 ]
          time dist);
  Plot.legend_on h [| "Proximal"; "Distal" |];
  Plot.output h

let alpha6_test () =
  Rig.build @@ Alpha7.make ~on_multi:1. ~desens_div:4. ()
  |> prox_vs_distal "alpha6_prox_distal.png" "Alpha6" Diffusion.ach_2D
