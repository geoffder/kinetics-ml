open Base

type graph_spec = { names : string list
                  ; wiring : (string * float * int) list
                  }

module type BuildSpec = sig
  val tstop : float
  val dt : float
  val spec : graph_spec
end

module Gaba = struct
  (* Petrini et al., 2003
   * Rate Constants:
   *   k1 = k2 = 8       [1 / (mM * ms)]
   *   k-1 = k-2 = 0.12  [1 / ms]
   *   beta1 = 0.04      [1 / ms]
   *   alpha1 = 0.2      [1 / ms]
   *   d1 = 0.013        [1 / ms]
   *   r1 = 0.0013       [1 / ms]
   *   beta2 = 3.45      [1 / ms]
   *   alpha2 = 1        [1 / ms]
   *   d2 = 1.45         [1 / ms]
   *   r2 = 0.1          [1 / ms] *)

  let make ?tstop:(tstop=25.0) ?dt:(dt=0.001) () =
    (module struct
      let tstop = tstop
      let dt = dt
      let spec : graph_spec =
        { names = [ "R"; "AR"; "AR*"; "AD"; "A2R"; "A2R*"; "A2D" ]
        ; wiring =
            [ ("R>AR",     8.0,        2)  (* kon * 2  [agonist bind]   *)
            ; ("AR>R",     0.12,       0)  (* koff * 1 [agonist unbind] *)
            ; ("AR>AD",    0.013,      0)  (* d1       [deactivate]     *)
            ; ("AD>AR",    0.0013,     0)  (* r1       [reactivate]     *)
            ; ("AR>AR*",   0.04,       0)  (* beta1    [channel open]   *)
            ; ("AR*>AR",   0.2,        0)  (* alpha1   [channel close]  *)
            ; ("AR>A2R",   8.0,        1)  (* kon * 1  [agonist bind]   *)
            ; ("A2R>AR",   0.12 *. 2.0, 0)  (* koff * 2 [agonist unbind] *)
            ; ("A2R>A2D",  1.45,       0)  (* d2       [deactivate]     *)
            ; ("A2D>A2R",  0.1,        0)  (* r2       [reactivate]     *)
            ; ("A2R>A2R*", 3.45,       0)  (* beta2    [channel open]   *)
            ; ("A2R*>A2R", 1.0,        0)  (* alpha2   [channel close]  *)
            ]
        }
    end : BuildSpec)
end

module Alpha7 = struct
  (* Coggan et al., 2005
   * Rate Constants:
   *    k1 = k2 = 4.1e7   [1 / (M * s)]
   *    k-1 = k-2 = 82.2  [1 / s]
   *    des = 879         [1 / s]
   *    res = 26          [1 / s]
   *    beta = 86.2       [1 / s]
   *    alpha = 7641      [1 / s]
   * kon rates adjusted by 1e6 to [1 / (mM * ms)].
   * [1 / s] rates adjusted by 1e3 to [1 / ms]. *)
  let make
      ?tstop:(tstop=25.0)
      ?dt:(dt=0.001)
      ?on_multi:(on_multi=1.)
      ?desens_div:(desens_div=1.)
      ()
    =
    (module struct
      let tstop = tstop
      let dt = dt
      let kon = 41. *. on_multi
      let koff = 0.0822
      let des = 0.879 /. desens_div
      let spec : graph_spec =
        { names = [ "R"; "AR"; "A2R"; "A2D"; "A2R*" ]
        ; wiring =
            [ ("R>AR",     kon,        2)  (* k+1   [bind]        *)
            ; ("AR>R",     koff,       0)  (* k-1   [unbind]      *)
            ; ("AR>A2R",   kon,        1)  (* k+2   [bind]        *)
            ; ("A2R>AR",   koff *. 2.0, 0)  (* k-2   [unbind]      *)
            ; ("A2R>A2D",  des,        0)  (* des   [desensitize] *)
            ; ("A2D>A2R",  0.026,      0)  (* res   [resensitize] *)
            ; ("A2R>A2R*", 0.0862,     0)  (* beta  [opening]     *)
            ; ("A2R*>A2R", 7.641,      0)  (* alpha [closing]     *)
            ]
        }
    end : BuildSpec)
end

module Alpha3 = struct
  (* Coggan et al., 2005
   * Rate Constants:
   *    k1 = k2 = 2.3e6   [1 / (M * s)]
   *    k-1 = k-2 = 84    [1 / s]
   *    beta = 513        [1 / s]
   *    alpha = 1000      [1 / s]
   * kon rates adjusted by 1e6 to [1 / (mM * ms)].
   * [1 / s] rates adjusted by 1e3 to [1 / ms]. *)
  let make ?tstop:(tstop=25.0) ?dt:(dt=0.001) () =
    (module struct
      let tstop = tstop
      let dt = dt
      let kon = 2.3
      let koff = 0.084
      let spec : graph_spec =
        { names = [ "R"; "AR"; "A2R"; "A2R*" ]
        ; wiring =
            [ ("R>AR",     kon,        2)  (* k+1   [bind]        *)
            ; ("AR>R",     koff,       0)  (* k-1   [unbind]      *)
            ; ("AR>A2R",   kon,        1)  (* k+2   [bind]        *)
            ; ("A2R>AR",   koff *. 2.0, 0)  (* k-2   [unbind]      *)
            ; ("A2R>A2R*", 0.513,      0)  (* beta  [opening]     *)
            ; ("A2R*>A2R", 1.,         0)  (* alpha [closing]     *)
            ]
        }
    end : BuildSpec)
end
