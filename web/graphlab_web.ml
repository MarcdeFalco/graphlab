(* Interface JavaScript de Graphlab : expose un objet global [graphlab]
   utilisé par index.html.

   La page web détient le graphe (pour pouvoir l'éditer) et le transmet
   avec [setGraph] à chaque modification. OCaml fournit les générateurs,
   la disposition (Layout), les algorithmes instrumentés (Algos) et le
   remplissage de grille. *)

open Js_of_ocaml

let () = Random.self_init ()

(* ------------------------------------------------------------------ *)
(* Graphe courant *)

let graph = ref (Graph.create [||] false [])
let radius = ref 200.

(* Échelle « idéale » de la disposition, comme dans graphlab.ml *)
let ideal_radius g = float_of_int (max 2 (Graph.diameter g)) *. 200.

let set_graph labels directed (edges : int array) =
  let n = Array.length labels in
  let l = ref [] in
  let k = Array.length edges / 3 in
  for e = k - 1 downto 0 do
    let u = edges.(3 * e) and v = edges.((3 * e) + 1) and w = edges.((3 * e) + 2) in
    if u >= 0 && u < n && v >= 0 && v < n then l := (u, v, w) :: !l
  done;
  graph := Graph.create labels directed !l;
  radius := ideal_radius !graph

(* ------------------------------------------------------------------ *)
(* Générateurs *)

type param = { pname : string; plabel : string; default : float; pmin : float; pmax : float; pstep : float }

let p_n ?(label = "n") ?(min = 1.) ?(max = 40.) d =
  { pname = "n"; plabel = label; default = d; pmin = min; pmax = max; pstep = 1. }

let p_m d = { pname = "m"; plabel = "m"; default = d; pmin = 1.; pmax = 20.; pstep = 1. }
let p_p d = { pname = "p"; plabel = "probabilité p"; default = d; pmin = 0.; pmax = 1.; pstep = 0.05 }
let p_w = { pname = "w"; plabel = "poids max (0 : sans poids)"; default = 0.; pmin = 0.; pmax = 99.; pstep = 1. }

type generator = {
  gid : string;
  gname : string;
  params : param list;
  build : (string -> int) -> (string -> float) -> (string, int) Graph.t;
  grid : bool;  (** disposition en grille plutôt que par forces *)
}

let gen ?(grid = false) gid gname params build = { gid; gname; params; build; grid }

let generators =
  [
    gen "exemple" "Exemple orienté (a…f)" [] (fun _ _ -> Graph.exemple);
    gen "pondere" "Exemple pondéré (s…t)" [] (fun _ _ -> Graph.exemple_pondere);
    gen "negatif" "Exemple à poids négatifs" [] (fun _ _ -> Graph.exemple_negatif);
    gen "complet" "Graphe complet Kₙ" [ p_n 5.; p_w ] (fun i _ -> Graph.complet (i "n"));
    gen "cycle" "Cycle Cₙ" [ p_n ~min:3. 6.; p_w ] (fun i _ -> Graph.cycle (i "n"));
    gen "chemin" "Chemin Pₙ" [ p_n 6.; p_w ] (fun i _ -> Graph.path (i "n"));
    gen "etoile" "Étoile (n branches)" [ p_n 6.; p_w ] (fun i _ -> Graph.star (i "n"));
    gen "roue" "Roue (n rayons)" [ p_n ~min:3. 6.; p_w ] (fun i _ -> Graph.wheel (i "n"));
    gen "biparti" "Biparti complet Kₙ,ₘ" [ p_n ~max:20. 3.; p_m 3.; p_w ] (fun i _ ->
        Graph.complete_bipartite (i "n") (i "m"));
    gen ~grid:true "grille" "Grille n × n" [ p_n ~max:15. 5.; p_w ] (fun i _ -> Graph.grid (i "n"));
    gen "hypercube" "Hypercube Q_d" [ p_n ~label:"dimension d" ~max:6. 3.; p_w ] (fun i _ ->
        Graph.hypercube (i "n"));
    gen "mobius" "Möbius (n sommets)" [ p_n ~min:4. 8.; p_w ] (fun i _ -> Graph.mobius (i "n"));
    gen "petersen" "Graphe de Petersen" [ p_w ] (fun _ _ -> Graph.petersen);
    gen "arbre" "Arbre aléatoire" [ p_n 10.; p_w ] (fun i _ -> Graph.random_tree (i "n"));
    gen "alea" "Aléatoire G(n, p)" [ p_n 10.; p_p 0.3; p_w ] (fun i f ->
        Graph.random_graph (i "n") (f "p") false);
    gen "aleaor" "Aléatoire orienté" [ p_n 8.; p_p 0.25; p_w ] (fun i f ->
        Graph.random_graph (i "n") (f "p") true);
    gen "dag" "Orienté sans cycle aléatoire" [ p_n 8.; p_p 0.3; p_w ] (fun i f ->
        Graph.random_dag (i "n") (f "p"));
    gen "diviseurs" "Diviseurs de 1 à n" [ p_n ~max:99. 12. ] (fun i _ -> Graph.divisors (i "n"));
  ]

(* ------------------------------------------------------------------ *)
(* Conversions vers JavaScript *)

let js_strings a = Js.array (Array.map Js.string a)
let js_positions pos = Js.array (Array.map (fun (x, y) -> Js.array [| x; y |]) pos)

let js_graph g (pos : (float * float) array) =
  let edges = ref [] in
  Array.iteri
    (fun i l ->
      List.iter
        (fun (j, w) -> if g.Graph.directed || i < j then edges := Js.array [| i; j; w |] :: !edges)
        (List.rev l))
    g.Graph.edges;
  object%js
    val labels = js_strings g.Graph.vtx
    val directed = Js.bool g.Graph.directed
    val edges = Js.array (Array.of_list (List.rev !edges))
    val positions = js_positions pos
  end

let layout g = if Graph.nvertices g = 0 then [||] else Layout.eades g (ideal_radius g)

let grid_positions g =
  let n = Graph.nvertices g in
  let side = int_of_float (ceil (sqrt (float_of_int n))) in
  Array.init n (fun i -> (float_of_int (i mod side) *. 300., float_of_int (i / side) *. 300.))

let generate id (values : float array) =
  let gen = List.find (fun g -> g.gid = id) generators in
  let value name =
    let rec find k = function
      | [] -> 0.
      | p :: q ->
          if p.pname = name then if k < Array.length values then values.(k) else p.default
          else find (k + 1) q
    in
    find 0 gen.params
  in
  let g = gen.build (fun s -> int_of_float (value s)) value in
  let w = int_of_float (value "w") in
  let g = if w > 0 then Graph.with_random_weights g w else g in
  js_graph g (if gen.grid then grid_positions g else layout g)

let js_param p =
  object%js
    val name = Js.string p.pname
    val label = Js.string p.plabel
    val default = p.default
    val min = p.pmin
    val max = p.pmax
    val step = p.pstep
  end

(* ------------------------------------------------------------------ *)
(* Traces d'algorithmes *)

let trace : Algos.step array ref = ref [||]

let js_step (st : Algos.step) =
  object%js
    val current = st.current
    val edge = match st.edge with None -> Js.null | Some (i, j) -> Js.some (Js.array [| i; j |])
    val vclass = Js.array st.vclass
    val vgroup = Js.array st.vgroup
    val vnote = js_strings st.vnote
    val eclass = Js.array (Array.of_list (List.map (fun (i, j, c) -> Js.array [| i; j; c |]) st.eclass))
    val structure = Js.string st.structure
    val contents = js_strings (Array.of_list st.contents)
    val table = Js.array (Array.map js_strings st.table)
    val message = Js.string st.message
    val line = st.line
  end

let js_algo (a : Algos.info) =
  object%js
    val id = Js.string a.id
    val name = Js.string a.name
    val code = js_strings a.code
    val source = Js.bool a.source
    val weighted = Js.bool a.weighted
    val description = Js.string a.description
  end

(* ------------------------------------------------------------------ *)

let () =
  Js.export "graphlab"
    object%js
      method generators =
        Js.array
          (Array.of_list
             (List.map
                (fun g ->
                  object%js
                    val id = Js.string g.gid
                    val name = Js.string g.gname
                    val params = Js.array (Array.of_list (List.map js_param g.params))
                  end)
                generators))

      method generate id (values : float Js.js_array Js.t) =
        generate (Js.to_string id) (Js.to_array values)

      method setGraph (labels : Js.js_string Js.t Js.js_array Js.t) directed
          (edges : int Js.js_array Js.t) =
        set_graph (Array.map Js.to_string (Js.to_array labels)) (Js.to_bool directed)
          (Js.to_array edges);
        trace := [||]

      (* positions calculées par l'algorithme d'Eades *)
      method layout = js_positions (layout !graph)

      (* une itération de la disposition ; les positions sont un tableau
         plat [x0; y0; x1; y1; ...], le sommet [fixed] ne bouge pas *)
      method iterate (flat : float Js.js_array Js.t) c1 c2 c3 c4 (fixed : int) =
        let flat = Js.to_array flat in
        let n = Graph.nvertices !graph in
        if n < 2 || Array.length flat < 2 * n then Js.array flat
        else begin
          let pos = Array.init n (fun i -> (flat.(2 * i), flat.((2 * i) + 1))) in
          let centre p =
            let sx = ref 0. and sy = ref 0. in
            Array.iter
              (fun (x, y) ->
                sx := !sx +. x;
                sy := !sy +. y)
              p;
            (!sx /. float_of_int n, !sy /. float_of_int n)
          in
          let cx, cy = centre pos in
          let npos = Layout.iterate !graph pos (c1, c2, c3, c4) !radius in
          (* Layout.iterate ramène le graphe vers l'origine : on recentre *)
          let nx, ny = centre npos in
          let out = Array.make (2 * n) 0. in
          Array.iteri
            (fun i (x, y) ->
              let x, y = if i = fixed then pos.(i) else (x -. nx +. cx, y -. ny +. cy) in
              out.(2 * i) <- x;
              out.((2 * i) + 1) <- y)
            npos;
          Js.array out
        end

      method diameter = Graph.diameter !graph
      method algorithms = Js.array (Array.of_list (List.map js_algo Algos.catalogue))

      method run id (source : int) =
        let id = Js.to_string id in
        let a = List.find (fun (a : Algos.info) -> a.id = id) Algos.catalogue in
        let n = Graph.nvertices !graph in
        trace := if n = 0 then [||] else Array.of_list (a.run !graph (max 0 (min source (n - 1))));
        Array.length !trace

      method step k =
        if k < 0 || k >= Array.length !trace then Js.null else Js.some (js_step !trace.(k))

      (* classes utilisées dans toute la trace, pour n'afficher que la
         légende utile *)
      method traceClasses =
        let e = Array.make 9 false and v = Array.make 3 false and grp = ref false in
        Array.iter
          (fun (st : Algos.step) ->
            List.iter (fun (_, _, c) -> if c < 9 then e.(c) <- true) st.eclass;
            Array.iter (fun c -> if c < 3 then v.(c) <- true) st.vclass;
            if Array.exists (fun g -> g >= 0) st.vgroup then grp := true)
          !trace;
        object%js
          val edges = Js.array (Array.map Js.bool e)
          val vertices = Js.array (Array.map Js.bool v)
          val groups = Js.bool !grp
        end

      method floodOrder w h (walls : int Js.js_array Js.t) start mode =
        let walls = Array.map (fun x -> x <> 0) (Js.to_array walls) in
        let mode =
          match Js.to_string mode with
          | "stack" -> Algos.Flood_stack
          | "queue" -> Algos.Flood_queue
          | _ -> Algos.Flood_random
        in
        Js.array (Algos.flood_order w h walls start mode)
    end
