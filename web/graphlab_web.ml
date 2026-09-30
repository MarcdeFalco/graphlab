(* Interface JavaScript de Graphlab : expose un objet global [graphlab]
   utilisé par index.html. Le dessin et l'interaction sont faits en JS,
   les graphes, la disposition et les parcours viennent de Graph et Layout. *)

open Js_of_ocaml

let examples =
  [
    ("Complet 5", fun () -> Graph.complet 5);
    ("Cycle 5", fun () -> Graph.cycle 5);
    ("Möbius 5", fun () -> Graph.mobius 5);
    ("Hypercube 3", fun () -> Graph.hypercube 3);
    ("Hypercube 4", fun () -> Graph.hypercube 4);
    ("Grille 3", fun () -> Graph.grid 3);
    ("Grille 7", fun () -> Graph.grid 7);
    ("Exemple", fun () -> Graph.exemple);
    ("Diviseurs 11", fun () -> Graph.divisors 11);
    ("Diviseurs 99", fun () -> Graph.divisors 99);
  ]

let graph = ref (Graph.complet 5)
let pos = ref [||]
let graph_radius = ref 100.
let trace : Graph.search_trace option ref = ref None

(* Même calcul que set_graph dans graphlab.ml, avec un rayon de sommet de 10 *)
let set_graph g =
  graph := g;
  graph_radius := float_of_int (max 1 (Graph.diameter g)) *. 200.;
  pos := Layout.eades g !graph_radius;
  trace := None

let () = set_graph !graph

let js_string_array l = Js.array (Array.of_list (List.map Js.string l))

let edges () =
  let l = ref [] in
  Array.iteri
    (fun i adj ->
      List.iter
        (fun (j, _) -> l := Js.array [| i; j |] :: !l)
        (List.rev adj))
    !graph.Graph.edges;
  Js.array (Array.of_list (List.rev !l))

(* Graphe saisi par l'utilisateur : une arête « a b » par ligne ;
   une ligne réduite à un nom déclare un sommet isolé. *)
let parse_custom directed text =
  let names = ref [] in
  let index name =
    let rec find i = function
      | [] -> None
      | x :: q -> if x = name then Some i else find (i + 1) q
    in
    match find 0 (List.rev !names) with
    | Some i -> i
    | None ->
        names := name :: !names;
        List.length !names - 1
  in
  let edges = ref [] in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
         let words =
           String.split_on_char ' '
             (String.map (fun c -> if c = '\t' || c = ',' then ' ' else c) line)
           |> List.filter (fun w -> w <> "")
         in
         match words with
         | [] -> ()
         | [ a ] -> ignore (index a)
         | a :: b :: rest ->
             let w = match rest with w :: _ -> int_of_string_opt w | [] -> None in
             let i = index a in
             let j = index b in
             edges := (i, j, Option.value w ~default:1) :: !edges);
  if !names = [] then failwith "Graphe vide";
  Graph.create (Array.of_list (List.rev !names)) directed (List.rev !edges)

let status_code = function
  | Graph.Unknown -> 0
  | Graph.Discovered -> 1
  | Graph.Processed -> 2

let edge_status_code = function
  | Graph.Tree -> 1
  | Graph.Back -> 2
  | Graph.Forward -> 3
  | Graph.Cross -> 4
  | Graph.NoStatus -> 0

let opt_int = function Some t -> Js.some t | None -> Js.null

let step k =
  match !trace with
  | None -> Js.null
  | Some tr ->
      let st = List.nth tr.Graph.steps k in
      let n = Array.length st.Graph.status in
      let es = ref [] in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          let c = edge_status_code st.Graph.edge_status.(i).(j) in
          (* Les parcours itératifs ne classent pas les arêtes :
             on montre alors l'arbre donné par les parents. *)
          let c = if c = 0 && st.Graph.parent.(j) = Some i then 1 else c in
          if c <> 0 then es := Js.array [| i; j; c |] :: !es
        done
      done;
      Js.some
        object%js
          val current = st.Graph.current
          val status = Js.array (Array.map status_code st.Graph.status)
          val entry = Js.array (Array.map opt_int st.Graph.entry_time)
          val exit = Js.array (Array.map opt_int st.Graph.exit_time)
          val edgeStatus = Js.array (Array.of_list (List.rev !es))
        end

let matrix_text g =
  let n = Graph.nvertices g in
  let b = Buffer.create 256 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Buffer.add_string b (if Graph.connected g i j then "1 " else "0 ")
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let () =
  Js.export "graphlab"
    object%js
      method examples = js_string_array (List.map fst examples)

      method setExample name =
        let name = Js.to_string name in
        set_graph ((List.assoc name examples) ())

      method setCustom directed text =
        try
          set_graph (parse_custom (Js.to_bool directed) (Js.to_string text));
          Js.null
        with e -> Js.some (Js.string (Printexc.to_string e))

      method vertices = js_string_array (Array.to_list !graph.Graph.vtx)
      method edges = edges ()
      method directed = Js.bool !graph.Graph.directed

      method positions =
        Js.array (Array.map (fun (x, y) -> Js.array [| x; y |]) !pos)

      method setPosition i x y = !pos.(i) <- (x, y)
      method randomize = pos := Layout.init !graph !graph_radius

      method iterate c1 c2 c3 c4 (fixed : int) =
        let npos = Layout.iterate !graph !pos (c1, c2, c3, c4) !graph_radius in
        Array.iteri (fun i p -> if i <> fixed then !pos.(i) <- p) npos

      method search kind src =
        let st =
          match Js.to_string kind with
          | "bfs" -> Graph.BFS
          | "dfs" -> Graph.DFS
          | _ -> Graph.DFS_rec
        in
        trace := Some (Graph.search !graph.Graph.directed st !graph src)

      method resetSearch = trace := None

      method stepCount =
        match !trace with None -> 0 | Some t -> List.length t.Graph.steps

      method step k = step k
      method ladj = Js.string (Graph.text_ladj !graph)
      method matrix = Js.string (matrix_text !graph)
      method nedges = Graph.nedges !graph
      method diameter = Graph.diameter !graph
    end
