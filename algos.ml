(* Algorithmes de graphes instrumentés.

   Chaque algorithme renvoie une trace : la liste des étapes à montrer.
   Une étape décrit l'état des sommets (classe, groupe, annotation), des
   arêtes, de la structure de données utilisée (file, pile, ...), un
   tableau éventuel, un message en français et la ligne du pseudo-code
   correspondante. L'interface se contente d'afficher ces étapes. *)

type step = {
  current : int;  (** sommet courant, -1 si aucun *)
  edge : (int * int) option;  (** arête en cours d'examen *)
  vclass : int array;  (** 0 inconnu, 1 découvert / en attente, 2 traité *)
  vgroup : int array;  (** couleur ou composante, -1 si aucune *)
  vnote : string array;  (** annotation affichée à côté du sommet *)
  eclass : (int * int * int) list;  (** arêtes (i, j, classe) *)
  structure : string;  (** nom de la structure de données *)
  contents : string list;  (** son contenu, dans l'ordre de sortie *)
  table : string array array;  (** tableau, première ligne = en-têtes *)
  message : string;
  line : int;  (** ligne du pseudo-code, -1 si aucune *)
}

(* Classes d'arêtes *)
let e_tree = 1 (* arbre / arête retenue *)
let e_back = 2 (* arrière *)
let e_forward = 3 (* avant *)
let e_cross = 4 (* transverse *)
let e_scan = 5 (* en cours d'examen *)
let e_reject = 6 (* rejetée *)
let e_conflict = 7 (* conflit *)
let e_candidate = 8 (* candidate (Prim) *)

(* Au-delà, on arrête d'enregistrer pour ne pas saturer le navigateur *)
let max_steps = 4000

type recorder = {
  g : (string, int) Graph.t;
  n : int;
  vclass : int array;
  vgroup : int array;
  vnote : string array;
  ecls : (int * int, int) Hashtbl.t;
  mutable cur : int;
  mutable cedge : (int * int) option;
  mutable sname : string;
  mutable contents : string list;
  mutable table : unit -> string array array;
  mutable steps : step list;
  mutable count : int;
}

let make g =
  let n = Graph.nvertices g in
  {
    g;
    n;
    vclass = Array.make n 0;
    vgroup = Array.make n (-1);
    vnote = Array.make n "";
    ecls = Hashtbl.create 16;
    cur = -1;
    cedge = None;
    sname = "";
    contents = [];
    table = (fun () -> [||]);
    steps = [];
    count = 0;
  }

let label r i = r.g.Graph.vtx.(i)
let set_edge r i j c = Hashtbl.replace r.ecls (i, j) c
let clear_edge r i j = Hashtbl.remove r.ecls (i, j)
let edge_class r i j = Hashtbl.find_opt r.ecls (i, j)

let snap r line message =
  if r.count < max_steps then begin
    r.count <- r.count + 1;
    let eclass = Hashtbl.fold (fun (i, j) c acc -> (i, j, c) :: acc) r.ecls [] in
    r.steps <-
      {
        current = r.cur;
        edge = r.cedge;
        vclass = Array.copy r.vclass;
        vgroup = Array.copy r.vgroup;
        vnote = Array.copy r.vnote;
        eclass;
        structure = r.sname;
        contents = r.contents;
        table = r.table ();
        message;
        line;
      }
      :: r.steps
  end

let finish r =
  if r.count >= max_steps then begin
    r.count <- 0;
    snap r (-1) (Printf.sprintf "Trace tronquée à %d étapes (graphe trop grand)." max_steps)
  end;
  List.rev r.steps

let sprintf = Printf.sprintf

(* Voisins par numéro croissant, pour un déroulement prévisible *)
let neighbors g i = List.sort (fun (a, _) (b, _) -> compare a b) g.Graph.edges.(i)

(* Voisins en ignorant l'orientation *)
let undirected_neighbors g =
  let n = Graph.nvertices g in
  let adj = Array.make n [] in
  Array.iteri
    (fun i l ->
      List.iter
        (fun (j, w) ->
          if not (List.mem_assoc j adj.(i)) then adj.(i) <- (j, w) :: adj.(i);
          if not (List.mem_assoc i adj.(j)) then adj.(j) <- (i, w) :: adj.(j))
        l)
    g.Graph.edges;
  Array.map (List.sort (fun (a, _) (b, _) -> compare a b)) adj

(* Liste des arêtes, une seule fois chacune si le graphe n'est pas orienté *)
let edge_list g =
  let l = ref [] in
  Array.iteri
    (fun i adj ->
      List.iter
        (fun (j, w) -> if g.Graph.directed || i <= j then l := (i, j, w) :: !l)
        adj)
    g.Graph.edges;
  List.rev !l

let has_negative g = List.exists (fun (_, _, w) -> w < 0) (edge_list g)
let string_of_dist = function None -> "∞" | Some d -> string_of_int d
let opt_label r = function None -> "—" | Some p -> label r p

let vertex_table r headers columns =
  Array.append [| Array.of_list ("sommet" :: headers) |]
    (Array.init r.n (fun i ->
         Array.of_list (label r i :: List.map (fun f -> f i) columns)))

(* ------------------------------------------------------------------ *)
(* Parcours en largeur *)

let bfs_code =
  [|
    "pour tout sommet v : vu[v] ← faux";
    "vu[s] ← vrai ; dist[s] ← 0 ; F ← file contenant s";
    "tant que F n'est pas vide :";
    "    u ← défiler(F)";
    "    pour tout voisin v de u :";
    "        si vu[v] : rien à faire";
    "        sinon : vu[v] ← vrai ; dist[v] ← dist[u] + 1 ; parent[v] ← u";
    "                enfiler(F, v)";
    "    u est traité";
  |]

let bfs g s =
  let r = make g in
  let dist = Array.make r.n None in
  let parent = Array.make r.n None in
  let q = Queue.create () in
  let update () = r.contents <- List.map (label r) (List.of_seq (Queue.to_seq q)) in
  r.sname <- "File";
  r.table <-
    (fun () ->
      vertex_table r [ "dist"; "parent" ]
        [ (fun i -> string_of_dist dist.(i)); (fun i -> opt_label r parent.(i)) ]);
  snap r 0 "Aucun sommet n'est encore vu.";
  r.vclass.(s) <- 1;
  dist.(s) <- Some 0;
  r.vnote.(s) <- "0";
  Queue.push s q;
  update ();
  r.cur <- s;
  snap r 1 (sprintf "On part de %s, à distance 0." (label r s));
  while not (Queue.is_empty q) do
    let u = Queue.pop q in
    update ();
    r.cur <- u;
    snap r 3 (sprintf "On défile %s." (label r u));
    List.iter
      (fun (v, _) ->
        r.cedge <- Some (u, v);
        if r.vclass.(v) = 0 then begin
          let d = Option.get dist.(u) + 1 in
          r.vclass.(v) <- 1;
          dist.(v) <- Some d;
          parent.(v) <- Some u;
          r.vnote.(v) <- string_of_int d;
          set_edge r u v e_tree;
          Queue.push v q;
          update ();
          snap r 7
            (sprintf "%s n'était pas vu : distance %d, on l'enfile." (label r v) d)
        end
        else snap r 5 (sprintf "%s est déjà vu." (label r v)))
      (neighbors g u);
    r.cedge <- None;
    r.vclass.(u) <- 2;
    snap r 8 (sprintf "Tous les voisins de %s ont été examinés." (label r u))
  done;
  r.cur <- -1;
  snap r (-1) "Parcours terminé : les distances (en nombre d'arêtes) sont minimales.";
  finish r

(* ------------------------------------------------------------------ *)
(* Parcours en profondeur avec une pile *)

let dfs_stack_code =
  [|
    "P ← pile contenant s";
    "tant que P n'est pas vide :";
    "    u ← dépiler(P)";
    "    si u est déjà visité : passer au suivant";
    "    visiter u";
    "    pour tout voisin v de u (du plus grand au plus petit) :";
    "        si v n'est pas visité : empiler(P, v)";
  |]

let dfs_stack g s =
  let r = make g in
  let order = ref 0 in
  let parent = Array.make r.n None in
  let stack = Stack.create () in
  let update () =
    r.contents <- List.map (fun (v, _) -> label r v) (List.of_seq (Stack.to_seq stack))
  in
  r.sname <- "Pile (sommet en tête)";
  r.table <-
    (fun () ->
      vertex_table r [ "ordre"; "parent" ]
        [ (fun i -> r.vnote.(i)); (fun i -> opt_label r parent.(i)) ]);
  Stack.push (s, None) stack;
  r.vclass.(s) <- 1;
  update ();
  snap r 0 (sprintf "La pile contient %s." (label r s));
  while not (Stack.is_empty stack) do
    let u, from = Stack.pop stack in
    update ();
    r.cur <- u;
    if r.vclass.(u) = 2 then snap r 3 (sprintf "%s est déjà visité : on l'ignore." (label r u))
    else begin
      incr order;
      r.vclass.(u) <- 2;
      r.vnote.(u) <- string_of_int !order;
      parent.(u) <- from;
      (match from with Some p -> set_edge r p u e_tree | None -> ());
      snap r 4 (sprintf "On visite %s (n° %d)." (label r u) !order);
      List.iter
        (fun (v, _) ->
          if r.vclass.(v) <> 2 then begin
            Stack.push (v, Some u) stack;
            r.vclass.(v) <- 1;
            r.cedge <- Some (u, v);
            update ();
            snap r 6 (sprintf "On empile %s." (label r v))
          end)
        (List.rev (neighbors g u));
      r.cedge <- None
    end
  done;
  r.cur <- -1;
  snap r (-1) (sprintf "Parcours terminé : %d sommets visités." !order);
  finish r

(* ------------------------------------------------------------------ *)
(* Parcours en profondeur récursif complet, avec classification des arêtes *)

let dfs_rec_code =
  [|
    "pour tout sommet s (la source d'abord) :";
    "    si s est inconnu : explorer(s)";
    "explorer(u) :";
    "    u est en cours ; début[u] ← t ; t ← t + 1";
    "    pour tout voisin v de u :";
    "        si v est inconnu : arête d'arbre ; explorer(v)";
    "        si v est en cours : arête arrière (cycle !)";
    "        si v est terminé : arête avant ou transverse";
    "    u est terminé ; fin[u] ← t ; t ← t + 1";
  |]

(* Renvoie la trace et l'ordre de fin (utilisé par Kosaraju) *)
let dfs_rec_full ?(record = true) g s =
  let r = make g in
  let time = ref 0 in
  let entry = Array.make r.n (-1) in
  let exit = Array.make r.n (-1) in
  let finished = ref [] in
  let calls = ref [] in
  let snap r l m = if record then snap r l m in
  let note u =
    r.vnote.(u) <-
      (if exit.(u) >= 0 then sprintf "%d/%d" entry.(u) exit.(u)
       else sprintf "%d/" entry.(u))
  in
  r.sname <- "Pile d'appels (appel courant en tête)";
  r.table <-
    (fun () ->
      vertex_table r [ "début"; "fin" ]
        [
          (fun i -> if entry.(i) >= 0 then string_of_int entry.(i) else "");
          (fun i -> if exit.(i) >= 0 then string_of_int exit.(i) else "");
        ]);
  let rec explore u =
    calls := u :: !calls;
    r.contents <- List.map (label r) !calls;
    r.vclass.(u) <- 1;
    entry.(u) <- !time;
    incr time;
    note u;
    r.cur <- u;
    r.cedge <- None;
    snap r 3 (sprintf "On commence l'exploration de %s (début %d)." (label r u) entry.(u));
    List.iter
      (fun (v, _) ->
        (* en non orienté, une arête déjà classée depuis l'autre bout est ignorée *)
        if g.Graph.directed || edge_class r v u = None then begin
          r.cur <- u;
          r.cedge <- Some (u, v);
          match r.vclass.(v) with
          | 0 ->
              set_edge r u v e_tree;
              snap r 5 (sprintf "%s est inconnu : arête d'arbre, on l'explore." (label r v));
              explore v
          | 1 ->
              set_edge r u v e_back;
              snap r 6 (sprintf "%s est en cours : arête arrière, il y a un cycle." (label r v))
          | _ ->
              if entry.(u) < entry.(v) then begin
                set_edge r u v e_forward;
                snap r 7 (sprintf "%s est terminé et descendant de %s : arête avant." (label r v) (label r u))
              end
              else begin
                set_edge r u v e_cross;
                snap r 7 (sprintf "%s est terminé : arête transverse." (label r v))
              end
        end)
      (neighbors g u);
    r.vclass.(u) <- 2;
    exit.(u) <- !time;
    incr time;
    note u;
    finished := u :: !finished;
    calls := List.tl !calls;
    r.contents <- List.map (label r) !calls;
    r.cur <- u;
    r.cedge <- None;
    snap r 8 (sprintf "%s est terminé (fin %d)." (label r u) exit.(u))
  in
  let roots = s :: List.filter (fun v -> v <> s) (List.init r.n (fun i -> i)) in
  List.iter
    (fun v ->
      if r.vclass.(v) = 0 then begin
        r.cur <- v;
        if v <> s then snap r 1 (sprintf "%s n'a pas été atteint : nouvel arbre." (label r v));
        explore v
      end)
    roots;
  r.cur <- -1;
  r.cedge <- None;
  let back = Hashtbl.fold (fun _ c acc -> acc || c = e_back) r.ecls false in
  snap r (-1)
    (if back then "Parcours terminé. Il y a une arête arrière : le graphe contient un cycle."
     else "Parcours terminé. Aucune arête arrière : le graphe est sans cycle.");
  (finish r, !finished)

let dfs_rec g s = fst (dfs_rec_full g s)

(* ------------------------------------------------------------------ *)
(* Dijkstra *)

let dijkstra_code =
  [|
    "dist[s] ← 0 ; dist[v] ← ∞ pour tout v ≠ s";
    "tant qu'il reste un sommet non traité de distance finie :";
    "    u ← sommet non traité de distance minimale";
    "    u est traité : dist[u] est définitive";
    "    pour tout voisin v de u, par une arête de poids w :";
    "        si dist[u] + w < dist[v] :";
    "            dist[v] ← dist[u] + w ; parent[v] ← u";
  |]

let dijkstra g s =
  let r = make g in
  let dist = Array.make r.n None in
  let parent = Array.make r.n None in
  let candidates () =
    List.filter (fun i -> r.vclass.(i) = 1) (List.init r.n (fun i -> i))
    |> List.sort (fun a b -> compare (dist.(a), a) (dist.(b), b))
  in
  let update () =
    r.contents <-
      List.map (fun i -> sprintf "%s (%s)" (label r i) (string_of_dist dist.(i))) (candidates ())
  in
  r.sname <- "File de priorité (minimum en tête)";
  r.table <-
    (fun () ->
      vertex_table r [ "dist"; "parent" ]
        [ (fun i -> string_of_dist dist.(i)); (fun i -> opt_label r parent.(i)) ]);
  Array.iteri (fun i _ -> r.vnote.(i) <- "∞") r.vnote;
  dist.(s) <- Some 0;
  r.vnote.(s) <- "0";
  r.vclass.(s) <- 1;
  update ();
  r.cur <- s;
  snap r 0
    (if has_negative g then
       "Attention : le graphe a des poids négatifs, Dijkstra peut donner un résultat faux."
     else sprintf "dist[%s] = 0, les autres distances sont infinies." (label r s));
  let rec loop () =
    match candidates () with
    | [] -> ()
    | u :: _ ->
        r.vclass.(u) <- 2;
        r.cur <- u;
        update ();
        snap r 3
          (sprintf "%s est le sommet non traité le plus proche : dist = %s, définitive." (label r u)
             (string_of_dist dist.(u)));
        let du = Option.get dist.(u) in
        List.iter
          (fun (v, w) ->
            if r.vclass.(v) <> 2 then begin
              r.cedge <- Some (u, v);
              let nd = du + w in
              let better = match dist.(v) with None -> true | Some d -> nd < d in
              if better then begin
                (match parent.(v) with Some p -> clear_edge r p v | None -> ());
                dist.(v) <- Some nd;
                parent.(v) <- Some u;
                r.vnote.(v) <- string_of_int nd;
                r.vclass.(v) <- 1;
                set_edge r u v e_tree;
                update ();
                snap r 6
                  (sprintf "%d + %d = %d est meilleur : dist[%s] ← %d, parent %s." du w nd
                     (label r v) nd (label r u))
              end
              else
                snap r 5
                  (sprintf "%d + %d = %d n'améliore pas dist[%s] = %s." du w nd (label r v)
                     (string_of_dist dist.(v)))
            end)
          (neighbors g u);
        r.cedge <- None;
        loop ()
  in
  loop ();
  r.cur <- -1;
  snap r (-1) "Terminé : les arêtes vertes forment l'arbre des plus courts chemins.";
  finish r

(* ------------------------------------------------------------------ *)
(* Bellman-Ford *)

let bellman_ford_code =
  [|
    "dist[s] ← 0 ; dist[v] ← ∞ pour tout v ≠ s";
    "répéter n − 1 fois :";
    "    pour toute arête (u, v) de poids w :";
    "        si dist[u] + w < dist[v] : dist[v] ← dist[u] + w ; parent[v] ← u";
    "pour toute arête (u, v) de poids w :";
    "    si dist[u] + w < dist[v] : il existe un cycle de poids négatif";
  |]

let bellman_ford g s =
  let r = make g in
  let dist = Array.make r.n None in
  let parent = Array.make r.n None in
  let arcs =
    List.concat_map
      (fun (u, v, w) -> if g.Graph.directed || u = v then [ (u, v, w) ] else [ (u, v, w); (v, u, w) ])
      (edge_list g)
  in
  r.table <-
    (fun () ->
      vertex_table r [ "dist"; "parent" ]
        [ (fun i -> string_of_dist dist.(i)); (fun i -> opt_label r parent.(i)) ]);
  Array.iteri (fun i _ -> r.vnote.(i) <- "∞") r.vnote;
  dist.(s) <- Some 0;
  r.vnote.(s) <- "0";
  r.vclass.(s) <- 2;
  r.cur <- s;
  snap r 0 (sprintf "dist[%s] = 0, les autres distances sont infinies." (label r s));
  let relax (u, v, w) =
    match dist.(u) with
    | None -> false
    | Some du -> ( match dist.(v) with None -> true | Some dv -> du + w < dv)
  in
  let stable = ref false in
  let pass = ref 1 in
  while (not !stable) && !pass < r.n do
    stable := true;
    r.cur <- -1;
    r.cedge <- None;
    snap r 1 (sprintf "Passe %d sur %d." !pass (r.n - 1));
    List.iter
      (fun (u, v, w) ->
        if relax (u, v, w) then begin
          stable := false;
          let nd = Option.get dist.(u) + w in
          (match parent.(v) with Some p -> clear_edge r p v | None -> ());
          dist.(v) <- Some nd;
          parent.(v) <- Some u;
          r.vnote.(v) <- string_of_int nd;
          r.vclass.(v) <- 2;
          set_edge r u v e_tree;
          r.cur <- v;
          r.cedge <- Some (u, v);
          snap r 3 (sprintf "Arête %s → %s : dist[%s] ← %d." (label r u) (label r v) (label r v) nd)
        end)
      arcs;
    if !stable then begin
      r.cedge <- None;
      snap r 1 (sprintf "Aucune amélioration pendant la passe %d : on peut s'arrêter." !pass)
    end;
    incr pass
  done;
  r.cur <- -1;
  r.cedge <- None;
  (match List.find_opt relax arcs with
  | Some (u, v, _) ->
      set_edge r u v e_conflict;
      r.cedge <- Some (u, v);
      snap r 5
        (sprintf "L'arête %s → %s peut encore être relâchée : cycle de poids négatif accessible."
           (label r u) (label r v))
  | None -> snap r 4 "Aucune arête ne peut être relâchée : les distances sont exactes.");
  finish r

(* ------------------------------------------------------------------ *)
(* Prim *)

let prim_code =
  [|
    "clé[v] ← ∞ pour tout v ; clé[s] ← 0 ; T ← ∅";
    "tant qu'il reste un sommet hors de T de clé finie :";
    "    u ← sommet hors de T de clé minimale";
    "    ajouter u (et l'arête parent[u] — u) à l'arbre T";
    "    pour tout voisin v de u hors de T, par une arête de poids w :";
    "        si w < clé[v] : clé[v] ← w ; parent[v] ← u";
  |]

let prim g s =
  let r = make g in
  let adj = undirected_neighbors g in
  let key = Array.make r.n None in
  let parent = Array.make r.n None in
  let total = ref 0 in
  let candidates () =
    List.filter (fun i -> r.vclass.(i) = 1) (List.init r.n (fun i -> i))
    |> List.sort (fun a b -> compare (key.(a), a) (key.(b), b))
  in
  let update () =
    r.contents <-
      List.map (fun i -> sprintf "%s (%s)" (label r i) (string_of_dist key.(i))) (candidates ())
  in
  let set_tree a b c =
    if g.Graph.directed && not (Graph.connected g a b) then set_edge r b a c else set_edge r a b c
  in
  let clear_tree a b =
    clear_edge r a b;
    clear_edge r b a
  in
  r.sname <- "Candidats (clé minimale en tête)";
  r.table <-
    (fun () ->
      vertex_table r [ "clé"; "parent" ]
        [ (fun i -> string_of_dist key.(i)); (fun i -> opt_label r parent.(i)) ]);
  key.(s) <- Some 0;
  r.vclass.(s) <- 1;
  update ();
  r.cur <- s;
  snap r 0
    (if g.Graph.directed then "Le graphe est orienté : Prim ignore le sens des arêtes."
     else sprintf "On part de %s." (label r s));
  let rec loop () =
    match candidates () with
    | [] -> ()
    | u :: _ ->
        r.vclass.(u) <- 2;
        r.cur <- u;
        r.cedge <- None;
        (match parent.(u) with
        | Some p ->
            set_tree p u e_tree;
            total := !total + Option.get key.(u)
        | None -> ());
        update ();
        snap r 3
          (match parent.(u) with
          | Some p ->
              sprintf "On ajoute %s par l'arête %s — %s de poids %s (total %d)." (label r u)
                (label r p) (label r u) (string_of_dist key.(u)) !total
          | None -> sprintf "On ajoute %s à l'arbre." (label r u));
        List.iter
          (fun (v, w) ->
            if r.vclass.(v) <> 2 then begin
              r.cedge <- Some (u, v);
              let better = match key.(v) with None -> true | Some k -> w < k in
              if better then begin
                (match parent.(v) with Some p -> clear_tree p v | None -> ());
                key.(v) <- Some w;
                parent.(v) <- Some u;
                r.vnote.(v) <- string_of_int w;
                r.vclass.(v) <- 1;
                set_tree u v e_candidate;
                update ();
                snap r 5 (sprintf "clé[%s] ← %d (par %s)." (label r v) w (label r u))
              end
              else
                snap r 4
                  (sprintf "%d ne fait pas mieux que clé[%s] = %s." w (label r v)
                     (string_of_dist key.(v)))
            end)
          adj.(u);
        r.cedge <- None;
        loop ()
  in
  loop ();
  r.cur <- -1;
  let reached = Array.for_all (fun c -> c = 2) r.vclass in
  snap r (-1)
    (if reached then sprintf "Arbre couvrant de poids minimal obtenu : poids total %d." !total
     else sprintf "Le graphe n'est pas connexe : arbre couvrant de la composante, poids %d." !total);
  finish r

(* ------------------------------------------------------------------ *)
(* Kruskal *)

let kruskal_code =
  [|
    "trier les arêtes par poids croissant";
    "chaque sommet forme sa propre composante";
    "pour toute arête u — v, dans cet ordre :";
    "    si u et v sont dans des composantes différentes :";
    "        garder u — v ; fusionner les deux composantes";
    "    sinon : rejeter u — v (elle fermerait un cycle)";
  |]

let kruskal g =
  let r = make g in
  let uf = Array.init r.n (fun i -> i) in
  let rec find i = if uf.(i) = i then i else begin
      let root = find uf.(i) in
      uf.(i) <- root;
      root
    end
  in
  let groups () =
    (* numéros de composante consécutifs pour l'affichage *)
    let ids = Hashtbl.create 16 in
    Array.iteri
      (fun i _ ->
        let root = find i in
        if not (Hashtbl.mem ids root) then Hashtbl.add ids root (Hashtbl.length ids);
        r.vgroup.(i) <- Hashtbl.find ids root)
      r.vgroup
  in
  let edges =
    List.stable_sort (fun (_, _, a) (_, _, b) -> compare a b)
      (List.filter (fun (u, v, _) -> u <> v) (edge_list g))
  in
  let show l = List.map (fun (u, v, w) -> sprintf "%s—%s (%d)" (label r u) (label r v) w) l in
  r.sname <- "Arêtes restantes (triées)";
  r.contents <- show edges;
  snap r 0 (sprintf "%d arêtes triées par poids croissant." (List.length edges));
  groups ();
  snap r 1 "Chaque sommet est seul dans sa composante (une couleur par composante).";
  let total = ref 0 in
  let kept = ref 0 in
  let rec loop = function
    | [] -> ()
    | (u, v, w) :: rest ->
        r.contents <- show rest;
        r.cedge <- Some (u, v);
        if find u <> find v then begin
          uf.(find u) <- find v;
          set_edge r u v e_tree;
          total := !total + w;
          incr kept;
          groups ();
          snap r 4
            (sprintf "%s et %s sont dans des composantes différentes : on garde l'arête (total %d)."
               (label r u) (label r v) !total)
        end
        else begin
          set_edge r u v e_reject;
          snap r 5 (sprintf "%s et %s sont déjà reliés : on rejette l'arête." (label r u) (label r v))
        end;
        loop rest
  in
  loop edges;
  r.cedge <- None;
  snap r (-1)
    (if !kept = r.n - 1 then sprintf "Arbre couvrant de poids minimal : poids total %d." !total
     else sprintf "Forêt couvrante de poids minimal (graphe non connexe) : poids total %d." !total);
  finish r

(* ------------------------------------------------------------------ *)
(* Tri topologique (algorithme de Kahn) *)

let topo_code =
  [|
    "calculer le degré entrant d[v] de chaque sommet";
    "F ← file des sommets de degré entrant nul";
    "tant que F n'est pas vide :";
    "    u ← défiler(F) ; ajouter u à la fin de l'ordre";
    "    pour tout successeur v de u :";
    "        d[v] ← d[v] − 1 ; si d[v] = 0 : enfiler(F, v)";
    "s'il reste des sommets non placés : le graphe a un cycle";
  |]

let topo g =
  let r = make g in
  let indeg = Array.make r.n 0 in
  Array.iter (List.iter (fun (j, _) -> indeg.(j) <- indeg.(j) + 1)) g.Graph.edges;
  let q = Queue.create () in
  let order = ref [] in
  let update () = r.contents <- List.map (label r) (List.of_seq (Queue.to_seq q)) in
  r.sname <- "File (degré entrant nul)";
  r.table <-
    (fun () ->
      [|
        [| "ordre" |];
        [| String.concat ", " (List.rev_map (label r) !order) |];
      |]);
  if not g.Graph.directed then begin
    snap r (-1) "Le tri topologique n'a de sens que pour un graphe orienté.";
    finish r
  end
  else begin
    Array.iteri (fun i d -> r.vnote.(i) <- string_of_int d) indeg;
    snap r 0 "Les annotations donnent le degré entrant de chaque sommet.";
    Array.iteri
      (fun i d ->
        if d = 0 then begin
          Queue.push i q;
          r.vclass.(i) <- 1
        end)
      indeg;
    update ();
    snap r 1 "On enfile les sommets sans prédécesseur.";
    let k = ref 0 in
    while not (Queue.is_empty q) do
      let u = Queue.pop q in
      incr k;
      order := u :: !order;
      r.vclass.(u) <- 2;
      r.vnote.(u) <- sprintf "n°%d" !k;
      r.cur <- u;
      update ();
      snap r 3 (sprintf "%s est placé en position %d." (label r u) !k);
      List.iter
        (fun (v, _) ->
          indeg.(v) <- indeg.(v) - 1;
          r.cedge <- Some (u, v);
          set_edge r u v e_tree;
          if indeg.(v) = 0 then begin
            Queue.push v q;
            r.vclass.(v) <- 1;
            update ()
          end;
          if r.vclass.(v) <> 2 then r.vnote.(v) <- string_of_int indeg.(v);
          snap r 5
            (sprintf "d[%s] ← %d%s" (label r v) indeg.(v)
               (if indeg.(v) = 0 then " : on l'enfile." else ".")))
        (neighbors g u);
      r.cedge <- None
    done;
    r.cur <- -1;
    snap r 6
      (if !k = r.n then
         sprintf "Ordre topologique : %s." (String.concat ", " (List.rev_map (label r) !order))
       else
         sprintf "%d sommet(s) n'ont jamais atteint un degré entrant nul : le graphe a un cycle."
           (r.n - !k));
    finish r
  end

(* ------------------------------------------------------------------ *)
(* Composantes connexes *)

let components_code =
  [|
    "c ← 0";
    "pour tout sommet s :";
    "    si s n'a pas encore de composante :";
    "        parcourir depuis s : chaque sommet atteint reçoit la composante c";
    "        c ← c + 1";
  |]

let components g =
  let r = make g in
  let adj = undirected_neighbors g in
  let c = ref 0 in
  r.sname <- "File";
  snap r 0
    (if g.Graph.directed then "Graphe orienté : on cherche les composantes faiblement connexes."
     else "Aucun sommet n'a de composante.");
  for s = 0 to r.n - 1 do
    if r.vgroup.(s) < 0 then begin
      let q = Queue.create () in
      Queue.push s q;
      r.vgroup.(s) <- !c;
      r.vnote.(s) <- string_of_int !c;
      r.cur <- s;
      snap r 2 (sprintf "%s n'a pas de composante : on lance un parcours (composante %d)." (label r s) !c);
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        r.cur <- u;
        r.vclass.(u) <- 2;
        List.iter
          (fun (v, _) ->
            if r.vgroup.(v) < 0 then begin
              r.vgroup.(v) <- !c;
              r.vnote.(v) <- string_of_int !c;
              Queue.push v q;
              r.cedge <- Some (u, v);
              r.contents <- List.map (label r) (List.of_seq (Queue.to_seq q));
              snap r 3 (sprintf "%s est dans la composante %d." (label r v) !c)
            end)
          adj.(u);
        r.cedge <- None
      done;
      incr c;
      r.contents <- [];
      snap r 4 (sprintf "Composante terminée ; c ← %d." !c)
    end
  done;
  r.cur <- -1;
  snap r (-1) (sprintf "%d composante(s) connexe(s)." !c);
  finish r

(* ------------------------------------------------------------------ *)
(* Composantes fortement connexes (Kosaraju) *)

let scc_code =
  [|
    "1. parcours en profondeur complet de G, en notant l'ordre de fin";
    "2. construire le graphe transposé (arcs inversés)";
    "3. pour tout sommet s, par date de fin décroissante :";
    "       si s n'a pas de composante : explorer s dans le transposé";
    "       les sommets atteints forment une composante fortement connexe";
  |]

let scc g =
  let r = make g in
  if not g.Graph.directed then begin
    snap r (-1)
      "En non orienté, les composantes fortement connexes sont les composantes connexes.";
    finish r
  end
  else begin
    let _, finished = dfs_rec_full ~record:false g 0 in
    (* finished : dernier terminé en tête *)
    List.iteri (fun k u -> r.vnote.(u) <- sprintf "fin n°%d" (r.n - k)) finished;
    r.sname <- "Sommets par fin décroissante";
    r.contents <- List.map (label r) finished;
    snap r 0 "Premier parcours en profondeur : on note l'ordre dans lequel les sommets se terminent.";
    let rev = Array.make r.n [] in
    Array.iteri (fun i l -> List.iter (fun (j, w) -> rev.(j) <- (i, w) :: rev.(j)) l) g.Graph.edges;
    snap r 1 "On travaille maintenant sur le graphe transposé.";
    let c = ref 0 in
    let rec explore u =
      r.vgroup.(u) <- !c;
      r.vclass.(u) <- 2;
      r.cur <- u;
      snap r 4 (sprintf "%s rejoint la composante %d." (label r u) !c);
      List.iter
        (fun (v, _) ->
          if r.vgroup.(v) < 0 then begin
            set_edge r v u e_tree;
            explore v
          end)
        (List.sort compare rev.(u))
    in
    List.iteri
      (fun k s ->
        r.contents <- List.map (label r) (List.filteri (fun i _ -> i > k) finished);
        if r.vgroup.(s) < 0 then begin
          r.cur <- s;
          snap r 3 (sprintf "%s n'a pas de composante : exploration dans le transposé." (label r s));
          explore s;
          incr c
        end)
      finished;
    r.cur <- -1;
    snap r (-1) (sprintf "%d composante(s) fortement connexe(s)." !c);
    finish r
  end

(* ------------------------------------------------------------------ *)
(* Test de biparticité *)

let bipartite_code =
  [|
    "pour tout sommet s non coloré : couleur[s] ← 0 ; F ← file contenant s";
    "    tant que F n'est pas vide : u ← défiler(F)";
    "        pour tout voisin v de u :";
    "            si v n'est pas coloré : couleur[v] ← 1 − couleur[u] ; enfiler(F, v)";
    "            sinon si couleur[v] = couleur[u] : conflit, le graphe n'est pas biparti";
    "le graphe est biparti";
  |]

let bipartite g =
  let r = make g in
  let adj = undirected_neighbors g in
  let conflict = ref false in
  r.sname <- "File";
  snap r (-1) "Aucun sommet n'est coloré.";
  (try
     for s = 0 to r.n - 1 do
       if r.vgroup.(s) < 0 then begin
         let q = Queue.create () in
         r.vgroup.(s) <- 0;
         Queue.push s q;
         r.cur <- s;
         r.contents <- [ label r s ];
         snap r 0 (sprintf "%s reçoit la couleur 0." (label r s));
         while not (Queue.is_empty q) do
           let u = Queue.pop q in
           r.cur <- u;
           r.vclass.(u) <- 2;
           List.iter
             (fun (v, _) ->
               r.cedge <- Some (u, v);
               if r.vgroup.(v) < 0 then begin
                 r.vgroup.(v) <- 1 - r.vgroup.(u);
                 Queue.push v q;
                 set_edge r u v e_tree;
                 r.contents <- List.map (label r) (List.of_seq (Queue.to_seq q));
                 snap r 3 (sprintf "%s reçoit la couleur %d." (label r v) r.vgroup.(v))
               end
               else if r.vgroup.(v) = r.vgroup.(u) then begin
                 set_edge r u v e_conflict;
                 conflict := true;
                 snap r 4
                   (sprintf "%s et %s sont voisins et de même couleur : pas biparti." (label r u)
                      (label r v));
                 raise Exit
               end)
             adj.(u);
           r.cedge <- None
         done
       end
     done
   with Exit -> ());
  r.cur <- -1;
  if not !conflict then begin
    r.cedge <- None;
    snap r 5 "Aucun conflit : le graphe est biparti (les deux couleurs donnent la partition)."
  end;
  finish r

(* ------------------------------------------------------------------ *)
(* Coloration gloutonne (ordre de Welsh-Powell) *)

let coloring_code =
  [|
    "trier les sommets par degré décroissant";
    "pour tout sommet u dans cet ordre :";
    "    couleur[u] ← plus petite couleur non utilisée par ses voisins";
    "nombre de couleurs utilisées : k (majorant du nombre chromatique)";
  |]

let coloring g =
  let r = make g in
  let adj = undirected_neighbors g in
  let order =
    List.stable_sort
      (fun a b -> compare (List.length adj.(b)) (List.length adj.(a)))
      (List.init r.n (fun i -> i))
  in
  r.sname <- "Sommets restants (par degré décroissant)";
  r.contents <- List.map (fun i -> sprintf "%s (%d)" (label r i) (List.length adj.(i))) order;
  snap r 0 "Les sommets sont triés par degré décroissant.";
  let k = ref 0 in
  List.iteri
    (fun idx u ->
      let used = List.filter_map (fun (v, _) -> if r.vgroup.(v) >= 0 then Some r.vgroup.(v) else None) adj.(u) in
      let c = ref 0 in
      while List.mem !c used do incr c done;
      r.vgroup.(u) <- !c;
      r.vnote.(u) <- string_of_int !c;
      r.vclass.(u) <- 2;
      k := max !k (!c + 1);
      r.cur <- u;
      r.contents <-
        List.filteri (fun i _ -> i > idx) order
        |> List.map (fun i -> sprintf "%s (%d)" (label r i) (List.length adj.(i)));
      snap r 2
        (sprintf "%s : couleurs des voisins {%s}, il reçoit la couleur %d." (label r u)
           (String.concat ", " (List.map string_of_int (List.sort_uniq compare used)))
           !c))
    order;
  r.cur <- -1;
  snap r 3 (sprintf "%d couleur(s) utilisée(s)." !k);
  finish r

(* ------------------------------------------------------------------ *)
(* Floyd-Warshall *)

let floyd_code =
  [|
    "d[i][j] ← poids(i, j) s'il y a une arête, 0 si i = j, ∞ sinon";
    "pour k de 0 à n − 1 :";
    "    pour tout i, j : d[i][j] ← min(d[i][j], d[i][k] + d[k][j])";
  |]

let floyd g =
  let r = make g in
  let n = r.n in
  let d = Array.make_matrix n n None in
  for i = 0 to n - 1 do
    d.(i).(i) <- Some 0
  done;
  Array.iteri
    (fun i l ->
      List.iter
        (fun (j, w) ->
          d.(i).(j) <-
            (match d.(i).(j) with Some x when x <= w -> Some x | _ -> Some w))
        l)
    g.Graph.edges;
  r.table <-
    (fun () ->
      Array.append
        [| Array.append [| "" |] (Array.init n (label r)) |]
        (Array.init n (fun i ->
             Array.append [| label r i |] (Array.map string_of_dist d.(i)))));
  if n > 30 then begin
    r.table <- (fun () -> [||]);
    snap r (-1) "Graphe trop grand pour afficher la matrice (30 sommets au plus)."
  end
  else snap r 0 "Matrice initiale : poids des arêtes.";
  for k = 0 to n - 1 do
    let changed = ref 0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        match (d.(i).(k), d.(k).(j)) with
        | Some a, Some b -> (
            match d.(i).(j) with
            | Some c when c <= a + b -> ()
            | _ ->
                d.(i).(j) <- Some (a + b);
                incr changed)
        | _ -> ()
      done
    done;
    r.cur <- k;
    Array.iteri (fun i _ -> r.vclass.(i) <- if i < k then 2 else if i = k then 1 else 0) r.vclass;
    snap r 2
      (sprintf "Chemins passant par les sommets jusqu'à %s : %d case(s) améliorée(s)." (label r k)
         !changed)
  done;
  r.cur <- -1;
  Array.fill r.vclass 0 n 2;
  let neg = List.exists (fun i -> match d.(i).(i) with Some x -> x < 0 | None -> false) (List.init n (fun i -> i)) in
  snap r (-1)
    (if neg then "Un coefficient diagonal est négatif : il y a un cycle de poids négatif."
     else "Matrice des distances entre tous les couples de sommets.");
  finish r

(* ------------------------------------------------------------------ *)
(* Catalogue *)

type info = {
  id : string;
  name : string;
  code : string array;
  source : bool;  (** demande un sommet de départ *)
  weighted : bool;  (** utilise les poids *)
  description : string;
  run : (string, int) Graph.t -> int -> step list;
}

let catalogue =
  [
    { id = "bfs"; name = "Parcours en largeur (BFS)"; code = bfs_code; source = true; weighted = false;
      description = "Explore le graphe par couches successives à l'aide d'une file ; donne les distances en nombre d'arêtes depuis la source.";
      run = bfs };
    { id = "dfs"; name = "Parcours en profondeur (pile)"; code = dfs_stack_code; source = true; weighted = false;
      description = "Version itérative du parcours en profondeur : une pile remplace la file du parcours en largeur.";
      run = dfs_stack };
    { id = "dfsrec"; name = "Parcours en profondeur récursif"; code = dfs_rec_code; source = true; weighted = false;
      description = "Parcours complet avec dates de début et de fin, et classification des arêtes (arbre, arrière, avant, transverse). Une arête arrière révèle un cycle.";
      run = dfs_rec };
    { id = "dijkstra"; name = "Dijkstra"; code = dijkstra_code; source = true; weighted = true;
      description = "Plus courts chemins depuis la source pour des poids positifs : on traite à chaque étape le sommet non traité le plus proche.";
      run = dijkstra };
    { id = "bellman"; name = "Bellman-Ford"; code = bellman_ford_code; source = true; weighted = true;
      description = "Plus courts chemins avec des poids quelconques par relâchements successifs de toutes les arêtes ; détecte les cycles de poids négatif.";
      run = bellman_ford };
    { id = "floyd"; name = "Floyd-Warshall"; code = floyd_code; source = false; weighted = true;
      description = "Distances entre tous les couples de sommets par programmation dynamique sur les sommets intermédiaires autorisés.";
      run = (fun g _ -> floyd g) };
    { id = "prim"; name = "Prim"; code = prim_code; source = true; weighted = true;
      description = "Arbre couvrant de poids minimal construit en ajoutant à chaque étape le sommet le plus proche de l'arbre.";
      run = prim };
    { id = "kruskal"; name = "Kruskal"; code = kruskal_code; source = false; weighted = true;
      description = "Arbre couvrant de poids minimal : on examine les arêtes par poids croissant et on garde celles qui ne ferment pas de cycle (union-find).";
      run = (fun g _ -> kruskal g) };
    { id = "topo"; name = "Tri topologique (Kahn)"; code = topo_code; source = false; weighted = false;
      description = "Ordonne les sommets d'un graphe orienté sans cycle de sorte que tout arc aille vers la droite ; échoue s'il y a un cycle.";
      run = (fun g _ -> topo g) };
    { id = "cc"; name = "Composantes connexes"; code = components_code; source = false; weighted = false;
      description = "Un parcours par composante ; chaque composante reçoit une couleur.";
      run = (fun g _ -> components g) };
    { id = "scc"; name = "Composantes fortement connexes (Kosaraju)"; code = scc_code; source = false; weighted = false;
      description = "Deux parcours en profondeur : un sur le graphe, puis un sur le transposé par dates de fin décroissantes.";
      run = (fun g _ -> scc g) };
    { id = "bipartite"; name = "Test de biparticité"; code = bipartite_code; source = false; weighted = false;
      description = "Tente de colorer le graphe avec deux couleurs par un parcours en largeur ; un conflit prouve un cycle impair.";
      run = (fun g _ -> bipartite g) };
    { id = "coloring"; name = "Coloration gloutonne"; code = coloring_code; source = false; weighted = false;
      description = "Colore les sommets par degré décroissant (ordre de Welsh-Powell) avec la plus petite couleur disponible.";
      run = (fun g _ -> coloring g) };
  ]

(* ------------------------------------------------------------------ *)
(* Remplissage d'une grille (version web de flood.ml) *)

type flood_mode = Flood_stack | Flood_queue | Flood_random

(* Renvoie les cases dans l'ordre où elles sont coloriées. Comme dans
   flood.ml, une case est coloriée quand elle sort de la structure, et
   les voisins sont ajoutés dans un ordre aléatoire. *)
let flood_order w h (wall : bool array) start mode =
  let painted = Array.copy wall in
  let order = ref [] in
  let shuffle l =
    let a = Array.of_list l in
    for i = Array.length a - 1 downto 1 do
      let j = Random.int (i + 1) in
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    done;
    Array.to_list a
  in
  let neighbours c =
    let x = c mod w and y = c / w in
    List.filter_map
      (fun (dx, dy) ->
        let x' = x + dx and y' = y + dy in
        if 0 <= x' && x' < w && 0 <= y' && y' < h && not painted.((y' * w) + x') then
          Some ((y' * w) + x')
        else None)
      [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
    |> shuffle
  in
  let paint c =
    if painted.(c) then []
    else begin
      painted.(c) <- true;
      order := c :: !order;
      neighbours c
    end
  in
  (if not wall.(start) then
     match mode with
     | Flood_stack ->
         let s = Stack.create () in
         Stack.push start s;
         while not (Stack.is_empty s) do
           List.iter (fun c -> Stack.push c s) (paint (Stack.pop s))
         done
     | Flood_queue ->
         let q = Queue.create () in
         Queue.push start q;
         while not (Queue.is_empty q) do
           List.iter (fun c -> Queue.push c q) (paint (Queue.pop q))
         done
     | Flood_random ->
         (* un sac : on tire un élément au hasard *)
         let bag = ref [| start |] and size = ref 1 in
         while !size > 0 do
           let k = Random.int !size in
           let c = !bag.(k) in
           !bag.(k) <- !bag.(!size - 1);
           decr size;
           List.iter
             (fun c' ->
               if !size >= Array.length !bag then
                 bag := Array.append !bag (Array.make (Array.length !bag) 0);
               !bag.(!size) <- c';
               incr size)
             (paint c)
         done);
  Array.of_list (List.rev !order)
