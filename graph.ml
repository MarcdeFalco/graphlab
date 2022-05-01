type ('a,'b) t = { 
    directed : bool;
    vtx: 'a array;
    edges : (int * 'b) list array
}

let nvertices g = Array.length g.vtx
let nedges g =
    let n = nvertices g in
    let c = ref 0 in
    for i = 0 to n-1 do
        c := !c + List.length g.edges.(i)
    done;
    !c

let connected g i j =
    List.mem j (List.map fst g.edges.(i))

let add_edge g i j w =
    g.edges.(i) <- (j,w) :: g.edges.(i);
    if not g.directed
    then g.edges.(j) <- (i,w) :: g.edges.(j)

let add_edges g l =
    List.iter (fun (i,j,w) -> add_edge g i j w) l

let create vtx directed edges = 
    let g = {
    directed = directed;
    vtx = vtx;
    edges = Array.make (Array.length vtx) []
    } in
    add_edges g edges;
    g

let del_edge g i j =
    g.edges.(i) <- List.filter (fun (k,_) -> k <> j) g.edges.(i);
    if not g.directed
    then g.edges.(j) <- List.filter (fun (k,_) -> k <> i) g.edges.(j)

let average_degree g =
    let n = nvertices g in
    let p = nedges g in
    float_of_int p /. float_of_int n
        
let edges_list_from_incidence directed n f =
    List.filter (fun (a,b,c) -> f (a,b)) 
        (List.concat (List.init n (fun i ->
        List.init (if directed then n else i) (fun j -> (i, j, 1)))))

let grid n =
    let vtx = Array.init (n*n) (fun i ->
            Printf.sprintf "%d,%d"
            (i mod n) (i / n)) in
    create vtx false (edges_list_from_incidence false (n*n)
        (fun (i, j) ->
            let x, y = i mod n, i / n in
            let x', y' = j mod n, j / n in
               (x = x' && abs (y - y') = 1)
            || (y = y' && abs (x - x') = 1)))

let mobius n = 
    let vtx = Array.init n (fun i -> string_of_int i) in
    create vtx false (edges_list_from_incidence false n
        (fun (i, j) ->
            (i+1) mod n = j || (i+n/2) mod n = j))

let cycle n =
    let vtx = Array.init n (fun i -> string_of_int i) in
    create vtx false (edges_list_from_incidence false n
        (fun (i, j) -> (i+1) mod n = j))

let complet n =
    let vtx = Array.init n (fun i -> string_of_int i) in
    create vtx false (edges_list_from_incidence false n
        (fun (i, j) -> i <> j))

let rec count_bit n =
    if n = 0 then 0
    else (n mod 2) + count_bit (n/2)

let hypercube n = 
    let p = 1 lsl n in
    let vtx = Array.init p (fun i -> string_of_int i) in
    create vtx false (edges_list_from_incidence false p
        (fun (i, j) -> count_bit (i lxor j) = 1))

let divisors n =
    let vtx = Array.init n (fun i -> string_of_int (i+1)) in
    create vtx true (edges_list_from_incidence true n
        (fun (i, j) ->
            i < j && (j+1) mod (i+1) == 0))

let exemple =
    let vtx = Array.init 6 (fun i -> Printf.sprintf "%c" (char_of_int
        (int_of_char 'a' + i))) in
    create vtx true [ (0,1,1); (0,2,1); (1,3,1); 
            (1,4,1); (2,1,1); (2,5,1);
            (3,4,1); (5,0,1) ]

type status = Discovered | Unknown | Processed
type edge_status = Tree | Back | Forward | Cross | NoStatus

type search_step = {
    current : int;
    parent : int option array;
    entry_time : int option array;
    exit_time : int option array;
    status : status array;
    edge_status : edge_status array array
}

type search_trace = {
    source : int;
    steps : search_step list
}

type ('a, 'b) search_structure = {
    init : unit -> 'b;
    take : 'b -> 'a;
    add : 'a -> 'b  -> unit;
    is_empty : 'b -> bool;
    to_list : 'b -> 'a list
}

let stack_search = {
    init = Stack.create;
    take = Stack.pop;
    add = Stack.push;
    is_empty = Stack.is_empty;
    to_list = fun s -> List.of_seq (Stack.to_seq s)
}

let queue_search = {
    init = Queue.create;
    take = Queue.take;
    add = Queue.push;
    is_empty = Queue.is_empty;
    to_list = fun s -> List.of_seq (Queue.to_seq s)
}

let matrix_copy m =
    let n = Array.length m in
    Array.init n (fun i -> Array.copy m.(i))

let rec dfs_rec dir g status edge_status parent entry_time exit_time x time =
    let n = Array.length g.vtx in
    entry_time.(x) <- Some !time;
    incr time;
    status.(x) <- Discovered;

    let step = {
        status = Array.copy status;
        current = x;
        entry_time = Array.copy entry_time;
        exit_time = Array.copy exit_time;
        edge_status = matrix_copy edge_status;
        parent = Array.copy parent
    } in
    let steps = ref [step] in

    List.iter (fun (i, _) ->
        if (g.directed || edge_status.(i).(x) = NoStatus)
        then begin
            match status.(i) with
            | Unknown -> begin
                parent.(i) <- Some x;
                edge_status.(x).(i) <- Tree;
                steps := (dfs_rec dir g status edge_status parent entry_time exit_time i time) @ !steps
            end
            | Discovered -> edge_status.(x).(i) <- Back
            | Processed when entry_time.(x) < entry_time.(i)
                -> edge_status.(x).(i) <- Cross
            | Processed 
                -> edge_status.(x).(i) <- Forward
        end) g.edges.(x);

    exit_time.(x) <- Some !time;
    incr time;
    status.(x) <- Processed;

    let step = {
        status = Array.copy status;
        current = x;
        entry_time = Array.copy entry_time;
        exit_time = Array.copy exit_time;
        edge_status = matrix_copy edge_status;
        parent = Array.copy parent
    } in
    step :: !steps

let search_rec dir g src =
    let n = Array.length g.vtx in
    let status = Array.make n Unknown in
    let parent = Array.make n None in
    let entry_time = Array.make n None in
    let exit_time = Array.make n None in
    let edge_status = Array.make_matrix n n NoStatus in

    let steps = dfs_rec dir g status edge_status 
        parent entry_time exit_time src (ref 0) in
    {
        steps = List.rev steps;
        source = src
    }


let search_with_struct g src ss =
    let to_visit = ss.init () in
    let n = Array.length g.vtx in
    
    let status = Array.make n Unknown in
    let parent = Array.make n None in
    let edge_status = Array.make_matrix n n NoStatus in

    let entry_time = Array.make n None in
    let exit_time = Array.make n None in

    let steps = ref [] in

    ss.add src to_visit;
    while not (ss.is_empty to_visit) do
        let x = ss.take to_visit in
        if status.(x) <> Processed
        then begin
            status.(x) <- Processed;
            List.iter (fun (i,_) ->
                if status.(i) = Unknown
                then begin
                    parent.(i) <- Some x;
                    status.(i) <- Discovered;
                    ss.add i to_visit
                end) g.edges.(x);

            let step = {
                status = Array.copy status;
                current = x;
                entry_time = Array.copy entry_time;
                exit_time = Array.copy exit_time;
                edge_status = matrix_copy edge_status;
                parent = Array.copy parent
            } in
            steps := step :: !steps
        end
    done;
    {
        steps = List.rev !steps;
        source = src
    }

type search_type = DFS | BFS | DFS_rec

let search dir st g src =
    match st with
    | DFS -> search_with_struct g src stack_search
    | DFS_rec -> search_rec dir g src 
    | BFS -> search_with_struct g src queue_search

let text_matrix g = 
    let s = ref "" in
    let n = Array.length g.vtx in
    for _ = 0 to n-1 do
        (*
        List.iter (fun j ->
            s := !s ^ (if g.mat.(i).(j) then "1 " else "0 ")) g.edges.(i);
            *)
        s := !s ^ "\n"
    done;
    !s

let text_ladj g =
    let s = ref "" in
    let n = Array.length g.vtx in
    for i = 0 to n-1 do
        s := !s ^ Printf.sprintf "%d : [" i;
        List.iter (fun (j,_) ->
            s := !s ^ Printf.sprintf "%d," j) g.edges.(i);
        s := !s ^ "]\n"
    done;
    !s

let adjacency_matrix g =
    let n = Array.length g.vtx in
    Array.init n (fun i ->
        let a = Array.make n None in
        List.iter (fun (j,w) -> a.(j) <- Some w) g.edges.(i);
        a)

let add_opt o1 o2 =
    match o1, o2 with
    | Some v1, Some v2 -> Some (v1 + v2)
    | _ -> None
    
let min_opt o1 o2 =
    match o1, o2 with
    | Some v1, Some v2 -> Some (min v1 v2)
    | Some v1, _ -> Some v1
    | _, Some v2 -> Some v2
    | _ -> None

let max_opt o1 o2 =
    match o1, o2 with
    | Some v1, Some v2 -> Some (max v1 v2)
    | Some v1, _ -> Some v1
    | _, Some v2 -> Some v2
    | _ -> None

let floyd_warshall g =
    let n = Array.length g.vtx in
    let m = adjacency_matrix g in
    for k = 0 to n-1 do
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                m.(i).(j) <- min_opt m.(i).(j) (add_opt m.(i).(k) m.(k).(j))
            done
        done
    done;
    m

let copy g =
    {
        directed = g.directed;
        vtx = Array.copy g.vtx;
        edges = Array.copy g.edges
    }

let symetrize g =
    let g = copy g in
    Array.iteri (fun i a ->
        List.iter (fun (j,w) ->
            if not (connected g j i)
                then g.edges.(j) <- (i,w) :: g.edges.(j)) a) g.edges;
    g

let diameter g = 
    let m = floyd_warshall (symetrize g) in
    let fmax = Array.fold_left max_opt None in
    match fmax (Array.map fmax m) with
    | Some v -> v
    | None -> 0

