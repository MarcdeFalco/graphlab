type 'a t = { 
    vtx: 'a array;
    mat: bool array array
}

let nvertices g = Array.length g.vtx
let nedges g =
    let n = nvertices g in
    let c = ref 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            if g.mat.(i).(j)
            then incr c
        done
    done;
    !c

let average_degree g =
    let n = nvertices g in
    let p = nedges g in
    float_of_int p /. float_of_int n

let init_matrix n p f =
    Array.init n
        (fun i -> Array.init p (f i))
        
let symetric g =
    let n = nvertices g in
    for i = 0 to n - 1 do
        for j = 0 to n-1 do
            if g.mat.(i).(j)
            then g.mat.(j).(i) <- true
        done
    done

let grid n = {
    vtx = Array.init (n*n) (fun i ->
            Printf.sprintf "%d,%d"
            (i mod n) (i / n));
    mat = init_matrix (n*n) (n*n) (fun i j ->
        let x, y = i mod n, i / n in
        let x', y' = j mod n, j / n in
           (x = x' && abs (y - y') = 1)
        || (y = y' && abs (x - x') = 1))
}
        

let mobius n = {
    vtx = Array.init n (fun i -> string_of_int i);
    mat = init_matrix n n (fun i j -> 
        (i+1) mod n = j || (i+n/2) mod n = j)
}

let cycle n = {
    vtx = Array.init n (fun i -> string_of_int i);
    mat = init_matrix n n (fun i j -> (i+1) mod n = j)
}

let complet n = {
    vtx = Array.init n (fun i -> string_of_int i);
    mat = init_matrix n n (fun i j -> i <> j)
}

let rec count_bit n =
    if n = 0 then 0
    else (n mod 2) + count_bit (n/2)

let hypercube n = 
    let p = 1 lsl n in
    {
        vtx = Array.init p (fun i -> string_of_int i);
        mat = init_matrix p p (fun i j -> count_bit (i lxor j) = 1)
    }

let divisors n =
    {
        vtx = Array.init n (fun i -> string_of_int (i+1));
        mat = init_matrix n n (fun i j ->
            i < j && (j+1) mod (i+1) == 0)
    }

let exemple =
    {
        vtx = Array.init 6 (fun i -> Printf.sprintf "%c" (char_of_int
        (int_of_char 'a' + i)));
        mat = let m = Array.make_matrix 6 6 false in
            m.(0).(1) <- true;
            m.(0).(2) <- true;
            m.(1).(3) <- true;
            m.(1).(4) <- true;
            m.(2).(1) <- true;
            m.(2).(5) <- true;
            m.(3).(4) <- true;
            m.(5).(0) <- true;
            m
    }

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

    for i = 0 to n-1 do
        if g.mat.(x).(i) && (dir || edge_status.(i).(x) = NoStatus)
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
        end
    done;

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

    let unpack o = match o with
        | None -> failwith "Argh"
        | Some v -> v in
    let expr = Array.make (2*n) "" in
    for i = 0 to n-1 do
        expr.( unpack entry_time.(i) ) <- g.vtx.(i);
        expr.( unpack exit_time.(i) ) <- g.vtx.(i)
    done;
    Printf.printf "%s\n%!"
        (String.concat ""
            (Array.to_list expr));

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
            for i = 0 to n-1 do
                if g.mat.(x).(i) && status.(i) = Unknown
                then begin
                    parent.(i) <- Some x;
                    status.(i) <- Discovered;
                    ss.add i to_visit
                end
            done;
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
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            s := !s ^ (if g.mat.(i).(j) then "1 " else "0 ")
        done;
        s := !s ^ "\n"
    done;
    !s

let text_ladj g =
    let s = ref "" in
    let n = Array.length g.vtx in
    for i = 0 to n-1 do
        s := !s ^ Printf.sprintf "%d : [" i;
        for j = 0 to n-1 do
            if g.mat.(i).(j)
            then s := !s ^ Printf.sprintf "%d," j
        done;
        s := !s ^ "]\n"
    done;
    !s


