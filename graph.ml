type 'a t = { 
    vtx: 'a array;
    mat: bool array array
}

let nvertices g = Array.length g.vtx
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

let mobius n = {
    vtx = Array.init n (fun i -> i);
    mat = init_matrix n n (fun i j -> 
        (i+1) mod n = j || (i+n/2) mod n = j)
}

let cycle n = {
    vtx = Array.init n (fun i -> i);
    mat = init_matrix n n (fun i j -> (i+1) mod n = j)
}

let rec count_bit n =
    if n = 0 then 0
    else (n mod 2) + count_bit (n/2)

let hypercube n = 
    let p = 1 lsl n in
    {
        vtx = Array.init p (fun i -> i);
        mat = init_matrix p p (fun i j -> count_bit (i lxor j) = 1)
    }

let divisors n =
    {
        vtx = Array.init n (fun i -> i+1);
        mat = init_matrix n n (fun i j ->
            i < j && (j+1) mod (i+1) == 0)
    }

type search_trace = {
    source : int;
    steps : search_step list
} and search_step = {
    visited : int list;
    to_visit : int list;
    current : int;
    pred : (int * int) list
}

type ('a, 'b) search_structure = {
    init : unit -> 'b;
    take : 'b -> 'a;
    add : 'a -> 'b  -> unit;
    is_empty : 'b -> bool
}

let stack_search = {
    init = Stack.create;
    take = Stack.pop;
    add = Stack.push;
    is_empty = Stack.is_empty
}

let queue_search = {
    init = Queue.create;
    take = Queue.take;
    add = Queue.push;
    is_empty = Queue.is_empty
}

let search g x ss =
    let to_visit = ss.init () in
    let n = Array.length g.vtx in
    let visited = Array.make n false in
    let pred = Array.make n None in

    ss.add x to_visit;
    while not (ss.is_empty to_visit) do
        let x = ss.take to_visit in
        if not visited.(x)
        then begin
            visited.(x) <- true;
            for i = 0 to n-1 do
                if g.mat.(x).(i) && pred.(i) = None
                then begin
                    pred.(i) <- Some x;
                    ss.add i to_visit
                end
            done
        end
    done;
    pred



