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
        mat = init_matrix p p (fun i j ->
            i < j && count_bit (i lxor j) = 1)
    }

let divisors n =
    {
        vtx = Array.init n (fun i -> i+1);
        mat = init_matrix n n (fun i j ->
            i < j && (j+1) mod (i+1) == 0)
    }

