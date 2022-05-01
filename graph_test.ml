let _ =
    let g = Graph.grid 10 in
    let d = Graph.diameter g in
    Printf.printf "%d\n%!" d
