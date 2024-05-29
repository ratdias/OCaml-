exception KeepLooking
type position = int*int
let starting_point = (2,3)
let goal pos =  ((pos = (3,5))||(pos = (5,1))) 

let maze_moves (pos: position): position list =
  match pos with
  |(1,1) -> [(2,1)] 
  |(1,2) -> [(2,2); (1,3)]
  |(1,3) -> [(1,2); (2,3); (1,4)] 
  |(1,4) -> [(1,3); (1,5)]  
  |(1,5) -> [(1,4); (2,5)]
  |(2,1) -> [(3,1); (1,1)]  
  |(2,2) -> [(3,2); (1,2)]  
  |(2,3) -> [(1,3)]
  |(2,4) -> [(2,5); (3,4)]
  |(2,5) -> [(1,5); (2,4)]
  |(3,1) -> [(2,1); (3,2)]
  |(3,2) -> [(3,1); (2,2); (4,2); (3,3)]
  |(3,3) -> [(3,4); (3,2); (4,3)]
  |(3,4) -> [(2,4); (4,4); (3,3)]
  |(3,5) -> [(4,5)]  
  |(4,1) -> [(4,2)]
  |(4,2) -> [(3,2); (4,1)]
  |(4,3) -> [(3,3); (5,3)]
  |(4,4) -> [(3,4); (4,5)]
  |(4,5) -> [(4,4); (3,5); (5,5)]
  |(5,1) -> [(5,2)] 
  |(5,2) -> [(5,3); (5,1)]  
  |(5,3) -> [(4,3); (5,2); (5,4)]
  |(5,4) -> [(5,3)]
  |(5,5) -> [(4,5)]  
  |_ ->  raise (Invalid_argument "invalid argument")


let check_possible_moves (possible_moves: position list) (path_so_far: position list): position list =
  List.filter (fun p -> Bool.not (List.mem p path_so_far)) possible_moves


let check_excp f x y =
  try ignore (f x y);
    false
  with _ -> true

let maze () : position list option =
  let rec mazing cur_pos path_so_far =
    if goal cur_pos then Some path_so_far
    else (
      match check_possible_moves (maze_moves cur_pos) path_so_far with
      |[] ->raise KeepLooking
      |a::[] -> mazing a (path_so_far@[a])
      |a::b::[] -> if check_excp mazing a (path_so_far@[a]) then mazing b (path_so_far@[b]) else mazing a (path_so_far@[a])
      |a::b::c::[] -> if check_excp mazing a (path_so_far@[a]) then 
                      (if check_excp mazing b (path_so_far@[b]) then mazing c (path_so_far@[c]) else mazing b (path_so_far@[b]))
                      else mazing a (path_so_far@[a])
    )
    in try mazing starting_point [(2,3)] with
    |KeepLooking -> None
    
