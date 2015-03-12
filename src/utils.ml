let concat_list f ls =
  List.concat (List.map f ls)

let opt f = function
  | None -> None
  | Some a -> Some(f a)


let rec fold f env = function
  | [] -> ([], env)
  | x::xs ->
    let (x, env)   = f env x in
    let (xs, env)  = fold f env xs in
    (x::xs, env)
