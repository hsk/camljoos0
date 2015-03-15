let opt f = function
  | None -> None
  | Some a -> Some(f a)
