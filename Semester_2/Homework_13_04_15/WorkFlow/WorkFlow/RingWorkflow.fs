module RingWorkflow
let (/) x y =
   match y with
   | 0 -> None
   | _ -> Some (x / y)

let (%) x y =
   match y with
   | 0 -> None
   | _ -> Some (x % y)

let optPlus (x : int option) (y : int option) = 
  match x, y with
  | _, None -> x
  | None, Some a -> Some a
  | Some a, Some b -> Some (a + b)
type RingBuilder(n) =
    member this.Bind(m : int option, f) = Option.bind f m
    member this.Bind(m : int, f) = 
      let m = Some m
      Option.bind f m
    member this.Return(x) = 
      let mutable res = None
      if (x < 0) && (x % n <> Some 0) then 
        res <- optPlus (x % n) (Some n)
      else
        res <- x % n
      res
    member this.ReturnFrom(x) = 
      match x with
      | Some x -> x % n
      | None -> None
    member this.Combine (a, b) = 
        match a, b with
        | Some x, Some y ->
            Some (x + y)
        | Some x, None ->
            Some x
        | None, Some y ->
            Some y
        | None, None ->
            None
    member this.Delay(f) = f ()