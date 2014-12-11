(* hw4 *)

type treasure =
  | StarBox
  | NameBox of string
type key =
  | Bar
  | Node of key * key
type map =
  | End of treasure
  | Branch of map * map
  | Guide of string * map
exception IMPOSSIBLE

type akey =
  | ABar
  | Box of string
  | Pair of akey * akey
  | Left of akey
  | Right of akey
let left k =
  match k with
  | Pair (l, r) -> l
  | ABar -> raise IMPOSSIBLE
  | _ -> Left k
let right k =
  match k with
  | Pair (l, r) -> r
  | ABar -> raise IMPOSSIBLE
  | _ -> Right k
let pair (l, r) =
  match l with
  | Left lk -> (
    match r with
    | Right rk -> (
      if lk = rk then lk
      else Pair (l, r)
    )
    | _ -> Pair (l, r)
  )
  | _ -> Pair (l, r)

let add_unique l x =
  if List.mem x l then l
  else x::l

let conditions m =
  let rec amsi m c n =
    match m with
    | End (StarBox) -> (ABar, c, (add_unique n None))
    | End (NameBox x) ->
      let k = (Box x) in
      (k, c, (add_unique n (Some x)))
    | Guide (x, m) ->
      let a = (Box x) in
      let (b, c, n) = amsi m c n in
      (pair (a, b), c, n)
    | Branch (p, q) ->
      let (a, c, n) = amsi q c n in
      let (ab, c, n) = amsi p c n in
      let c = (left ab, a)::c in
      (right ab, c, n)
  in
  let (_, c, n) = amsi m [] [] in
  (c, n)

let sgm c n =
  List.fold_left
    (
      fun c m ->
        match m with
        | None -> (ABar, ABar)::c
        | Some x -> (Box x, Box x)::c
    )
    c
    n

type anum =
  | O
  | I
  | V of int * int

let append l x =
  List.rev (x::(List.rev l))

let replace_nth l i x =
  let j = ref (-1) in
  List.rev (List.rev_map (fun y -> j := !j + 1; if !j = i then x else y) l)

let numbering c =
  let rec num1 k l e n =
    match k with
    | ABar -> (0, l, e, (add_unique n None))
    | Box x -> (
      let i = e x in
      match i with
      | None -> (
        let i = List.length l in
        let l = append l I in
        let e = fun y -> if y = x then Some i else e y in
        (i, l, e, (add_unique n (Some x)))
      )
      | Some i -> (i, l, e, n)
    )
    | Pair (kl, kr) -> (
      let (kli, l, e, n) = num1 kl l e n in
      let (kri, l, e, n) = num1 kr l e n in
      let i = List.length l in
      let l = append l (V (kli, kri)) in
      (i, l, e, n)
    )
    | Left k -> (
      let (ki, l, e, n) = num1 k l e n in
      let kn = List.nth l ki in
      match kn with
      | O -> raise IMPOSSIBLE
      | I -> (
        let kli = List.length l in
        let l = append l I in
        let kri = List.length l in
        let l = append l I in
        let l = replace_nth l ki (V (kli, kri)) in
        (kli, l, e, n)
      )
      | V (p, q) -> (p, l, e, n)
    )
    | Right k -> (
      let (ki, l, e, n) = num1 k l e n in
      let kn = List.nth l ki in
      match kn with
      | O -> raise IMPOSSIBLE
      | I -> (
        let kli = List.length l in
        let l = append l I in
        let kri = List.length l in
        let l = append l I in
        let l = replace_nth l ki (V (kli, kri)) in
        (kri, l, e, n)
      )
      | V (p, q) -> (q, l, e, n)
    )
  in
  let num2 (k1, k2) l e c n =
    let (i1, l, e, n) = num1 k1 l e n in
    let (i2, l, e, n) = num1 k2 l e n in
    (l, e, (i1, i2)::c, n)
  in
  List.fold_left
    (fun (l, e, c, n) k -> num2 k l e c n)
    ([O], (fun y -> None), [], [])
    c

let consume l c =
  let rec con0 l c m =
    let rec con1 l (i1, i2) m =
      if i1 = i2 then
        (l, [])
      else if List.mem (i1, i2) m then
        raise IMPOSSIBLE
      else (
        let n1 = List.nth l i1 in
        let n2 = List.nth l i2 in
        match n1 with
        | O -> (
          match n2 with
          | O -> (l, [])
          | I -> (replace_nth l i2 n1, [])
          | V (p2, q2) -> raise IMPOSSIBLE
        )
        | I -> (
          match n2 with
          | O -> (replace_nth l i1 n2, [])
          | I -> (l, [(i1, i2)])
          | V (p2, q2) -> (replace_nth l i1 n2, [])
        )
        | V (p1, q1) -> (
          match n2 with
          | O -> raise IMPOSSIBLE
          | I -> (replace_nth l i2 n1, [])
          | V (p2, q2) -> (
            let (l, d1) = con1 l (p1, p2) ((i1, i2)::m) in
            let (l, d2) = con1 l (q1, q2) ((i1, i2)::m) in
            (l, List.rev_append d1 d2)
          )
        )
      )
    in
    match c with
    | [] -> l
    | h::t -> (
      if List.mem (l, c) m then
        l
      else (
        let m = (l, c)::m in
        let (l, d) = con1 l h [] in
        let c = (List.rev_append (List.rev t) d) in
        con0 l c m
      )
    )
  in
  con0 l c []

let construct l e n =
  let rec key i m =
    if List.mem i m then
      raise IMPOSSIBLE
    else (
      let k = List.nth l i in
      match k with
      | O -> Bar
      | I -> Bar
      | V (p, q) -> Node (key p (i::m), key q (i::m))
    )
  in
  List.rev_map
    (
      fun b -> match b with
      | None -> Bar
      | Some x -> (
        let io = e x in
        match io with
        | None -> raise IMPOSSIBLE
        | Some i -> key i []
      )
    )
    n

let unique k =
  let rec uni k l =
    match k with
    | [] -> l
    | h::t -> (
      let p = List.hd l in
      if p = h then
        uni t l
      else
        uni t (h::l)
    )
  in
  let k = List.sort compare k in
  match k with
  | [] -> []
  | h::t -> List.rev(uni t [h])

let getReady m =
  let (c, n) = conditions m in
  let (l, e, c, n) = numbering (sgm c n) in
  let l = consume l c in
  let k = construct l e n in
  unique k
