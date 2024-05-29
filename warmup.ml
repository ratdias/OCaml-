type formula = And  of formula * formula
                | Implies of formula * formula
                | Or of formula * formula
                | Not of formula 
                | Prop of string
                | Bool of bool

type subst = (string * bool) list

let show_list (show: 'a -> string) (lst: 'a list) : string =
  let rec sl lst =
    match lst with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl lst ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst (s: subst) : string = show_list show_string_bool_pair s

let explode (s: string) : char list =
  let len = String.length s in
  let rec f i = if i = len then [] else s.[i] :: f (i+1) in
  f 0

let rm_duplicates (lst: 'a list) : 'a list =
  let collect_unique elem to_keep =
    if List.mem elem to_keep then to_keep else elem::to_keep
  in List.fold_right collect_unique lst []

let rec find_sub (s : string) (sub : subst) =
  match sub with
  | (str, bo) :: xs when String.equal s str -> bo
  | (str, bo) :: xs -> find_sub s xs
  | _ -> raise (Invalid_argument "invalid argument")

let rec eval (form : formula) (sub : subst): bool =
  match form with 
  |Bool b -> b
  |Prop s -> find_sub s sub
  |Not a -> if (eval a sub) then false else true
  |And (a,b) ->  (eval a sub) && (eval b sub)
  |Or (a,b) -> (eval a sub) || (eval b sub)
  |Implies (a, b) ->  if (eval a sub )then false || (eval b sub) else true || (eval b sub)


let unique_cons y ys = if List.mem y ys then ys else y :: ys

let remove_from_right xs = List.fold_right unique_cons xs []

let rec freevarsHelp (form : formula): string list = 
  match form with 
  |Prop s -> [s]
  |Bool b -> []
  |And (a,b) | Or (a,b) | Implies (a, b) -> freevarsHelp a @ freevarsHelp b
  |Not a  -> freevarsHelp a

let freevars (form : formula): string list = remove_from_right (freevarsHelp form)

let rec bool_comb x so_far =
  if x = 0 then so_far
  else
    let new_so_far = List.concat (List.map (fun l -> [true :: l; false :: l]) so_far) in
    bool_comb (x - 1) new_so_far

let all_bool_combinations x = bool_comb x [[]]

let rec assign_bools (b_lst: bool list) (s_lst: string list) (so_far: subst): subst =
  match b_lst, s_lst with 
  |[],[]->  so_far
  |x::xs, s::ss -> assign_bools xs ss ((s, x) :: so_far)
  |_,_ -> raise (Invalid_argument "invalid argument")
  

let tautology_checker (f:formula) : subst option =
  let rec all_possible_subst blst slst subslst =
    match blst with
    |[] -> subslst
    |x::xs -> all_possible_subst xs slst ((assign_bools x slst []) :: subslst)
  in
  let rec look_for_falses form sublst=
    match sublst with
    |[] -> None
    |sub::subss -> if Bool.not(eval form sub) then Some sub
                  else look_for_falses form subss
    in
    look_for_falses f (all_possible_subst (all_bool_combinations (List.length (freevars f))) (freevars f) [])

let t2 = Or (Implies (Prop "P", Prop "Q"), Implies (Prop "Q", Prop "P"))

let nt1 = Or (Prop "P", Prop "Q")
