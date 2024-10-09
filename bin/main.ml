(* let () = print_endline "Hello, World!" *)

(* définition type pterm *)

type pterm =
  | Var of string
  | App of pterm * pterm
  | Abs of string * pterm

let rec print_term (t : pterm) : string =
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"

let compteur_var : int ref = ref 0

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "X" ^ (string_of_int !compteur_var)

let rec alphaconv (t : pterm) (tab : (string * string) list) : pterm =
  match t with
  | Var x -> (
      try Var (List.assoc x tab)
      with Not_found -> Var x)
  | App (t1, t2) -> App (alphaconv t1 tab, alphaconv t2 tab)
  | Abs (x, t) ->
      let new_var = nouvelle_var () in
      let new_tab = (x, new_var) :: tab in
      Abs (new_var, alphaconv t new_tab)

(* Exemple de test *)
let term = Abs ("x", App (Abs ("y", App (Var "x", Var "y")), Var "x"))
let term1 = Var "y"
let () = print_endline (print_term term1)
let () = print_endline (print_term term)
let term_alphaconv = alphaconv term []

(* Afficher le terme après alpha-conversion *)
let () =
  print_endline (print_term term_alphaconv)




let rec substitution (x : string) (n: pterm) (t : pterm) : pterm =
  match t with
  | Var y -> 
      if y = x then n  (* Remplace par n si y est libre et égal à x *)
      else t           (* Sinon, retourne t tel quel *)
  | App (t1, t2) -> 
      App (substitution x n t1, substitution x n t2)  (* Applique la substitution aux deux sous-terms *)
  | Abs (y, body) ->
      if y = x then 
        t  (* Si y est lié, ne remplace pas pour éviter la capture *)
      else 
        Abs (y, substitution x n body)  (* Applique la substitution dans le corps de l'abstraction *)

(* Exemple d'utilisation *)
let term = App (Abs ( "x", Abs ("y", Abs ("x", Abs ( "t", App (Var "r", Var "s"))))), Var "x")
let substituted_term = substitution "x" (Var "n") term
let () = print_endline (print_term substituted_term)




let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None  (* Les variables seules ne sont pas réduites *)
  
  | Abs (_, _) -> None  (* Une abstraction seule ne se réduit pas dans CbV *)
  
  | App (Abs (x, body), v) when is_value v ->
      (* Application d'une abstraction à une valeur : applique la substitution *)
      Some (substitution x v body)
  
  | App (v1, t2) when is_value v1 ->
      (* Le premier terme est une valeur, alors on réduit le second terme *)
      (match ltr_cbv_step t2 with
      | Some t2' -> Some (App (v1, t2'))
      | None -> None)
  
  | App (t1, t2) ->
      (* On réduit le premier terme s'il n'est pas encore une valeur *)
      (match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> None)

(* Fonction auxiliaire pour vérifier si un terme est une valeur *)
and is_value (t : pterm) : bool =
  match t with
  | Abs (_, _) -> true
  | _ -> false


  
  
  (* 6. Ecrire une fonction de normalisation  *)
  (* Supposons que ltr_cbv_step est déjà définie *)
  
  let rec ltr_cbv_norm (t : pterm) : pterm =
    match ltr_cbv_step t with
    | Some reduced_term -> ltr_cbv_norm reduced_term  (* Réduire récursivement *)
    | None -> t  (* Aucun pas de réduction possible, retourne le terme normalisé *)
    
    (* Exemple d'utilisation *)
    let term = App (Abs ("z", Abs("t", Var "z")), Abs ("v", Var "z"))
    let reduced_term = ltr_cbv_norm term
    
    let () = print_endline (print_term reduced_term)
    (*
    let term = App ( App (Abs ("x", Var "x"), Abs ("y", Var "y")), Abs ("z", Var "z")) (* Exemple de terme *)
    
    let normalized_term = ltr_cbv_norm term
    
let () = print_endline (print_term normalized_term)


exception Timeout

let rec ltr_cbv_norm_timeout (t : pterm) (timeout : int) : pterm =
  let rec aux t timeout =
    if timeout <= 0 then raise Timeout  (* Vérifier le timeout *)
    else
      match ltr_cbv_step t with
      | Some reduced_term -> aux reduced_term (timeout - 1)  (* Réduire et décrémenter le timeout *)
      | None -> t  (* Aucun pas de réduction possible, retourne le terme normalisé *)
  in
  aux t timeout

  
let () = 
  try
    let term = App ( Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y"))) in (* Exemple de terme *)
    let normalized_term = ltr_cbv_norm_timeout term 100 in
    print_endline (print_term normalized_term)
  with
  | Timeout -> print_endline "La normalisation a échoué : timeout atteint."


  (* 7. D´efinir une s´erie d’exemples de termes contenant, entre autres, I, δ, Ω, S, S K K, S I I, les encodages
  de 0, 1, 2, 3 et les encodages des op´erations arithm´etiques usuelles. *)

(* Combinateurs *)
let i = Abs ("x", Var "x")
let delta = Abs ("x", App (Var "x", Var "x"))
let omega = App (delta, delta)
let k = Abs ("x", Abs ("y", Var "x"))
let s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))

let skk = App (App (s, k), k)
let sii = App (App (s, i), i)

(* Nombres de Church *)
let zero = Abs ("f", Abs ("x", Var "x"))
let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))
let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))
let three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))

(* Opérations arithmétiques *)
let plus = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
let mult = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))


let test_terms = [
  ("I", i);
  ("δ", delta);
  ("Ω", omega);
  ("K", k);
  ("S", s);
  ("S K K", skk);
  ("S I I", sii);
  ("0", zero);
  ("1", one);
  ("2", two);
  ("3", three);
  ("plus 1 2", App (App (plus, one), two));
  ("mult 2 3", App (App (mult, two), three));
  ("succ 2", App (succ, two))
]


let () =
  List.iter (fun (name, term) ->
    print_endline ("Testing: " ^ name);
    try
      let normalized_term = ltr_cbv_norm_timeout term 100 in
      print_endline (print_term normalized_term)
    with
    | Timeout -> print_endline "Timeout exceeded: reduction took too long."
  ) test_terms *)
