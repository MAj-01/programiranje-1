(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Collatzovo zaporedje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Collatzovo zaporedje se začne s pozitivnim naravnim številom $a_0$ ter
 nadaljuje kot:

 $$a_{n + 1} = \begin{cases} a_n / 2, & \text{če je } a_n \text{ sodo} \\ 3 a_n
 + 1, & \text{če je } a_n \text{ liho} \end{cases}$$

 Sestavite funkcijo `collatz : int -> int list`, ki sprejme začetni člen
 zaporedja in vrne seznam vseh členov, dokler zaporedje ne doseže $1$.
[*----------------------------------------------------------------------------*)

let collatz a = 
  let rec collatz_aux a acc =
    match a with
    | 1 -> List.rev (1 :: acc)
    | a when a mod(2) = 0 -> collatz_aux (a / 2) (a :: acc)
    | _ -> collatz_aux (3 * a + 1) (a :: acc)
in collatz_aux a []

let primer_ogrevanje_1 = collatz 1024
(* val primer_ogrevanje_1 : int list =
  [1024; 512; 256; 128; 64; 32; 16; 8; 4; 2; 1] *)

let primer_ogrevanje_2 = collatz 27
(* val primer_ogrevanje_2 : int list =
  [27; 82; 41; 124; 62; 31; 94; 47; 142; 71; 214; 107; 322; 161; 484; 242;
   121; 364; 182; 91; 274; 137; 412; 206; 103; 310; 155; 466; 233; 700; 350;
   175; 526; 263; 790; 395; 1186; 593; 1780; 890; 445; 1336; 668; 334; 167;
   502; 251; 754; 377; 1132; 566; 283; 850; 425; 1276; 638; 319; 958; 479;
   1438; 719; 2158; 1079; 3238; 1619; 4858; 2429; 7288; 3644; 1822; 911;
   2734; 1367; 4102; 2051; 6154; 3077; 9232; 4616; 2308; 1154; 577; 1732;
   866; 433; 1300; 650; 325; 976; 488; 244; 122; 61; 184; 92; 46; 23; 70; 35;
   106; 53; 160; 80; 40; 20; 10; 5; 16; 8; 4; 2; 1] *)

(*----------------------------------------------------------------------------*
 ### Fiksne točke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `fiksne_tocke : ('a -> 'a) -> 'a list -> 'a list`, ki za
 dano funkcijo in seznam vrne podseznam vseh elementov, ki so fiksne točke.
[*----------------------------------------------------------------------------*)

let fiksne_tocke f sez = 
  let rec fiksne_aux f sez acc =
    match sez with
    | [] -> List.rev acc
    | x :: xs -> 
      if f x = x then fiksne_aux f xs (x :: acc) else fiksne_aux f xs acc
in fiksne_aux f sez [] 

let primer_ogrevanje_3 = fiksne_tocke (fun x -> x * x) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_3 : int list = [0; 1] *)

let primer_ogrevanje_4 = fiksne_tocke List.rev [[3]; [1; 4; 1]; [5; 9; 2; 6]; [5; 3; 5]; []; [8; 9; 7; 9; 3; 2; 3]]
(* val primer_ogrevanje_4 : int list list = [[3]; [1; 4; 1]; [5; 3; 5]; []] *)

(*----------------------------------------------------------------------------*
 ### Združevanje z ločilom
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sep_concat : 'a -> 'a list list -> 'a list`, ki združi
 seznam seznamov, pri čemer med elemente različnih seznamov ter na začetek in
 konec vstavi dano ločilo.
[*----------------------------------------------------------------------------*)

let sep_concat locilo sez = 
  let rec sep_concat_aux_1 locilo sez acc =
    match sez with
    | [] ->  locilo :: (List.rev acc)
    | x :: xs ->
      let rec sep_concat_aux_2 sez2 acc2 =
        match sez2 with 
        | [] -> locilo :: acc2
        | y :: ys -> sep_concat_aux_2 ys (y :: acc2)
    in sep_concat_aux_1 locilo xs (sep_concat_aux_2 x acc) 
in sep_concat_aux_1 locilo sez []

let primer_ogrevanje_5 = sep_concat 42 [[1; 2; 3]; [4; 5]; []; [6]]
(* val primer_ogrevanje_5 : int list = [42; 1; 2; 3; 42; 4; 5; 42; 42; 6; 42] *)

let primer_ogrevanje_6 = sep_concat 42 []
(* val primer_ogrevanje_6 : int list = [42] *)

(*----------------------------------------------------------------------------*
 ### Razbitje seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `partition : int -> 'a list -> 'a list`, ki sprejme pozitivno
 naravno število $k$ in seznam $[a_0, \dots, a_n]$ ter ga razdeli na zaporedne
 podsezname $[a_0, \dots, a_{k - 1}], [a_k, \dots, a_{2 k - 1}], \dots$, pri
 čemer je zadnji podseznam lahko tudi krajši.
[*----------------------------------------------------------------------------*)

let partition st sez = 
  let rec partition_aux sez pom_sez i acc =
    match sez with
    | [] -> 
      if pom_sez = [] then List.rev acc
      else List.rev (List.rev pom_sez :: acc)
    | x :: xs -> 
      if i < st then partition_aux xs (x :: pom_sez) (i + 1) acc
      else partition_aux xs [x] 1 ( List.rev pom_sez :: acc)
in partition_aux sez [] 0 [] 



let primer_ogrevanje_7 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_ogrevanje_7 : int list list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] *)

let primer_ogrevanje_8 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_8 : int list list =
  [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)

let phi1 (a, b) = (b, a)
let psi1 (b, a) = (a, b)

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let phi2 nekaj_iz_a_plus_b = 
  match nekaj_iz_a_plus_b with
  | In1 a -> In2 a
  | In2 b -> In1 b
let psi2 nekaj_iz_b_plus_a = 
  match nekaj_iz_b_plus_a with
  | In1 b -> In2 b
  | In2 a -> In1 a

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3 (a, (b, c)) = ((a, b), c)
let psi3 ((a, b), c) = (a, (b, c))

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4 = 
  function
  | In1 a -> In1 (In1 a)
  | In2 (In1 b) -> In1 (In2 b)
  | In2 (In2 c) -> In2 c
let psi4 = 
  function
  | In1 (In1 a) -> In1 a
  | In1 (In2 b) -> In2 (In1 b)
  | In2 c -> In2 (In2 c)

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 =
  function
  | (a, In1 b) -> In1 (a, b)
  | (a, In2 c) -> In2 (a, c)
let psi5 =
  function
  | In1 (a, b) -> (a, In1 b)
  | In2 (a, c) -> (a, In2 c) 

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6  (f: ('b, 'c) sum -> 'a) : ('b -> 'a) * ('c -> 'a) =
  let za_b = (fun (b : 'b) -> f (In1 b)) in
  let za_c = (fun (c : 'c) -> f (In2 c)) in
  (za_b, za_c)

let psi6 ((f, g) : ('b -> 'a) * ('c -> 'a)) : ('b, 'c) sum -> 'a = 
  function
  | In1 b -> f b
  | In2 c -> g c
(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 (f : 'c -> ('a * 'b)) : ('c -> 'a) * ('c -> 'b) =
  (fun c -> fst (f (c))),
  (fun c -> snd (f c))  



let psi7 ((f, g) : ('c -> 'a) * ('c -> 'b)) : 'c -> ('a * 'b) = 
  fun c -> (f c, g c)

(*----------------------------------------------------------------------------*
 ## Permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Permutacije so preureditve elementov $\{0, 1, \dots, n-1\}$, torej bijektivne
 preslikave $$p \colon \{0, 1, \dots, n-1\} \to \{0, 1, \dots, n-1\}.$$ V nalogi
 bomo permutacije predstavili s seznamom števil, v katerem je na $i$-tem mestu
 seznama zapisana slika elementa $i$.
 Na primer, permutaciji $0 \, 1 \, 2 \, 3 \, 4 \, 5 \choose 5 \, 3 \, 2 \, 1 \,
 4 \, 0$ in $0 \, 1 \, 2 \, 3 \, 4 \, 5 \, 6 \, 7 \, 8 \, 9 \choose 3 \, 9 \, 1
 \, 7 \, 5 \, 4 \, 6 \, 8 \, 2 \, 0$ bi zapisali s seznamoma:
[*----------------------------------------------------------------------------*)

let permutacija_1 = [5; 3; 2; 1; 4; 0]
let permutacija_2 = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0]
(* val permutacija_1 : int list = [5; 3; 2; 1; 4; 0] *)
(* val permutacija_2 : int list = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0] *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kompozitum : int list -> int list -> int list`, ki sprejme
 dve permutaciji in vrne njun kompozitum. Za permutaciji $p$ in $q$, je njun
 kompozitum funkcija

 $$ p \circ q \colon i \mapsto p ( q ( i ) ). $$

 Predpostavite lahko, da sta seznama enakih dolžin.
[*----------------------------------------------------------------------------*)

let kompozitum p1 p2 = 
  let rec kom_aux_2 p1 k st =
    match p1 with
    | [] -> failwith "Cikla sta neustrezna"
    | y :: ys -> 
      if k = st then y
      else kom_aux_2 ys (k + 1) st
  in
  let rec kom_aux p2 acc =
    match p2 with
    | [] -> List.rev acc
    | x :: xs -> kom_aux xs ((kom_aux_2 p1 0 x) :: acc)  
in kom_aux p2 [] 
let primer_permutacije_1 = kompozitum permutacija_1 permutacija_1
(* val primer_permutacije_1 : int list = [0; 1; 2; 3; 4; 5] *)

let primer_permutacije_2 = kompozitum permutacija_2 permutacija_2
(* val primer_permutacije_2 : int list = [7; 0; 9; 8; 4; 5; 6; 2; 1; 3] *)

(*----------------------------------------------------------------------------*
 ### Inverz
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napiši funkcijo `inverz : int list -> int list`, ki vrne inverz dane
 permutacije $p$, torej tako permutacijo $p^{-1}$, da velja $$p \circ p^{-1} =
 \mathrm{id},$$ kjer je $\mathrm{id}$ indentiteta.
[*----------------------------------------------------------------------------*)

let inverz p = 
  let rec inverz_aux_2 p i st =
    match p with
    | [] -> failwith "Napaka"
    | y :: ys -> 
      if y = st then i
      else inverz_aux_2 ys (i + 1) st 
  in
  let rec inverz_aux st acc =
    if st >= List.length p then List.rev acc
    else inverz_aux (st+1) ((inverz_aux_2 p 0 st) :: acc)
in inverz_aux 0 []
     

let primer_permutacije_3 = inverz permutacija_1
(* val primer_permutacije_3 : int list = [5; 3; 2; 1; 4; 0] *)

let primer_permutacije_4 = inverz permutacija_2
(* val primer_permutacije_4 : int list = [9; 2; 8; 0; 5; 4; 6; 3; 7; 1] *)

(*let primer_permutacije_5 = kompozitum permutacija_2 (inverz permutacija_2)*)
(* val primer_permutacije_5 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(*----------------------------------------------------------------------------*
 ### Razcep na cikle
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `cikli : int list -> int list list`, ki za dano permutacijo
 vrne seznam ciklov, ki to permutacijo sestavljajo. Vsak element $\{0, 1, \dots,
 n-1\}$ naj se pojavi v natanko enem ciklu.
[*----------------------------------------------------------------------------*)

let cikli p =
  let rec najdi_naslednika p i st =
    match p with
    | [] -> failwith "Cikel je nepravilen."
    | y :: ys ->
      if i = st then y
      else najdi_naslednika ys (i + 1) st
  in
  let rec najdi_cikel p i trenutni pom_sez acc =
    if List.mem trenutni pom_sez then
      List.rev pom_sez
    else
      let naslednji = najdi_naslednika p 0 trenutni in
      najdi_cikel p (i + 1) naslednji (trenutni :: pom_sez) acc
  in
  let n = List.length p in
  let rec obdelaj_vse_indekse i rezultat =
    if i >= n then
      List.rev rezultat
    else
      if List.mem i (List.flatten rezultat) then
        obdelaj_vse_indekse (i + 1) rezultat
      else
        let cikel = najdi_cikel p i i [] [] in
        obdelaj_vse_indekse (i + 1) (cikel :: rezultat)
in
obdelaj_vse_indekse 0 []


let primer_permutacije_6 = cikli permutacija_1
(* val primer_permutacije_6 : int list list = [[0; 5]; [1; 3]; [2]; [4]] *)

let primer_permutacije_7 = cikli permutacija_2
(* val primer_permutacije_7 : int list list =
  [[0; 3; 7; 8; 2; 1; 9]; [4; 5]; [6]] *)

let primer_permutacije_8 = cikli (inverz permutacija_2)
(* val primer_permutacije_8 : int list list =
  [[0; 9; 1; 2; 8; 7; 3]; [4; 5]; [6]] *)

(*----------------------------------------------------------------------------*
 ### Transpozicije permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Vsako permutacijo lahko zapišemo kot produkt transpozicij, torej menjav dveh
 elementov. Na primer, permutacijo $0 \, 1 \, 2 \, 3 \choose 1 \, 0 \, 3 \, 2$
 dobimo kot produkt transpozicij $(0, 1) \circ (2, 3)$.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `iz_transpozicij : int -> (int * int) list -> int list`, ki
 sprejme dolžino permutacije in seznam transpozicij ter vrne permutacijo, ki jim
 ustreza.
[*----------------------------------------------------------------------------*)

let iz_transpozicij st sez_t = 
  let pretvori_v_seznam t =
    match t with
    | (a, b) -> [a; b]
  in 
  let rec seznam_stevil n =
    if n <= 0 then []
    else seznam_stevil (n - 1) @ [n - 1]
  in
  let poisci_manjkajoce popolni_sez nepopolni_sez =
    let manjkajoce = List.filter (fun x -> not (List.mem x nepopolni_sez)) popolni_sez in 
    List.map (fun x -> [x; x]) manjkajoce
  in
  let dodaj_manjkajoce sez st =
    let pretvorjen_sez = List.map pretvori_v_seznam sez in
    let samo_st = List.flatten pretvorjen_sez in
    if List.length samo_st = st then pretvorjen_sez
    else 
      let pop_sez = seznam_stevil st in
      let manjkajoca = poisci_manjkajoce pop_sez samo_st in
      pretvorjen_sez @ manjkajoca
  in
  let rec dodaj_obratno_permutacijo novi_sez st acc = 
    match novi_sez with
    | [] -> novi_sez @ acc
    | [a; b]:: xs -> dodaj_obratno_permutacijo xs st ([b; a] :: acc)
    | _ -> failwith "Error"
  in
  let rec glavna sez i acc =
    if i >= st then List.rev acc
    else
      let rec poisci_par i sez =
        match sez with
        | [] -> failwith "Error"
        | [a; b] :: xs -> if a = i then b else poisci_par i xs
        | _ -> failwith "Error."
      in
      let vrednost = poisci_par i sez in
      glavna sez (i + 1) (vrednost :: acc)  
  in  
  let novi_sez = dodaj_manjkajoce sez_t st in
  let koncni_seznam = dodaj_obratno_permutacijo novi_sez st [] @ novi_sez in  
glavna koncni_seznam 0 []

let primer_permutacije_9 = iz_transpozicij 4 [(0, 1); (2, 3)]
(* val primer_permutacije_9 : int list = [1; 0; 3; 2] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `v_transpozicije : int list -> (int * int) list`, ki zapiše
 permutacijo kot produkt transpozicij, torej menjav dveh elementov. Možnih
 produktov je več, veljati mora le, da je kompozicija dobljenih ciklov enaka
 prvotni permutaciji.

 *Namig: Pomagate si lahko z lastnostjo, da poljubni cikel razpade na
 transpozicije po naslednji formuli*
 $$(i_1, i_2, i_3, \ldots, i_{k-1}, i_k) = (i_1, i_k)\circ(i_1,
 i_{k-1})\circ(i_1, i_3)\circ(i_1, i_2).$$
[*----------------------------------------------------------------------------*)

let v_transpozicije p = 
  let prvi seznam =
    match seznam with
    | [] -> failwith "Napačen seznam"
    | x :: _ -> x
  in
  let ostanek seznam = 
    match seznam with
    | [] -> failwith "Napačen seznam"
    | x :: xs -> xs
  in  
  let rec razdreti_cikle seznam prvi_element acc =
    match seznam with
    | y :: ys -> razdreti_cikle ys prvi_element ((prvi_element, y) :: acc)
    | [] -> List.rev acc
  in
  let seznam_v_tuple seznam =
    match seznam with
    | [a; b] -> (a, b)
    | _ -> failwith "Napačen seznam."
  in
  let rec transpozicije seznam acc =
    match seznam with 
    | [] -> List.rev acc
    | x :: xs -> 
      let n = List.length x in
      if n = 1 then transpozicije xs acc
      else 
        if n = 2 then transpozicije xs (seznam_v_tuple x :: acc)
        else 
          let novi_cikel = razdreti_cikle (ostanek x) (prvi x) [] in
          transpozicije xs (novi_cikel @ acc)
in transpozicije (cikli p) []  





let primer_permutacije_10 = v_transpozicije permutacija_1
(* val primer_permutacije_10 : (int * int) list = [(0, 5); (1, 3)] *)

let primer_permutacije_11 = v_transpozicije permutacija_2
(* val primer_permutacije_11 : (int * int) list =
  [(0, 9); (0, 1); (0, 2); (0, 8); (0, 7); (0, 3); (4, 5)] *)

(*----------------------------------------------------------------------------*
 ## Sudoku
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sudoku je igra, v kateri mrežo $9 \times 9$ dopolnimo s števili od $1$ do $9$,
 tako da se nobeno število v nobeni vrstici, stolpcu ali eni od devetih škatel
 velikosti $3 \times 3$ ne ponovi. Primer začetne postavitve in ustrezne rešitve
 je:

 ```plaintext
 +-------+-------+-------+       +-------+-------+-------+
 | 5 4 . | . 7 . | . . . |       | 5 4 3 | 6 7 8 | 9 1 2 |
 | 6 . . | 1 9 5 | . . . |       | 6 7 2 | 1 9 5 | 3 4 8 |
 | . 9 8 | . . . | . 6 . |       | 1 9 8 | 3 4 2 | 5 6 7 |
 +-------+-------+-------+       +-------+-------+-------+
 | 8 . . | . 6 . | . . 3 |       | 8 1 9 | 7 6 4 | 2 5 3 |
 | 4 . . | 8 . 3 | . . 1 |       | 4 2 6 | 8 5 3 | 7 9 1 |
 | 7 . . | . 2 . | . . 6 |       | 7 3 5 | 9 2 1 | 4 8 6 |
 +-------+-------+-------+       +-------+-------+-------+
 | . 6 . | . . 7 | 8 . . |       | 9 6 1 | 5 3 7 | 8 2 4 |
 | . . . | 4 1 9 | . . 5 |       | 2 8 7 | 4 1 9 | 6 3 5 |
 | . . . | . 8 . | . 7 9 |       | 3 5 4 | 2 8 6 | 1 7 9 |
 +-------+-------+-------+       +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Delno izpolnjen sudoku bomo predstavili s tabelo tabel tipa `int option array
 array`, kjer bomo prazna mesta označili z `None`, rešen sudoku pa s tabelo
 tabel običajnih števil.
[*----------------------------------------------------------------------------*)

type mreza = int option array array
type resitev = int array array

(*----------------------------------------------------------------------------*
 Na primer, zgornjo mrežo in rešitev bi predstavili s seznamoma:
[*----------------------------------------------------------------------------*)

let primer_mreze : mreza = [|
  [|Some 5; Some 4; None;   None;   Some 7; None;   None;   None;   None|];
  [|Some 6; None;   None;   Some 1; Some 9; Some 5; None;   None;   None|];
  [|None;   Some 9; Some 8; None;   None;   None;   None;   Some 6; None|];
  [|Some 8; None;   None;   None;   Some 6; None;   None;   None;   Some 3|];
  [|Some 4; None;   None;   Some 8; None;   Some 3; None;   None;   Some 1|];
  [|Some 7; None;   None;   None;   Some 2; None;   None;   None;   Some 6|];
  [|None;   Some 6; None;   None;   None;   Some 7; Some 8; None;   None|];
  [|None;   None;   None;   Some 4; Some 1; Some 9; None;   None;   Some 5|];
  [|None;   None;   None;   None;   Some 8; None;   None;   Some 7; Some 9|]
|]

let primer_resitve : resitev = [|
  [|5; 4; 3; 6; 7; 8; 9; 1; 2|];
  [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
  [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
  [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
  [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
  [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
  [|9; 6; 1; 5; 3; 7; 8; 2; 4|];
  [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
  [|3; 5; 4; 2; 8; 6; 1; 7; 9|];
|]
(* val primer_mreze : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; None|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)
(* val primer_resitve : resitev =
  [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
    [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
    [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
    [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
    [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Dopolnitev mreže
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dodaj : int -> int -> int -> mreza -> mreza` tako da `dodaj
 i j n m` vrne mrežo, ki je povsod enaka mreži `m`, le na mestu v vrstici `i` in
 stolpcu `j` ima zapisano število `n`.

 **Pozor:** OCaml dopušča spreminjanje tabel (o tem se bomo učili kasneje). Vaša
 funkcija naj te možnosti ne uporablja, temveč naj sestavi in vrne novo tabelo.
[*----------------------------------------------------------------------------*)

type mreza = int option array array

let dodaj i j n m =
  let rec posodobi_stolpec vrstica trenutni_i i n acc =
    if trenutni_i >= Array.length vrstica then Array.of_list (List.rev acc)
    else
      if trenutni_i = i then
        posodobi_stolpec vrstica (trenutni_i + 1) i n (Some n :: acc)
      else
        posodobi_stolpec vrstica (trenutni_i + 1) i n (vrstica.(trenutni_i) :: acc)
  in
  let rec obdelaj_vrstice trenutni_i i j n mreza acc =
    if trenutni_i >= Array.length mreza then Array.of_list (List.rev acc)
    else
      let trenutna_vrstica = mreza.(trenutni_i) in
      if trenutni_i = i then
        let nova_vrstica = posodobi_stolpec trenutna_vrstica 0 j n [] in
        obdelaj_vrstice (trenutni_i + 1) i j n mreza (nova_vrstica :: acc)
      else
        obdelaj_vrstice (trenutni_i + 1) i j n mreza (trenutna_vrstica :: acc)
  in
obdelaj_vrstice 0 i j n m []


let primer_sudoku_1 = primer_mreze |> dodaj 0 8 2
(* val primer_sudoku_1 : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; Some 2|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Izpiši mrežo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkciji `izpis_mreze : mreza -> string` in `izpis_resitve : resitev
 -> string`, ki sprejmeta mrežo oziroma rešitev in vrneta niz, ki predstavlja
 izpis v zgornji obliki.
[*----------------------------------------------------------------------------*)

let izpis_mreze m =
  let rec vrstica_izpis vrstica i acc =
    if i >= Array.length vrstica then String.concat " " ("|" :: (List.rev acc))
    else
      if i = 2 || i = 5 || i = 8 then
        match vrstica.(i) with
        | Some x -> vrstica_izpis vrstica (i + 1) ("|" :: (string_of_int x :: acc))
        | None -> vrstica_izpis vrstica (i + 1) ("|" :: ("." :: acc))
      else
        match vrstica.(i) with
        | Some x -> vrstica_izpis vrstica (i + 1) (string_of_int x :: acc)
        | None -> vrstica_izpis vrstica (i + 1) ("." :: acc)
  in
  let rec glavna_mreza m i acc =
    if i >= Array.length m then String.concat "\n" ("+-------+-------+-------+" :: (List.rev acc))
    else
      let vrstica = vrstica_izpis m.(i) 0 [] in
      if i = 2 || i = 5 || i = 8 then
        glavna_mreza m (i + 1) ("+-------+-------+-------+" :: (vrstica :: acc))
      else 
        glavna_mreza m (i + 1) (vrstica :: acc)
  in
  glavna_mreza m 0 []


let primer_sudoku_2 = primer_mreze |> izpis_mreze |> print_endline
(* 
  +-------+-------+-------+
  | 5 4 . | . 7 . | . . . |
  | 6 . . | 1 9 5 | . . . |
  | . 9 8 | . . . | . 6 . |
  +-------+-------+-------+
  | 8 . . | . 6 . | . . 3 |
  | 4 . . | 8 . 3 | . . 1 |
  | 7 . . | . 2 . | . . 6 |
  +-------+-------+-------+
  | . 6 . | . . 7 | 8 . . |
  | . . . | 4 1 9 | . . 5 |
  | . . . | . 8 . | . 7 9 |
  +-------+-------+-------+
  
  val primer_sudoku_2 : unit = ()
*)

let izpis_resitve m =
  let rec vrstica_izpis vrstica i acc =
    if i >= Array.length vrstica then String.concat " " ("|" :: (List.rev acc))
    else
      if i = 2 || i = 5 || i = 8 then
        match vrstica.(i) with
        | x -> vrstica_izpis vrstica (i + 1) ("|" :: ((string_of_int x) :: acc))
      else
        match vrstica.(i) with
        | x -> vrstica_izpis vrstica (i + 1) (string_of_int x :: acc)
  in
  let rec glavna_mreza m i acc =
    if i >= Array.length m then String.concat "\n" ("+-------+-------+-------+" :: (List.rev acc))
    else
      let vrstica = vrstica_izpis m.(i) 0 [] in
      if i = 2 || i = 5 || i = 8 then
        glavna_mreza m (i + 1) ("+-------+-------+-------+" :: (vrstica :: acc))
      else 
        glavna_mreza m (i + 1) (vrstica :: acc)
  in
  glavna_mreza m 0 []

let primer_sudoku_3 = primer_resitve |> izpis_resitve |> print_endline
(*
  +-------+-------+-------+
  | 5 4 3 | 6 7 8 | 9 1 2 |
  | 6 7 2 | 1 9 5 | 3 4 8 |
  | 1 9 8 | 3 4 2 | 5 6 7 |
  +-------+-------+-------+
  | 8 1 9 | 7 6 4 | 2 5 3 |
  | 4 2 6 | 8 5 3 | 7 9 1 |
  | 7 3 5 | 9 2 1 | 4 8 6 |
  +-------+-------+-------+
  | 9 6 1 | 5 3 7 | 8 2 4 |
  | 2 8 7 | 4 1 9 | 6 3 5 |
  | 3 5 4 | 2 8 6 | 1 7 9 |
  +-------+-------+-------+

  val primer_sudoku_3 : unit = ()
*)

(*----------------------------------------------------------------------------*
 ### Preveri, ali rešitev ustreza mreži
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ustreza : mreza -> resitev -> bool`, ki preveri, ali rešitev
 ustreza dani mreži. Rešitev ustreza mreži, če se na vseh mestih, kjer je v
 mreži podana številka, v rešitvi nahaja enaka številka.
[*----------------------------------------------------------------------------*)

let ustreza m r = 
  let rec vrstica_stanje vrstica_m vrstica_r i =
    if i >= Array.length vrstica_m then true
    else
      match vrstica_m.(i) with
      | Some x -> 
        if string_of_int x = string_of_int vrstica_r.(i) then vrstica_stanje vrstica_m vrstica_r (i + 1)
        else false
      | None -> vrstica_stanje  vrstica_m vrstica_r (i + 1)
  in
  let rec glavna m r i =
    if i >= Array.length m then true
    else
      if vrstica_stanje m.(i) r.(i) 0 then glavna m r (i + 1)
      else false
in 
glavna m r 0


let primer_sudoku_4 = ustreza primer_mreze primer_resitve
(* val primer_sudoku_4 : bool = true *)

(*----------------------------------------------------------------------------*
 ### Kandidati za dano prazno mesto
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcije `ni_v_vrstici`, `ni_v_stolpcu` in `ni_v_skatli`, vse tipa
 `mreza * int -> int -> bool`, ki preverijo, ali se v določeni vrstici, stolpcu
 oziroma škatli mreže ne nahaja dano število. Vrstice, stolpci in škatle so
 indeksirani kot:

 ```plaintext
     0 1 2   3 4 5   6 7 8
   +-------+-------+-------+
 0 |       |       |       |
 1 |   0   |   1   |   2   |
 2 |       |       |       |
   +-------+-------+-------+
 3 |       |       |       |
 4 |   3   |   4   |   5   |
 5 |       |       |       |
   +-------+-------+-------+
 6 |       |       |       |
 7 |   6   |   7   |   8   |
 8 |       |       |       |
   +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

let ni_v_vrstici (m, index) st  =
  let rec poisci_vrstica m index st i =
    let vrstica = m.(index) in
    if i >= Array.length vrstica then true
    else 
      match vrstica.(i) with 
      | Some x -> 
        if x = st then false
        else poisci_vrstica m index st (i + 1)
      | None -> poisci_vrstica m index st (i + 1)
in
poisci_vrstica m index st 0 

let primer_sudoku_5 = ni_v_vrstici (primer_mreze, 0) 1
(* val primer_sudoku_5 : bool = true *)

let primer_sudoku_6 = ni_v_vrstici (primer_mreze, 1) 1
(* val primer_sudoku_6 : bool = false *)

let ni_v_stolpcu (m, index) st =
  let rec poisci_stolpec m index st i =
      if i >= Array.length m then true
      else
        let element = m.(i).(index) in
        match element with
        | Some x ->
          if x = st then false
          else poisci_stolpec m index st (i + 1)
        | None -> poisci_stolpec m index st (i + 1)
in
poisci_stolpec m index st 0

let ni_v_skatli (m, skatla) st =
  let rec poisci_skatla m skatla st i j =
    let zacetek_vrstica = (skatla / 3) * 3 in
    let zacetek_stolpec = (skatla mod 3) * 3 in
    let vrstica = zacetek_vrstica + i in
    let stolpec = zacetek_stolpec + j in
    if i >= 3 then true
    else
      if j >= 3 then poisci_skatla m skatla st (i + 1) 0
      else
        let element = m.(vrstica).(stolpec) in
        match element with
        | Some x ->
          if x = st then false
          else poisci_skatla m skatla st i (j + 1)
        | None -> poisci_skatla m skatla st i (j + 1)
in
poisci_skatla m skatla st 0 0

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kandidati : mreza -> int -> int -> int list option`, ki
 sprejme mrežo in indeksa vrstice in stolpca praznega mesta ter vrne seznam vseh
 številk, ki se lahko pojavijo na tem mestu. Če je polje že izpolnjeno, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)


let kandidati m i j = 
  let presek sez1 sez2 =
    List.filter (fun x -> List.mem x sez2) sez1
  in 
  let st_skatle i j =
    (i / 3) * 3 + (j / 3)
  in 
  let rec skatla_rek skatla index acc = 
    if index > 9 then List.rev acc
    else 
      if ni_v_skatli (m, skatla) index then skatla_rek skatla (index + 1) (index :: acc)
      else skatla_rek skatla (index + 1) acc
  in
  let rec vrstica_rek i index acc = 
    if index > 9 then List.rev acc
    else 
      if ni_v_vrstici (m, i) index then vrstica_rek i (index + 1) (index :: acc)
      else vrstica_rek i (index + 1) acc
  in
  let rec stolpec_rek j index acc = 
    if index > 9 then List.rev acc
    else
      if ni_v_stolpcu (m, j) index then stolpec_rek j (index + 1) (index :: acc)
      else stolpec_rek j (index + 1) acc
  in
  let glavna m i j =
    let skatla_index = st_skatle i j in 
    let sez_skatla = skatla_rek skatla_index 1 [] in
    let sez_vrstica = vrstica_rek i 1 [] in
    let sez_stolpec = stolpec_rek j 1 [] in
    let presek_1 = presek sez_skatla sez_vrstica in
    match m.(i).(j) with
    | Some x -> None
    | None -> Some (presek presek_1 sez_stolpec) 
in
glavna m i j    


let primer_sudoku_7 = kandidati primer_mreze 0 2
(* val primer_sudoku_7 : int list option = Some [1; 2; 3] *)

let primer_sudoku_8 = kandidati primer_mreze 0 0
(* val primer_sudoku_8 : int list option = None *)

(*----------------------------------------------------------------------------*
 ### Iskanje rešitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `resi : mreza -> resitev option`, ki izpolni mrežo sudokuja.
 Če je dana mreža rešljiva, mora funkcija najti rešitev, ki ustreza začetni
 mreži in jo vrniti v obliki `Some resitev`, sicer naj vrne `None`.
 Predpostavite lahko, da je rešitev enolična, zato lahko funkcija vrne prvo, ki
 jo najde.

 *Namig: Poiščite celico mreže z najmanj kandidati in rekurzivno preizkusite vse
 možnosti.*
[*----------------------------------------------------------------------------*)

let rec resi m =
  let rec prazni i j acc =
    let n = Array.length m in
    if i >= n then 
      List.rev acc
    else if j >= n then 
      prazni (i + 1) 0 acc
    else
      match m.(i).(j) with
      | None -> 
        prazni i (j + 1) ((i, j) :: acc)
      | Some _ -> 
        prazni i (j + 1) acc
  in
  let primerjaj_celici c1 c2 =
    match c1, c2 with
    | None, None -> None
    | Some (i1, j1, k1), None -> Some (i1, j1, k1)
    | None, Some (i2, j2, k2) -> Some (i2, j2, k2)
    | Some (i1, j1, k1), Some (i2, j2, k2) ->
      if List.length k1 < List.length k2 then
        Some (i1, j1, k1)
      else
        Some (i2, j2, k2)
  in
  let rec najboljsa_celica sez trenutna_najb =
    match sez with
    | [] -> trenutna_najb
    | celica :: ostale_celice ->
      let poisci_kandidate (i, j) = kandidati m i j in
      match poisci_kandidate celica with
      | None -> None 
      | Some sez_k ->

  (*Naprej žal nisem uspel rešiti.*)

let primer_sudoku_9 = resi primer_mreze
(* val primer_sudoku_9 : resitev option =
  Some
   [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
     [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
     [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
     [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
     [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)
