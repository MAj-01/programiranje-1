(*----------------------------------------------------------------------------*
 # Uvod v funkcijsko programiranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg koeficient vektor =
  List.map (fun x -> koeficient *. x) vektor




let razteg koeficient vektor =
  List.map ( ( *. ) koeficient ) vektor

     

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej v1 v2 =
  List.map2 (+.) v1 v2

let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt v1 v2 = 
  let zmnozi = List.map2 ( *.) v1 v2 in
  vsota_seznama zmnozi
  

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma v1 = 
  let pomnozi_komponente = List.map (fun x -> x *. x) in
  sqrt (vsota_seznama (pomnozi_komponente v1))



let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot v1 v2 =
   acos (skalarni_produkt v1 v2 /. (norma v1 *. norma v2))

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani v1 =
  let n = norma v1 in
  List.map (fun x -> x /. n) v1

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija v1 v2 = 
  let koef = skalarni_produkt v1 v2 /. skalarni_produkt v2 v2 in
  List.map (fun x -> koef *. x) v2

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina = 
  "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

let zamakni st niz = 
  let nov_niz = String.make st ' ' in 
  niz  
  |> String.split_on_char '\n'
  |> List.map (String.cat nov_niz)
  |> String.concat "\n"

      
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul niz = 
  niz
  |> List.map (ovij "li")
  |> String.concat "\n"
  |> zamakni 2
  |> (fun vsebina -> "\n" ^ vsebina ^ "\n")
  |> ovij "ul"
  


let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico niz = 
  let index = String.index niz ',' in 
  let prvi_del = String.sub niz 0 index 
  and drugi_del = String.sub niz (index + 1) (String.length niz - index - 1) in
  (String.trim prvi_del, String.trim drugi_del)

  

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz =
  niz
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map razdeli_vrstico


let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruhnjabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f sez =
  List.map (fun (x, y) -> (x, f y))

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik nsez = 
  let cenik =
    cenik
    |> pretvori_v_seznam_parov
    |> pretvori_druge_komponente float_of_string
  in
  let cena_izdelka (izdelek, kolicina) =
    let cena = List.assoc izdelek cenik in 
    float_of_int kolicina *. cena
  in 
  nsez
  |> pretvori_v_seznam_parov
  |> pretvori_druge_komponente int_of_string
  |> List.map cena_izdelka
  |> vsota_seznama



let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)






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

let kompozitum (p1 : int list) (p2 : int list) : int list =
  let rec kom_aux_2 (lst : int list) (index : int) (target : int) =
    match lst with
    | [] -> failwith "Indeks ni veljaven"
    | y :: ys ->
      if index = target then 
        y
      else 
        kom_aux_2 ys (index + 1) target
  in
  let rec kom_aux (p2 : int list) (acc : int list) =
    match p2 with
    | [] -> List.rev acc
    | x :: xs ->
      let value = kom_aux_2 p1 0 x in
      kom_aux xs (value :: acc)
  in
  kom_aux p2 []
let primer_permutacije_1 = kompozitum permutacija_1 permutacija_1
(* val primer_permutacije_1 : int list = [0; 1; 2; 3; 4; 5] *)

let primer_permutacije_2 = kompozitum permutacija_2 permutacija_2
(* val primer_permutacije_2 : int list = [7; 0; 9; 8; 4; 5; 6; 2; 1; 3] *)