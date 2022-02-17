(* let __ () =
  if true then 1 else 2

let __ f x  = if f x then true else f x
let __ f x  = if f x then f x else false

let rec foo1 r = function
  | _::xs -> foo1 (r && true) xs
  | _ ->  (r && false)

let rec foo2 r = function
  | _::xs -> foo2 (r || true) xs
  | _ ->  (r || false) *)

class istack = object
  val mutable v = [0; 2]

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v
end;;

class dummyclass = object
end;;

let amazingnumber1 = 1

let amazingnumber2 = if 1 == 1 then 1 else 1

type colour = Black | White

type rgb = {red : int; blue : int; green : int}
let anime : int -> int = (fun x -> x + 1)
let anime2 x : int =  x + 1

(* let stungerfunctyon1 x = x ^ "AZAZA" ^ x
let stungerfunctyon2 x = (x ^ "AZAZA") ^ xS
let stungerfunctyon3 x = x ^ ("AZAZA" ^ x) *)
let morestungerfunctyon2 x = let y = x + 1 in y + 1 + y
let morestungerfunctyon3 x = (x + 1) + x ;;

let stungerfunctyon4 x = x + (1 + x) ;;

let stungerfunctyon5 x = (fun y -> y + (1 + x)) x ;;
let funnyfunctyon1 x = x + amazingnumber1
(* let funnyfunctyon2 x = x + 2
let funnyfunctyon3 x = x + 3
let funnyfunctyon4 x = x + 4
let funnyfunctyon5 x = x + 5
let funnyfunctyon6 x = x + 6
let funnyfunctyon7 x = x + 7
let funnyfunctyon8 x = x + 8
let funnyfunctyon9 x = x + 9
let funnyfunctyon10 x = x + 10
let funnyfunctyon11 x = x + 11
let funnyfunctyon12 x = x + 12
let funnyfunctyon13 x = x + 13
let funnyfunctyon14 x = x + 14
let funnyfunctyon15 x = x + 15
let funnyfunctyon16 x = x + 16
let funnyfunctyon17 x = x + 17
let funnyfunctyon18 x = x + 18
let funnyfunctyon19 x = x + 19
let funnyfunctyon20 x = x + 20
let funnyfunctyon21 x = x + 21
let funnyfunctyon22 x = x + 22
let funnyfunctyon23 x = x + 23
let funnyfunctyon24 x = x + 24
let funnyfunctyon25 x = x + 25
let funnyfunctyon26 x = x + 26
let funnyfunctyon27 x = x + 27
let funnyfunctyon28 x = x + 28
let funnyfunctyon29 x = x + 29
let funnyfunctyon30 x = x + 30
let funnyfunctyon31 x = x + 31
let funnyfunctyon32 x = x + 32
let funnyfunctyon33 x = x + 33
let funnyfunctyon34 x = x + 34
let funnyfunctyon35 x = x + 35
let funnyfunctyon36 x = x + 36
let funnyfunctyon37 x = x + 37
let funnyfunctyon38 x = x + 38
let funnyfunctyon39 x = x + 39
let funnyfunctyon40 x = x + 40
let funnyfunctyon41 x = x + 41
let funnyfunctyon42 x = x + 42
let funnyfunctyon43 x = x + 43
let funnyfunctyon44 x = x + 44
let funnyfunctyon45 x = x + 45
let funnyfunctyon46 x = x + 46
let funnyfunctyon47 x = x + 47
let funnyfunctyon48 x = x + 48
let funnyfunctyon49 x = x + 49
let funnyfunctyon50 x = x + 50
let funnyfunctyon51 x = x + 51
let funnyfunctyon52 x = x + 52
let funnyfunctyon53 x = x + 53
let funnyfunctyon54 x = x + 54
let funnyfunctyon55 x = x + 55
let funnyfunctyon56 x = x + 56
let funnyfunctyon57 x = x + 57
let funnyfunctyon58 x = x + 58
let funnyfunctyon59 x = x + 59
let funnyfunctyon60 x = x + 60
let funnyfunctyon61 x = x + 61
let funnyfunctyon62 x = x + 62
let funnyfunctyon63 x = x + 63
let funnyfunctyon64 x = x + 64
let funnyfunctyon65 x = x + 65
let funnyfunctyon66 x = x + 66
let funnyfunctyon67 x = x + 67
let funnyfunctyon68 x = x + 68
let funnyfunctyon69 x = x + 69
let funnyfunctyon70 x = x + 70
let funnyfunctyon71 x = x + 71
let funnyfunctyon72 x = x + 72
let funnyfunctyon73 x = x + 73
let funnyfunctyon74 x = x + 74
let funnyfunctyon75 x = x + 75
let funnyfunctyon76 x = x + 76
let funnyfunctyon77 x = x + 77
let funnyfunctyon78 x = x + 78
let funnyfunctyon79 x = x + 79
let funnyfunctyon80 x = x + 80
let funnyfunctyon81 x = x + 81
let funnyfunctyon82 x = x + 82
let funnyfunctyon83 x = x + 83
let funnyfunctyon84 x = x + 84
let funnyfunctyon85 x = x + 85
let funnyfunctyon86 x = x + 86
let funnyfunctyon87 x = x + 87
let funnyfunctyon88 x = x + 88
let funnyfunctyon89 x = x + 89
let funnyfunctyon90 x = x + 90
let funnyfunctyon91 x = x + 91
let funnyfunctyon92 x = x + 92
let funnyfunctyon93 x = x + 93
let funnyfunctyon94 x = x + 94
let funnyfunctyon95 x = x + 95
let funnyfunctyon96 x = x + 96
let funnyfunctyon97 x = x + 97
let funnyfunctyon98 x = x + 98
let funnyfunctyon99 x = x + 99 *)