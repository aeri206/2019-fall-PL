(* Dept of CSE, 2016-12805, Cho Aeri HW 1-3 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna


let rec parenize : tourna -> string = fun tourna ->
    match tourna with
    | LEAF team -> (
                match team with 
                | Korea -> "Korea"
                | France -> "France" 
                | Usa -> "Usa"
                | Brazil -> "Brazil"
                | Japan -> "Japan"
                | Nigeria -> "Nigeria"
                | Cameroon -> "Cameroon"
                | Poland -> "Poland"
                | Portugal -> "Portugal"
                | Italy -> "Italy"
                | Germany -> "Germany"
                | Norway -> "Norway"
                | Sweden -> "Sweden"
                | England -> "England"
                | Argentina -> "Argentina"
            )
    | NODE (lteam,  rteam) -> 
        "("^ (parenize lteam) ^" "^ (parenize rteam) ^")"
