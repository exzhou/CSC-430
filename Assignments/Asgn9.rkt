import Html exposing (..)
import List   

type ExprC
  = NumC Int
  | StringC String
  | IdC String
  | AppC ExprC (List ExprC)
  | IfC ExprC ExprC ExprC
  
type Value
  = NumV Int
  | StringV String
  | BoolV Bool
  | OpV String
  | ErrorV String
  
type Binding
  = Binding String Value


topEnv = [(Binding "a" (StringV "Aryan")),
          (Binding "true" (BoolV True)),
          (Binding "false" (BoolV False)),
          (Binding "+" (OpV "+")),
          (Binding "-" (OpV "-")),
          (Binding "<=" (OpV "<="))]

lookup : List Binding -> String -> Value
lookup li symb = 
  case li of
    [] -> (NumV 0)
    x :: xs ->
      case x of
        Binding str exprc -> 
          if str == symb then exprc
          else lookup xs symb

  

interp : ExprC -> Value
interp exprc =
  case exprc of
     NumC n ->
      NumV n
     StringC str ->
      StringV str
     IdC symb ->
      (lookup topEnv symb)
     IfC ifc thenc elsec ->
       case (interp ifc) of
         (BoolV True) -> (interp thenc)
         (BoolV False) -> (interp elsec)
         other -> (ErrorV "bad")
     AppC fun args ->
       case (interp fun) of 
         OpV op ->
           case op of
             "<=" ->  
               case (getBinop op (List.map interp args)) of
                 0 -> (BoolV False)
                 1 -> (BoolV True)
                 other -> (ErrorV "bad")
             other -> (NumV (getBinop op (List.map interp args)))
         other ->
            ErrorV "bad"
      


getBinop : String -> List Value -> Int
getBinop op vals =
  case op of
    "+" -> case vals of
      [] -> 0
      x::xs -> 
        case x of 
          NumV n -> 
            n + (getBinop op xs)
          other -> 0
    "-" -> case vals of
      [] -> 0
      x::xs -> 
        case x of 
          NumV n -> 
            n - (getBinop op xs)
          other -> 0
    "<=" -> case vals of
      [] -> 0
      [x1, x2] -> 
        case x1 of 
          NumV n1 -> 
            case x2 of 
              NumV n2 -> 
                if n1 <= n2 then 1
                else 0
              other -> 0
          other -> 0
      other -> 0
              
    other ->
            0
  

serialize : Value -> String
serialize val = 
  case val of
    NumV n ->
      String.fromInt n
    StringV str ->
      str
    BoolV bool ->
      Debug.toString bool
    OpV op ->
      Debug.toString op
    ErrorV err ->
      err

main =
  div []
    [ h1 [] [ text "My Tests:" ]
    , ul []
        [li [] [ Html.text (serialize (lookup topEnv "false")) ]
        , li [] [ Html.text (serialize (lookup topEnv "+")) ]
        , li [] [ Html.text (serialize (NumV 1)) ]
        , li [] [ Html.text (serialize (StringV "Eric")) ]
        , li [] [ Html.text (serialize (BoolV True)) ] 
        , li [] [ Html.text (serialize (interp (StringC "Eric")))]
        , li [] [ Html.text (serialize (interp (NumC 1))) ]
        , li [] [ Html.text (serialize (interp (IdC "+"))) ]
        , li [] [ Html.text (serialize (interp (IdC "-"))) ]
        , li [] [ Html.text (serialize (interp (IdC "true")))]
        , li [] [ Html.text (serialize (interp (IdC "false")))]
        , li [] [ Html.text (serialize (interp (AppC (IdC "+") [(NumC 1), (NumC 2)])))]
        , li [] [ Html.text (serialize (interp (AppC (IdC "-") [(NumC 1), (NumC 2)])))]
        , li [] [ Html.text (serialize (interp (AppC (IdC "<=") [(NumC 1), (NumC 2)])))]
        , li [] [ Html.text (serialize (interp (AppC (IdC "<=") [(NumC 2), (NumC 1)])))]
        , li [] [ Html.text (serialize (interp (IfC (IdC "true") (IdC "true") (IdC "false"))))]
        , li [] [ Html.text (serialize (interp (IfC (IdC "false") (StringC "a") (NumC 1))))]
        , li [] [ Html.text (serialize (interp (IfC (IdC "true") (StringC "a") (NumC 1))))]
        , li [] [ Html.text (serialize (interp (IfC (AppC (IdC "<=") [(NumC 100), (NumC 1)]) (IdC "true") (IdC "false"))))]
  ]]