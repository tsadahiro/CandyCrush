module CandyCrush exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time
import Random
import Html.Events.Extra.Pointer exposing (..)


main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

type CandyType = Red
               | Blue
               | Green
               | Yellow
               | Purple
       
type alias Piece = {x:Int
                   ,y:Int
                   ,color:Int
                   ,matching:Bool
                   ,moving:Bool
                   ,current: Position
                   ,start: Position
                   }
       
type State = Waiting
           | Moving Piece
           | Matching
           | Deleting
           | Dropping
           | Adding

type alias Position = {x : Float, y : Float}
             
type alias Model = {conf:List Piece
                   ,state : State
                   ,elapsed: Int
                   }
    
type Msg = Roll
         | Generated (List Int)
         | MoveStart Piece {x:Float, y:Float}
         | MoveEnd {x:Float, y:Float}
         | Move {x:Float, y:Float}
         | Elapsed Time.Posix
         | Added (List Int)

hSize : Int
hSize = 5
vSize : Int
vSize = 5
        
init: () -> (Model, Cmd Msg)
init _ = ({conf = List.map newPiece (List.range 0 (hSize*vSize-1))
          ,state = Waiting
          ,elapsed = 10
          }
         ,Random.generate Generated (Random.list 25 (Random.int 0 4)))

newPiece: Int -> Piece
newPiece i =
    {x=(modBy hSize i)
    ,y=(i // hSize)
    ,color = 0
    ,matching = False
    ,moving = False
    ,current = Position 0 0
    ,start = Position 0 0
    }

hMatchAt: Piece -> Model -> List Piece
hMatchAt p model =
    let
        right = List.filter (\q -> q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x ) model.conf
        others = List.filter (\q -> not(q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x) ) model.conf
    in
       if (List.length right) >= 3 then
           List.concat 
               [(List.map (\q -> {q | matching =True }) right)
               ,others]
       else
           model.conf

vMatchAt: Piece -> Model -> List Piece
vMatchAt p model =
    let
        below = List.filter (\q -> q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y ) model.conf
        others = List.filter (\q -> not(q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y) ) model.conf
    in
        if (List.length below) >= 3 then
            List.concat 
               [(List.map (\q -> {q | matching =True }) below)
               ,others]
        else
            model.conf
        
match: Model -> Model
match model =
    let
        newconf = List.foldl (\p conf -> vMatchAt p {model | conf = conf}) model.conf model.conf
        newnewconf = List.foldl (\p conf -> hMatchAt p {model | conf = conf}) newconf newconf
        isMatched = List.any (\p -> p.matching) newnewconf
    in
        if isMatched then
            {model | conf =Debug.log"" newnewconf ,state=Matching}
        else
            {model| state = Waiting}

deleteMatch: Model -> Model
deleteMatch model =
    {model | conf = List.filter(\p -> not p.matching) model.conf
    ,state=Deleting
    ,elapsed=0 }

dropColumn: List Piece -> List Piece
dropColumn conf =
    List.sortBy .y conf |> List.indexedMap (\i p -> {p| y=i})
        
drop: Model -> Model
drop model =
    let
        conf = List.concat <| List.map(\j ->drop1 j model) (List.range 0 4)
    in
        {model | conf=conf
        ,state = Adding
        ,elapsed = 0}


drop1: Int -> Model -> List Piece
drop1  j model =
    
       dropColumn (List.filter (\p -> p.x==j) model.conf)

add: List Int -> Model -> Model
add rList model =
    let
        conf=model.conf
        newconf = List.concat <|
                  List.map
                  (\j -> add1 j (List.take 5 <| List.drop (5*j) rList) model)
                  (List.range 0 4)
    in
        {model | conf = newconf
        --,state = Matching}
        ,state = Waiting}

add1: Int -> List Int -> Model -> List Piece
add1 j rList model =
    let
        column = List.filter (\p -> p.x==j) model.conf
        colors = List.take (5-(List.length column)) rList
        addend = List.indexedMap
                 (\i c -> {x = j
                          ,y=((List.length column) +i)
                          ,color = c
                          ,matching = False
                          ,moving = False
                          ,current = Position 0 0
                          ,start = Position 0 0
                          })
                 colors
    in
        List.concat [column,addend]

            

swap: Piece -> Model -> Model
swap end model =
    case model.state of
        Moving start ->
            let
                others = List.filter(\p -> ((p.x/=end.x) || (p.y/=end.y)) && ((p.x/=start.x) || (p.y/=start.y))) model.conf
                distance = (abs (start.x - end.x))+(abs (start.y - end.y))
            in
                if distance /= 1 then
                    {model | state = Waiting}
                else
                    let
                        swapped = match <|
                                  {model | conf = List.concat[
                                                   [{start|color =end.color
                                                    ,start=Position 0 0
                                                    ,current=Position 0 0
                                                    }
                                                   ,{end|color=start.color
                                                    ,start=Position 0 0
                                                    ,current=Position 0 0
                                                    }]
                                                  ,others]
                                  ,elapsed = 0}
                        existMatching = List.foldl (\p matching -> p.matching || matching) False swapped.conf
                    in
                        if existMatching then
                            swapped
                        else
                            model
        _ -> model
        
update msg model =
    case msg of
        Generated rlist ->( match<| {model|conf=randomize model.conf rlist}, Cmd.none)
        MoveStart piece pos ->
            let
                moving = List.map (\p -> {p|moving=True
                                         ,start=pos
                                         ,current=pos
                                         }
                                  ) <|
                         List.filter (\p -> piece.x == p.x && piece.y == p.y) model.conf
                others = List.filter (\p -> piece.x /= p.x || piece.y /= p.y) model.conf
                conf = List.concat [others, moving]
            in
                ({model | state = (Moving piece)
                 ,conf = conf
                 },Cmd.none)
        MoveEnd pos ->
            let
                newx = floor (pos.x / (toFloat unit))
                newy = hSize - floor (pos.y / (toFloat unit))
                dest = List.head <| List.filter (\p -> p.x==newx && p.y==newy) model.conf
            in
                case dest of
                    Just piece -> (match <|swap piece model , Cmd.none)
                    _ -> (model , Cmd.none)
        Added rlist -> (add rlist model,Cmd.none)
        Move pos ->
            let
                dummy = Debug.log "moving" pos
            in
                ({model | conf = List.map (\p -> if p.moving then
                                                     {p | current = pos}
                                                 else
                                                     p
                                          ) model.conf
                 }, Cmd.none)
        Elapsed t ->
            case model.state of
                Waiting ->
                    (match model, Cmd.none)
                Matching ->
                    if model.elapsed > 1 then
                        (deleteMatch model,Cmd.none)
                    else
                        ({model | elapsed = (model.elapsed+1)},Cmd.none)
                Deleting ->
                    if model.elapsed > 1 then
                        (drop model
                        ,Random.generate Added (Random.list 25 (Random.int 0 4)))
                    else
                        ({model | elapsed = (model.elapsed+1)},Cmd.none)
                _ -> ({model | elapsed = (model.elapsed+1)},Cmd.none)
        _ ->(model,Cmd.none)

randomize conf rlist =
    List.indexedMap (\idx p -> {p | color = Maybe.withDefault 0
                                    (List.head (List.drop idx rlist))}) conf
    
pallet colornum =
    case colornum of
        0 -> "violet"
        1 -> "pink"
        2 -> "skyblue"
        3 -> "lightgreen"
        4 -> "orange"
        _ -> "black"

unit = 50
pieceView: Piece -> Svg Msg
pieceView pdata =
    let
        xpixel = if pdata.moving then
                    (toFloat <| pdata.x*unit+100)+pdata.current.x-pdata.start.x
                else
                    toFloat (pdata.x*unit+100)
        ypixel = if pdata.moving then
                    (toFloat <| hSize*unit-pdata.y*unit+100)+pdata.current.y-pdata.start.y
                else
                    toFloat <| hSize*unit-pdata.y*unit+100
    in
    g [transform ("translate(" ++ (String.fromFloat xpixel)
                      ++ "," ++ (String.fromFloat ypixel) ++ ")")
      ,onDown (relativePos >> (MoveStart pdata))
      ,onMove (relativePos >> Move)
      ,onUp (relativePos >> MoveEnd)
      ]
    [rect [x "0"
          ,y "0"
          ,width (String.fromInt unit)
          ,height (String.fromInt unit)
          ,stroke "white"
          ,strokeWidth "4px"
          ,fill (if pdata.matching then
                        "red"
                    else
                        "#aaa")
          ]
         [
         ]
    ,circle [cx (String.fromInt (unit//2))
            ,cy (String.fromInt (unit//2))
            ,r (String.fromInt (unit//3))
            ,fill ( pallet pdata.color)
            ]
         []
    ]

relativePos : Event -> {x:Float, y:Float}
relativePos event =
    {x=(Tuple.first event.pointer.offsetPos)-100
    ,y=(Tuple.second event.pointer.offsetPos)-100
    }
    
view model =
    Html.div [Html.Attributes.style "touch-action" "none"]
        [svg [width "500"
             ,height "500"
             ]
             (List.map pieceView model.conf)
        ]
        
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 100 Elapsed

