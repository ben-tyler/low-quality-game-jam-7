port module Main exposing (..)

import Browser
import Browser.Events as E
import Html.Events as HE
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src, style)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Html.Attributes exposing (hidden)

type Hold 
    = PickingUp GameObject 
    | NotHolding 
    | Carry GameObject 
    | PlaceInOven GameObject 
    | PlaceOnBenchOne GameObject
    | PlaceOnBenchTwo GameObject

type alias Model =
    { idcounter : Int
    , benchOneItems : List GameObject
    , benchTwoItems : List GameObject
    , oven : List GameObject
    , player : GameObject
    , holding : Hold
    , ovenGO : GameObject
    , level : Level
    , benchOneGO : GameObject
    , benchTwoGO : GameObject
    , banner : String
    , cookies : List GameObject
    }

type RLevel = RLevel Level | WinGame
type alias Level = 
    { level : Int
    , benchOneItems : List GameObject
    , benchTwoItems : List GameObject
    , winCondition : List GameObject -> Bool
    , loseCondition : List GameObject -> Bool
    , ovenText : String
    , nextLevel : RLevel
    }

levelOne : Level
levelOne = 
    { level = 1
    , benchOneItems = 
        []
        |> addToBenchOne  (GameObject 4 "butter.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 3 "flower.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 2 "egg.gif" 240 130 50 Nothing) 
    , benchTwoItems = []
    , winCondition = 
        (\ oven -> List.length oven == 3)
    , loseCondition = 
        ( \ oven -> False )
    , ovenText = "This is your oven, place ingredients to bake a cookie"
    , nextLevel = RLevel levelTwo
    }

levelTwo : Level
levelTwo = 
    { level = 2
    , benchOneItems = 
        []
        |> addToBenchOne  (GameObject 4 "butter.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 3 "flower.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 2 "egg.gif" 240 130 50 Nothing) 
    , benchTwoItems = []
    , winCondition = 
        (\ oven ->
            case List.reverse oven of 
                x::xs::xxs::rest -> 
                    if x.id == 2 && xs.id == 4 && xxs.id == 3 then 
                        True
                    else
                        False
                _ -> False
        )
    , loseCondition = 
        (\ oven ->
            case List.reverse oven of 
                x::xs::rest -> 
                    if x.id /= 2 || xs.id /= 4 then 
                        True
                    else
                        False

                x::rest -> 
                    if x.id /= 2 then 
                        True
                    else
                        False
                _ -> False
        )
    , ovenText = "First Add Egg, Then butter and Finally the flour"
    , nextLevel = RLevel levelThree
    }


levelThree : Level
levelThree = 
    { level = 2
    , benchOneItems = 
        []
        |> addToBenchOne  (GameObject 4 "butter.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 3 "flower.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 18 "sugar.gif" 240 130 50 Nothing) 
        |> addToBenchOne  (GameObject 2 "egg.gif" 240 130 50 Nothing) 
        
    , benchTwoItems = []
    , winCondition = 
        (\ oven ->
            case List.reverse oven of 
                x::xs::xxs::xxxs::rest -> 
                    if x.id == 3 && xs.id == 2 && xxs.id == 4 && xxxs.id == 18 then 
                        True
                    else
                        False
                _ -> False
        )
    , loseCondition = 
        (\ oven ->
            case List.reverse  oven of 
                x::xs::rest -> 
                    if x.id /= 3 || xs.id /= 2 then 
                        True
                    else
                        False

                x::rest -> 
                    if x.id /= 3 then 
                        True
                    else
                        False
                _ -> False
        )
    , ovenText = "First Add Flouer, Then Egg and Finally the Butter and then sugar"
    , nextLevel = WinGame
    }

applyLevelToModel : Level -> Model -> Model
applyLevelToModel level model = 
    { model 
        | level = level
        , benchOneItems = level.benchOneItems
        , benchTwoItems = level.benchTwoItems
        , oven = []
        , holding = NotHolding
    }

type alias GameObject =
    { id : Int
    , gif : String
    , x : Int
    , y : Int
    , size : Int
    , moveTo : Maybe (Int, Int)
    }

speed : Int
speed = 4

collisionSize : Int
collisionSize = 10

init : () -> ( Model, Cmd Msg )
init _ =
    ( { idcounter = 10
      , benchOneItems = []
      , benchTwoItems = [] 
      , oven = []
      , player = GameObject 1 "player.gif" 40 200 128 Nothing
      , holding = NotHolding
      , ovenGO = (GameObject 5 "oven.gif" 600 302 128 Nothing) 
      , level = levelOne
      , benchOneGO = (GameObject -1 "bench.gif" 180 450 90 Nothing) 
      , benchTwoGO = (GameObject -2 "bench.gif" 330 450 90 Nothing) 
      , banner = "Low Quaility Game Jam 7"
      , cookies = []
      } |> applyLevelToModel levelOne
    , Cmd.none
    )

addToCookies : List GameObject -> List GameObject
addToCookies cookies = 
    cookies ++ 
    [ GameObject 
        26 
        "cookie.gif" 
        600
        302
        90
        (Just ((10 + List.length cookies * 50), 10))
    ]

addToBenchOne :  GameObject -> List GameObject -> List GameObject
addToBenchOne object benchOne =
    { object
        | x = 200
        , y = -30 * List.length benchOne + 420
    } ::benchOne 

addToBenchTwo :  GameObject -> List GameObject -> List GameObject
addToBenchTwo object bench =
    { object
        | x = 350
        , y = -30 * List.length bench + 420
    } :: bench


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ E.onAnimationFrameDelta AnimationFrame
        ]

view : Model -> Html Msg
view model =
    div 
        [ HE.on "click" (Decode.map UpdatePosition decoder )
        ]
        [ img 
            [src "background.gif"]
            []
        , div  
            [ style "position" "absolute"
            , style "top" <| String.fromInt  251 ++ "px"
            , style "left" <| String.fromInt 600 ++ "px"
            , style "background" "white"
            , style "padding" "5px"
            , style "width" "150px"
            ] 
            [ text model.level.ovenText
            ]
        , viewGameObject model.ovenGO
        , viewGameObject model.player
        , viewGameObject model.benchOneGO
        , viewGameObject model.benchTwoGO
        , div []   <| List.map viewGameObject model.benchOneItems
        , div []   <| List.map viewGameObject model.benchTwoItems
        , case model.holding of
            NotHolding -> div [] []
            PickingUp g -> div  [] []
            Carry g -> viewGameObject g
            PlaceInOven g -> viewGameObject g
            PlaceOnBenchOne g -> viewGameObject g
            PlaceOnBenchTwo g -> viewGameObject g
        , div []   <| List.map viewGameObject model.cookies
        , Html.h1 
            [ style "position" "absolute"
            , style "top" <| String.fromInt  50 ++ "px"
            , style "left" <| String.fromInt 10 ++ "px"
            ] [text model.banner]
        , div [hidden True] 
            [img [src "cookie.gif"] [], img [src "sugar.gif"] []]
        ]


type alias MouseClick =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }

decoder : Decode.Decoder MouseClick
decoder =
    Decode.map4 MouseClick
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        (Decode.at [ "target", "offsetWidth" ] Decode.float)


viewGameObject : GameObject -> Html Msg
viewGameObject gameObject = 
    div 
            [ style "position" "absolute"
            , style "top" <| String.fromInt gameObject.y ++ "px"
            , style "left" <| String.fromInt gameObject.x ++ "px"
            , HE.onClick <| ClickObject gameObject.id
            ] 
            [ div 
                    [style "position" "relative"]
                    [ img
                        [ src gameObject.gif
                        , style "width" <| String.fromInt gameObject.size ++ "px"
                        , style "height" <| String.fromInt gameObject.size ++ "px"
                        ]
                        []
                    ]
            ]


type Msg
    = AnimationFrame Float
    | UpdatePosition MouseClick
    | ClickObject Int


collide : { a | x : Int, y : Int} -> { b | x : Int, y : Int }  -> Bool
collide go1 go2 = 
    go1.x > go2.x - collisionSize && go1.x < go2.x + collisionSize && go1.y > go2.y - collisionSize && go1.y < go2.y + collisionSize 

moveGameObject : GameObject -> GameObject
moveGameObject go =
    case go.moveTo of 
        Just (x, y) -> 
            if collide go {x = x, y = y} then 
                {go | moveTo = Nothing }
            else 
                case ( x < go.x, y < go.y) of
                    (True, True) -> 
                        {go | x = go.x - speed, y = go.y - speed }

                    (True, False) -> 
                        { go | x = go.x - speed, y = go.y + speed }

                    (False, False) -> 
                        { go | x = go.x + speed, y = go.y + speed }
                       
                    (False, True) -> 
                        { go | x = go.x + speed, y = go.y - speed }

        Nothing -> go


offsetBench : GameObject -> GameObject                 
offsetBench b = 
    { b | x = b.x , y = b.y - 80 }

updateWithCarry : GameObject -> Model -> MouseClick -> (Model, Cmd Msg)
updateWithCarry g model m = 
    let p = model.player in 
    ( { model 
        | player =  { p | moveTo = Just (m.offsetX - p.size // 2, m.offsetY - p.size // 2)}
        , holding = Carry g
    }
    , Cmd.none
    )

checkBenchesForItem : Model -> Int -> (Bool, Bool)
checkBenchesForItem model id =
    ( List.any ( \ i -> i.id == id ) model.benchOneItems || id == model.benchOneGO.id
    , List.any ( \ i -> i.id == id ) model.benchTwoItems || id == model.benchTwoGO.id
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame f -> 
            let
                nextModel = { model | cookies = List.map moveGameObject model.cookies}
            in
            case nextModel.holding of 
                PlaceOnBenchTwo g -> 
                    let 
                        p = nextModel.player 
                        x = nextModel.benchTwoGO |> offsetBench
                    in 
                    
                    if collide p x then 
                        ( { nextModel 
                            | holding = NotHolding
                            , benchTwoItems = model.benchTwoItems |> addToBenchTwo g
                        }
                        , killme ""
                        )
                    else
                        ( { nextModel 
                            | holding = PlaceOnBenchTwo <| moveGameObject { g | moveTo = Just (p.x + p.size // 2, p.y + p.size // 2) }
                            , player = moveGameObject { p | moveTo = Just (x.x, x.y)}
                            
                        }
                        , Cmd.none 
                        )


                PlaceOnBenchOne g -> 
                    let 
                        p = nextModel.player 
                        x = nextModel.benchOneGO |> offsetBench
                    in 
                    if collide p x then 
                        ( { nextModel 
                            | holding = NotHolding
                            , benchOneItems = model.benchOneItems |> addToBenchOne g
                        }
                        , killme ""
                        )
                    else
                        ( { nextModel 
                            | holding = PlaceOnBenchOne <| moveGameObject { g | moveTo = Just (p.x + p.size // 2, p.y + p.size // 2) }
                            , player = moveGameObject { p | moveTo = Just (x.x, x.y)}
                            
                        }
                        , Cmd.none 
                        )

                PlaceInOven g -> 
                    handleOven nextModel g 

                NotHolding -> 
                    ( { nextModel 
                        | player = moveGameObject model.player
                      }
                    , Cmd.none 
                    )

                PickingUp g -> 
                    let p = nextModel.player in 
                    if collide g p then 
                        ( { nextModel 
                            | player = moveGameObject { p | moveTo = Just (g.x, g.y) }
                            , holding = Carry g
                            , benchOneItems = List.filter ( \ j -> g.id /= j.id ) model.benchOneItems
                            , benchTwoItems =  List.filter ( \ j -> g.id /= j.id ) model.benchTwoItems
                            }
                        , killme ""
                        )

                    else 
                        ( { nextModel | player = moveGameObject { p | moveTo = Just (g.x, g.y) }}, Cmd.none )

                Carry g -> 
                    let p = model.player in 
                    ( { nextModel 
                        | holding = Carry <| moveGameObject { g | moveTo = Just (p.x + p.size // 2, p.y + p.size // 2) }
                        , player = moveGameObject model.player
                        
                     }
                    , Cmd.none 
                    )
                    
           
        UpdatePosition m ->
            if m.offsetHeight == 600 then 
                let p = model.player in 
                case model.holding of 
                    NotHolding -> 
                        ( { model 
                                | player = 
                                    { p |
                                        moveTo = Just (m.offsetX - p.size // 2, m.offsetY - p.size // 2)
                                    }
                        }
                        , Cmd.none
                        )

                    Carry g -> 
                        updateWithCarry g model m

                    PlaceInOven g -> 
                        updateWithCarry g model m

                    PlaceOnBenchOne g -> 
                        updateWithCarry g model m

                    PlaceOnBenchTwo g -> 
                        updateWithCarry g model m

                    _ -> (model, Cmd.none)


            else 
                (model, Cmd.none)
           


        ClickObject id -> 
            case model.holding of 
                NotHolding -> 
                    let
                        pickup bench = 
                            case bench |> List.take 1  of 
                                [] -> NotHolding
                                x::xs -> PickingUp x 

                    in
                    case checkBenchesForItem model id of 
                        (True, False) -> 
                            ({ model 
                                | holding = pickup model.benchOneItems
                             }
                            , Cmd.none
                            )

                        (False, True) -> 
                            ({ model 
                                | holding = pickup model.benchTwoItems
                             }
                            , Cmd.none
                            )

                        _ -> (model, Cmd.none)

                Carry g -> 
                    if id == model.ovenGO.id then 
                        ( { model | holding = PlaceInOven g}, Cmd.none) 
                    else 
                        case checkBenchesForItem model id of 
                            (True, False) -> 
                                ({ model 
                                    | holding = PlaceOnBenchOne g
                                }
                                , Cmd.none
                                )

                            (False, True) -> 
                                ({ model 
                                    | holding = PlaceOnBenchTwo g
                                }
                                , Cmd.none
                                )

                            _ -> (model, Cmd.none)


                _ -> (model, Cmd.none)


handleOven : Model -> GameObject -> ( Model, Cmd Msg )
handleOven nextModel g =
    let 
        p = nextModel.player 
        nextNextModel = 
            { nextModel 
                | holding = NotHolding
                , oven = g :: nextModel.oven 
            }
    in 
    if collide p nextNextModel.ovenGO then 
        if nextNextModel.level.winCondition nextNextModel.oven then 
            case nextNextModel.level.nextLevel of 
                RLevel l -> 
                    ( applyLevelToModel l nextNextModel
                        |> ( \ m -> { m | cookies = addToCookies m.cookies})
                    , killme ""
                    )

                WinGame ->
                    ( { nextNextModel | banner = "YOU WIN"} |> ( \ m -> { m | cookies = addToCookies m.cookies}), killme "")

        else if nextNextModel.level.loseCondition nextNextModel.oven then 
            ( applyLevelToModel nextNextModel.level { nextNextModel | banner = "NO! Try Again"} 
            , killme ""
            )
        else
            ( { nextNextModel | banner = "Low Quaility Game Jam 7" }
            , killme ""
            )
    else 
        ( { nextModel 
            | holding = PlaceInOven <| moveGameObject { g | moveTo = Just (p.x + p.size // 2, p.y + p.size // 2) }
            , player = moveGameObject { p | moveTo = Just (nextModel.ovenGO.x, nextModel.ovenGO.y)}
            
        }
        , Cmd.none 
        )


port killme : String -> Cmd msg