module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (fromList, Location, Matrix)
import Time exposing (millisecond, second, Time)


main : Program Never Board Msg
main =
    Html.program
        { init = ( initialBoard, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Cell =
    Int


type alias Board =
    Matrix Cell



-- CONSTANTS


livingCell : Cell
livingCell =
    1


deadCell : Cell
deadCell =
    0


initialBoardAsList : List (List number)
initialBoardAsList =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]


neighborCoords =
    [ ( -1, -1 )
    , ( -1, 0 )
    , ( -1, 1 )
    , ( 0, -1 )
    , ( 0, 1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    ]



-- HELPER FUNCTIONS


initialBoard : Board
initialBoard =
    fromList initialBoardAsList


isAlive : Cell -> Bool
isAlive c =
    c == livingCell


getNeighbors : Board -> Location -> List Cell
getNeighbors board location =
    let
        width =
            Matrix.colCount board

        height =
            Matrix.rowCount board

        getNewRow row =
            (row + Matrix.row location) % height

        getNewCol col =
            (col + Matrix.col location) % width

        newLocation ( row, col ) =
            Matrix.loc (getNewRow row) (getNewCol col)
                |> (\l -> Matrix.get l board)
                |> Maybe.withDefault 0
    in
        neighborCoords
            |> List.map newLocation


getNextCellGeneration : List Cell -> Cell -> Cell
getNextCellGeneration neighbors cell =
    let
        neighborsSum =
            neighbors
                |> List.foldl (+) 0

        getLivingCellNextGen =
            if neighborsSum == 2 || neighborsSum == 3 then
                livingCell
            else
                deadCell

        getDeadCellNextGen =
            if neighborsSum == 3 then
                livingCell
            else
                deadCell
    in
        if isAlive cell then
            getLivingCellNextGen
        else
            getDeadCellNextGen



-- UPDATE


type Msg
    = NoOp
    | Tick


update : Msg -> Board -> ( Board, Cmd Msg )
update msg board =
    case msg of
        NoOp ->
            ( board, Cmd.none )

        Tick ->
            ( nextGen board, Cmd.none )


nextGen : Board -> Board
nextGen previousBoard =
    let
        getNeighborsForLocation =
            getNeighbors previousBoard

        getCellNextGen l c =
            getNextCellGeneration (getNeighborsForLocation l) c
    in
        Matrix.mapWithLocation getCellNextGen previousBoard



-- VIEW


view : Board -> Html Msg
view board =
    div []
        [ viewBoard board
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        boardList =
            Matrix.toList board

        evalCell cell =
            if cell == 1 then
                "alive"
            else
                "dead"

        renderCell cell =
            div [ class (evalCell cell) ] []

        renderRow cells =
            div [ class "board-row" ]
                (List.map renderCell cells)
    in
        div [ class "board" ]
            (List.map renderRow boardList)



-- SUBSCRIPTIONS


subscriptions : Board -> Sub Msg
subscriptions model =
    Time.every (750 * millisecond) (\_ -> Tick)
