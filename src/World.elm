module World exposing (..)

import Matrix exposing (Location, Matrix, loc)
import Tile exposing (Tile(..), isEggTile, isEmptyTile, isLifeTile, isResourceTile, tileEnergy)


type alias World =
    Matrix Tile


getLifeTiles : World -> List ( Location, Tile )
getLifeTiles world =
    getAllTilesWithLocation world
        |> List.filter (\( _, tile ) -> isLifeTile tile)


getEggTiles : World -> List ( Location, Tile )
getEggTiles world =
    getAllTilesWithLocation world
        |> List.filter (\( _, tile ) -> isEggTile tile)


getAllTilesWithLocation : World -> List ( Location, Tile )
getAllTilesWithLocation world =
    let
        lastColIndex =
            Matrix.colCount world - 1

        lastRowIndex =
            Matrix.rowCount world - 1

        cols =
            List.range 0 lastColIndex

        rows =
            List.range 0 lastRowIndex
    in
    List.concatMap (\col -> List.map (\row -> getTileWithLocation world row col) rows) cols


getTileWithLocation : World -> Int -> Int -> ( Location, Tile )
getTileWithLocation world row col =
    let
        location =
            loc row col

        tile =
            Matrix.get location world
    in
    ( location, Maybe.withDefault Empty tile )


moveTowardsBestResource : World -> Location -> World
moveTowardsBestResource world lifeLoc =
    let
        closestResource =
            getAllTilesWithLocation world
                |> List.filter (\( _, tile ) -> isResourceTile tile)
                |> List.sortBy (\( tileLoc, tile ) -> relativeTileValue lifeLoc tileLoc tile)
                |> List.reverse
                |> List.head

        lifeEnergy =
            case Matrix.get lifeLoc world of
                Just (Life energy) ->
                    energy

                _ ->
                    Debug.crash <| "Expected to find life at tile " ++ toString lifeLoc
    in
    case closestResource of
        Just ( resourceLoc, Resource _ ) ->
            let
                _ =
                    Debug.log "Moving towards closest resource" resourceLoc

                lRow =
                    Matrix.row lifeLoc

                lCol =
                    Matrix.col lifeLoc

                rRow =
                    Matrix.row resourceLoc

                rCol =
                    Matrix.col resourceLoc

                newLifeEnergy =
                    lifeEnergy - 1
            in
            if lCol > (rCol + 1) then
                Matrix.set lifeLoc Empty world
                    |> Matrix.update (loc lRow (lCol - 1)) (moveLifeToTile newLifeEnergy)
            else if lCol < (rCol - 1) then
                Matrix.set lifeLoc Empty world
                    |> Matrix.update (loc lRow (lCol + 1)) (moveLifeToTile newLifeEnergy)
            else if lRow > (rRow + 1) then
                Matrix.set lifeLoc Empty world
                    |> Matrix.update (loc (lRow - 1) lCol) (moveLifeToTile newLifeEnergy)
            else if lRow < (rRow - 1) then
                Matrix.set lifeLoc Empty world
                    |> Matrix.update (loc (lRow + 1) lCol) (moveLifeToTile newLifeEnergy)
            else
                Debug.crash "Trying to move life tile towards a resource but it's already right next to it?!"

        _ ->
            Matrix.set lifeLoc (Life (lifeEnergy - 1)) world
                |> Debug.log "No more resources to move to. Staying put."


moveLifeToTile : Int -> Tile -> Tile
moveLifeToTile lifeEnergy newTile =
    case newTile of
        Life newTileEnergy ->
            Life (lifeEnergy + newTileEnergy) |> Debug.log "Two lifeforms collided!"

        Egg _ ->
            Life lifeEnergy |> Debug.log "An egg was squahed :("

        _ ->
            Life lifeEnergy


relativeTileValue : Location -> Location -> Tile -> Float
relativeTileValue origin tileLoc tile =
    let
        distance =
            distanceBetweenLocations origin tileLoc

        energy =
            tileEnergy tile
    in
    toFloat energy - distance


distanceBetweenLocations : Location -> Location -> Float
distanceBetweenLocations l1 l2 =
    let
        r1 =
            Matrix.row l1

        c1 =
            Matrix.col l1

        r2 =
            Matrix.row l2

        c2 =
            Matrix.col l2
    in
    abs <| sqrt <| toFloat <| (c1 - c2) ^ 2 + (r1 - r2) ^ 2


getFirstNeighbouringResource : World -> Location -> Maybe ( Location, Tile )
getFirstNeighbouringResource world origin =
    getFirstNeighbourBy isResourceTile world origin
        |> Debug.log "First neighbouring resource"


getFirstNeighbouringEmpty : World -> Location -> Maybe ( Location, Tile )
getFirstNeighbouringEmpty world origin =
    getFirstNeighbourBy isEmptyTile world origin
        |> Debug.log "First neighbouring empty tile"


getFirstNeighbourBy : (Tile -> Bool) -> World -> Location -> Maybe ( Location, Tile )
getFirstNeighbourBy f world origin =
    getNeighbours world origin
        |> List.map (\( location, mTile ) -> ( location, Maybe.withDefault Empty mTile ))
        |> List.filter (\( _, tile ) -> f tile)
        |> List.head


getNeighbours : World -> Location -> List ( Location, Maybe Tile )
getNeighbours world origin =
    let
        row =
            Matrix.row origin

        col =
            Matrix.col origin

        neighbourLocs =
            [ ( row - 1, col - 1 )
            , ( row - 1, col )
            , ( row - 1, col + 1 )
            , ( row, col - 1 )
            , ( row, col + 1 )
            , ( row + 1, col - 1 )
            , ( row + 1, col )
            , ( row + 1, col + 1 )
            ]
    in
    List.map (\( x, y ) -> ( loc x y, Matrix.get ( x, y ) world )) neighbourLocs
