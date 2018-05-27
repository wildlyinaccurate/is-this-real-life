module Tile exposing (..)


type Tile
    = Life Int
    | Resource Int
    | Empty


tileEnergy : Tile -> Int
tileEnergy tile =
    case tile of
        Resource energy ->
            energy

        Life energy ->
            energy

        Empty ->
            0


isLifeTile : Tile -> Bool
isLifeTile tile =
    case tile of
        Life _ ->
            True

        _ ->
            False


isResourceTile : Tile -> Bool
isResourceTile tile =
    case tile of
        Resource _ ->
            True

        _ ->
            False


increaseTileEnergy : Int -> Tile -> Tile
increaseTileEnergy amount tile =
    case tile of
        Life energy ->
            tile
                |> Debug.log "Life was blessed with random energy but the world is cruel coveted the energy for itself"

        Resource energy ->
            Resource (energy + amount)
                |> Debug.log "Resource grew itself"

        Empty ->
            Resource amount |> Debug.log "New resource spawned"


incrementLifeEnergy : Tile -> Tile
incrementLifeEnergy tile =
    case tile of
        Life energy ->
            Life (energy + 1)
                |> Debug.log "Life took some energy from a resource"

        _ ->
            tile |> Debug.log "Tried to increment life energy but tile was"
