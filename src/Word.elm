module Word exposing (Word, all, map, new, toSet)

import Set exposing (Set)


type Word
    = Word (List Char)


new : String -> Maybe Word
new string =
    let
        chars =
            String.toList string
    in
    if List.length chars >= 3 && List.length chars <= 15 && List.all Char.isAlpha chars then
        Just <| Word <| List.map Char.toUpper chars

    else
        Nothing


toSet : Word -> Set Char
toSet (Word chars) =
    Set.fromList chars


all : (Char -> Bool) -> Word -> Bool
all isOkay (Word chars) =
    List.all isOkay chars


map : (Char -> a) -> Word -> List a
map f (Word chars) =
    List.map f chars
