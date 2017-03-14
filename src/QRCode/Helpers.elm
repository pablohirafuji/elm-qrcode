module QRCode.Helpers exposing
    ( listResult
    , breakStr
    , transpose
    )



listResult : ( a -> Result x b ) -> List b -> List a -> Result x (List b)
listResult fun listb lista =
    case lista of
        head :: tail ->
            fun head
                |> Result.map (\r -> r :: listb)
                |> Result.andThen (flip (listResult fun) tail)

        [] ->
            Result.Ok (List.reverse listb)


-- From elm-community/string-extra

breakStr : Int -> String -> List String
breakStr width string =
    if width == 0 || string == ""
        then [ string ]
        else breaker width string []


breaker : Int -> String -> List String -> List String
breaker width string acc =
    case string of
        "" ->
            List.reverse acc

        _ ->
            breaker width
                (String.dropLeft width string)
                ((String.slice 0 width string) :: acc)


-- From elm-community/list-extra

transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
                (x :: heads) :: transpose (xs :: tails)
