module PaginatedList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type PaginatedList a
    = PaginatedList 
        { values : List a
        , totalPage : Int
        , page : Int -- the current page 
        }

-- create 
fromList : Int -> Int -> Int -> List a -> PaginatedList a
fromList totalPage page itemPerPage fav =
    let
        val = List.drop ((page - 1) * itemPerPage)
            <| List.take (page * itemPerPage) fav
    in
  
    PaginatedList { values = val, totalPage = totalPage, page = page}

-- info
values : PaginatedList a -> List a
values (PaginatedList info) =
    info.values

-- view 
view : (Int -> msg) -> PaginatedList a -> Html msg
view toMsg (PaginatedList info) =
    let
        viewPageLink pageNum =
            pageLink toMsg pageNum (pageNum == info.page)  
    in
    if info.totalPage > 1 then
        nav [class "pagination"] 
        [ ul [class "pagination__list"] 
            <| List.map viewPageLink
            <| List.range 1 info.totalPage
        ]
  else
    text ""
    


pageLink : (Int -> msg) -> Int -> Bool -> Html msg
pageLink toMsg targetNum isActie =
    li [classList [("pagination__item", True), ("pagination__item-active", isActie)]] 
        [ a [class "pagination__link", href "", onClick (toMsg targetNum)] [ text (String.fromInt targetNum)] ]