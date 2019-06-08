module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task as Task exposing (Task)
import Url



-- ---------------------------
-- MODEL
-- ---------------------------


type alias User =
    { name : String
    }


type alias Recommend =
    { hotelList : List Hotel
    }


type alias Hotel =
    { hotelId : Int
    , name : String
    , description : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , user : User
    , recommend : Recommend
    }


getUser : Cmd Msg
getUser =
    Task.perform GotUser <|
        Task.succeed <|
            User "えび"


getRecommend : Cmd Msg
getRecommend =
    Task.perform GotRecommend <|
        Task.succeed <|
            Recommend
                [ Hotel 1 "さくらちゃんランド" "ヤギ"
                , Hotel 2 "にゃんこ宿" "子猫が可愛い癒やしの宿です。"
                , Hotel 3 "ワンワンホテル" "大型犬とはしゃげるホテルです。"
                ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (User "") (Recommend [])
    , Cmd.batch [ getUser, getRecommend ]
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotUser User
    | GotRecommend Recommend


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        GotUser user ->
            ( { model | user = user }, Cmd.none )

        GotRecommend recommend ->
            ( { model | recommend = recommend }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    let
        { user, recommend } =
            model
    in
    { title = "URL Interceptor"
    , body =
        [ headerView user
        , section [] <|
            [ h1 [] [ text "あなたにオススメの宿" ]
            , div [ class "recommend-hotel-list" ] <|
                List.map hotelView recommend.hotelList
            ]
        ]
    }


headerView : User -> Html Msg
headerView user =
    header [ class "cmn-header" ]
        [ p [] [ text <| "ようこそ " ++ user.name ++ " さん" ]
        ]


hotelView : Hotel -> Html Msg
hotelView hotel =
    a [ class "page-top-hotellist-article__link" ]
        [ article [ class "page-top-hotellist-article" ]
            [ img [ class "__thumb", src "https://4.bp.blogspot.com/-LR5Lja-lZ4E/WZVgz8oz0zI/AAAAAAABGE8/dA0DAXWkQFIY23wjjILccR7m8KXHSAzzACLcBGAs/s400/building_hotel_small.png" ] []
            , h3 [ class "__title" ] [ text hotel.name ]
            , p [ class "__description" ]
                [ text hotel.description
                ]
            ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
