module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task as Task exposing (Task)
import Url
import Url.Parser as P exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Q



-- ---------------------------
-- Routing
-- ---------------------------


type Route
    = Top
    | HotelPage HotelId
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map Top P.top
        , P.map HotelPage (P.s "hotel" </> P.int)
        ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound <| P.parse routeParser url



-- ---------------------------
-- MODEL
-- ---------------------------


type alias User =
    { userName : String
    }


type alias Recommend =
    { hotelOverviewList : List HotelOverview
    }


type alias HotelId =
    Int


type alias HotelOverview =
    { hotelId : HotelId
    , hotelName : String
    , description : String
    }


type alias Hotel =
    { hotelName : String
    , planList : List HotelPlan
    }


type alias HotelPlanId =
    Int


type alias HotelPlan =
    { planId : Int
    , planName : String
    , accommodationFee : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , user : User
    , recommend : Recommend
    , hotel : Hotel
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
                [ HotelOverview 1 "さくらちゃんランド" "ヤギ"
                , HotelOverview 2 "にゃんこ宿" "子猫が可愛い癒やしの宿です。"
                , HotelOverview 3 "ワンワンホテル" "大型犬とはしゃげるホテルです。"
                ]


getHotel : HotelId -> Cmd Msg
getHotel hotelId =
    Task.attempt GotHotel <|
        case hotelId of
            1 ->
                Task.succeed <|
                    Hotel "さくらちゃんランド"
                        [ HotelPlan 1 "スイートルームプラン" 10000
                        , HotelPlan 2 "さくらちゃんと戯れプラン" 20000
                        ]

            2 ->
                Task.succeed <|
                    Hotel "にゃんこ宿"
                        [ HotelPlan 3 "にゃんにゃんプラン" 15000 ]

            3 ->
                Task.succeed <|
                    Hotel "ワンワンホテル"
                        [ HotelPlan 4 "ワンワンプラン" 13000 ]

            _ ->
                Task.fail "Hotel Not Found"


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url (User "") (Recommend []) (Hotel "" [])
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
    | GotHotel (Result String Hotel)


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

        GotHotel result ->
            case result of
                Ok hotel ->
                    ( { model | hotel = hotel }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    let
        { url, user, recommend, hotel } =
            model
    in
    case urlToRoute url of
        Top ->
            { title = "トップページ"
            , body =
                [ headerView user
                , section [] <|
                    [ h1 [] [ text "あなたにオススメの宿" ]
                    , div [ class "recommend-hotel-list" ] <|
                        List.map hotelOverviewView recommend.hotelOverviewList
                    ]
                ]
            }

        HotelPage _ ->
            { title = hotel.hotelName ++ "プラン一覧"
            , body =
                [ headerView user
                ]
            }

        NotFound ->
            { title = hotel.hotelName ++ "Not found"
            , body =
                [ headerView user
                , p [] [ text "のっとふぁうんど" ]
                ]
            }


headerView : User -> Html Msg
headerView user =
    let
        { userName } =
            user
    in
    header [ class "cmn-header" ]
        [ p [] [ text <| "ようこそ " ++ userName ++ " さん" ]
        ]


hotelOverviewView : HotelOverview -> Html Msg
hotelOverviewView { hotelName, description } =
    a [ class "page-top-hotellist-article__link" ]
        [ article [ class "page-top-hotellist-article" ]
            [ img [ class "__thumb", src "https://4.bp.blogspot.com/-LR5Lja-lZ4E/WZVgz8oz0zI/AAAAAAABGE8/dA0DAXWkQFIY23wjjILccR7m8KXHSAzzACLcBGAs/s400/building_hotel_small.png" ] []
            , h3 [ class "__title" ] [ text hotelName ]
            , p [ class "__description" ]
                [ text description
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
