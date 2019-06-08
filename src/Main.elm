module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task as Task exposing (Task)
import Url
import Url.Builder as UrlBuilder
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
    , isTopLoaded : Bool
    , isHotelPageLoaded : Bool
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


urlToCommands : Model -> Url.Url -> List (Cmd Msg)
urlToCommands model url =
    let
        { isTopLoaded, isHotelPageLoaded } =
            model
    in
    case urlToRoute url of
        Top ->
            [ getUser, getRecommend ]

        HotelPage hotelId ->
            [ getUser, getHotel hotelId ]

        NotFound ->
            [ getUser ]


urlToOptimizedCommands : Model -> Url.Url -> List (Cmd Msg)
urlToOptimizedCommands model url =
    let
        { isTopLoaded, isHotelPageLoaded } =
            model
    in
    if isTopLoaded || isHotelPageLoaded then
        []

    else
        urlToCommands model url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            Model key url False False (User "") (Recommend []) (Hotel "" [])
    in
    ( initialModel
    , Cmd.batch <| urlToCommands initialModel url
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
                    ( model
                    , Cmd.batch <|
                        Nav.pushUrl model.key (Url.toString url)
                            :: urlToCommands model url
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.batch <|
                urlToOptimizedCommands model url
            )

        GotUser user ->
            ( { model | user = user }, Cmd.none )

        GotRecommend recommend ->
            ( { model | recommend = recommend, isTopLoaded = True }, Cmd.none )

        GotHotel result ->
            case result of
                Ok hotel ->
                    ( { model | hotel = hotel, isHotelPageLoaded = True }, Cmd.none )

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
            let
                { hotelName, planList } =
                    hotel
            in
            { title = hotel.hotelName ++ "プラン一覧"
            , body =
                [ headerView user
                , a [ href <| UrlBuilder.relative [ "..", ".." ] [] ] [ text "戻る" ]
                , h1 [] [ text hotelName ]
                , section [] <|
                    List.map
                        (\{ planName, accommodationFee } ->
                            article []
                                [ h2 [] [ text planName ]
                                , div [ class "page-hotel-planlist-reserve" ]
                                    [ span [] [ text <| "￥" ++ String.fromInt accommodationFee ]
                                    , a [ href <| UrlBuilder.relative [ "reserve" ] [] ] [ text "予約する" ]
                                    ]
                                ]
                        )
                        planList
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
hotelOverviewView { hotelId, hotelName, description } =
    a [ href <| UrlBuilder.relative [ "hotel", String.fromInt hotelId ] [], class "page-top-hotellist-article__link" ]
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
