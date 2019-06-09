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
    | ReservePage HotelId HotelPlanId
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map Top P.top
        , P.map HotelPage (P.s "hotel" </> P.int)
        , P.map ReservePage (P.s "hotel" </> P.int </> P.s "plan" </> P.int </> P.s "reserve")
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
    , hotelName : String
    , planName : String
    , accommodationFee : Int
    }


type ReservePageState
    = ReserveContentInput
    | ReserveContentConfirm
    | Reserved


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , user : User
    , recommend : Recommend
    , hotel : Hotel
    , plan : HotelPlan
    , numOfPeople : Int
    , reservePageState : ReservePageState
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


sweetPlan : HotelPlan
sweetPlan =
    HotelPlan 1 "さくらちゃんランド" "スイートルームプラン" 10000


sakuraChanPlan : HotelPlan
sakuraChanPlan =
    HotelPlan 2 "さくらちゃんランド" "さくらちゃんと戯れプラン" 20000


nyanyanPlan : HotelPlan
nyanyanPlan =
    HotelPlan 3 "にゃんこ宿" "にゃんにゃんプラン" 15000


wanwanPlan : HotelPlan
wanwanPlan =
    HotelPlan 4 "ワンワンホテル" "ワンワンプラン" 13000


getHotel : HotelId -> Cmd Msg
getHotel hotelId =
    Task.attempt GotHotel <|
        case hotelId of
            1 ->
                Task.succeed <|
                    Hotel "さくらちゃんランド"
                        [ sweetPlan
                        , sakuraChanPlan
                        ]

            2 ->
                Task.succeed <|
                    Hotel "にゃんこ宿"
                        [ nyanyanPlan ]

            3 ->
                Task.succeed <|
                    Hotel "ワンワンホテル"
                        [ wanwanPlan ]

            _ ->
                Task.fail "Hotel Not Found"


getPlan : HotelPlanId -> Cmd Msg
getPlan planId =
    Task.attempt GotPlan <|
        case planId of
            1 ->
                Task.succeed sweetPlan

            2 ->
                Task.succeed sakuraChanPlan

            3 ->
                Task.succeed <| nyanyanPlan

            4 ->
                Task.succeed <| wanwanPlan

            _ ->
                Task.fail "Hotel Not Found"


urlToCommands : Model -> Url.Url -> List (Cmd Msg)
urlToCommands model url =
    case urlToRoute url of
        Top ->
            [ getUser, getRecommend ]

        HotelPage hotelId ->
            [ getUser, getHotel hotelId ]

        ReservePage hotelId planId ->
            [ getUser, getPlan planId ]

        NotFound ->
            [ getUser ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            Model
                key
                url
                (User "")
                (Recommend [])
                (Hotel "" [])
                (HotelPlan 0 "" "" 0)
                1
                ReserveContentInput
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
    | GotPlan (Result String HotelPlan)
    | GoToReserve HotelId HotelPlanId
    | UpdateNumOfPeople String
    | ToConfirm
    | ToReserved


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
            ( { model | url = url }, Cmd.none )

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

        GotPlan result ->
            case result of
                Ok plan ->
                    ( { model | plan = plan }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GoToReserve hotelId planId ->
            let
                url =
                    model.url

                reserveURLPath =
                    UrlBuilder.absolute [ "hotel", String.fromInt hotelId, "plan", String.fromInt planId, "reserve" ] []
            in
            ( model, Nav.load reserveURLPath )

        UpdateNumOfPeople numOfPeopleText ->
            ( { model
                | numOfPeople =
                    Maybe.withDefault model.numOfPeople <|
                        String.toInt numOfPeopleText
              }
            , Cmd.none
            )

        ToConfirm ->
            ( { model | reservePageState = ReserveContentConfirm }, Cmd.none )

        ToReserved ->
            ( { model | reservePageState = Reserved }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Browser.Document Msg
view model =
    let
        { url, user, recommend, hotel, plan, numOfPeople, reservePageState } =
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

        HotelPage hotelId ->
            let
                { hotelName, planList } =
                    hotel
            in
            { title = hotel.hotelName ++ "プラン一覧"
            , body =
                [ headerView user
                , a [ href "/" ] [ text "戻る" ]
                , h1 [] [ text hotelName ]
                , section [] <|
                    List.map
                        (\{ planId, planName, accommodationFee } ->
                            article []
                                [ h2 [] [ text planName ]
                                , div [ class "page-hotel-planlist-reserve" ]
                                    [ span [] [ text <| "￥" ++ String.fromInt accommodationFee ]
                                    , button [ onClick <| GoToReserve hotelId planId ] [ text "予約する" ]
                                    ]
                                ]
                        )
                        planList
                ]
            }

        ReservePage hotelId planId ->
            let
                { hotelName, planName, accommodationFee } =
                    plan
            in
            case reservePageState of
                ReserveContentInput ->
                    { title = planName ++ "のご予約"
                    , body =
                        [ headerView user
                        , a [ href <| UrlBuilder.absolute [ "hotel", String.fromInt hotelId ] [] ] [ text "戻る" ]
                        , h1 [] [ text <| planName ++ " - " ++ hotelName ]
                        , section [ class "page-hotel-reserve-content" ]
                            [ div []
                                [ input [ type_ "number", value <| String.fromInt numOfPeople, onInput UpdateNumOfPeople ] []
                                , span [] [ text "人" ]
                                ]
                            , button [ onClick ToConfirm ] [ text "予約確認へ" ]
                            ]
                        ]
                    }

                ReserveContentConfirm ->
                    { title = planName ++ "のご予約の確認"
                    , body =
                        [ headerView user
                        , h1 [] [ text "ご予約の確認" ]
                        , section [ class "" ]
                            [ h2 [] [ text <| String.fromInt <| accommodationFee * numOfPeople ]
                            , button [ onClick ToReserved ] [ text "予約確定" ]
                            ]
                        ]
                    }

                Reserved ->
                    { title = "ご予約ありがとうございました。"
                    , body =
                        [ headerView user
                        , h1 [] [ text "ご予約ありがとうございました。" ]
                        , a [ href "/" ] [ text "トップへ戻る" ]
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
    a [ href <| UrlBuilder.absolute [ "hotel", String.fromInt hotelId ] [], class "page-top-hotellist-article__link" ]
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
