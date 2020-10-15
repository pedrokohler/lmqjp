port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, button, div, h1, img, input, label, p, span, text)
import Html.Attributes exposing (checked, disabled, required, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode
import Time exposing (Month(..))
import Url


port saveCustomer : Json.Encode.Value -> Cmd msg



---- MODEL ----


type Model
    = SuccessCustomerDetail Customer Nav.Key Url.Url


type alias Customer =
    { id : Maybe String -- Nothing in case of new records
    , name : String
    , birthYear : Int
    , major : String
    , university : String
    , school : String
    , fromEvent : String
    , eventLocation : String
    , date : Maybe Date -- Should be today's date for new customer
    , ageOnDate : Int
    , fromApp : Bool
    , fromInternet : Bool
    , editable : Bool
    , unedited : Bool
    , datePicker : DatePicker.DatePicker
    , datePickerInitialized : Bool
    }


settings : Bool -> DatePicker.Settings
settings editable =
    let
        inputAttributes =
            [ required False
            , disabled (not editable)
            ]
    in
    { defaultSettings | inputAttributes = inputAttributes }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        customer =
            Customer
                (Just "3GuYXa126Af8yDfVQLLY")
                "Ana Luiza Diniz"
                1992
                "Direito"
                "Newton Paiva"
                "Santa Maria"
                ""
                "Santa Maria"
                (Just <| Date.fromCalendarDate 2020 Sep 29)
                0
                True
                False
                False
                True
                datePicker
                False
    in
    ( SuccessCustomerDetail
        customer
        key
        url
    , Cmd.map ToDatePicker datePickerFx
    )



---- UPDATE ----


type Msg
    = UpdateName String
    | UpdateMajor String
    | UpdateUniversity String
    | UpdateSchool String
    | UpdateFromEvent String
    | UpdateEventLocation String
    | UpdateBirthYear String
    | UpdateFromApp Bool
    | UpdateFromInternet Bool
    | ToggleEditable
    | SaveCustomer
    | ToDatePicker DatePicker.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateName newName, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | name = newName, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateMajor newMajor, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | major = newMajor, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateUniversity newUniversity, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | university = newUniversity, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateSchool newSchool, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | school = newSchool, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateFromEvent newFromEvent, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | fromEvent = newFromEvent, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateEventLocation newEventLocation, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer | eventLocation = newEventLocation, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateBirthYear newBirthYear, SuccessCustomerDetail customer key url ) ->
            let
                intBirthYear =
                    Maybe.withDefault customer.birthYear (String.toInt newBirthYear)
            in
            ( SuccessCustomerDetail
                { customer
                    | birthYear = intBirthYear
                    , ageOnDate = calculateAgeOnDate customer.date intBirthYear
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromApp newFromApp, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer
                    | fromApp = newFromApp
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromInternet newFromInternet, SuccessCustomerDetail customer key url ) ->
            ( SuccessCustomerDetail
                { customer
                    | fromInternet = newFromInternet
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( ToggleEditable, SuccessCustomerDetail customer key url ) ->
            let
                newEditable =
                    not customer.editable
            in
            ( SuccessCustomerDetail
                { customer | editable = newEditable }
                key
                url
            , Cmd.none
            )

        ( SaveCustomer, SuccessCustomerDetail customer key url ) ->
            case customer.date of
                Nothing ->
                    ( model, Cmd.none )

                Just date ->
                    ( SuccessCustomerDetail
                        { customer | unedited = True }
                        key
                        url
                    , saveCustomer <| customerEncoder customer
                    )

        ( ToDatePicker subMsg, SuccessCustomerDetail customer key url ) ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (settings customer.editable) subMsg customer.datePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            customer.date

                justInitialized =
                    if customer.datePickerInitialized then
                        False

                    else
                        True
            in
            ( SuccessCustomerDetail
                { customer
                    | date = newDate
                    , ageOnDate = calculateAgeOnDate newDate customer.birthYear
                    , datePicker = newDatePicker
                    , datePickerInitialized = True
                    , unedited = justInitialized
                }
                key
                url
            , Cmd.none
            )

        ( LinkClicked urlRequest, SuccessCustomerDetail customer key _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, SuccessCustomerDetail customer key _ ) ->
            ( SuccessCustomerDetail customer key url
            , Cmd.none
            )


calculateAgeOnDate : Maybe Date -> Int -> Int
calculateAgeOnDate maybeDate birthYear =
    case maybeDate of
        Nothing ->
            0

        Just date ->
            Date.year date - birthYear


customerEncoder : Customer -> Json.Encode.Value
customerEncoder customer =
    Json.Encode.object
        [ ( "name", Json.Encode.string customer.name )
        , ( "birthYear", Json.Encode.int customer.birthYear )
        , ( "major", Json.Encode.string customer.major )
        , ( "university", Json.Encode.string customer.university )
        , ( "school", Json.Encode.string customer.school )
        , ( "fromEvent", Json.Encode.string customer.fromEvent )
        , ( "eventLocation", Json.Encode.string customer.eventLocation )
        , ( "date"
          , case customer.date of
                Maybe.Nothing ->
                    Json.Encode.null

                Just date ->
                    Json.Encode.string <| Date.toIsoString date
          )
        , ( "ageOnDate", Json.Encode.int customer.ageOnDate )
        , ( "fromApp", Json.Encode.bool customer.fromApp )
        , ( "fromInternet", Json.Encode.bool customer.fromInternet )
        , ( "id"
          , case customer.id of
                Just id ->
                    Json.Encode.string id

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "LMQJP"
    , body =
        case model of
            SuccessCustomerDetail customer key url ->
                [ div []
                    [ h1 [] [ text "Customer Detail" ]
                    , customerForm customer
                    , datePickerInput customer
                    , div [] [ text <| "Age on date: " ++ String.fromInt customer.ageOnDate ]
                    , pageControl customer
                    ]
                ]
    }


customerForm : Customer -> Html Msg
customerForm customer =
    let
        editable =
            customer.editable
    in
    div []
        [ editableField "Name" customer.name UpdateName editable
        , editableField "Birth year" (String.fromInt customer.birthYear) UpdateBirthYear editable
        , editableField "Major" customer.major UpdateMajor editable
        , editableField "University" customer.university UpdateUniversity editable
        , editableField "School" customer.school UpdateSchool editable
        , editableField "From Event" customer.fromEvent UpdateFromEvent editable
        , editableField "Event Location" customer.eventLocation UpdateEventLocation editable
        , checkbox "From app" customer.fromApp UpdateFromApp editable
        , checkbox "From internet" customer.fromInternet UpdateFromInternet editable
        ]


editableField : String -> String -> (String -> Msg) -> Bool -> Html Msg
editableField labelText field toMsg editable =
    div []
        [ label [] [ text <| labelText ++ ": " ]
        , input
            [ disabled (not editable)
            , value field
            , onInput toMsg
            , type_ "text"
            ]
            []
        ]


checkbox : String -> Bool -> (Bool -> Msg) -> Bool -> Html Msg
checkbox labelText field toMsg editable =
    div []
        [ label [] [ text <| labelText ++ ": " ]
        , input
            [ disabled (not editable)
            , checked field
            , onCheck toMsg
            , type_ "checkbox"
            ]
            []
        ]


datePickerInput : Customer -> Html Msg
datePickerInput customer =
    div []
        [ span [] [ text "Date" ]
        , DatePicker.view customer.date (settings customer.editable) customer.datePicker
            |> Html.map ToDatePicker
        ]


pageControl : Customer -> Html Msg
pageControl customer =
    let
        editButtonText =
            if customer.editable then
                "Disable editing"

            else
                "Enable editing"

        editButtonIcon =
            if customer.editable then
                "/x-square.svg"

            else
                "/edit.svg"
    in
    div []
        [ button
            [ onClick ToggleEditable
            ]
            [ text editButtonText
            , img
                [ src editButtonIcon
                ]
                []
            ]
        , button
            [ onClick SaveCustomer
            , disabled customer.unedited
            ]
            [ text "Save" ]
        , button [] [ text "Back" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
