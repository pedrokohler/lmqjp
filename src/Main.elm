port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, button, div, h1, img, input, label, p, span, text)
import Html.Attributes exposing (checked, disabled, required, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode
import Time exposing (Month(..))


port saveCustomer : Json.Encode.Value -> Cmd msg



---- MODEL ----


type Model
    = SuccessCustomerDetail Customer


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


init : ( Model, Cmd Msg )
init =
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
    , Cmd.map (ToDatePicker customer) datePickerFx
    )



---- UPDATE ----


type Msg
    = UpdateName Customer String
    | UpdateMajor Customer String
    | UpdateUniversity Customer String
    | UpdateSchool Customer String
    | UpdateFromEvent Customer String
    | UpdateEventLocation Customer String
    | UpdateBirthYear Customer String
    | UpdateFromApp Customer Bool
    | UpdateFromInternet Customer Bool
    | ToggleEditable Customer
    | SaveCustomer Customer
    | ToDatePicker Customer DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName customer newName ->
            ( SuccessCustomerDetail
                { customer | name = newName, unedited = False }
            , Cmd.none
            )

        UpdateMajor customer newMajor ->
            ( SuccessCustomerDetail
                { customer | major = newMajor, unedited = False }
            , Cmd.none
            )

        UpdateUniversity customer newUniversity ->
            ( SuccessCustomerDetail
                { customer | university = newUniversity, unedited = False }
            , Cmd.none
            )

        UpdateSchool customer newSchool ->
            ( SuccessCustomerDetail
                { customer | school = newSchool, unedited = False }
            , Cmd.none
            )

        UpdateFromEvent customer newFromEvent ->
            ( SuccessCustomerDetail
                { customer | fromEvent = newFromEvent, unedited = False }
            , Cmd.none
            )

        UpdateEventLocation customer newEventLocation ->
            ( SuccessCustomerDetail
                { customer | eventLocation = newEventLocation, unedited = False }
            , Cmd.none
            )

        UpdateBirthYear customer newBirthYear ->
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
            , Cmd.none
            )

        UpdateFromApp customer newFromApp ->
            ( SuccessCustomerDetail
                { customer
                    | fromApp = newFromApp
                    , unedited = False
                }
            , Cmd.none
            )

        UpdateFromInternet customer newFromInternet ->
            ( SuccessCustomerDetail
                { customer
                    | fromInternet = newFromInternet
                    , unedited = False
                }
            , Cmd.none
            )

        ToggleEditable customer ->
            let
                newEditable =
                    not customer.editable
            in
            ( SuccessCustomerDetail
                { customer | editable = newEditable }
            , Cmd.none
            )

        SaveCustomer customer ->
            case customer.date of
                Nothing ->
                    ( model, Cmd.none )

                Just date ->
                    ( SuccessCustomerDetail
                        { customer | unedited = True }
                    , saveCustomer <| customerEncoder customer
                    )

        ToDatePicker customer subMsg ->
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


view : Model -> Html Msg
view model =
    case model of
        SuccessCustomerDetail customer ->
            div []
                [ h1 [] [ text "Customer Detail" ]
                , customerForm customer
                , datePickerInput customer
                , div [] [ text <| "Age on date: " ++ String.fromInt customer.ageOnDate ]
                , pageControl customer
                ]


customerForm : Customer -> Html Msg
customerForm customer =
    div []
        [ editableField "Name" customer.name UpdateName customer
        , editableField "Birth year" (String.fromInt customer.birthYear) UpdateBirthYear customer
        , editableField "Major" customer.major UpdateMajor customer
        , editableField "University" customer.university UpdateUniversity customer
        , editableField "School" customer.school UpdateSchool customer
        , editableField "From Event" customer.fromEvent UpdateFromEvent customer
        , editableField "Event Location" customer.eventLocation UpdateEventLocation customer
        , checkbox "From app" customer.fromApp UpdateFromApp customer
        , checkbox "From internet" customer.fromInternet UpdateFromInternet customer
        ]


editableField : String -> String -> (Customer -> String -> Msg) -> Customer -> Html Msg
editableField labelText field toMsg customer =
    div []
        [ label [] [ text <| labelText ++ ": " ]
        , input
            [ disabled (not customer.editable)
            , value field
            , onInput <| toMsg customer
            , type_ "text"
            ]
            []
        ]


checkbox : String -> Bool -> (Customer -> Bool -> Msg) -> Customer -> Html Msg
checkbox labelText field toMsg customer =
    div []
        [ label [] [ text <| labelText ++ ": " ]
        , input
            [ disabled (not customer.editable)
            , checked field
            , onCheck <| toMsg customer
            , type_ "checkbox"
            ]
            []
        ]


datePickerInput : Customer -> Html Msg
datePickerInput customer =
    div []
        [ span [] [ text "Date" ]
        , DatePicker.view customer.date (settings customer.editable) customer.datePicker
            |> Html.map (ToDatePicker customer)
        ]


pageControl : Customer -> Html Msg
pageControl customer =
    let
        editButtonText =
            if customer.editable then
                "Desabilitar edição"

            else
                "Habilitar edição"

        editButtonIcon =
            if customer.editable then
                "/x-square.svg"

            else
                "/edit.svg"
    in
    div []
        [ button
            [ onClick <| ToggleEditable customer
            ]
            [ text editButtonText
            , img
                [ src editButtonIcon
                ]
                []
            ]
        , button
            [ onClick <| SaveCustomer customer
            , disabled customer.unedited
            ]
            [ text "Salvar" ]
        , button [] [ text "Voltar" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
