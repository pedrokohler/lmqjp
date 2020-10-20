port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, button, div, h1, img, input, label, p, span, text)
import Html.Attributes exposing (checked, class, disabled, src, style, type_, value)
import Html.Events exposing (keyCode, onCheck, onClick, onInput)
import Json.Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Time exposing (Month(..))
import Url
import Url.Parser exposing ((</>), Parser)


port saveCustomer : Json.Encode.Value -> Cmd msg


port loadCustomer : String -> Cmd msg


port saveError : (Json.Encode.Value -> msg) -> Sub msg


port loadError : (Json.Encode.Value -> msg) -> Sub msg


port saveSuccess : (Json.Encode.Value -> msg) -> Sub msg


port loadSuccess : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type Model
    = Success Customer Nav.Key Url.Url
      -- | ValidationFailed Customer Errors Nav.Key Url.Url
    | Failed Customer ErrorMessage (Cmd Msg) Nav.Key Url.Url
    | Loading Customer Nav.Key Url.Url


type alias ErrorMessage =
    String


type alias CustomerData =
    { id : String
    , name : String
    , birthYear : Int
    , major : String
    , university : String
    , school : String
    , fromEvent : String
    , eventLocation : String
    , date : String
    , ageOnDate : Int
    , fromApp : Bool
    , fromInternet : Bool
    , madeAPurchase : Bool
    }


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
    , madeAPurchase : Bool
    , editable : Bool
    , unedited : Bool
    , datePicker : DatePicker.DatePicker
    , datePickerInitialized : Bool
    }


type Route
    = CustomerDetail String
    | CustomerList
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map CustomerDetail (Url.Parser.s "customer" </> Url.Parser.string)
        , Url.Parser.map CustomerList (Url.Parser.s "customer")
        ]


settings : Bool -> DatePicker.Settings
settings editable =
    let
        inputAttributes =
            [ Html.Attributes.required False
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
                -- (Just "ihfoYuRczbno6qPrmPM8")
                Nothing
                ""
                1
                ""
                ""
                ""
                ""
                ""
                -- (Just <| Date.fromCalendarDate 2008 Dec 1)
                Nothing
                0
                False
                False
                False
                True
                True
                datePicker
                False
    in
    ( Success
        customer
        key
        url
    , Cmd.batch
        [ Cmd.map ToDatePicker datePickerFx
        , Nav.pushUrl key (Url.toString url)
        ]
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
    | UpdateMadeAPurchase Bool
    | ToggleEditable
    | SaveCustomer
    | LoadCustomer String
    | ToDatePicker DatePicker.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OnSaveError (Result Json.Decode.Error String)
    | OnLoadError (Result Json.Decode.Error String)
    | OnSaveSuccess (Result Json.Decode.Error String)
    | OnLoadSuccess (Result Json.Decode.Error CustomerData)
    | ClearError (Cmd Msg)
    | GoToCustomerList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateName newName, Success customer key url ) ->
            ( Success
                { customer | name = newName, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateMajor newMajor, Success customer key url ) ->
            ( Success
                { customer | major = newMajor, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateUniversity newUniversity, Success customer key url ) ->
            ( Success
                { customer | university = newUniversity, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateSchool newSchool, Success customer key url ) ->
            ( Success
                { customer | school = newSchool, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateFromEvent newFromEvent, Success customer key url ) ->
            ( Success
                { customer | fromEvent = newFromEvent, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateEventLocation newEventLocation, Success customer key url ) ->
            ( Success
                { customer | eventLocation = newEventLocation, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateBirthYear newBirthYear, Success customer key url ) ->
            let
                intBirthYear =
                    Maybe.withDefault customer.birthYear (String.toInt newBirthYear)
            in
            ( Success
                { customer
                    | birthYear = intBirthYear
                    , ageOnDate = calculateAgeOnDate customer.date intBirthYear
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromApp newFromApp, Success customer key url ) ->
            ( Success
                { customer
                    | fromApp = newFromApp
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromInternet newFromInternet, Success customer key url ) ->
            ( Success
                { customer
                    | fromInternet = newFromInternet
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateMadeAPurchase newMadeAPurchase, Success customer key url ) ->
            ( Success
                { customer
                    | madeAPurchase = newMadeAPurchase
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( ToggleEditable, Success customer key url ) ->
            let
                newEditable =
                    not customer.editable
            in
            ( Success
                { customer | editable = newEditable }
                key
                url
            , Cmd.none
            )

        ( SaveCustomer, Success customer key url ) ->
            case customer.date of
                Nothing ->
                    ( Failed customer "Please insert a date before saving." Cmd.none key url
                      -- @todo change to validation error
                    , Cmd.none
                    )

                Just date ->
                    ( Loading
                        { customer | unedited = True }
                        key
                        url
                    , saveCustomer <| customerEncoder customer
                    )

        ( ToDatePicker subMsg, Success customer key url ) ->
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
            ( Success
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

        ( LinkClicked urlRequest, Success customer key _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            case Maybe.withDefault NotFound (Url.Parser.parse routeParser url) of
                CustomerDetail id ->
                    case model of
                        Success customer key _ ->
                            if Maybe.withDefault "" customer.id == id then
                                ( model, Cmd.none )

                            else
                                ( Loading customer key url
                                , loadCustomer id
                                )

                        _ ->
                            ( model
                            , loadCustomer id
                            )

                _ ->
                    ( model, Cmd.none )

        ( OnSaveError result, Loading customer key url ) ->
            case result of
                Ok message ->
                    ( Failed { customer | unedited = False } message Cmd.none key url
                    , Cmd.none
                    )

                Err error ->
                    ( Failed
                        { customer | unedited = False }
                        "An error occurred while trying to save the customer. Please try again."
                        Cmd.none
                        key
                        url
                    , Cmd.none
                    )

        ( OnLoadError result, Loading customer key url ) ->
            case result of
                Ok message ->
                    ( Failed customer message (Nav.pushUrl key "/customer") key url
                    , Cmd.none
                    )

                Err error ->
                    ( Failed
                        customer
                        "An error occurred while trying to load the customer. Please try again."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( OnSaveSuccess result, Loading customer key url ) ->
            case result of
                Ok id ->
                    ( Success { customer | id = Just id, editable = False } key url
                    , Nav.pushUrl key ("/customer/" ++ id)
                    )

                Err error ->
                    ( Failed
                        customer
                        "The save process worked fine. But an error ocurred gettind the ID of the new customer. We'll send you back to the customer list."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( OnLoadSuccess maybePartialCustomer, Loading customer key url ) ->
            case maybePartialCustomer of
                Ok partialCustomer ->
                    case Date.fromIsoString <| String.left 10 partialCustomer.date of
                        Ok date ->
                            ( Success
                                { customer
                                    | id = Just partialCustomer.id
                                    , name = partialCustomer.name
                                    , birthYear = partialCustomer.birthYear
                                    , major = partialCustomer.major
                                    , university = partialCustomer.university
                                    , school = partialCustomer.school
                                    , fromEvent = partialCustomer.fromEvent
                                    , eventLocation = partialCustomer.eventLocation
                                    , date = Just date
                                    , ageOnDate = partialCustomer.ageOnDate
                                    , fromApp = partialCustomer.fromApp
                                    , fromInternet = partialCustomer.fromInternet
                                    , madeAPurchase = partialCustomer.madeAPurchase
                                    , editable = False
                                    , unedited = True
                                }
                                key
                                url
                            , Cmd.none
                            )

                        Err error ->
                            ( Failed
                                customer
                                ("Something went wrong while converting the date. " ++ error)
                                (Nav.pushUrl key "/customer")
                                key
                                url
                            , Cmd.none
                            )

                Err error ->
                    ( Failed
                        customer
                        "Something went wrong while decoding the customer's data."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( ClearError command, Failed customer _ _ key url ) ->
            ( Success
                customer
                key
                url
            , command
            )

        ( GoToCustomerList, Success customer key url ) ->
            ( Success customer key url
            , Nav.pushUrl key "/customer"
            )

        _ ->
            ( model, Cmd.none )


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
        , ( "madeAPurchase", Json.Encode.bool customer.madeAPurchase )
        , ( "id"
          , case customer.id of
                Just id ->
                    Json.Encode.string id

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


customerDecoder : Json.Decode.Decoder CustomerData
customerDecoder =
    Json.Decode.succeed CustomerData
        |> required "id" Json.Decode.string
        |> required "name" Json.Decode.string
        |> required "birthYear" Json.Decode.int
        |> required "major" Json.Decode.string
        |> required "university" Json.Decode.string
        |> required "school" Json.Decode.string
        |> required "fromEvent" Json.Decode.string
        |> required "eventLocation" Json.Decode.string
        |> required "date" Json.Decode.string
        |> required "ageOnDate" Json.Decode.int
        |> required "fromApp" Json.Decode.bool
        |> required "fromInternet" Json.Decode.bool
        |> required "madeAPurchase" Json.Decode.bool


messageDecoder : Json.Decode.Decoder String
messageDecoder =
    field "message" string



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "LMQJP"
    , body =
        let
            isNewCustomer customer =
                case customer.id of
                    Nothing ->
                        True

                    Just id ->
                        False

            pageTitle customerIsNew =
                if customerIsNew then
                    "New Customer"

                else
                    "Customer Detail"

            commonBody customer =
                div []
                    [ h1 [] [ text <| pageTitle (isNewCustomer customer) ]
                    , customerForm customer
                    , datePickerInput customer
                    , div [] [ text <| "Age on date: " ++ String.fromInt customer.ageOnDate ]
                    , pageControl customer (isNewCustomer customer)
                    ]
        in
        case model of
            Success customer key url ->
                [ commonBody customer ]

            Loading customer key url ->
                [ div [ class "backdrop" ] []
                , div [ class "loader" ] []
                , commonBody customer
                ]

            Failed customer errorMessage command key url ->
                [ div [ class "backdrop" ] []
                , div [ class "message-modal" ]
                    [ text errorMessage
                    , button [ onClick <| ClearError command ] [ text "Ok" ]
                    ]
                , commonBody customer
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
        , editableField "From event" customer.fromEvent UpdateFromEvent editable
        , editableField "Event location" customer.eventLocation UpdateEventLocation editable
        , checkbox "From app" customer.fromApp UpdateFromApp editable
        , checkbox "From internet" customer.fromInternet UpdateFromInternet editable
        , checkbox "Made a purchase" customer.madeAPurchase UpdateMadeAPurchase editable
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


pageControl : Customer -> Bool -> Html Msg
pageControl customer isNewCustomer =
    let
        display =
            if isNewCustomer then
                "none"

            else
                "inline-block"

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
            , style "display" display
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
        , button [ onClick <| GoToCustomerList ] [ text "Back" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ saveError (Json.Decode.decodeValue messageDecoder >> OnSaveError)
        , loadError (Json.Decode.decodeValue messageDecoder >> OnLoadError)
        , saveSuccess (Json.Decode.decodeValue messageDecoder >> OnSaveSuccess)
        , loadSuccess (Json.Decode.decodeValue customerDecoder >> OnLoadSuccess)
        ]



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
