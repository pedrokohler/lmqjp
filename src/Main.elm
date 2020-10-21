port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, a, button, div, h1, img, input, label, p, span, table, td, text, th, tr)
import Html.Attributes exposing (checked, class, disabled, href, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import List.Extra
import Time exposing (Month(..))
import Url
import Url.Parser exposing ((</>), Parser)


port saveCustomer : Json.Encode.Value -> Cmd msg


port loadCustomer : String -> Cmd msg


port loadCustomerList : () -> Cmd msg


port saveCustomerError : (Json.Encode.Value -> msg) -> Sub msg


port loadCustomerError : (Json.Encode.Value -> msg) -> Sub msg



-- @todo implement loadCustomerListError function


port loadCustomerListError : (Json.Encode.Value -> msg) -> Sub msg


port saveCustomerSuccess : (Json.Encode.Value -> msg) -> Sub msg


port loadCustomerSuccess : (Json.Encode.Value -> msg) -> Sub msg


port loadCustomerListSuccess : (Json.Encode.Value -> msg) -> Sub msg



---- MODEL ----


type Model
    = Success (List CustomerData) CustomerListLoaded Customer Nav.Key Url.Url
      -- | ValidationFailed Customer Errors Nav.Key Url.Url
    | Failed (List CustomerData) CustomerListLoaded Customer ErrorMessage (Cmd Msg) Nav.Key Url.Url
    | Loading (List CustomerData) CustomerListLoaded Customer Nav.Key Url.Url


type alias ErrorMessage =
    String


type alias CustomerListLoaded =
    Bool


type alias CustomerData =
    { id : String
    , name : String
    , nationality : String
    , birthYear : Int
    , major : String
    , university : String
    , school : String
    , fromEvent : String
    , eventCity : String
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
    , nationality : String
    , birthYear : Int
    , major : String
    , university : String
    , school : String
    , fromEvent : String
    , eventCity : String
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
    | NewCustomer
    | CustomerList
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map NewCustomer (Url.Parser.s "customer" </> Url.Parser.s "new")
        , Url.Parser.map CustomerList (Url.Parser.s "customer")
        , Url.Parser.map CustomerDetail (Url.Parser.s "customer" </> Url.Parser.string)
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


emptyCustomer : DatePicker.DatePicker -> Customer
emptyCustomer datePicker =
    Customer
        -- (Just "ihfoYuRczbno6qPrmPM8")
        Nothing
        ""
        "Brasileira"
        0
        ""
        ""
        ""
        ""
        "Belo Horizonte"
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


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( Success
        []
        False
        (emptyCustomer datePicker)
        key
        url
    , Cmd.batch
        [ Cmd.map ToDatePicker datePickerFx
        , Nav.replaceUrl key (Url.toString url)
        ]
    )



---- UPDATE ----


type Msg
    = UpdateName String
    | UpdateMajor String
    | UpdateNationality String
    | UpdateUniversity String
    | UpdateSchool String
    | UpdateFromEvent String
    | UpdateEventCity String
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
    | OnSaveCustomerError (Result Json.Decode.Error String)
    | OnLoadCustomerError (Result Json.Decode.Error String)
    | OnLoadCustomerListError (Result Json.Decode.Error String)
    | OnSaveCustomerSuccess (Result Json.Decode.Error String)
    | OnLoadCustomerSuccess (Result Json.Decode.Error CustomerData)
    | OnLoadCustomerListSuccess (Result Json.Decode.Error (List CustomerData))
    | ClearError (Cmd Msg)
    | GoToCustomerList
    | GoToNewCustomer


getCustomerFromCustumerData : DatePicker.DatePicker -> CustomerData -> Customer
getCustomerFromCustumerData datePicker customerData =
    let
        emptyData =
            emptyCustomer datePicker

        partialCustomer =
            { emptyData
                | id = Just customerData.id
                , name = customerData.name
                , nationality = customerData.nationality
                , birthYear = customerData.birthYear
                , major = customerData.major
                , university = customerData.university
                , school = customerData.school
                , fromEvent = customerData.fromEvent
                , eventCity = customerData.eventCity
                , eventLocation = customerData.eventLocation
                , ageOnDate = customerData.ageOnDate
                , fromApp = customerData.fromApp
                , fromInternet = customerData.fromInternet
                , madeAPurchase = customerData.madeAPurchase
                , editable = False
                , unedited = True
            }
    in
    case Date.fromIsoString <| String.left 10 customerData.date of
        Ok date ->
            { partialCustomer | date = Just date }

        Err error ->
            partialCustomer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateName newName, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | name = newName, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateMajor newMajor, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | major = newMajor, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateNationality newNationality, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | nationality = newNationality, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateUniversity newUniversity, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | university = newUniversity, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateSchool newSchool, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | school = newSchool, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateFromEvent newFromEvent, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | fromEvent = newFromEvent, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateEventCity newEventCity, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | eventCity = newEventCity, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateEventLocation newEventLocation, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer | eventLocation = newEventLocation, unedited = False }
                key
                url
            , Cmd.none
            )

        ( UpdateBirthYear newBirthYear, Success customerList customerListLoaded customer key url ) ->
            let
                intBirthYear =
                    Maybe.withDefault customer.birthYear (String.toInt newBirthYear)
            in
            ( Success
                customerList
                customerListLoaded
                { customer
                    | birthYear = intBirthYear
                    , ageOnDate = calculateAgeOnDate customer.date intBirthYear
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromApp newFromApp, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer
                    | fromApp = newFromApp
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateFromInternet newFromInternet, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer
                    | fromInternet = newFromInternet
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( UpdateMadeAPurchase newMadeAPurchase, Success customerList customerListLoaded customer key url ) ->
            ( Success
                customerList
                customerListLoaded
                { customer
                    | madeAPurchase = newMadeAPurchase
                    , unedited = False
                }
                key
                url
            , Cmd.none
            )

        ( ToggleEditable, Success customerList customerListLoaded customer key url ) ->
            let
                newEditable =
                    not customer.editable
            in
            ( Success
                customerList
                customerListLoaded
                { customer | editable = newEditable }
                key
                url
            , Cmd.none
            )

        ( SaveCustomer, Success customerList customerListLoaded customer key url ) ->
            case customer.date of
                Nothing ->
                    ( Failed
                        customerList
                        customerListLoaded
                        customer
                        "Please insert a date before saving."
                        Cmd.none
                        key
                        url
                    , Cmd.none
                    )

                Just date ->
                    ( Loading
                        customerList
                        customerListLoaded
                        { customer | unedited = True }
                        key
                        url
                    , saveCustomer <| customerEncoder customer
                    )

        ( ToDatePicker subMsg, Success customerList customerListLoaded customer key url ) ->
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
                customerList
                customerListLoaded
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

        ( LinkClicked urlRequest, Success _ _ _ key _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            case Maybe.withDefault NotFound (Url.Parser.parse routeParser url) of
                CustomerDetail id ->
                    case model of
                        Success customerList customerListLoaded lastCustomer key _ ->
                            let
                                ( datePicker, datePickerFx ) =
                                    DatePicker.init

                                maybeCustomerData =
                                    List.Extra.find (\el -> el.id == id) customerList
                            in
                            case maybeCustomerData of
                                Just customerData ->
                                    ( Success
                                        customerList
                                        customerListLoaded
                                        (getCustomerFromCustumerData datePicker <| customerData)
                                        key
                                        url
                                    , Cmd.map ToDatePicker datePickerFx
                                    )

                                Nothing ->
                                    ( Loading customerList customerListLoaded lastCustomer key url
                                    , loadCustomer id
                                    )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                CustomerList ->
                    case model of
                        Success customerList customerListLoaded customer key _ ->
                            if customerListLoaded then
                                ( Success customerList customerListLoaded customer key url
                                , Cmd.none
                                )

                            else
                                ( Loading customerList customerListLoaded customer key url
                                , loadCustomerList ()
                                )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                NewCustomer ->
                    case model of
                        Success customerList customerListLoaded customer key _ ->
                            let
                                ( datePicker, datePickerFx ) =
                                    DatePicker.init
                            in
                            ( Success customerList customerListLoaded (emptyCustomer datePicker) key url
                            , Cmd.map ToDatePicker datePickerFx
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        ( OnSaveCustomerError result, Loading customerList customerListLoaded customer key url ) ->
            case result of
                Ok message ->
                    ( Failed
                        customerList
                        customerListLoaded
                        { customer | unedited = False }
                        message
                        Cmd.none
                        key
                        url
                    , Cmd.none
                    )

                Err error ->
                    ( Failed
                        customerList
                        customerListLoaded
                        { customer | unedited = False }
                        "An error occurred while trying to save the customer. Please try again."
                        Cmd.none
                        key
                        url
                    , Cmd.none
                    )

        ( OnLoadCustomerError result, Loading customerList customerListLoaded customer key url ) ->
            case result of
                Ok message ->
                    ( Failed
                        customerList
                        customerListLoaded
                        customer
                        message
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

                Err error ->
                    ( Failed
                        customerList
                        customerListLoaded
                        customer
                        "An error occurred while trying to load the customer. Please try again."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( OnSaveCustomerSuccess result, Loading customerList customerListLoaded customer key url ) ->
            case result of
                Ok id ->
                    ( Success
                        customerList
                        customerListLoaded
                        { customer | id = Just id, editable = False }
                        key
                        url
                    , Nav.pushUrl key ("/customer/" ++ id)
                    )

                Err error ->
                    ( Failed
                        customerList
                        customerListLoaded
                        customer
                        "The save process worked fine. But an error ocurred gettind the ID of the new customer. We'll send you back to the customer list."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( OnLoadCustomerSuccess maybePartialCustomer, Loading customerList customerListLoaded customer key url ) ->
            case maybePartialCustomer of
                Ok partialCustomer ->
                    case Date.fromIsoString <| String.left 10 partialCustomer.date of
                        Ok date ->
                            let
                                ( datePicker, datePickerFx ) =
                                    DatePicker.init
                            in
                            ( Success
                                customerList
                                customerListLoaded
                                (getCustomerFromCustumerData datePicker <| partialCustomer)
                                key
                                url
                            , Cmd.map ToDatePicker datePickerFx
                            )

                        Err error ->
                            ( Failed
                                customerList
                                customerListLoaded
                                customer
                                ("Something went wrong while converting the date. " ++ error)
                                (Nav.pushUrl key "/customer")
                                key
                                url
                            , Cmd.none
                            )

                Err error ->
                    ( Failed
                        customerList
                        customerListLoaded
                        customer
                        "Something went wrong while decoding the customer's data."
                        (Nav.pushUrl key "/customer")
                        key
                        url
                    , Cmd.none
                    )

        ( OnLoadCustomerListSuccess maybeCustomerList, Loading _ customerListLoaded customer key url ) ->
            onLoadCustomerListSuccessHandler maybeCustomerList
                customerListLoaded
                customer
                ""
                Cmd.none
                key
                url
                "Loading"

        ( OnLoadCustomerListSuccess maybeCustomerList, Success _ customerListLoaded customer key url ) ->
            onLoadCustomerListSuccessHandler maybeCustomerList
                customerListLoaded
                customer
                ""
                Cmd.none
                key
                url
                "Success"

        ( OnLoadCustomerListSuccess maybeCustomerList, Failed _ customerListLoaded customer errorMessage command key url ) ->
            onLoadCustomerListSuccessHandler maybeCustomerList
                customerListLoaded
                customer
                errorMessage
                command
                key
                url
                "Failed"

        ( ClearError command, Failed customerList customerListLoaded customer _ _ key url ) ->
            ( Success
                customerList
                customerListLoaded
                customer
                key
                url
            , command
            )

        ( GoToCustomerList, Success customerList customerListLoaded customer key url ) ->
            ( Success customerList customerListLoaded customer key url
            , Nav.pushUrl key "/customer"
            )

        ( GoToNewCustomer, Success customerList customerListLoaded customer key url ) ->
            ( Success customerList customerListLoaded customer key url
            , Nav.pushUrl key "/customer/new"
            )

        _ ->
            ( model, Cmd.none )


onLoadCustomerListSuccessHandler :
    Result Json.Decode.Error (List CustomerData)
    -> Bool
    -> Customer
    -> String
    -> Cmd Msg
    -> Nav.Key
    -> Url.Url
    -> String
    -> ( Model, Cmd Msg )
onLoadCustomerListSuccessHandler maybeCustomerList customerListLoaded customer errorMessage command key url modelType =
    case maybeCustomerList of
        Ok customerList ->
            let
                fallbackOutputModel =
                    if modelType == "Success" then
                        Success
                            customerList
                            True
                            customer
                            key
                            url

                    else if modelType == "Failed" then
                        Failed
                            customerList
                            True
                            customer
                            errorMessage
                            command
                            key
                            url

                    else
                        Loading
                            customerList
                            True
                            customer
                            key
                            url
            in
            case Maybe.withDefault NotFound (Url.Parser.parse routeParser url) of
                CustomerList ->
                    ( Success
                        customerList
                        True
                        customer
                        key
                        url
                    , Cmd.none
                    )

                _ ->
                    ( fallbackOutputModel
                    , Cmd.none
                    )

        Err error ->
            ( Failed
                []
                customerListLoaded
                customer
                ("Something went wrong while decoding the customer list data. " ++ Json.Decode.errorToString error)
                (Nav.replaceUrl key "/customer")
                key
                url
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
        , ( "nationality", Json.Encode.string customer.nationality )
        , ( "major", Json.Encode.string customer.major )
        , ( "university", Json.Encode.string customer.university )
        , ( "school", Json.Encode.string customer.school )
        , ( "fromEvent", Json.Encode.string customer.fromEvent )
        , ( "eventCity", Json.Encode.string customer.eventCity )
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
        |> required "nationality" Json.Decode.string
        |> required "birthYear" Json.Decode.int
        |> required "major" Json.Decode.string
        |> required "university" Json.Decode.string
        |> required "school" Json.Decode.string
        |> required "fromEvent" Json.Decode.string
        |> required "eventCity" Json.Decode.string
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
            route url =
                Maybe.withDefault NotFound (Url.Parser.parse routeParser url)

            isNewCustomer url =
                case route url of
                    NewCustomer ->
                        True

                    _ ->
                        False

            pageTitle customerIsNew =
                if customerIsNew then
                    "New Customer"

                else
                    "Customer Detail"

            commonBody customer url =
                div []
                    [ h1 [] [ text <| pageTitle (isNewCustomer url) ]
                    , customerForm customer
                    , datePickerInput customer
                    , div [] [ text <| "Age on date: " ++ String.fromInt customer.ageOnDate ]
                    , pageControl customer (isNewCustomer url)
                    ]
        in
        case model of
            Success customerList _ customer _ url ->
                case route url of
                    NewCustomer ->
                        [ commonBody customer url ]

                    CustomerDetail id ->
                        [ commonBody customer url ]

                    CustomerList ->
                        [ h1 [] [ text "Customer List" ]
                        , button [ onClick GoToNewCustomer ] [ text "New Customer" ]
                        , table []
                            (List.append
                                [ tr []
                                    [ th [] [ text "#" ]
                                    , th [] [ text "Name" ]
                                    , th [] [ text "Made A Purchase" ]
                                    , th [] [ text "Nationality" ]
                                    , th [] [ text "Birth Year" ]
                                    , th [] [ text "Major" ]
                                    , th [] [ text "University" ]
                                    , th [] [ text "School" ]
                                    , th [] [ text "From Event" ]
                                    , th [] [ text "Event City" ]
                                    , th [] [ text "Event Location" ]
                                    , th [] [ text "Date" ]
                                    , th [] [ text "Age On Date" ]
                                    , th [] [ text "From App" ]
                                    , th [] [ text "From Internet" ]
                                    ]
                                ]
                                (List.indexedMap customerDataRows customerList)
                            )
                        ]

                    NotFound ->
                        [ h1 [] [ text "Error 404: Page not found" ] ]

            Loading _ _ _ _ _ ->
                [ div [ class "backdrop" ] []
                , div [ class "loader" ] []
                ]

            Failed _ _ _ errorMessage command _ _ ->
                [ div [ class "backdrop" ] []
                , div [ class "message-modal" ]
                    [ text errorMessage
                    , button [ onClick <| ClearError command ] [ text "Ok" ]
                    ]
                ]
    }


customerDataRows : Int -> CustomerData -> Html Msg
customerDataRows index customerData =
    let
        stringFromBool : Bool -> String
        stringFromBool bool =
            if bool then
                "True"

            else
                ""
    in
    tr []
        [ th [] [ text <| String.fromInt (index + 1) ]
        , th [] [ a [ href <| "/customer/" ++ customerData.id ] [ text customerData.name ] ]
        , th [] [ text <| stringFromBool customerData.madeAPurchase ]
        , th [] [ text customerData.nationality ]
        , th [] [ text <| String.fromInt customerData.birthYear ]
        , th [] [ text customerData.major ]
        , th [] [ text customerData.university ]
        , th [] [ text customerData.school ]
        , th [] [ text customerData.fromEvent ]
        , th [] [ text customerData.eventCity ]
        , th [] [ text customerData.eventLocation ]
        , th [] [ text <| String.left 10 customerData.date ]
        , th [] [ text <| String.fromInt customerData.ageOnDate ]
        , th [] [ text <| stringFromBool customerData.fromApp ]
        , th [] [ text <| stringFromBool customerData.fromInternet ]
        ]


customerForm : Customer -> Html Msg
customerForm customer =
    let
        editable =
            customer.editable
    in
    div []
        [ editableField "Name" customer.name UpdateName editable
        , editableField "Nationality" customer.nationality UpdateNationality editable
        , editableField "Birth year" (String.fromInt customer.birthYear) UpdateBirthYear editable
        , editableField "Major" customer.major UpdateMajor editable
        , editableField "University" customer.university UpdateUniversity editable
        , editableField "School" customer.school UpdateSchool editable
        , editableField "From event" customer.fromEvent UpdateFromEvent editable
        , editableField "Event city" customer.eventCity UpdateEventCity editable
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
        [ saveCustomerError (Json.Decode.decodeValue messageDecoder >> OnSaveCustomerError)
        , loadCustomerError (Json.Decode.decodeValue messageDecoder >> OnLoadCustomerError)
        , loadCustomerListError (Json.Decode.decodeValue messageDecoder >> OnLoadCustomerListError)
        , saveCustomerSuccess (Json.Decode.decodeValue messageDecoder >> OnSaveCustomerSuccess)
        , loadCustomerSuccess (Json.Decode.decodeValue customerDecoder >> OnLoadCustomerSuccess)
        , loadCustomerListSuccess (Json.Decode.decodeValue (Json.Decode.list customerDecoder) >> OnLoadCustomerListSuccess)
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
