module SuaveMusicStore.View

open Suave.Form
open Suave.Html

let cssLink href = link ["href",href;"rel","stylesheet";"type","text/css"]
let h2 s= tag "h2" [] [Text s]
let ul nodes= tag "ul" [] nodes
let li = tag "li" []
let em s= tag "em" [] [Text s]
let table x= tag "table" [] x
let th x= tag "th" [] x
let tr x= tag "tr" [] x
let td x= tag "td" [] x
let strong s= tag "strong" [] [Text s]
let form x= tag "form" ["method", "POST"] x
let submitInput value= input ["type","submit";"value",value]
let formInput=Suave.Form.input
let ulAttr attr nodes=tag "ul" attr nodes

type Field<'a>={
    Label : string
    Html : Form<'a> -> Suave.Html.Node
}

type Fieldset<'a>={
    Legend : string
    Fields : Field<'a> list
}

type FormLayout<'a>={
    Fieldsets : Fieldset<'a> list
    SubmitText : string
    Form : Form<'a>
}

let renderForm (layout : FormLayout<_>)=
    form [
        for set in layout.Fieldsets ->
            tag "fieldset" [] [
                yield tag "legend" [] [Text set.Legend]
                for field in set.Fields do
                    yield div ["class","editor-label"] [Text field.Label]
                    yield div ["class","editor-field"] [field.Html layout.Form]
            ]
        yield submitInput layout.SubmitText
    ]

let home (bestSellers : string [] list)= [
    img ["src","/home-showcase.png"]
    h2 "Fresh off the grill"
    ulAttr ["id","album-list"] [
        for album in bestSellers ->
            li [
                a (sprintf Path.Store.details (album.[0] |> int)) [] [
                    img ["src",album.[2]]
                    span [] [Text album.[1]]
                ]
            ]
    ]
]

let notFound=[
    h2 "Page not found"
    p [] [
        Text "Could not found the requested resource."
    ]
    p [] [
        Text "Back to:"
        a Path.home [] [Text "HOME"]
    ]
]

let genreAlreadyExist genre=[
    h2 "Genre already exist"
    p [] [
        Text "Can't add genre "
        Text genre
        Text " because it already exist in database!"
    ]
    p [] [
        Text "Back to "
        a Path.Admin.manage [] [Text "Manage"]
    ]
]

let artistAlreadyExist artist=[
    h2 "Genre already exist"
    p [] [
        Text "Can't add artist "
        Text artist
        Text " because it already exist in database!"
    ]
    p [] [
        Text "Back to "
        a Path.Admin.manage [] [Text "Manage"]
    ]
]

let store genres= [
    h2 "Browse Genres"
    p [] [
        Text (sprintf "Select from %d genres:" (List.length genres))
    ]
    ul [
        for genre in genres ->
            let url=Path.withParam (Path.Store.browseKey, genre) Path.Store.browse
            li [a url [] [Text genre]]
    ]
]

let browse genre (albums:string list list)= [
    h2 (sprintf "Genre: %s" genre)
    ul [
        for album in albums ->
            li [a (sprintf Path.Store.details (album.Head |> int)) [] [Text album.Tail.Head]]
    ]
]

let details (id : int) (albumDetails: string list) = [
    h2 (albumDetails.[1])
    p [] [a albumDetails.[3] [] [Text "See on YOUTUBE"]]  //[ img ["src",albumDetails.Head.[3]]]
    div ["id","album-details"] [
        for (caption, t) in [ "Artist : ",albumDetails.[0]
                              "Price : ",sprintf "$%.2f" (float32 albumDetails.[2])
                              "Description : ",albumDetails.[4]] -> p [] [
                                                                             em caption
                                                                             Text t
                                                                         ]
        yield p ["class","button"] [
            a (sprintf Path.Cart.addAlbumToCart  id) [] [Text "Add to cart"]
        ]
    ]
]

let manage (albums:string array list)=[
    h2 "Index"
    p [] [
        Text "To add a new album you must add first a new genre and a new artist if they doesn't exist"
        br []
        a Path.Admin.createAlbum [] [Text "Add New Album"]
        br []
        br []
        a Path.Admin.createGenre [] [Text "Add New Genre"]
        br []
        a Path.Admin.createArtist [] [Text "Add New Artist"]
    ]
    table [
        yield tr [
            for t in ["Artist";"Title";"Genre";"Price";"Action"] -> th [Text t]
        ]
    
        for album in albums ->
        tr [
            for t in 1..album.Length-2 -> td [Text album.[t]]
            yield td [
                 Text (sprintf "$%.2f" (float32 album.[album.Length-1]))
            ]
            yield td [
                a (sprintf Path.Admin.editAlbum (album.[0] |> int)) [] [Text "Edit"]
                Text " | "
                a (sprintf Path.Admin.albumToDelete (album.[0] |> int)) [] [Text "Delete "]
            ]
        ]
    ]
]

let albumToDelete (albumTitle:string) =[
    h2 "Delete Confirmation"
    p [] [
        Text "Are you sure you want to delete the album titled"
        br []
        strong albumTitle
        Text "?"
    ]
    form [
        submitInput "Delete"
    ]
    div [] [
        a Path.Admin.manage [] [Text "Back to list"]
    ]
]

let createAlbum genres artists=[
    h2 "Create New Album"
    renderForm
        {
            Form=Form.album
            Fieldsets=
                [{
                     Legend="Album"
                     Fields=
                         [{
                              Label="Genre"
                              Html=selectInput (fun f -> <@ f.GenreId @>) genres None
                          }
                          {
                              Label="Artist"
                              Html= selectInput (fun f -> <@ f.ArtistId @>) artists None
                          }
                          {
                              Label="Title"
                              Html=formInput (fun f -> <@ f.Title @>) []
                          }
                          {
                              Label="Price"
                              Html=formInput (fun f -> <@ f.Price @>) [] 
                          }
                          {
                              Label="Album Art URL"
                              Html=formInput (fun f -> <@ f.ArtUrl @>) []
                          }]
                }]
            SubmitText="Create"
        }
    div [] [
        a Path.Admin.manage [] [Text "Back to list"]
    ]
]

let editAlbum (album: string list) genres artists=[
    h2 "Edit"
    renderForm
        {
            Form=Form.album
            Fieldsets=
                [{
                    Legend="Album"
                    Fields=
                       [{
                            Label="Genre"
                            Html=selectInput (fun f -> <@ f.GenreId @>) genres (Some (decimal album.[1]))
                        }
                        {
                            Label="Artist"
                            Html=selectInput (fun f -> <@ f.ArtistId @>) artists (Some (decimal album.[2]))
                        }
                        {
                            Label="Title"
                            Html=formInput (fun f -> <@ f.Title@>) ["value", album.[3]]
                        }
                        {
                            Label="Price"
                            Html=formInput (fun f -> <@ f.Price@>) ["value", formatDec (decimal album.[4])]
                        }
                        {
                            Label="Album Art URL"
                            Html=formInput (fun f -> <@ f.ArtUrl @>) ["value", album.[5]]
                        }]
                }]
            SubmitText="Save Changes"
        }
    div [] [
        a Path.Admin.manage [] [Text "Back to list"]
    ]
]

let addNewGenre=[
    h2 "Add New Genre"
    renderForm
        {
            Form=Form.genre
            Fieldsets=
                [{
                    Legend="New Genre"
                    Fields=
                        [{
                            Label="Name"
                            Html=formInput (fun f -> <@ f.Name @>) []
                         }
                         {
                              Label="Description"
                              Html=formInput (fun f -> <@ f.Description @>) [] 
                        }]
                }]
            SubmitText="Submit New Genre"
        }
    div [] [
        a Path.Admin.manage [] [Text "Back to list"]
    ]
]

let addNewArtist=[
    h2 "Add New Artist"
    renderForm
        {
            Form=Form.artist
            Fieldsets=
                [{
                    Legend="New Artist"
                    Fields=
                        [{
                            Label="Name"
                            Html=formInput (fun f -> <@ f.Name @>) []
                        }]
                }]
            SubmitText="Submit New Artist"
        }
    div [] [
        a Path.Admin.manage [] [Text "Back to list"]
    ]
]

let partNav cartItems=
    ulAttr ["id","navlist"] [
        li [a Path.home [] [Text "Home"]]
        li [a Path.Store.overview [] [Text "Store"]]
        li [a Path.Cart.overview [] [Text (sprintf "Cart (%d)" cartItems)]]
        li [a Path.Admin.manage [] [Text "Admin"]]
    ]

let partUser (user: string option)=
    div ["id","part-user"] [
        match user with
        | Some user -> yield Text (sprintf "Logged on as %s, " user)
                       yield a Path.Account.logoff [] [Text "Log Off"]    
        | None -> yield a Path.Account.logon [] [Text "Log On"]
    ]

//let partGenres genres=

let logon msg=[
    h2 "Log On"
    p [] [
        Text "Please enter your user name and password."
        a Path.Account.register [] [Text "Register"]
        Text " if you don't have an account yet."
    ]
    div ["id","logon-message"] [
        Text msg
    ]
    renderForm
        {
            Form=Form.logon
            Fieldsets=
                [{
                    Legend="Account Information"
                    Fields=
                        [{
                            Label="User Name"
                            Html=formInput (fun f -> <@ f.Username @>) [] 
                         }
                         {
                            Label="Password"
                            Html=formInput (fun f -> <@ f.Password @>) []
                        }]
                }]
            SubmitText="Log On"
        }
]

let emptyCart=[
    h2 "Your cart is empty"
    Text "Find some great music in our"
    a Path.home [] [Text " store "]
    Text "!"
]

let nonEmptyCart (carts : string array list)=[
    h2 "Review your cart :"
    p ["class","button"] [
        a Path.Cart.checkout [] [Text "CHeckout>>"]
    ]
    div ["id","update-message"] [Text ""]
    table [
        yield tr [
            for h in ["Album name"; "Artist name"; "Price(each)";"Quantity";""] -> th [Text h]
        ]
        for cart in carts ->
            tr [
                td [
                    a (sprintf Path.Store.details (cart.[1] |> int)) [] [Text cart.[3]]
                ]
                td [
                    Text cart.[5]
                ]
                td [
                    Text cart.[4]
                ]
                td [
                    Text cart.[2]
                ]
                td [
                    a (sprintf Path.Cart.removeAlbumFromCart (cart.[0] |> int)) [] [Text "Remove From Cart"]
                ]
            ]
        yield tr [
            let total=carts |> List.sumBy (fun c -> (decimal c.[2])*(decimal c.[4]))
            for d in ["Total";"";"";formatDec total] -> td [Text d]
        ]
    ]
]

let cart=function
    | [] -> emptyCart
    | list -> nonEmptyCart list

let register msg=[
    h2 "Create a New Account"
    p [] [
        Text "Use the form bellow to create a new account."
    ]
    div ["id", "register-message"][
        Text msg
    ]
    renderForm
        {
            Form=Form.register
            Fieldsets=
                [{
                    Legend="Create a New Account"
                    Fields=
                        [{
                            Label="User Name (max 30 characters)"
                            Html= formInput ( fun f -> <@ f.Username @>) []
                         }
                         {
                            Label="Email Address"
                            Html= formInput ( fun f -> <@ f.Email @>) []
                         }
                         {
                             Label= "Password (between 6 and 20 characters)"
                             Html = formInput (fun f -> <@ f.Password @>) []
                         }
                         {
                             Label="Confirm Password"
                             Html= formInput (fun f -> <@ f.ConfirmPassword @>) []
                        }]
                }]
            SubmitText = "Register"
        }
]

let checkout = [
    h2 "Address And Payment"
    renderForm
        { 
            Form = Form.checkout 
            Fieldsets = 
                [{ 
                    Legend = "Shipping Information"
                    Fields = 
                        [{ 
                            Label = "First Name"
                            Html = formInput (fun f -> <@ f.FirstName @>) [] 
                         }
                         { 
                            Label = "Last Name"
                            Html = formInput (fun f -> <@ f.LastName @>) [] 
                         }
                         { 
                            Label = "Address"
                            Html = formInput (fun f -> <@ f.Address @>) []
                         }
                         { 
                            Label = "City"
                            Html = formInput (fun f -> <@ f.City @>) [] 
                         }
                         { 
                            Label = "Country"
                            Html = formInput (fun f -> <@ f.Country @>) []
                         }
                         { 
                            Label = "PostalCode"
                            Html = formInput (fun f -> <@ f.PostalCode @>) [] 
                         }
                         { 
                            Label = "Phone"
                            Html = formInput (fun f -> <@ f.Phone @>) []
                        }] 
                 }
                 { 
                    Legend = "Payment"
                    Fields = 
                        [{ 
                            Label = "Promo Code"
                            Html = formInput (fun f -> <@ f.PromoCode @>) [] 
                         }] 
                }]
            SubmitText = "Submit Order"
        }
]

let checkoutComplete=[
    h2 "Checkout Complete"
    p [] [
        Text "Thanks for your order"
    ]
    p [] [
        Text "How about shopping for some more music in our "
        a Path.Store.overview [] [Text "store ?"]
    ]
]

let index partNav partUser container=
    html [] [
        head [] [
            title [] "Suave Music Store"
            cssLink "/Site.css"
        ]
        body [] [
            div ["id","header"] [
                tag "h1" [] [
                    a Path.home [] [Text "F# Suave Music  Store"]
                ]
                partNav
                partUser
            ]

            div ["id","main"] container

            div ["id","footer"] [
                Text "built with "
                a "http://fsharp.org" [] [Text "F#"]
                Text " and "
                a "http://suave.io" [] [Text " Suave.IO"]
            ]
        ]
    ]
    |> htmlToString
    