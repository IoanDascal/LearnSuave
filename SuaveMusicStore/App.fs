module SuaveMusicStore.App

open System
open Suave  
open Suave.Authentication               // always open suave
open Suave.Filters
open Suave.Form
open Suave.Model.Binding
open Suave.Operators
open Suave.RequestErrors
open Suave.Cookie
open Suave.State.CookieStateStore
open Suave.Successful      // for OK-result

type UserLoggedOnSession={
    Username : string
    Role : string
}

type Session=
    | NoSession  
    | CartIdOnly of string
    | UserLoggedOn of UserLoggedOnSession

let session f=statefulForSession
              >=> context ( fun x -> match x |> HttpContext.state with
                                     | None -> f NoSession
                                     | Some state -> match state.get "cartid", state.get "username", state.get "role" with
                                                     | Some cartId, None, None -> f (CartIdOnly cartId)
                                                     | _, Some username, Some role -> f (UserLoggedOn {Username=username; Role=role})
                                                     | _ -> f NoSession
              )                         

let html container= 
    let result cartItems user=OK (View.index (View.partNav cartItems) (View.partUser user) container)
                              >=> Writers.setMimeType "text/html; charset=utf-8"
    session (function
        | UserLoggedOn {Username=username } ->
            Db.connection.Open()
            let items=Db.getCartDetails Db.connection username |> List.sumBy (fun l -> (l.[2] |> int))
            Db.connection.Close()
            result items (Some username)
        | CartIdOnly cartId -> 
            Db.connection.Open()
            let items=Db.getCartDetails Db.connection cartId |> List.sumBy (fun l -> (l.[2] |> int))
            Db.connection.Close()
            result items None
        | NoSession -> result 0 None
    )

let home=warbler (fun _ ->
    Db.connection.Open()
    let bestSellers=Db.getBestSellers Db.connection
    Db.connection.Close()
    View.home bestSellers |> html
)

let rec listToString list=
    match list with
    | [l] -> l.ToString()
    | head::tail -> head.ToString() + "  " + listToString tail 
    | [] -> ""

let browse=
    request (fun r -> match r.queryParam Path.Store.browseKey with
                      | Choice1Of2 genre -> Db.connection.Open()
                                            let result=Db.getAlbumsForGenre Db.connection genre
                                            Db.connection.Close()
                                            result
                                            |> View.browse genre
                                            |> html
                      | Choice2Of2 msg -> BAD_REQUEST msg)

let overview=warbler (fun _ ->
    Db.connection.Open()
    let result=Db.getGenres Db.connection
    Db.connection.Close()
    result 
    |> List.map snd 
    |> View.store
    |>html)

let details id=warbler (fun _ ->
    Db.connection.Open()
    let result=Db.getAlbumDetails Db.connection id
    Db.connection.Close()
    result 
    |> View.details id
    |> html)

let manage=warbler (fun _ ->
    Db.connection.Open()
    let result=Db.getAlbumsList Db.connection |> List.sortBy (fun r -> r.[1])
    Db.connection.Close()
    result
    |> View.manage
    |> html)

let deleteAlbum id= 
    choose [
        GET >=> warbler (fun _ ->
            Db.connection.Open()
            let result=Db.getAlbumToDelete Db.connection id 
            Db.connection.Close()
            result
            |> listToString
            |> View.albumToDelete
            |> html)
        POST >=> warbler (fun _ ->
            Db.connection.Open()
            Db.deleteAlbum Db.connection id |> ignore
            Db.connection.Close()
            Redirection.FOUND Path.Admin.manage)
    ]  

let bindToForm form handler=
    bindReq (bindForm form) handler BAD_REQUEST
  
let createAlbum=
    choose [
        GET >=> warbler (fun _ ->
            Db.connection.Open()
            let genres=Db.getGenres Db.connection
                       |> List.map (fun g -> decimal (fst g),snd g) |> List.sortBy snd
            Db.connection.Close()
            Db.connection.Open()
            let artists=Db.getArtists Db.connection
                        |> List.map (fun a -> decimal (fst a),snd a) |> List.sortBy snd
            Db.connection.Close()
            html (View.createAlbum genres artists)
        )
        POST >=> bindToForm Form.album (fun form ->
            Db.connection.Open()
            Db.createAlbum Db.connection (int form.ArtistId) (int form.GenreId) (float form.Price) form.Title |> ignore
            Db.connection.Close()
            Redirection.FOUND Path.Admin.manage
        )
    ]

let editAlbum id=
    choose [
        GET >=> warbler (fun _ ->
            Db.connection.Open()
            let albumToEdit=(Db.getAlbumToUpdate Db.connection id).Head
            Db.connection.Close()
            Db.connection.Open()
            let genres=Db.getGenres Db.connection
                       |> List.map (fun g -> decimal (fst g),snd g) |> List.sortBy snd
            Db.connection.Close()
            Db.connection.Open()
            let artists=Db.getArtists Db.connection
                        |> List.map (fun a -> decimal (fst a),snd a) |> List.sortBy snd
            Db.connection.Close()
            html (View.editAlbum albumToEdit genres artists)
        )
        POST >=> bindToForm Form.album (fun form ->
            Db.connection.Open()
            Db.updateAlbum Db.connection (int form.ArtistId) (int form.GenreId) (float form.Price) form.Title form.ArtUrl id |> ignore
            Db.connection.Close()
            Redirection.FOUND Path.Admin.manage
        )
    ]

let addNewGenre=
    let mutable existingGenres=Array.empty<string>
    choose [
        GET >=> warbler (fun _ ->
            Db.connection.Open()
            let genres=List.toArray (Db.getGenres Db.connection)
                       |> Array.map snd
            Db.connection.Close()
            existingGenres <- genres
            html (View.addNewGenre)
        )
        POST >=> bindToForm Form.genre (fun form ->
            match Array.contains form.Name existingGenres with
            | true -> html (View.genreAlreadyExist form.Name)
            | false -> Db.connection.Open()
                       Db.addNewGenre Db.connection form.Name form.Description |> ignore
                       Db.connection.Close()
                       Redirection.FOUND Path.Admin.manage
        )
    ]  

let addNewArtist=
    let mutable existingArtist=Array.empty<string>
    choose [
        GET >=> warbler (fun _ ->
            Db.connection.Open()
            let artists=Db.getArtists Db.connection
                        |> List.toArray
                        |> Array.map snd
            Db.connection.Close()
            existingArtist <- artists
            html (View.addNewArtist)
        )
        POST >=> bindToForm Form.artist (fun form ->
            match Array.contains form.Name existingArtist with
            | true -> html (View.artistAlreadyExist form.Name)
            | false -> Db.connection.Open()
                       Db.addNewArtist Db.connection form.Name |> ignore
                       Db.connection.Close()
                       Redirection.FOUND Path.Admin.manage
        )
    ] 

let passHash (pass : string)=
    use sha=Security.Cryptography.SHA256.Create()
    Text.Encoding.UTF8.GetBytes(pass)
    |> sha.ComputeHash
    |> Array.map (fun b -> b.ToString("x2"))
    |> String.concat ""

let sessionStore setF=context (fun x ->
    match HttpContext.state x with
    | Some state -> setF state
    | None -> never
)

let returnPathOrHome=
    request (fun x ->
        let path=match (x.queryParam "returnPath") with
                 | Choice1Of2 path -> path
                 | _ -> Path.home
        Redirection.FOUND path
    )

let reset=unsetPair SessionAuthCookie
          >=> unsetPair StateCookie
          >=> Redirection.FOUND Path.home

let redirectWithReturnPath redirection=
    request (fun x -> let path= x.url.AbsolutePath
                      Redirection.FOUND (redirection |> Path.withParam ("returnPath", path))
             )

let loggedOn fSuccess=authenticate Session 
                                   false
                                   (fun () -> Choice2Of2 (redirectWithReturnPath Path.Account.logon))
                                   (fun _ -> Choice2Of2 reset)
                                   fSuccess

let admin fSuccess=loggedOn (session (function
                       | UserLoggedOn {Role="admin"} -> fSuccess
                       | UserLoggedOn _ -> FORBIDDEN "Only for Admin"
                       | _ -> UNAUTHORIZED "Not logged in"
                   ))

let authenticateUser (user : string, _ : string, role : string)=
    authenticated Session false
    >=> session (function
        | CartIdOnly cartId -> 
            Db.connection.Open()
            let carts=Db.getCarts Db.connection cartId
            Db.connection.Close()
            for cart in carts do
                Db.connection.Open()
                let cartUser=Db.getCart Db.connection user (cart.[1] |> int)
                Db.connection.Close()
                match cartUser with
                | Some  (recordid,count) -> let totalCount=(count |> int) + (cart.[2] |> int)
                                            Db.connection.Open()
                                            Db.increaseCountToAlbumInCart Db.connection totalCount recordid |> ignore
                                            Db.connection.Close()
                                            Db.connection.Open()
                                            Db.removeAlbumFromCart Db.connection (cart.[0] |> int) |> ignore
                                            Db.connection.Close()
                | None -> Db.connection.Open()
                          Db.updateCartId Db.connection user cart.[0] |> ignore
                          Db.connection.Close()
            sessionStore (fun store -> store.set "cartid" "")
        | _ -> succeed
    )
    >=> sessionStore (fun store ->
        store.set "username" user
            >=> store.set "role" role
    )
    >=> returnPathOrHome

let logon=
    choose [
        GET >=> (View.logon "" |> html)
        POST >=> bindToForm Form.logon (fun form ->
            let (Password password)=form.Password
            Db.connection.Open()
            let user=Db.validateUser Db.connection form.Username (passHash password)
            Db.connection.Close()
            match user with
            | Some usr -> authenticateUser usr
            | None -> View.logon "Username or password is invalid" |> html
        )
    ]

let register=
    choose [
        GET >=> (View.register "" |> html)
        POST >=> bindToForm Form.register (fun form ->
            Db.connection.Open()
            let user=Db.getUser Db.connection form.Username
            Db.connection.Close()
            match user with
            | usr::_ -> sprintf "Sorry, username %s is already taken. Try another one." usr
                     |> View.register
                     |> html
            | [] -> let (Password password)=form.Password
                    let user=(form.Username,"","user")
                    Db.connection.Open()
                    Db.addNewUser Db.connection form.Username form.Email (passHash password) |> ignore
                    Db.connection.Close()
                    authenticateUser user
        )
    ]

let cart=
    session (function
        | NoSession -> View.emptyCart |> html
        | UserLoggedOn {Username=cartId} | CartIdOnly cartId ->
            Db.connection.Open()
            let result=Db.getCartDetails Db.connection cartId
            Db.connection.Close()
            result
            |> View.cart 
            |> html
    )

let addToCart (albumId : int)=
    session (function 
             | NoSession -> let cartId=Guid.NewGuid().ToString("N")
                            Db.connection.Open()
                            let result=Db.getCart Db.connection cartId albumId
                            Db.connection.Close()
                            match result with
                            | Some cart -> let count=snd cart |> int
                                           Db.connection.Open()
                                           Db.increaseCountToAlbumInCart Db.connection (count+1) (fst cart) |> ignore
                                           Db.connection.Close()
                            | None -> Db.connection.Open()
                                      Db.addNewAlbumToCart Db.connection cartId albumId |> ignore
                                      Db.connection.Close()
                            sessionStore (fun store -> store.set "cartid" cartId)
             | UserLoggedOn {Username=cartId} | CartIdOnly cartId -> 
                            Db.connection.Open()
                            let result=Db.getCart Db.connection cartId albumId
                            Db.connection.Close()
                            match result with
                            | Some cart -> let count=snd cart |> int
                                           Db.connection.Open()
                                           Db.increaseCountToAlbumInCart Db.connection (count+1) (fst cart) |> ignore
                                           Db.connection.Close()
                            | None -> Db.connection.Open()
                                      Db.addNewAlbumToCart Db.connection cartId albumId |> ignore
                                      Db.connection.Close()
                            succeed
    )
    >=> Redirection.FOUND Path.Cart.overview

let removeFromCart (recordId : int)=
    session (function 
             | NoSession -> never
             | UserLoggedOn {Username=cartId} | CartIdOnly cartId ->
                 Db.connection.Open()
                 Db.removeAlbumFromCart Db.connection recordId |> ignore
                 Db.connection.Close()
                 Db.connection.Open()
                 let result=Db.getCartDetails Db.connection cartId
                 Db.connection.Close()
                 result
                 |> View.cart
                 |> html
    )

let checkout=
    session (function 
        | NoSession | CartIdOnly _ -> never
        | UserLoggedOn {Username=username} -> 
            choose [
                GET >=> (View.checkout |> html)
                POST >=> warbler (fun _ -> bindToForm Form.checkout (fun form ->
                    Db.connection.Open()
                    let carts=Db.getCartDetails Db.connection username
                    Db.connection.Close()
                    let total= carts |> List.sumBy (fun c -> (float c.[2])*(float c.[4]))
                    Db.connection.Open()
                    Db.addOrder Db.connection username form.FirstName form.LastName form.Address form.City form.Country form.PostalCode form.Phone total |> ignore
                    Db.connection.Close()
                    Db.connection.Open()
                    let orderId=Db.getOrderId Db.connection username
                    Db.connection.Close()
                    for cart in carts do
                        Db.connection.Open()
                        Db.addOrderDetails Db.connection orderId.Value cart.[1] (cart.[2] |> int) (cart.[4] |> float) |> ignore
                        Db.connection.Close()
                        Db.connection.Open()
                        let recordId=Db.getCart Db.connection username (cart.[1] |> int)
                        Db.connection.Close()
                        Db.connection.Open()
                        Db.removeAlbumFromCart Db.connection ((fst recordId.Value) |> int) |> ignore
                        Db.connection.Close()
                    View.checkoutComplete |> html
                ))
            ]
    )

let webPart=
    choose [
        path Path.home >=> home
        path Path.Store.overview >=> overview
        path Path.Store.browse >=> browse
        pathScan Path.Store.details details
        path Path.Admin.manage >=> admin manage
        pathScan Path.Admin.albumToDelete (fun id -> admin (deleteAlbum id))
        path Path.Admin.createAlbum >=> createAlbum
        pathScan Path.Admin.editAlbum (fun id -> admin (editAlbum  id))
        path Path.Admin.createGenre >=> admin addNewGenre
        path Path.Admin.createArtist >=> admin addNewArtist
        path Path.Account.logon >=> logon
        path Path.Account.logoff >=> reset
        path Path.Account.register >=> register
        path Path.Cart.overview >=> cart
        pathScan Path.Cart.addAlbumToCart addToCart
        pathScan Path.Cart.removeAlbumFromCart removeFromCart
        path Path.Cart.checkout >=> loggedOn checkout
        pathRegex "(.*)\.(css|png|gif)" >=> Files.browseHome
        html View.notFound
    ]



startWebServer defaultConfig webPart
