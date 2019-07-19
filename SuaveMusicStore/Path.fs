module SuaveMusicStore.Path

type IntPath=PrintfFormat<(int -> string),unit,string,string,int>

let home = "/"
let withParam (key,value) path=sprintf "%s?%s=%s" path key value

module Store=
    let overview= "/store"
    let browse = "/store/browse"
    let details : IntPath = "/store/details/%d"
    let browseKey= "genre"

module Admin=
    let manage="/admin/manage"
    let albumToDelete : IntPath="/admin/delete/%d"
    let createAlbum="/admin/create"
    let editAlbum : IntPath ="/admin/edit/%d"
    let createGenre="/admin/new_genre"
    let createArtist="/admin/new_artist"

module Account=
    let logon="/account/logon"
    let logoff="/account/logoff"
    let register="/account/register"

module Cart=
    let overview="/cart"
    let addAlbumToCart : IntPath="/cart/add/%d"
    let removeAlbumFromCart : IntPath="/cart/remove/%d"
    let checkout="/cart/checkout"    