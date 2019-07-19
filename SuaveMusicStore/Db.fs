//    https://stackoverflow.com/questions/16864344/using-postgres-with-f
module SuaveMusicStore.Db

open Npgsql

let [<Literal>] ConnectionString=
    "Server=127.0.0.1;Port=5432;Database=suavemusicstore;User Id=postgres;Password=ioan1001cucu;"

let connection: NpgsqlConnection = new NpgsqlConnection(ConnectionString)
let equal = ('=').ToString()
 
let getGenres (connect: NpgsqlConnection)=
    let queryString="select * from genres;"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield ((dataR.Item "genreid").ToString(),(dataR.Item "name").ToString())
    } |> Seq.toList

let getAlbumsForGenre (connect: NpgsqlConnection) (genre:string)=
    let query="select albumid, title from albums inner join genres on albums.genreid"+equal+"genres.genreid where genres.name"+equal
    let queryString=String.concat  "'" [ query;genre;";"]
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()]
    } |> Seq.toList

let getAlbumDetails (connect:NpgsqlConnection) (id:int)=
    let queryString="select artists.name,albums.title,albums.price,albums.albumarturl,genres.description from albums,artists,genres where genres.genreid"+equal+"albums.genreid and albums.artistid"+equal+"artists.artistid and albums.albumid"+equal+id.ToString()+";"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()]
    } |> Seq.toList |> List.head 

let getAlbumsList (connect:NpgsqlConnection) =
    let queryString="select albums.albumid,artists.name,albums.title,genres.name,albums.price from albums,artists,genres where artists.artistid"+equal+"albums.artistid and albums.genreid"+equal+"genres.genreid;"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [|for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()|]
    } |> Seq.toList

let getAlbumToDelete (connect:NpgsqlConnection) (id:int)=
    let queryString="select albums.title from albums where albums.albumid"+equal+id.ToString()+";"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield dataR.Item "title"
    } |> Seq.toList

let getAlbumToUpdate (connect:NpgsqlConnection) (id:int)=
    let queryString="select * from albums where albumid="+id.ToString()+";"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()]
    } |> Seq.toList

let deleteAlbum (connect:NpgsqlConnection) (id:int)=
    let command = new NpgsqlCommand(("delete from albums where albumid"+equal+id.ToString()+";"), connect)
    command.ExecuteNonQuery();

let getArtists (connect:NpgsqlConnection)=
    let queryString="select * from artists;" 
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield ((dataR.Item "artistid").ToString(), (dataR.Item "name").ToString())
    } |> Seq.toList

let createAlbum (connect:NpgsqlConnection) (artistId:int) (genreId:int) (price:float) (title:string) (artUrl:string)=
    let albumTitle = String.map (fun ch -> if ch=''' then '`' else ch) title
    let queryStringCreateAlbum="insert into albums(genreid,artistid,title,price,albumarturl) values("
    let queryCreateAlbum=queryStringCreateAlbum+genreId.ToString()+","+artistId.ToString()+",'"+albumTitle+"',"+price.ToString()+","+artUrl+");"
    let command = new NpgsqlCommand(queryCreateAlbum,connect)
    command.ExecuteNonQuery()

let updateAlbum (connect:NpgsqlConnection) (artistId:int) (genreId:int) (price:float) (title:string) (artUrl:string) (id:int)=
    let albumTitle = String.map (fun ch -> if ch=''' then '`' else ch) title
    let queryUpdateAlbum="update albums set genreid="+genreId.ToString()+",artistid="+artistId.ToString()+",title='"+albumTitle+"',price="+price.ToString()+",albumarturl='"+artUrl+"' where albumid"+equal+id.ToString()+";"
    let command = new NpgsqlCommand(queryUpdateAlbum, connect)
    command.ExecuteNonQuery()

let addNewGenre (connect : NpgsqlConnection) (name: string) (descrition : string)=
    let newGenre=String.map (fun ch -> if ch=''' then '`' else ch) name 
    let newDescription=String.map (fun ch -> if ch=''' then '`' else ch) descrition
    let queryStringNewGenre="insert into genres(name,description) values("
    let queryNewGenre=queryStringNewGenre+"'"+newGenre+"','"+newDescription+"');"
    let command=new NpgsqlCommand(queryNewGenre,connect)
    command.ExecuteNonQuery()

let addNewArtist (connect : NpgsqlConnection) (name: string)=
    let newArtist=String.map (fun ch -> if ch=''' then '`' else ch) name
    let queryNewArtist="insert into artists(name) values('"+newArtist+"');"
    let command=new NpgsqlCommand(queryNewArtist,connect)
    command.ExecuteNonQuery()

let validateUser (connect:NpgsqlConnection) (username : string) (password : string)=
    let queryString="select username,password,role from users where users.username"+equal+"'"+username+"' and users.password"+equal+"'"+password+"';"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
           yield ((dataR.Item "username").ToString(),(dataR.Item "password").ToString(),(dataR.Item "role").ToString())
    } |> Seq.tryHead


let getCart (connect:NpgsqlConnection) (cartId : string)  (albumId  : int)=
    let queryString="select recordid,count from carts where carts.cartid='"+cartId+"'and carts.albumid="+albumId.ToString()+";"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR=command.ExecuteReader()
    seq {
        while dataR.Read() do
           yield ((dataR.Item "recordid").ToString(),(dataR.Item "count").ToString())
    } |> Seq.tryHead

let addNewAlbumToCart (connect : NpgsqlConnection) (cartId : string) (albumId : int)=
    let queryString="insert into carts(cartid,albumid,count,datecreated) values ('"+cartId+"',"+albumId.ToString()+",1,now());"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let increaseCountToAlbumInCart (connect : NpgsqlConnection) (count : int) (recordid : string)=
    let queryString="update carts set count="+count.ToString()+"where recordid='"+recordid+"';"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let getCartDetails (connect : NpgsqlConnection) (cartId : string)=
    let queryString="select carts.recordid,carts.albumid,carts.count,albums.title,albums.price,artists.name from albums,carts,artists where carts.cartid='"+cartId+"' and carts.albumid=albums.albumid and albums.artistid=artists.artistid;"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [|for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()|]
    } |> Seq.toList

let removeAlbumFromCart (connect : NpgsqlConnection) (recordId : int)=
    let queryString="delete from carts where recordid="+recordId.ToString()+";"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let getCarts (connect : NpgsqlConnection) (cartId : string)=
    let queryString="select recordid,albumid,count from carts where carts.cartid='"+cartId+"';"
    let command=new NpgsqlCommand(queryString,connect)
    let dataR=command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()]
    } |> Seq.toList

let updateCartId (connect : NpgsqlConnection) (cartId : string) (recordId : string)=
    let queryString="update carts set cartid='"+cartId+"' where recordid='"+recordId+"';"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let getUser (connect : NpgsqlConnection) (userName : string)=
    let queryString="select username from users where users.username='"+userName+"';"
    let command=new NpgsqlCommand(queryString,connect)
    let dataR=command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield (dataR.Item "username").ToString()
    } |> Seq.toList

let addNewUser (connect : NpgsqlConnection) (userName : string) (eMail : string) (password : string)=
    let queryString="insert into users(username,email,password,role) values('"+userName+"', '"+eMail+"', '"+password+"', 'user');"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let addOrder (connect : NpgsqlConnection) (userName : string) (firstName : string) (lastName : string) (address : string) (city : string) (country : string) (postalcode : string) (phone : string) (total : float)=
    let queryString="insert into orders(orderdate,username,firstname,lastname,address,city,postalcode,country,phone,total) values (now(),'"+userName+"','"+firstName+"','"+lastName+"','"+address+"','"+city+"','"+postalcode+"','"+country+"','"+phone+"',"+total.ToString()+");"
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let getOrderId (connect : NpgsqlConnection) (userName : string)=
    let queryString="select orderid from orders where username='" + userName+"' order by orderdate desc;"
    let command=new NpgsqlCommand(queryString,connect)
    let dataR=command.ExecuteReader()
    seq {
        while dataR.Read() do
           yield (dataR.Item "orderid").ToString()
    } |> Seq.tryHead 

let addOrderDetails (connect : NpgsqlConnection) (orderId : string) (albumId : string) (quantity : int) (unitprice : float)=
    let queryString="insert into orderdetails(orderid,albumid,quantity,unitprice) values('"+orderId+"','"+albumId+"',"+quantity.ToString()+","+unitprice.ToString()+");" 
    let command=new NpgsqlCommand(queryString,connect)
    command.ExecuteNonQuery()

let getBestSellers (connect : NpgsqlConnection)=
    let queryString="select * from bestsellers"
    let command = new NpgsqlCommand(queryString, connect)
    let dataR = command.ExecuteReader()
    seq {
        while dataR.Read() do
            yield [|for i in [0..dataR.FieldCount-1] -> dataR.[i].ToString()|]
    } |> Seq.toList
    
(*
let printResults =    
    conn.Open()
    try
        let resultSet = getResultSet conn queryString
        for r in resultSet do
            //for d in r do
                //printf "%s\t" (r.ToString())
           // printfn ""
    finally
        conn.Close()
*)
