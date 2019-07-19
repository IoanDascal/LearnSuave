module SuaveMusicStore.Form

open Suave.Form

type Album={
    ArtistId : decimal
    GenreId : decimal
    Title : string
    Price : decimal
    ArtUrl : string
}

let album : Form<Album>=
    Form ([ TextProp ((fun f -> <@ f.Title @>), [maxLength 100])
            TextProp ((fun f -> <@ f.ArtUrl @>), [maxLength 100])
            DecimalProp ((fun f -> <@ f.Price @>), [min 0.01M; max 100.0M; step 0.01M])
            ],
          [])

type Genre={
    Name : string
    Description : string
}

let genre : Form<Genre>=
    Form ([ TextProp ((fun f -> <@ f.Name @>), [maxLength 50])
            TextProp ((fun f -> <@ f.Description @>), [maxLength 500])
            ],
          [])

type Artist={
    Name : string
}

let artist : Form<Artist>=
    Form ([ TextProp ((fun f -> <@f.Name @>), [maxLength 100])
            ],
          [])

type Logon={
    Username : string
    Password : Password
}

let logon : Form<Logon>=Form ([],[])

type Register={
    Username : string
    Email : string
    Password : Password
    ConfirmPassword : Password
}

let pattern=passwordRegex @"(\w){6,20}"

let passwordsMatch=(fun f -> f.Password=f.ConfirmPassword), "Passwords must match"

let register : Form<Register>=
    Form ([TextProp ((fun f -> <@f.Username @>),[maxLength 30])
           PasswordProp ((fun f -> <@ f.Password @>), [pattern])
           PasswordProp ((fun f -> <@ f.ConfirmPassword @>), [pattern])
          ],
          [passwordsMatch])

type Checkout={
    FirstName : string
    LastName : string
    Address : string
    City : string
    Country : string
    PostalCode : string
    Phone : string
    PromoCode : string 
}

let checkout : Form<Checkout>=
    Form ([TextProp ((fun f -> <@ f.FirstName @>),[maxLength 30])
           TextProp ((fun f -> <@ f.LastName @>), [maxLength 30])
           TextProp ((fun f -> <@ f.Address @>), [maxLength 100])
           TextProp ((fun f -> <@ f.City @>), [maxLength 50])
           TextProp ((fun f -> <@ f.Country @>), [maxLength 100])
           TextProp ((fun f -> <@ f.PostalCode @>), [maxLength 10])
           TextProp ((fun f -> <@ f.Phone @>), [maxLength 15])
           TextProp ((fun f -> <@ f.PromoCode @>), [maxLength 100])
          ],
          [])
