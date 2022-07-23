module Gen.Msg exposing (Msg(..))

import Gen.Params.Home_
import Gen.Params.NotFound
import Gen.Params.Books.Id_
import Pages.Home_
import Pages.NotFound
import Pages.Books.Id_


type Msg
    = Home_ Pages.Home_.Msg
    | Books__Id_ Pages.Books.Id_.Msg

