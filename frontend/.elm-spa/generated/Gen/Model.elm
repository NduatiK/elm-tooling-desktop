module Gen.Model exposing (Model(..))

import Gen.Params.Books.Id_
import Gen.Params.Home_
import Gen.Params.NotFound
import Pages.Books.Id_
import Pages.Home_


type Model
    = Redirecting_
    | Home_ Gen.Params.Home_.Params Pages.Home_.Model
    | NotFound Gen.Params.NotFound.Params
    | Books__Id_ Gen.Params.Books.Id_.Params Pages.Books.Id_.Model
