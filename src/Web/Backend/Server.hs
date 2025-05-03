module Web.Backend.Server
    ( module Web.Backend.Server.Login
    , homepage
    )
where

import Web.Backend.Server.Login
import Web.Frontend.Homepage
import Happstack.Server
import Web.Backend.Auth (getUser)

-- homepage :: ServerPart Response
-- homepage = do
--     user <- getUser
--     ok $ toResponse $ homepagePage user

homepage :: ServerPart Response
homepage = ok $ toResponse $ homepagePage Nothing