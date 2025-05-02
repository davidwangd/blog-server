module Web.Backend.Auth
    ( login
    , register
    , verifyUserLevel
    ) where

import Web.Backend.Data
import Web.JWT
import Database.SQLite.Simple
import qualified Data.Text as T
import Web.Backend.Sql
import Safe
import Control.Monad

data UserResult = Either String User

queryUserByName :: Text -> IO (Maybe User)
queryUserByName uname = do
    conn <- openDB
    liftM headMay $ (query conn "SELECT * FROM user WHERE username = ?" (Only uname))

encode :: Text -> Text
encode = id   -- TODO: encode the password

getVerifier :: IO VerifySigner
getVerifier = return $ toVerify $ hmacSecret $ T.pack $ "secret"

getSigner :: IO EncodeSigner
getSigner = return $ hmacSecret $ T.pack $ "secret"

issuer :: Text
issuer = T.pack "davidwang"

getInviteList :: IO [(Text, Int)]
getInviteList = return $ [("test_invite_number", 3), ("test_guest", 2)]

checkLoginInfo :: Text -> Text -> IO UserResult
checkLoginInfo uname pss = do
    let pass' = encode pass
    conn <- openDB
    users <- queryUserByName uname
    return $ case users of
        Nothing -> Left "UserNotExisists"
        Just u  -> if (password u) != pass' 
            then Left "Password Error"
            else Right u

registerUser :: Text -> Text -> Text -> IO UserResult
registerUser uname pass invite = do
    invites <- getInviteList
    conn <- openDB
    case lookup invite invites of
        Nothing -> return $ Left "InvalidInvitationCode"
        Just lvl -> do
            users <- queryUserByName uname
            if users != Nothing
                then return $ Left "UserAlreadyExsists"
                else do
                    let pass' = encode pass
                    let cUser = User { userId = -1, username = uname, password = pass', level = level}
                    insert cUser conn
                    lastId <- lastInsertRowId
                    return $ Right $ cUser { userId = lastId }

signJWT :: User -> IO Text
signJWT user = do
    signer <- getSigner
    return $ encodeSigned signer head claims
    where head   = mempty {}
          claims = mempty { iss = stringOrURI issuer,
                          , sub = stringOrURI $ (username user ++ "|" ++ (show $ userId user))
                          }

parseJWT :: Text -> IO (Maybe User)
parseJWT jwt = do
    verifier <- getVerifier
    conn <- openDB
    case parsedJwt of
        Nothing -> return Nothing
        Just txt -> do
            let eles = splitOn "|" txt
                uname = head eles
                uid = (read $ last eles) :: Int
            res <- queryById uid conn
            case res of
                Nothing -> return $ Nothing
                Just user -> if (userId user) == uid && (username user) == uname then res else Nothing
    where parsedJwt = decodeAndVerifySignature verifier jwt

verifyUserLevel :: [ServerPart a] -> ServerPart a
verifyUserLevel res = do
    jwt <- optional $ lookCookieValue "session"
    case jwt of
        Nothing -> head res
        Just txt -> do
            user <- parseJWT txt
            case user of
                Nothing -> head res
                Just user -> res !! ((min (length res - 1) (level user)))

register :: ServerPart UserResult
register = error "unimplemented"

login :: ServerPart UserResult
login = error "unimplemented"