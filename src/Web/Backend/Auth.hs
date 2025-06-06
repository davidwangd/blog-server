{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Auth
    ( login
    , register
    , logout
    , verifyUserLevel
    , getUser
    ) where

import Web.Backend.Data
import Web.JWT
import Web.JWT (Signature(..))
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import Web.Backend.Sql
import Safe
import Control.Monad
import Happstack.Server
import Data.List.Split
import Data.Maybe
import Control.Monad.Trans
import Data.Default
import EnvReader

import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.MD5 as MD5
import GHC.Stack

type UserResult = Either String User

queryUserByName :: Text -> IO (Maybe User)
queryUserByName uname = do
    conn <- openDB
    liftM headMay $ (query conn "SELECT * FROM user WHERE username = ?" (Only uname))

encode :: Text -> Text
encode = T.pack . C.unpack . MD5.hash . C.pack . T.unpack

getVerifier :: HasCallStack => IO VerifySigner
getVerifier = do
    v <- readEnv' "JWTSecret" :: IO String
    return . toVerify . hmacSecret . T.pack $ v

getSigner :: HasCallStack => IO EncodeSigner
getSigner = do
    v <- readEnv' "JWTSecret" :: IO String
    return . hmacSecret . T.pack $ v

issuer :: HasCallStack => IO Text
issuer = do
    v <- readEnv' "issuer" :: IO String  
    return . T.pack $ v

getInviteList :: IO [(Text, Int)]
getInviteList = readEnv' "invite_list"

checkLoginInfo :: Text -> Text -> IO UserResult
checkLoginInfo uname pass = do
    let pass' = encode pass
    conn <- openDB
    users <- queryUserByName uname
    return $ case users of
        Nothing -> Left "UserNotExisists"
        Just u  -> if (password u) /= pass' 
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
            if users /= Nothing
                then return $ Left "UserAlreadyExsists"
                else do
                    let pass' = encode pass
                    let cUser = (def { userId = -1, username = uname, password = pass', level = lvl}) :: User
                    insert cUser conn
                    lastId <- lastInsertRowId conn
                    return $ Right $ cUser { userId = fromIntegral lastId }

signJWT :: User -> IO Text
signJWT user = do
    signer <- getSigner
    iss' <- issuer
    let head   = mempty 
        claims = mempty { iss = stringOrURI iss'
                        , sub = stringOrURI $ T.pack $ ((T.unpack $ username user) ++ "|" ++ (show $ userId user))
                        }
    return $ encodeSigned signer head claims

parseJWT :: Text -> IO (Maybe User)
parseJWT jwt = do
    verifier <- getVerifier
    conn <- openDB
    iss' <- issuer
    let parsedJwt = fmap claims $ decodeAndVerifySignature verifier jwt
    -- putStrLn $ (show jwt) ++ " | " ++ (show parsedJwt)
    case parsedJwt of
        Nothing -> return Nothing
        Just claims' -> do
            let eles = splitOn "|" (T.unpack $ fromMaybe "" $ fmap stringOrURIToText $ sub claims')
                uname = T.pack $ head eles
                uid = fromMaybe (-100) $ ((readMay $ last eles) :: Maybe Int)
            res <- queryById uid conn
            if iss claims' /= stringOrURI iss' 
                then return Nothing
                else case res of
                    Nothing -> return Nothing
                    Just user -> if (userId user) == uid && (username user) == uname then return res else return Nothing


userCookieInfo :: String
userCookieInfo = "userStatus"

getUser :: ServerPart (Maybe User)
getUser = msum 
    [ getUser'
    , return Nothing
    ]

getUser' :: ServerPart (Maybe User)
getUser' = do
    jwt <- lookCookieValue userCookieInfo
    user <- lift $ parseJWT $ T.pack jwt 
    -- lift $ putStrLn $ "user = " ++ show user
    return user

verifyUserLevel :: [ServerPart a] -> ServerPart a
verifyUserLevel res = do
    user <- getUser
    case user of
        Nothing -> head res
        Just user -> res !! ((min (length res - 1) (level user)))

instance Default BodyPolicy where
    def = defaultBodyPolicy "." 0 10000 10000

register :: ServerPart UserResult
register = do
    method POST
    decodeBody def 
    uname <- liftM LT.toStrict $ lookText "username"
    pass <- liftM LT.toStrict $ lookText "password"
    code <- liftM LT.toStrict $ lookText "invitecode"
    ures <- lift $ registerUser uname pass code
    case ures of
        Left err -> return $ Left err
        Right usr -> do
            jwt <- lift $ signJWT usr
            addCookie Session $ mkCookie userCookieInfo (T.unpack jwt)
            return $ Right usr

login :: ServerPart UserResult
login = do
    method POST
    decodeBody def
    uname <- liftM LT.toStrict $ lookText "username"
    pass <- liftM LT.toStrict $ lookText "password"
    user <- lift $ checkLoginInfo uname pass
    case user of
        Left err -> return $ Left err
        Right usr -> do
            jwt <- lift $ signJWT usr
            addCookie Session $ mkCookie userCookieInfo (T.unpack jwt)
            return $ Right usr

logout :: ServerPart ()
logout = do
    method GET
    expireCookie userCookieInfo