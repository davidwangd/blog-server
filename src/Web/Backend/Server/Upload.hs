{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Backend.Server.Upload
    ( handleUpload
    ) where

import Happstack.Server
import Data.List.Split
import System.FilePath.Posix
import Control.Monad.Trans (lift)
import Control.Monad
import System.Process

saveFile :: FilePath -> IO FilePath
saveFile tmpFile = do
    md5 <- liftM (take 12 . head . splitOn " ") $ readProcess "md5sum" [tmpFile] []
    let newFile = "public/uploads/" ++ md5 ++ (takeExtension tmpFile)
    readProcess "mkdir" ["-p", takeDirectory newFile] []
    readProcess "mv" [tmpFile, newFile] []
    return $ newFile

showFile :: (FilePath, String, ContentType) -> IO ()
showFile (tmpFile, originName, mimetype) = do
    putStrLn $ "Temporary file: " ++ tmpFile
    putStrLn $ "Original name: " ++ originName
    putStrLn $ "File type: " ++ show mimetype

renderObject :: FilePath -> FilePath -> ContentType -> String
renderObject url name mimetype =
    case ctType mimetype of
        "image" -> "![" ++ name ++ "](" ++ url ++ ")"
        "video" -> "<video controls><source src=\"" ++ url ++ "\" type=\"video/" ++ ctSubtype mimetype ++ "\"></video>"
        "audio" -> "<audio controls><source src=\"" ++ url ++ "\" type=\"audio/" ++ ctSubtype mimetype ++ "\"></audio>"
        _       -> "[" ++ name ++ "](" ++ url ++ ")"

handleUpload :: ServerPart Response
handleUpload = do
    method POST
    decodeBody (defaultBodyPolicy "/tmp/" 1000000000 10000000 10000000)
    (tmpFile, originName, mimetype) <- lookFile "file"
    -- lift $ showFile (tmpFile, originName, mimetype)
    newFile <- lift $ saveFile tmpFile
    ok $ toResponse $ renderObject (drop 6 newFile) originName mimetype
