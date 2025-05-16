{-# LANGUAGE OverloadedStrings #-}

module Web.Frontend.Template 
    ( addHeadTitle 
    , markdownWrapper
    , renderMarkdown
    , renderMarkdown'
    , addIcon
    )
where

import Text.Blaze.Html5 (html, (!), a, form, input, p, toHtml, label, Html)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text
import Text.Pandoc

addIcon :: Html
addIcon = H.link ! A.rel "icon" ! A.type_ "image/ico" ! A.href "/sources/fav.ico"

addHeadTitle :: Text -> Html -> Html
addHeadTitle title body = H.html $ do
    H.head $ do
        H.title (toHtml title)
        H.meta ! A.charset "utf-8"
        H.meta ! A.rel "stylesheet" ! A.href "/styles/github-markdown.css"
        H.style ! A.type_ "text/css" $ toHtml $
            "body {\n"++
            "    box-sizing: border-box;\n"++
            "    min-width: 200px;\n"++
            "    max-width: 980px;\n"++
            "    margin: 0 auto;\n"++
            "    padding: 45px;\n"++
            "}\n"++

            "@media (prefers-color-scheme: dark) {\n"++
            "    body {\n"++
            "        background-color: #0d1117;\n"++
            "    }\n"++
            "}\n"
        addIcon
        H.script $ "MathJax = { tex: { inlineMath: [['\\(', '\\)'], ['$', '$']]  } };"
        H.script ! A.src "/sources/mathjax_settings.js" $ ""
        H.script ! A.type_ "text/javascript" ! A.async "" ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" $ ""
        -- H.script ! A.src "/sources/mathjax.js" $ ""
    H.body $ do
        body

markdownWrapper :: Html -> Html
markdownWrapper = H.article ! A.class_ "markdown-body"

markdown2htmlM :: (PandocMonad m) => Text -> m Html
markdown2htmlM md = readMarkdown readerConfig md >>= writeHtml5 writerConfig
    where readerConfig = def { readerExtensions = githubMarkdownExtensions }
          writerConfig = def { writerHTMLMathMethod = MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
                             , writerExtensions = getDefaultExtensions "html" }

renderMarkdown' :: Text -> Html
renderMarkdown' md = case res of
        Left err -> H.p $ "error when parsing markdown with"
        Right ret -> markdownWrapper ret
    where res = runPure $ markdown2htmlM md

renderMarkdown :: Text -> IO Html
renderMarkdown md = do
    res <- runIO $ markdown2htmlM md
    ret <- handleError res
    return ret