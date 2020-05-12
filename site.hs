--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List (isSuffixOf)
import           Data.Monoid (mappend)
import           System.FilePath ((</>), takeDirectory, takeBaseName)
import           Hakyll

--------------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute $ \ident ->
    let fp = toFilePath ident in
    takeDirectory fp </> takeBaseName fp </> "index.html"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll "/index.html" (const "/"))


configuration :: IO Configuration
configuration = do
    dc <- readFile "deployCommand.sh"
    return $ defaultConfiguration {deployCommand=dc}

main :: IO ()
main = do
    configuration <- configuration
    hakyllWith configuration $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "videos/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route   cleanRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/*" $ do
            route $ cleanRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route cleanRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls
                    >>= cleanIndexUrls -- cleanup href in all anchor tags.

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
