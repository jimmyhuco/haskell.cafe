--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do

    -----------------------------------------------------------------------
    -- copy resources files and compress css
    -----------------------------------------------------------------------
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler



    -----------------------------------------------------------------------
    -- build tags
    -----------------------------------------------------------------------
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "[" ++ tag ++ "]"
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots pattern "content"

        let ctx =
              constField "title" title `mappend`
              listField "posts" (postCtx `mappend` (teaserField "teaser" "content")) (return posts) `mappend`
              defaultContext
        makeItem "" >>=
          loadAndApplyTemplate "templates/tag.html" ctx >>=
          loadAndApplyTemplate "templates/default.html" ctx >>=
          relativizeUrls



    -----------------------------------------------------------------------
    -- build posts and save pure post content into snapshot
    -----------------------------------------------------------------------
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags) >>=
        loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags) >>=
        relativizeUrls



    -----------------------------------------------------------------------
    -- build homepage
    -----------------------------------------------------------------------
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
        let indexCtx =
              listField "postFirst" (postCtxWithTags tags) (return $ take 1 posts) `mappend`
              listField "postRest" postCtx (return $ tail posts) `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls



    -----------------------------------------------------------------------
    -- build archive page
    -----------------------------------------------------------------------
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        let archiveCtx =
              listField "postRest" postCtx (return posts) `mappend`
              constField "title" "Archives" `mappend`
              defaultContext
        makeItem "" >>=
          loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls



    -----------------------------------------------------------------------
    -- build about page
    -----------------------------------------------------------------------
    match (fromList ["about.org"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/post.html" defaultContext >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls



    -----------------------------------------------------------------------
    -- compile all templates in templates directory
    -----------------------------------------------------------------------
    match "templates/*" $ compile templateBodyCompiler
--------------------------------------------------------------------------------



postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
