{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do

  let postCtx
        = dateField "date" "%Y-%m-%d"
        `mappend` defaultContext
      archiveCtx posts
        = listField "posts" postCtx (return posts)
        `mappend` constField "title" "存档"
        `mappend` defaultContext

  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  match "templates/*" $ do
    compile templateCompiler

  match "pages/*" $ do
    route $ gsubRoute "pages/" (const "")
          `composeRoutes` setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archivePostsCtx = archiveCtx posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archivePostsCtx
        >>= loadAndApplyTemplate "templates/default.html" archivePostsCtx
        >>= relativizeUrls

