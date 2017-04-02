{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import qualified Data.Set as S
import Text.Pandoc.Options

feed :: FeedConfiguration
feed = FeedConfiguration
  { feedTitle       = "Chaoya Li"
  , feedDescription = "在创造中发现快乐"
  , feedAuthorName  = "Chaoya Li"
  , feedAuthorEmail = "chaoya@chaoya.info"
  , feedRoot        = "http://www.chaoya.info"
  }

mathWriterOptions =
  let defaultExtensions = writerExtensions defaultHakyllWriterOptions
      mathExtensions    = foldr S.insert defaultExtensions
        [ Ext_tex_math_dollars
        , Ext_latex_macros
        ]
   in defaultHakyllWriterOptions
        { writerExtensions = mathExtensions
        , writerHTMLMathMethod = MathJax ""
        }

main :: IO ()
main = hakyll $ do

  -- templates
  match "templates/*" $ do
    compile templateCompiler

  -- static files
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  -- important pages like index, about and so on
  match "pages/*" $ do
    route $ gsubRoute "pages/" (const "")
          `composeRoutes` setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- articles
  let articleCtx = dateField "date" "%Y-%m-%d"
                 `mappend` defaultContext
  match "articles/*" $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions mathWriterOptions
      >>= loadAndApplyTemplate "templates/article.html" articleCtx
      >>= saveSnapshot "content"
      -- >>= loadAndApplyTemplate "templates/duoshuo.html" articleCtx
      >>= loadAndApplyTemplate "templates/isso.html" articleCtx
      >>= loadAndApplyTemplate "templates/default.html" articleCtx
      >>= relativizeUrls

  -- make a list for articles
  create ["articles.html"] $ do
    route idRoute
    compile $ do
      articles <- loadAll "articles/*" >>= recentFirst
      let articlesCtx = listField "articles" articleCtx (return articles)
                      `mappend` constField "title" "文章"
                      `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/articles.html" articlesCtx
        >>= loadAndApplyTemplate "templates/default.html" articlesCtx
        >>= relativizeUrls

  -- make feed
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      articles <- loadAllSnapshots "articles/*" "content" >>= fmap (take 10) . recentFirst
      renderAtom feed (articleCtx `mappend` bodyField "description") articles

