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

  -- build up tags
  tags <- buildTags "articles/*" (fromCapture "tags/*.html")
  let baseCtx = tagCloudField "tagcloud" 80.0 200.0 tags
              `mappend` defaultContext

  -- important pages like index, about and so on
  match "pages/*" $ do
    route $ gsubRoute "pages/" (const "")
          `composeRoutes` setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" baseCtx
        >>= relativizeUrls

  -- articles
  let articleCtx = dateField "date" "%Y-%m-%d"
                 `mappend` tagsField "tags" tags
                 `mappend` baseCtx
  match "articles/*" $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions mathWriterOptions
      >>= loadAndApplyTemplate "templates/article.html" articleCtx
      >>= saveSnapshot "content"
      -- >>= loadAndApplyTemplate "templates/duoshuo.html" articleCtx
      >>= loadAndApplyTemplate "templates/isso.html" articleCtx
      >>= loadAndApplyTemplate "templates/default.html" articleCtx
      >>= relativizeUrls

  -- make list of articles for each tag
  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      articles <- loadAll pattern >>= recentFirst
      let ctx = constField "title" ("标签为“" ++ tag ++ "”的文章")
                `mappend` listField "articles" articleCtx (return articles)
                `mappend` baseCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- make a list for articles
  create ["articles.html"] $ do
    route idRoute
    compile $ do
      articles <- loadAll "articles/*" >>= recentFirst
      let articlesCtx = listField "articles" articleCtx (return articles)
                      `mappend` constField "title" "文章"
                      `mappend` baseCtx
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

