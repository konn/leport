{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax                                    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Home where
import Fay.FFI
import qualified Fay.Text as T
import Fay.Text
import Fay.Yesod
import Prelude
import SharedTypes
#ifndef FAY
import Control.Monad (when)
import Control.Monad (void)
import Fay.Compiler.Prelude (unless)
import JQuery hiding (append)
#else
import Fay.JQuery
#endif

default (Text)

put :: Text -> Fay ()
put = ffi "console.log(%1)"

offsetTop :: JQuery -> Fay Double
offsetTop = ffi "%1.offset().top"

scrollTo :: Double -> Text -> JQuery -> Fay JQuery
scrollTo = ffi "%3.animate({scrollTop: %1}, %2)"

type Key = Int

#ifndef FAY
keyCode :: Event -> Key
keyCode = ffi "%1.keyCode"
#endif

main :: Fay ()
main = do
  prompt <- setText "Started." =<< select "#log"
  cmd <- select "#command"
  let newl = void $ do
        void $ appendToJQuery prompt =<< select "<br>"
        bot <- offsetTop cmd
        scrollTo bot "fast" =<< select "body"
      input txt = do
        void $ appendToJQuery prompt =<< setAttr "style" "color: gray"
                        =<< setText "ghci> " =<< select "<span>"
        void $ appendToJQuery prompt =<< setText txt =<< select "<span>"
        newl
  newl
  flip keypress cmd $ \ev -> when (keyCode ev == 13) $ do
    v <- getVal cmd
    put v
    unless (T.null v) $ do
      input v
      void $ setVal "" cmd
      call (RunReport v) $ \case
        Success ans -> do
          void $ appendToJQuery prompt
            =<< setAttr "style" "color: black"
            =<< setText ans
            =<< select "<span .success>"
          newl
        Failure ws -> do
          void $ appendToJQuery prompt
            =<< setAttr "style" "color: red"
            =<< setText (T.unlines ws)
            =<< select "<span .failure>"
          newl
