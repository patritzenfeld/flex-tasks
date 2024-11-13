{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module FlexTask.ContentSpec where


import Test.Hspec (
  Spec,
  describe,
  it,
  )
import Text.Cassius                     (Css, cassius)
import Text.Julius                      (JavascriptUrl, julius)

import FlexTask.Content
import FlexTask.FormUtil
import FlexTask.Generic.Form

import FlexTask.TestUtil                (shouldReturnSame)



spec :: Spec
spec = do
  let form = formify
        (Nothing @(Int,[String]))
        [[single "test",list Vertical ["test2","test3"]]]
  describe "addCss and addJs" $
    it "are order invariant" $
      getFormData (addJs testJs $ addCss testCss form)
      `shouldReturnSame`
      getFormData (addCss testCss $ addJs testJs form)
  describe "addCssAndJs" $
    it "should behave the same as applying CSS and JS one after the other" $
      getFormData (addCssAndJs testCss testJs form)
      `shouldReturnSame`
      getFormData (addCss testCss $ addJs testJs form)


testCss :: render -> Css
testCss = [cassius|
  p
    text-align: center
    color: red
|]


testJs :: JavascriptUrl url
testJs = [julius|
  myFunction(){
    console.log("test");
  }
|]
