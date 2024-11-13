{-# language OverloadedStrings #-}

module FlexTask.FormUtilSpec where


import Test.Hspec (
  Spec,
  anyErrorCall,
  describe,
  it,
  )

import FlexTask.TestUtil                (shouldNotThrow, shouldReturnSame)
import FlexTask.FormUtil
import FlexTask.Generic.Form



data TestEnum = First | Last deriving (Bounded,Enum,Eq)

instance Formify [TestEnum] where
  formifyImplementation = formifyInstanceMultiChoice


spec :: Spec
spec = do
  let
    form1 = formify
              (Nothing @(Int,[String]))
              [[single "test",list Vertical ["test2","test3"]]]
    form2 = formify
              (Nothing @String)
              [[single "form2"]]
    form3 = formify
              (Just [First])
              [[dropdownEnum
                  "form3"
                  (\a -> if a == First then "first" else "last")
              ]]
  describe "getFormData" $
    it "does not throw an error for Formify generated test forms" $ do
      getFormData form1 `shouldNotThrow` anyErrorCall
      getFormData form2 `shouldNotThrow` anyErrorCall
      getFormData form3 `shouldNotThrow` anyErrorCall
  describe "($$>))" $
    it "should be associative" $
        getFormData ((form1 $$> form2) $$> form3)
        `shouldReturnSame`
        getFormData (form1 $$> (form2 $$> form3))
