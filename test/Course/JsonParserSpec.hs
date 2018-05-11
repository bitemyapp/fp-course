{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.JsonParserSpec where

import           Data.Ratio        ((%))
import           Test.Hspec               (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.JsonParser (jsonArray, jsonFalse, jsonNull, jsonNumber,
                                    jsonObject, jsonString, jsonTrue, jsonValue)
import           Course.JsonValue  (JsonValue (..))
import           Course.List       (List (..))
import           Course.Parser     (ParseResult (..), isErrorResult, parse)

spec :: Spec
spec = do
  describe "jsonString" $ do
    it "parse whole ASCII input" $
      parse jsonString "\" abc\"" `shouldBe` Result "" " abc"
    it "parse only the first string of input" $
      parse jsonString "\"abc\"def" `shouldBe` Result "def" "abc"
    it "parse back slash (\\)" $
      parse jsonString "\"\\babc\"def" `shouldBe` Result "def" "\babc"
    it "parse unicode (\\u00abc)" $
      parse jsonString "\"\\u00abc\"def" `shouldBe` Result "def" "«c"
    it "parse unicode (\\u00ff)" $
      parse jsonString "\"\\u00ffabc\"def" `shouldBe` Result "def" "ÿabc"
    it "parse unicode (\\u00fa)" $
      parse jsonString "\"\\u00faabc\"def" `shouldBe` Result "def" "úabc"
    it "parsing string without quotes is an error" $
      isErrorResult (parse jsonString "abc") `shouldBe` True
    it "parsing string containing \\a is an error - \\a isn't a special character" $
      isErrorResult (parse jsonString "\"\\abc\"def") `shouldBe` True

  describe "jsonNumber" $ do
    it "positive whole" $ parse jsonNumber "234" `shouldBe` Result "" (234 % 1)
    it "negative whole" $ parse jsonNumber "-234" `shouldBe` Result "" ((-234) % 1)
    it "positive decimal" $ parse jsonNumber "123.45" `shouldBe` Result "" (2469 % 20)
    it "negative whole (2)" $ parse jsonNumber "-123" `shouldBe` Result "" ((-123) % 1)
    it "negative decimal" $ parse jsonNumber "-123.45" `shouldBe` Result "" ((-2469) % 20)
    it "negative sign on its own is error" $ isErrorResult (parse jsonNumber "-") `shouldBe` True
    it "alphabetic characters is error" $ isErrorResult (parse jsonNumber "abc") `shouldBe` True

  describe "jsonTrue" $ do
    it "parses true" $ parse jsonTrue "true" `shouldBe` Result "" "true"
    it "TRUE (caps) is an error" $ isErrorResult (parse jsonTrue "TRUE") `shouldBe` True

  describe "jsonFalse" $ do
    it "parses false" $ parse jsonFalse "false" `shouldBe` Result "" "false"
    it "FALSE (caps) is an error" $ isErrorResult (parse jsonFalse "FALSE") `shouldBe` True

  describe "jsonNull" $ do
    it "parses null" $ parse jsonNull "null" `shouldBe` Result "" "null"
    it "NULL (caps) is an error" $ isErrorResult (parse jsonNull "NULL") `shouldBe` True

  describe "jsonArray" $ do
    it "[]" $
      parse jsonArray "[]"
        `shouldBe`
          Result "" Nil
    it "[true]" $
      parse jsonArray "[true]"
        `shouldBe`
          Result "" (JsonTrue :. Nil)
    it "[true, \"abc\"]" $
      parse jsonArray "[true, \"abc\"]"
        `shouldBe`
          Result "" (JsonTrue :. JsonString "abc" :. Nil)
    it "[true, \"abc\", []]" $
      parse jsonArray "[true, \"abc\", []]"
        `shouldBe`
          Result "" (JsonTrue :. JsonString "abc" :. JsonArray Nil :. Nil)
    it "[true, \"abc\", [false]]" $ do
      let
        result =
          Result ""
            (  JsonTrue
            :. JsonString "abc"
            :. JsonArray (JsonFalse :. Nil)
            :. Nil
            )
      parse jsonArray "[true, \"abc\", [false]]" `shouldBe` result

  describe "jsonObject" $ do
    it "empty" $
      parse jsonObject "{}"
        `shouldBe`
          Result "" Nil
    it "one key" $
      parse jsonObject "{ \"key1\" : true }"
        `shouldBe`
          Result "" (("key1",JsonTrue) :. Nil)
    it "two keys" $
      parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
        `shouldBe`
          Result "" (("key1",JsonTrue):.("key2",JsonFalse):.Nil)
    it "two keys and left over input" $ do
      let
        result =
          Result "xyz" (("key1",JsonTrue):.("key2",JsonFalse):.Nil)
      parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz" `shouldBe` result

  describe "jsonValue" $ do
    it "true" $
      parse jsonValue "true"
        `shouldBe`
          Result "" JsonTrue
    it "object" $ do
      let
        result =
          Result ""
            (  ("key1",JsonTrue)
            :. ("key2",JsonArray (JsonRational (7 % 1) :. JsonFalse:.Nil))
            :. Nil
            )
      parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
        `shouldBe`
          result
    it "nested object" $ do
      let
        result =
          Result ""
            (  ("key1",JsonTrue)
            :. ("key2",JsonArray (JsonRational (7 % 1) :. JsonFalse :. Nil))
            :. ("key3",JsonObject (("key4",JsonNull) :. Nil))
            :. Nil
            )
        input =
          "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
      parse jsonObject input `shouldBe` result
