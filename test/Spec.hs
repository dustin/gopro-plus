{-# LANGUAGE QuasiQuotes #-}

import           Control.Lens
import qualified Data.Aeson            as J
import qualified Data.ByteString.Lazy  as BL
import           Text.RawString.QQ     (r)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           GoPro.Plus.Media

exampleMedia :: BL.ByteString
exampleMedia = [r|
{"_pages":{"current_page":1,"per_page":100,"total_items":2169,"total_pages":22},
"_embedded":{"errors":[],"media":[
{"captured_at":"2020-04-06T15:51:32Z","content_title":"GOPR1991.MP4","content_type":null,"created_at":"2020-04-07T03:35:27Z","gopro_user_id":"uid","file_size":412808724,"height":1080,"fov":null,"id":"62NzWRrpPXm7o","moments_count":0,"on_public_profile":false,"orientation":1,"play_as":"video","ready_to_edit":true,"ready_to_view":"ready","resolution":"1080p","source_duration":"109710","token":"token1","type":"Video","width":1920},
{"captured_at":"2020-04-06T00:33:22Z","content_title":null,"content_type":null,"created_at":"2020-04-06T01:39:39Z","gopro_user_id":"uid","file_size":7930021,"height":3000,"fov":null,"id":"EDavpqr4GVe8O","moments_count":0,"on_public_profile":false,"orientation":1,"play_as":"photo","ready_to_edit":true,"ready_to_view":"ready","resolution":"12000000","source_duration":"0","token":"token2","type":"Photo","width":4000}]}}
|]

testSearchParser :: Assertion
testSearchParser = do
  let l = J.decode exampleMedia :: Maybe Listing
  assertEqual (show l) (Just (PageInfo 1 100 2169 22)) (l ^? _Just . pages)
  assertEqual (show l) [Video, Photo] (l ^.. _Just . media . folded . medium_type)
  assertEqual (show l) ["62NzWRrpPXm7o", "EDavpqr4GVe8O"] (l ^.. _Just . media . folded . medium_id)

tests :: [TestTree]
tests = [
    testCase "Parsing" testSearchParser
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
