{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Map (Map)
import qualified Data.Map as M
import Test.Hspec
import Text.Trifecta
import Text.RawString.QQ
import LogFileParser

testLog :: String
testLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

parsedTestLog :: Log
parsedTestLog =
  Log $
  M.fromList
    [ ( "2025-02-05"
      , [ Activity (Time 8 0) "Breakfast"
        , Activity (Time 9 0) "Sanitizing moisture collector"
        , Activity (Time 11 0) "Exercising in high-grav gym"
        , Activity (Time 12 0) "Lunch"
        , Activity (Time 13 0) "Programming"
        , Activity (Time 17 0) "Commuting home in rover"
        , Activity (Time 17 30) "R&R"
        , Activity (Time 19 0) "Dinner"
        , Activity (Time 21 0) "Shower"
        , Activity (Time 21 15) "Read"
        , Activity (Time 22 0) "Sleep"
        ])
    , ( "2025-02-07"
      , [ Activity (Time 8 0) "Breakfast"
        , Activity (Time 9 0) "Bumped head, passed out"
        , Activity (Time 13 36) "Wake up, headache"
        , Activity (Time 13 37) "Go to medbay"
        , Activity (Time 13 40) "Patch self up"
        , Activity (Time 13 45) "Commute home for rest"
        , Activity (Time 14 15) "Read"
        , Activity (Time 21 0) "Dinner"
        , Activity (Time 21 15) "Read"
        , Activity (Time 22 0) "Sleep"
        ])
    ]

singleDay :: String
singleDay = [r|# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

parsedSingleDay :: (String, [Activity])
parsedSingleDay =
  ( "2025-02-05"
  , [ Activity (Time 8 0) "Breakfast"
    , Activity (Time 9 0) "Sanitizing moisture collector"
    , Activity (Time 11 0) "Exercising in high-grav gym"
    , Activity (Time 12 0) "Lunch"
    , Activity (Time 13 0) "Programming"
    , Activity (Time 17 0) "Commuting home in rover"
    , Activity (Time 17 30) "R&R"
    , Activity (Time 19 0) "Dinner"
    , Activity (Time 21 0) "Shower"
    , Activity (Time 21 15) "Read"
    , Activity (Time 22 0) "Sleep"
    ])

expectedTimeByActivity :: Map String Time
expectedTimeByActivity =
  M.fromList
     [ ("Breakfast", Time 2 0)
     , ("Bumped head, passed out", Time 4 36)
     , ("Commute home for rest", Time 1 (-30))
     , ("Commuting home in rover", Time 0 30)
     , ("Dinner", Time 2 15)
     , ("Exercising in high-grav gym", Time 1 0)
     , ("Go to medbay", Time 0 3)
     , ("Lunch", Time 1 0)
     , ("Patch self up", Time 0 5)
     , ("Programming", Time 4 0)
     , ("R&R", Time 2 (-30))
     , ("Read", Time 2 (-30))
     , ("Sanitizing moisture collector", Time 2 0)
     , ("Shower", Time 0 15)
     , ("Wake up, headache", Time 0 1)
     ]

expectedTimePerDayByActivity :: Map String (Map String Time)
expectedTimePerDayByActivity =
  M.fromList
    [ ( "2025-02-05"
      , M.fromList
          [ ("Breakfast", Time 1 0)
          , ("Commuting home in rover", Time 0 30)
          , ("Dinner", Time 2 0)
          , ("Exercising in high-grav gym", Time 1 0)
          , ("Lunch", Time 1 0)
          , ("Programming", Time 4 0)
          , ("R&R", Time 2 (-30))
          , ("Read", Time 1 (-15))
          , ("Sanitizing moisture collector", Time 2 0)
          , ("Shower", Time 0 15)
          ])
    , ( "2025-02-07"
      , M.fromList
          [ ("Breakfast", Time 1 0)
          , ("Bumped head, passed out", Time 4 36)
          , ("Commute home for rest", Time 1 (-30))
          , ("Dinner", Time 0 15)
          , ("Go to medbay", Time 0 3)
          , ("Patch self up", Time 0 5)
          , ("Read", Time 1 (-15))
          , ("Wake up, headache", Time 0 1)
          ])
    ]

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let ps = parseString
  describe "comment parser" $ do
    let pspc = ps parseComment mempty
    it "skips lone comment" $ do
      let result = pspc "-- dates not nececessarily sequential"
      maybeSuccess result `shouldBe` Just ()
  describe "date parser" $ do
    let pspd = ps parseDate mempty
    it "parses simple date" $ do
      let result = pspd "# 2025-02-05"
      maybeSuccess result `shouldBe` Just "2025-02-05"
    it "skips comments after date" $ do
      let result = pspd "# 2025-02-07 -- dates not nececessarily sequential"
      maybeSuccess result `shouldBe` Just "2025-02-07"
  describe "activity parser" $ do
    let pspa = ps parseActivity mempty
    it "parses an activity" $ do
      let result = pspa "08:00 Breakfast"
      maybeSuccess result `shouldBe` Just (Activity (Time 8 0) "Breakfast")
    it "parses an activity ignoring comment" $ do
      let result = pspa "08:00 Breakfast -- good"
      maybeSuccess result `shouldBe` Just (Activity (Time 8 0) "Breakfast")
    it "parses an activity with dash in the middle" $ do
      let result = pspa "08:00 Breakfast - good"
      maybeSuccess result `shouldBe` Just (Activity (Time 8 0) "Breakfast - good")
    it "parses an activity with a dash and a comment" $ do
      let result = pspa "08:00 Breakfast - good -- best so far"
      maybeSuccess result `shouldBe` Just (Activity (Time 8 0) "Breakfast - good")
  describe "day parser" $ do
    let pspd = ps parseDay mempty
    it "parses a day" $ do
      let result = pspd singleDay
      maybeSuccess result `shouldBe` Just parsedSingleDay
  describe "log parser" $ do
    let pspl = ps parseLog mempty
    it "parses a multi day log" $ do
      let result = pspl testLog
      maybeSuccess result `shouldBe` Just parsedTestLog
  describe "time by activity" $ do
    let pspl = ps parseLog mempty
    it "parses a multi day log" $ do
      let result = timeByActivity <$> pspl testLog
      maybeSuccess result `shouldBe` Just expectedTimeByActivity
  describe "time per day by activity" $ do
    let pspl = ps parseLog mempty
    it "parses a multi day log" $ do
      let result = timePerDayByActivity <$> pspl testLog
      maybeSuccess result `shouldBe` Just expectedTimePerDayByActivity
