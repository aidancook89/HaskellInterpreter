module Combinators where

-- This file contains some standard parser combinators.
-- Patricia Johann
-- 18 February 2014
(parser1 <|> parser2) s =
   let parser2IfNothing Nothing = parser2 s
       parser2IfNothing x       = x
   in
     parser2IfNothing (parser1 s)

(parser `modify` f) s =
   let modResult Nothing      = Nothing
       modResult (Just (x,y)) = Just (f x,y)
   in
     modResult (parser s)

(parser1 <&> parser2) s =
   let parser2After Nothing      = Nothing
       parser2After (Just (x,s)) = (parser2 `modify` (\y -> (x,y))) s
   in
     parser2After (parser1 s)

emptyseq s = Just ([],s)

optional pr = (pr `modify` (consonto [])) <|> emptyseq
               where consonto [] x = [x]


