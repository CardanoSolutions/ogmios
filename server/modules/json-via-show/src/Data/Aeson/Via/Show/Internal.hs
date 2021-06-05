--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Aeson.Via.Show.Internal
    ( -- * Data-Types
      Expr (..)

    -- * Logic
    , parseExpr
    , exprToJSON
    ) where

import Prelude

import Control.Arrow
    ( first )
import Data.Aeson
    ( ToJSON (..), Value (..), object, (.=) )
import Data.Char
    ( isUpper )
import Data.Text
    ( Text )

import qualified Data.Text as T

--
-- Data-Types
--

data Expr
    = Record Text [(Text, Expr)]
    | Product Text [Expr]
    | List [Expr]
    | Val Text
    deriving (Show, Eq)

exprToJSON :: Expr -> Value
exprToJSON = \case
    Val s ->
        String s
    List xs ->
        toJSON (exprToJSON <$> xs)
    Record c [(_, x)] ->
        object [c .= exprToJSON x]
    Record c xs ->
        object [c .= object (keyValToJSON <$> xs)]
    Product "True" [] ->
        Bool True
    Product "False" [] ->
        Bool False
    Product "Nothing" [] ->
        Null
    Product "Just" [x] ->
        exprToJSON x
    Product c [Val d, Val t, Product "UTC" []] ->
        exprToJSON $ Product c [Val (d <> " " <> t <> " UTC")]
    Product "Container" [List xs] ->
        case tryZip xs of
            Nothing -> exprToJSON (List xs)
            Just keyVal -> object (keyValToJSON <$> keyVal)
    Product c [x] ->
        object [c .= exprToJSON x]
    Product c xs ->
        object [c .= (exprToJSON <$> xs)]
  where
    keyValToJSON = \case
        (k, Record c xs) ->
            case xs of
            []       -> k .= c
            [(_, x)] -> k .= exprToJSON x
            _        -> k .= object (keyValToJSON <$> xs)
        (k, Product c xs) | c `notElem` ["True", "False", "Nothing"] ->
            case xs of
            []  -> k .= c
            [x] -> k .= exprToJSON x
            _   -> k .= (exprToJSON <$> xs)
        (k, v) ->
            k .= exprToJSON v

    tryZip = \case
        [] -> Just []
        (List [Val k,v]:q) -> let xs = tryZip q in ((k,v):)<$>xs
        _ -> Nothing

parseExpr :: Bool -> String -> (Expr, String)
parseExpr spaceAreSep = \case
    c:q | isUpper c ->
        let (cons, q') = parseCons (c:q) in parseProductOrRecord cons q'
    'f':'r':'o':'m':'L':'i':'s':'t':q ->
        parseProductOrRecord "Container" q
    '(':')':q ->
        (List [], q)
    '(':q ->
        case parseMany (Just ')') ',' (parseExpr False) q of
            ([e], q') -> (e, q')
            (es, q') -> (List es, q')
    '[':']':q ->
        (List [], q)
    '[':q ->
        first List (parseMany (Just ']') ',' (parseExpr False) q)
    str ->
        first Val (parseVal spaceAreSep str)

parseCons :: String -> (Text, String)
parseCons = first T.pack . go
  where
    go = \case
        [] -> ("", "")
        c:q | c `elem` ("[]{}(), " :: String) -> ("", c:q)
        c:q -> let (cs, q') = go q in (c:cs, q')

parseProductOrRecord :: Text -> String -> (Expr, String)
parseProductOrRecord cons = \case
    _:'{':q ->
        first (Record cons) (parseMany (Just '}') ',' parseKeyVal q)
    ' ':q ->
        first (Product cons) (parseMany Nothing ' ' (parseExpr True) q)
    q ->
        (Product cons [], q)

parseVal :: Bool -> String -> (Text, String)
parseVal spaceAreSep = first T.pack . go
  where
    go = \case
        [] -> ("", "")
        c:q | c `elem` ("}])," :: String) -> ("", c:q)
        ' ':q | spaceAreSep -> ("", ' ':q)
        c:q ->
            let (cs, q') = go q in
            if c `elem` ("\"'" :: String) then (cs, q') else (c:cs, q')

parseKeyVal :: String -> ((Text, Expr), String)
parseKeyVal s =
    let (k, q) = parseKey s in first (k,) (parseExpr False q)
  where
    parseKey :: String -> (Text, String)
    parseKey = first T.pack . go
      where
        go = \case
            [] -> ("", "") -- Absurd
            ' ':'=':' ':q -> ("", q)
            c:q -> let (cs, q') = go q in (c:cs, q')

parseMany :: Maybe Char -> Char -> (String -> (expr, String)) -> String -> ([expr], String)
parseMany end delim parseElem = \case
    [] ->
        ([], "")
    s ->
        case parseElem s of
            (e, "") ->
                ([e], "")
            (e, c:q) | Just c == end ->
                ([e], q)
            (e, c:' ':q) | c == delim ->
                let (es, q') = parseMany end delim parseElem q in (e:es, q')
            (e, c:q) | c == delim ->
                let (es, q') = parseMany end delim parseElem q in (e:es, q')
            (e, q) ->
                ([e], q)
