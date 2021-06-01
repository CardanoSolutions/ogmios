-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Test.Hspec.Json.Schema
    ( unsafeReadSchemaRef
    , prop_validateToJSON
    , SchemaRef(..)
    ) where

import Prelude

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Scientific
    ( Scientific, toRealFloat )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import GHC.Exts
    ( IsList (..) )
import JSONPointer
    ( Index (..), Key (..) )
import JSONSchema.Draft4
    ( Schema (..)
    , SchemaWithURI (..)
    , ValidatorFailure (..)
    , checkSchema
    , emptySchema
    , referencesViaFilesystem
    )
import JSONSchema.Fetch
    ( URISchemaMap )
import JSONSchema.Validator.Draft4.Any
    ( AllOfInvalid (..)
    , AnyOfInvalid (..)
    , EnumInvalid (..)
    , EnumValidator (..)
    , NotValidatorInvalid (..)
    , OneOfInvalid (..)
    , RefInvalid (..)
    , SchemaType (..)
    , TypeValidator (..)
    , TypeValidatorInvalid (..)
    )
import JSONSchema.Validator.Draft4.Array
    ( AdditionalItemsInvalid (..)
    , ItemsInvalid (..)
    , MaxItems (..)
    , MaxItemsInvalid (..)
    , MinItems (..)
    , MinItemsInvalid (..)
    , UniqueItemsInvalid (..)
    )
import JSONSchema.Validator.Draft4.Number
    ( Maximum (..)
    , MaximumInvalid (..)
    , Minimum (..)
    , MinimumInvalid (..)
    , MultipleOf (..)
    , MultipleOfInvalid (..)
    )
import JSONSchema.Validator.Draft4.Object
    ( DependenciesInvalid (..)
    , MaxProperties (..)
    , MaxPropertiesInvalid (..)
    , MinProperties (..)
    , MinPropertiesInvalid (..)
    , PropertiesRelatedInvalid (..)
    , Regex (..)
    , Required (..)
    , RequiredInvalid (..)
    )
import JSONSchema.Validator.Draft4.String
    ( MaxLength (..)
    , MaxLengthInvalid (..)
    , MinLength (..)
    , MinLengthInvalid (..)
    , PatternInvalid (..)
    , PatternValidator (..)
    )
import Test.QuickCheck
    ( Property, counterexample )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Text.PrettyPrint.ANSI.Leijen hiding
    ( (<$>) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

-- | A newtype for capturing schema reference. Can be a reference to a file.
newtype SchemaRef = SchemaRef
    { getSchemaRef :: Text
    } deriving (Show, IsString)

unsafeReadSchemaRef
    :: SchemaRef
    -> IO (URISchemaMap Schema, SchemaWithURI Schema)
unsafeReadSchemaRef (SchemaRef ref) = do
    let schema = SchemaWithURI (emptySchema { _schemaRef = Just ref }) Nothing
    refs <- unsafeEither =<< referencesViaFilesystem schema
    pure (refs, schema)

-- | Actual property that a given value a should satisfy.
prop_validateToJSON
    :: (a -> Json.Value)
    -> (URISchemaMap Schema, SchemaWithURI Schema)
    -> a
    -> Property
prop_validateToJSON encode refs a = monadicIO $ do
    let json = encode a
    errors <- run $ validateSchema refs json
    monitor $ counterexample $ unlines
        [ "JSON:", BL8.unpack $ Json.encodePretty json ]
    monitor $ counterexample $ unlines
        (show . prettyValidatorFailure <$> errors)
    assert (null errors)

-- | Schema validator, reading reference from the file-system.
validateSchema
    :: (URISchemaMap Schema, SchemaWithURI Schema)
    -> Json.Value
    -> IO [ValidatorFailure]
validateSchema (refs, schema) value = do
    validate <- unsafeEither (checkSchema refs schema)
    pure $ validate value

--
-- Pretty Print Failures
--

prettyValidatorFailure
    :: ValidatorFailure
    -> Doc
prettyValidatorFailure = \case
    FailureRef refInvalid ->
        prettyRefInvalid refInvalid

    FailurePropertiesRelated prop ->
        prettyPropertiesRelatedInvalid prop

    FailureRequired requiredInvalid  ->
        prettyRequiredInvalid requiredInvalid

    FailurePattern patternInvalid ->
        prettyPatternInvalid patternInvalid

    FailureItems itemsInvalid ->
        prettyItemsInvalid itemsInvalid

    FailureAdditionalItems additionalItemsInvalid ->
        prettyAdditionalItemsInvalid additionalItemsInvalid

    FailureAllOf allOfInvalid ->
        prettyAllOfInvalid allOfInvalid

    FailureAnyOf anyOfInvalid ->
        prettyAnyOfInvalid anyOfInvalid

    FailureOneOf oneOfInvalid ->
        prettyOneOfInvalid oneOfInvalid

    FailureMultipleOf (MultipleOfInvalid (MultipleOf n) k) ->
        scientific k <+> s "should be a multiple of" <+> integer (round n)

    FailureMinimum (MinimumInvalid (Minimum _exclusive limit) found) ->
        scientific found <+> s "should be greater than" <+> scientific limit

    FailureMaximum (MaximumInvalid (Maximum _exclusive limit) found) ->
        scientific found <+> s "should be lower than" <+> scientific limit

    FailureMinLength (MinLengthInvalid (MinLength len) str) ->
        squotes (t str) <+> s "should have at least" <+> int len <+> s "character(s)"

    FailureMaxLength (MaxLengthInvalid (MaxLength len) str) ->
        squotes (t str) <+> s "should have at most" <+> int len <+> s "character(s)"

    FailureMinItems (MinItemsInvalid (MinItems limit) _items) ->
        s "should have at least" <+> int limit <+> s "item(s)"

    FailureMaxItems (MaxItemsInvalid (MaxItems limit) _items) ->
        s "should have at most" <+> int limit <+> s "item(s)"

    FailureMinProperties (MinPropertiesInvalid (MinProperties limit) _items) ->
        s "should have at least" <+> int limit <+> s "propertie(s)"

    FailureMaxProperties (MaxPropertiesInvalid (MaxProperties limit) _items) ->
        s "should have at most" <+> int limit <+> s "propertie(s)"

    FailureUniqueItems (UniqueItemsInvalid _items) ->
        s "should have only unique items."

    FailureEnum (EnumInvalid (EnumValidator enum) _)  ->
        s "should be one of the following values:"
        <+>
        hcat (punctuate comma (s . BL8.unpack . Json.encode <$> toList enum))

    FailureType (TypeValidatorInvalid typeValidator _) ->
        s "should be of type:" <+> prettyTypeValidator typeValidator

    FailureNot (NotValidatorInvalid schema _) ->
        vsep
        [ s "should *not* match the following schema:"
        , indent 2 $ s $ BL8.unpack $ Json.encodePretty schema
        ]

    FailureDependencies (DependenciesInvalid deps) ->
        s "invalid dependencies:" <+> s (show $ fst <$> toList deps)

prettyRefInvalid
    :: RefInvalid ValidatorFailure
    -> Doc
prettyRefInvalid = \case
    RefResolution ref ->
        s "could not resolve reference (bad reference) at:" <+> t ref
    RefPointerResolution err ->
        s "could not resolve pointer:" <+> s (show err)
    RefLoop ref _ _ ->
        s "could not resolve reference (looping) at:" <+> t ref
    RefInvalid _ _schema errs ->
        case prune (prettyValidatorFailure <$> toList errs) of
            [] -> mempty
            failures -> vsep failures

prettyRequiredInvalid
    :: RequiredInvalid
    -> Doc
prettyRequiredInvalid (RequiredInvalid (Required required) found _)
    | required == found = mempty
    | otherwise = vsep
        [ s "missing required property"
        , indent 4 $ group $ vsep
            [ s "required: " <+> s (show $ toList required)
            , s "found:    " <+> s (show $ toList found)
            ]
        ]

prettyPropertiesRelatedInvalid
    :: PropertiesRelatedInvalid ValidatorFailure
    -> Doc
prettyPropertiesRelatedInvalid = \case
    PropertiesRelatedInvalid props patterns additional ->
        let errs =
                  [ prettyInvalidProperties (toList props) | not (null $ toList props) ]
                  ++
                  [ prettyInvalidPatterns (toList patterns) | not (null $ toList patterns) ]
        in case prune errs of
            [] -> mempty
            failures -> vsep
                [ s "invalid properties in object"
                , indent 4 $ group $ vsep $ failures ++
                    [ prettyInvalidAdditional additional | isJust additional ]
                ]
  where
    prettyInvalidProperties props =
        let invalidProps = [ (prop, errs) | (prop, errs) <- props, not (null errs) ]
        in group $ vsep $ prettyInvalidProperty <$> invalidProps

    prettyInvalidProperty (prop, [err]) =
        case prune [prettyValidatorFailure err] of
            [failure] -> label <+> failure
            _ -> mempty
      where
        label = s "invalid property" <+> squotes (t prop) <> s ":"

    prettyInvalidProperty (prop, errs) =
        case prune (prettyValidatorFailure <$> errs) of
            [] -> mempty
            failures  -> vsep
                ( s "invalid property" <+> squotes (t prop)
                : [indent 4 (group $ vsep failures)]
                )

    prettyInvalidPatterns patterns =
        group $ vsep $ prettyInvalidPattern <$> patterns

    prettyInvalidPattern ((Regex regex, Key key), [err]) =
        label <+> prettyValidatorFailure err
      where
        label = s "invalid regular expression /" <> t regex <> s "/ in" <+> t key <> s ":"
    prettyInvalidPattern ((Regex regex, Key key), errs) =
        case prune (prettyValidatorFailure <$> errs) of
            [] -> mempty
            failures -> vsep
                ( s "invalid regular expression /" <> t regex <> s "/ in" <+> t key <> s ":"
                : [indent 4 (group $ vsep failures)]
                )

    prettyInvalidAdditional = \case
        Nothing -> mempty
        Just{}  -> s "invalid additional properties"

prettyPatternInvalid
    :: PatternInvalid
    -> Doc
prettyPatternInvalid = \case
    PatternNotRegex ->
        s "pattern should be a valid regular expression"

    PatternInvalid (PatternValidator regex) str ->
        dquotes (t str) <+> s "should match /" <> t regex <> s "/"

prettyItemsInvalid
    :: ItemsInvalid ValidatorFailure
    -> Doc
prettyItemsInvalid = \case
    ItemsObjectInvalid items ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "invalid items in object:"
                , indent 4 failure
                ]
            _ -> mempty
    ItemsArrayInvalid items ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "invalid items in array:"
                , indent 4 failure
                ]
            _ -> mempty

prettyAdditionalItemsInvalid
    :: AdditionalItemsInvalid ValidatorFailure
    -> Doc
prettyAdditionalItemsInvalid = \case
    AdditionalItemsBoolInvalid ixs ->
        s "unexpected additional item(s) at position(s):" <+> s (prettyIxs ixs)
    AdditionalItemsObjectInvalid items ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "invalid additional item(s):"
                , indent 4 failure
                ]
            _ -> mempty
  where
    prettyIxs = show . toList . fmap (_unIndex . fst)

prettyAllOfInvalid
    :: AllOfInvalid ValidatorFailure
    -> Doc
prettyAllOfInvalid = \case
    AllOfInvalid items ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "failed to validate all given schemas:"
                , indent 4 failure
                ]
            _ -> mempty

prettyAnyOfInvalid
    :: AnyOfInvalid ValidatorFailure
    -> Doc
prettyAnyOfInvalid = \case
    AnyOfInvalid items ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "failed to validate any of given schemas:"
                , indent 4 failure
                ]
            _ -> mempty

prettyOneOfInvalid
    :: OneOfInvalid ValidatorFailure
    -> Doc
prettyOneOfInvalid = \case
    TooManySuccesses items _ ->
        s "expected exactly 1 schema to match. But found"
        <+>
        int (NE.length items)
        <+>
        s "matching schemas."
    NoSuccesses items _ ->
        case prune [prettyIndexedList items] of
            [failure] -> vsep
                [ s "failed to validate one of given schemas:"
                , indent 4 failure
                ]
            _ -> mempty

prettyTypeValidator
    :: TypeValidator
    -> Doc
prettyTypeValidator = \case
    TypeValidatorString typ  -> prettySchemaType typ
    TypeValidatorArray types -> hsep $ punctuate comma (prettySchemaType <$> toList types)

prettySchemaType
    :: SchemaType
    -> Doc
prettySchemaType = \case
    SchemaObject  -> s "object"
    SchemaArray   -> s "array"
    SchemaString  -> s "string"
    SchemaNumber  -> s "number"
    SchemaInteger -> s "integer"
    SchemaBoolean -> s "boolean"
    SchemaNull    -> s "null"

prettyIndexedList
    :: NonEmpty (Index, NonEmpty ValidatorFailure)
    -> Doc
prettyIndexedList items =
    case prune (prettyInvalidItem <$> toList items) of
        [] -> mempty
        failures -> group $ vsep failures
  where
    prettyInvalidItem (Index ix, err :| []) =
        case prune [prettyValidatorFailure err] of
            [failure] -> brackets (int ix) <+> failure
            _ -> mempty
    prettyInvalidItem (Index ix, errs) =
        case prune (prettyValidatorFailure <$> toList errs) of
            [] -> mempty
            failures -> vsep
                [ brackets (int ix)
                , indent 4 (group $ vsep failures)
                ]

--
-- Helpers
--

prune :: [Doc] -> [Doc]
prune = filter (\x -> show x /= show empty)

-- Like 'either', but fail in IO on 'Left'.
unsafeEither :: Show e => Either e a -> IO a
unsafeEither = either (fail . show) pure

-- Format a 'Scientific', converting it a 'Double'
scientific :: Scientific -> Doc
scientific = double . toRealFloat

-- An alias to format a 'String'
s :: String -> Doc
s = string

-- An alias to format a 'Text'
t :: Text -> Doc
t = string . T.unpack
