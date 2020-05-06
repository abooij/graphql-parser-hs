module Language.GraphQL.Draft.Generator.Selection where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                                as Gen
import qualified Hedgehog.Range                              as Range

import           Language.GraphQL.Draft.Generator.Primitives
import           Language.GraphQL.Draft.Syntax
import           Data.Singletons

genSelectionSet :: Gen SelectionSet
genSelectionSet = Gen.list (Range.linear 1 11) genSelection

genSelection :: Gen Selection
genSelection =
  Gen.recursive
  Gen.choice [ SelectionFragmentSpread <$> genFragmentSpread
             ]
             [ Gen.subtermM (SelectionField <$> genField) pure
             , Gen.subtermM (SelectionInlineFragment <$> genInlineFragment) pure
             ]

genFragmentSpread :: Gen FragmentSpread
genFragmentSpread = FragmentSpread
                    <$> genName
                    <*> genDirectives

genInlineFragment :: Gen InlineFragment
genInlineFragment = InlineFragment
                    <$> Gen.maybe genTypeCondition
                    <*> genDirectives
                    <*> genSelectionSet

genField :: Gen Field
genField = Field
           <$> Gen.maybe genAlias
           <*> genName
           <*> Gen.list (Range.linear 1 11) genArgument
           <*> genDirectives
           <*> genSelectionSet

genAlias :: Gen Alias
genAlias = Alias <$> genName

genDirective :: SingI vv => Gen (Directive vv)
genDirective = Directive
               <$> genName
               <*> Gen.list (Range.linear 1 11) genArgument

genDirectives :: SingI vv => Gen [Directive vv]
genDirectives = Gen.list (Range.linear 1 11) genDirective

genArgument :: SingI vv => Gen (Argument vv)
genArgument = Argument <$> genName <*> genValue

genTypeCondition :: Gen TypeCondition
genTypeCondition = genNamedType
