{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.GraphQL.Draft.Generator.Primitives where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Data.Scientific (fromFloatDigits)
import           Data.Singletons

import           Language.GraphQL.Draft.Syntax


-- | *Identifiers*

genText :: Gen Text
genText = Gen.text (Range.linear 1 11) Gen.unicode

genGraphqlName :: Gen Text
genGraphqlName = Gen.text (Range.singleton 1) Gen.alpha <>
                 Gen.text (Range.linear 1 11) Gen.alphaNum

genName :: Gen Name
genName = Name <$> genGraphqlName

genNamedType :: Gen NamedType
genNamedType = NamedType <$> genName

genDescription :: Gen Description
genDescription = Description <$> genText


-- | *Values*

genValue :: forall vv . SingI vv => Gen (Value vv)
genValue =
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive Gen.choice (go sing) subtermList
  where
    go :: Sing vv -> [GenT Identity (Value vv)]
    go SValueVariable = genList ++ [VVariable <$> genVariable]
    go SValueConstant = genList
    genList :: [GenT Identity (Value vv)]
    genList =
      [ pure VNull
      , VInt <$> fromIntegral <$> Gen.int32 (Range.linear 1 99999)
      , VEnum <$> genEnumValue
      , VFloat <$> fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999)
      , VString <$> genStringValue
      , VBoolean <$> Gen.bool
      ]
    subtermList =
      [ Gen.subtermM (VList <$> genListValue) pure
      , Gen.subtermM (VObject <$> genObjectValue) pure
      ]

genStringValue :: Gen StringValue
genStringValue = StringValue . unName <$> genName

genVariable :: Gen Variable
genVariable = Variable <$> genName

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue <$> genName

genListValue :: SingI vv => Gen (ListValue vv)
genListValue = ListValueG <$> Gen.list (Range.linear 1 11) genValue

genObjectValue :: SingI vv => Gen (ObjectValue vv)
genObjectValue = ObjectValueG <$> Gen.list (Range.linear 1 11) genObjectField

genObjectField :: SingI vv => Gen (ObjectField vv)
genObjectField = ObjectFieldG <$> genName <*> genValue

genDefaultValue :: Gen DefaultValue
genDefaultValue = genValue
