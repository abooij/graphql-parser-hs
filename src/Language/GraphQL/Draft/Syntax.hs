{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}

-- | Description: The GraphQL AST
module Language.GraphQL.Draft.Syntax
  ( Name(..)
  , isValidName
  , Document(..)
  , ExecutableDocument(..)
  , SchemaDocument(..)
  , Definition(..)
  , ExecutableDefinition(..)
  , partitionExDefs
  , OperationDefinition(..)
  , OperationType(..)
  , TypedOperationDefinition(..)
  , TypeSystemDefinition(..)
  , SchemaDefinition(..)
  , RootOperationTypeDefinition(..)
  , VariableDefinition(..)
  , Variable(..)
  , SelectionSet
  , Selection(..)
  , Field(..)
  , Alias(..)
  , Argument(..)
  , FragmentSpread(..)
  , InlineFragment(..)
  , FragmentDefinition(..)
  , TypeCondition
  , ValueVariability(..)
  , SValueVariability
  , ValueConstantSym0
  , ValueVariableSym0

  , Sing(..)
  , ValueConst
  , ValueVar
  , Value(..)
  , StringValue(..)
  , ListValueG(..)
  , ListValue
  , ListValueC
  , ObjectValueG(..)
  , ObjectValue
  , ObjectValueC
  , ObjectFieldG(..)
  , ObjectField
  , ObjectFieldC
  , DefaultValue
  , Directive(..)
  , GType(..)
  , getBaseType
  , Nullability(..)
  , showGT
  , ToGType(..)
  , toLT
  , toNT
  , showLT
  , isNullable
  , isNotNull
  , isListType
  , showNT
  , NamedType(..)
  , ListType(..)
  , Description(..)
  , TypeDefinition(..)
  , ObjectTypeDefinition(..)
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , EnumValue(..)
  , InputObjectTypeDefinition(..)
  , DirectiveDefinition(..)
  , DirectiveLocation(..)
  , ExecutableDirectiveLocation(..)
  , TypeSystemDirectiveLocation(..)
  ) where

import           Control.Monad.Fail         (fail)
import           Data.Bool                  (not)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift(..), Lit(RationalL))
import           Language.Haskell.TH.Lib    (litE)
import           Protolude                  hiding (lift)
import           Data.Singletons.TH

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Text.Regex.TDFA            as TDFA
import qualified Data.Scientific            as S

-- * Documents

-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document

newtype Name
  = Name { unName :: Text }
  deriving ( Eq, Ord, Show, Hashable, IsString, Lift, Semigroup
           , Monoid, J.ToJSONKey, J.ToJSON)

-- Ref: http://facebook.github.io/graphql/June2018/#sec-Names
isValidName :: Name -> Bool
isValidName (Name text) =
  TDFA.match compiledRegex $ T.unpack text
  where
    compiledRegex = TDFA.makeRegex ("^[_a-zA-Z][_a-zA-Z0-9]*$" :: BL.ByteString) :: TDFA.Regex

parseName :: Text -> J.Parser Name
parseName text =
  bool (fail $ T.unpack errorMessage) (pure name) $ isValidName name
  where
    name = Name text
    errorMessage = text <> " is not valid GraphQL name"

instance J.FromJSON Name where
  parseJSON = J.withText "Text" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

newtype Document
  = Document { getDefinitions :: [Definition] }
  deriving (Ord, Show, Eq, Lift)

data Definition
  = DefinitionExecutable !ExecutableDefinition
  | DefinitionTypeSystem !TypeSystemDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Definition

newtype ExecutableDocument
  = ExecutableDocument { getExecutableDefinitions :: [ExecutableDefinition] }
  deriving (Ord, Show, Eq, Lift, Hashable)

data ExecutableDefinition
  = ExecutableDefinitionOperation OperationDefinition
  | ExecutableDefinitionFragment FragmentDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDefinition

partitionExDefs
  :: [ExecutableDefinition]
  -> ([SelectionSet], [TypedOperationDefinition], [FragmentDefinition])
partitionExDefs =
  foldr f ([], [], [])
  where
    f d (selSets, ops, frags) = case d of
      ExecutableDefinitionOperation (OperationDefinitionUnTyped t) ->
        (t:selSets, ops, frags)
      ExecutableDefinitionOperation (OperationDefinitionTyped t) ->
        (selSets, t:ops, frags)
      ExecutableDefinitionFragment frag ->
        (selSets, ops, frag:frags)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema !SchemaDefinition
  | TypeSystemDefinitionType !TypeDefinition
  -- TypeSystemDefinitionDir !DirectiveDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDefinition

data SchemaDefinition
  = SchemaDefinition
  { _sdDirectives                   :: !(Maybe [Directive 'ValueConstant])
  , _sdRootOperationTypeDefinitions :: ![RootOperationTypeDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable SchemaDefinition

data RootOperationTypeDefinition
  = RootOperationTypeDefinition
  { _rotdOperationType     :: !OperationType
  , _rotdOperationTypeType :: !NamedType
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable RootOperationTypeDefinition

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable OperationType

newtype SchemaDocument
  = SchemaDocument [TypeDefinition]
  deriving (Ord, Show, Eq, Lift, Hashable)

data OperationDefinition
  = OperationDefinitionTyped !TypedOperationDefinition
  | OperationDefinitionUnTyped !SelectionSet
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable OperationDefinition

data TypedOperationDefinition
  = TypedOperationDefinition
  { _todType                :: !OperationType
  , _todName                :: !(Maybe Name)
  , _todVariableDefinitions :: ![VariableDefinition]
  , _todDirectives          :: ![Directive 'ValueVariable]
  , _todSelectionSet        :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypedOperationDefinition

data VariableDefinition
  = VariableDefinition
  { _vdVariable     :: !Variable
  , _vdType         :: !GType
  , _vdDefaultValue :: !(Maybe DefaultValue)
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable VariableDefinition

newtype Variable
  = Variable { unVariable :: Name }
  deriving ( Eq, Ord, Show, Hashable, Lift, J.ToJSONKey, J.FromJSONKey
           , J.ToJSON, J.FromJSON)

type SelectionSet = [Selection]

data Selection
  = SelectionField !Field
  | SelectionFragmentSpread !FragmentSpread
  | SelectionInlineFragment !InlineFragment
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Selection

data Field
  = Field
  { _fAlias        :: !(Maybe Alias)
  , _fName         :: !Name
  , _fArguments    :: ![Argument 'ValueVariable]
  , _fDirectives   :: ![Directive 'ValueVariable]
  , _fSelectionSet :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Field

newtype Alias
  = Alias { unAlias :: Name }
  deriving (Ord, Show, Eq, Hashable, Lift, J.ToJSON, J.FromJSON)

data Argument vv
  = Argument
  { _aName  :: !Name
  , _aValue :: !(Value vv)
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable (Argument vv)

-- * Fragments

data FragmentSpread
  = FragmentSpread
  { _fsName       :: !Name
  , _fsDirectives :: ![Directive 'ValueVariable]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FragmentSpread

data InlineFragment
  = InlineFragment
  { _ifTypeCondition :: !(Maybe TypeCondition)
  , _ifDirectives    :: ![Directive 'ValueVariable]
  , _ifSelectionSet  :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InlineFragment

data FragmentDefinition
  = FragmentDefinition
  { _fdName          :: !Name
  , _fdTypeCondition :: !TypeCondition
  , _fdDirectives    :: ![Directive 'ValueVariable]
  , _fdSelectionSet  :: !SelectionSet
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FragmentDefinition

type TypeCondition = NamedType

-- * Values

-- data ValueLeaf
--   = VLInt !Int32
--   | VLFloat !Double
--   | VLBoolean !Bool
--   | VLString !StringValue
--   | VLEnum !EnumValue
--   | VLNull
--   deriving (Ord, Show, Eq, Lift)

-- data ValueConst
--   = VCLeaf !ValueLeaf
--   | VCList !ListValueC
--   | VCObject !ObjectValueC
--   deriving (Ord, Show, Eq, Lift)

-- data Value
--   = VVariable !Variable
--   | VLeaf !ValueLeaf
--   | VList !ListValue
--   | VObject !ObjectValue
--   deriving (Ord, Show, Eq, Lift)

data ValueVariability = ValueConstant | ValueVariable
  deriving (Ord, Show, Eq, Generic)
-- TODO add strictness
data Value (vv :: ValueVariability) where
  VVariable :: Variable -> Value 'ValueVariable
  VInt :: Integer -> Value vv
  VFloat :: S.Scientific -> Value vv
  VString :: StringValue -> Value vv
  VBoolean :: Bool -> Value vv
  VNull :: Value vv
  VEnum :: EnumValue -> Value vv
  VList :: ListValue vv -> Value vv
  VObject :: ObjectValue vv -> Value vv
deriving instance Ord (Value vv)
deriving instance Show (Value vv)
deriving instance Eq (Value vv)
-- Can't easily get a Generic instance for GADTs, so just write this one out.
instance Hashable (Value vv) where
  hashWithSalt s (VVariable v) = s `hashWithSalt` v
  hashWithSalt s (VInt v) = s `hashWithSalt` v
  hashWithSalt s (VFloat v) = s `hashWithSalt` v
  hashWithSalt s (VString v) = s `hashWithSalt` v
  hashWithSalt s (VBoolean v) = s `hashWithSalt` v
  hashWithSalt s (VNull) = s
  hashWithSalt s (VEnum v) = s `hashWithSalt` v
  hashWithSalt s (VList v) = s `hashWithSalt` v
  hashWithSalt s (VObject v) = s `hashWithSalt` v

type ValueConst = Value 'ValueConstant
type ValueVar = Value 'ValueVariable

instance Lift (Value vv) where
  lift (VVariable v) = [e|VVariable $(lift v)|]
  lift (VInt i) = [e|VInt $(lift i)|]
  lift (VFloat sc) = [e|VFloat $(litE (RationalL (toRational sc)))|]
  lift (VString s) = [e|VString $(lift s)|]
  lift (VBoolean b) = [e|VBoolean $(lift b)|]
  lift (VNull) = [e|VNull|]
  lift (VEnum ev) = [e|VEnum $(lift ev)|]
  lift (VList xs) = [e|VList $(lift xs)|]
  lift (VObject o) = [e|VObject $(lift o)|]

newtype StringValue
  = StringValue { unStringValue :: Text }
  deriving (Ord, Show, Eq, Lift, Hashable)

newtype ListValueG a
  = ListValueG {unListValue :: [a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ListValue vv = ListValueG (Value vv)

type ListValueC = ListValueG ValueConst

newtype ObjectValueG a
  = ObjectValueG {unObjectValue :: [ObjectFieldG a]}
  deriving (Ord, Show, Eq, Lift, Hashable)

type ObjectValue vv = ObjectValueG (Value vv)

type ObjectValueC = ObjectValueG ValueConst

data ObjectFieldG a
  = ObjectFieldG
  { _ofName  :: Name
  , _ofValue :: a
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (ObjectFieldG a)

type ObjectField vv = ObjectFieldG (Value vv)
type ObjectFieldC = ObjectFieldG ValueConst

type DefaultValue = ValueConst

-- * Directives

data Directive vv
  = Directive
  { _dName      :: !Name
  , _dArguments :: ![Argument vv]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable (Directive vv)

-- * Type Reference

newtype Nullability
  = Nullability { unNullability :: Bool }
  deriving (Show, Ord, Eq, Lift, Generic, Hashable)

data GType
  = TypeNamed !Nullability !NamedType
  | TypeList !Nullability !ListType
  deriving (Eq, Ord, Show, Lift, Generic)

getBaseType :: GType -> NamedType
getBaseType = \case
  TypeNamed _ namedType -> namedType
  TypeList _ listType -> getBaseType $ unListType listType

class ToGType a where
  toGT :: a -> GType

toNT :: (ToGType a) => a -> GType
toNT ty = case toGT ty of
  TypeNamed _ nt -> TypeNamed (Nullability False) nt
  TypeList _ lt  -> TypeList (Nullability False) lt

instance ToGType GType where
  toGT t = t

instance J.ToJSON GType where
  toJSON = J.toJSON . showGT

instance Hashable GType

showGT :: GType -> Text
showGT = \case
  TypeNamed nullability nt -> showNT nt <> showNullable nullability
  TypeList nullability lt  -> showLT lt <> showNullable nullability

showNullable :: Nullability -> Text
showNullable = bool "!" "" . unNullability

showNT :: NamedType -> Text
showNT = unName . unNamedType

showLT :: ListType -> Text
showLT lt = "[" <> showGT (unListType lt) <> "]"

toLT :: (ToGType a) => a -> ListType
toLT = ListType . toGT

isNullable :: GType -> Bool
isNullable = \case
  (TypeNamed nullability _) -> unNullability nullability
  (TypeList nullability _)  -> unNullability nullability

isListType :: GType -> Bool
isListType = \case
  (TypeList _ _)  -> True
  (TypeNamed _ _) -> False


isNotNull :: GType -> Bool
isNotNull = not . isNullable

newtype NamedType
  = NamedType { unNamedType :: Name }
  deriving (Eq, Ord, Show, Hashable, Lift, J.ToJSON,
            J.ToJSONKey, J.FromJSON, J.FromJSONKey)

instance ToGType NamedType where
  toGT = TypeNamed (Nullability True)

newtype ListType
  = ListType {unListType :: GType }
  deriving (Eq, Ord, Show, Lift, Hashable)

instance ToGType ListType where
  toGT = TypeList (Nullability True)

-- * Type definition

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeDefinition

newtype Description
  = Description { unDescription :: Text }
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid, Hashable,
            J.ToJSON, J.FromJSON)

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdDescription          :: !(Maybe Description)
  , _otdName                 :: !Name
  , _otdImplementsInterfaces :: ![NamedType]
  , _otdDirectives           :: ![Directive 'ValueConstant]
  , _otdFieldsDefinition     :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ObjectTypeDefinition

data FieldDefinition
  = FieldDefinition
  { _fldDescription         :: !(Maybe Description)
  , _fldName                :: !Name
  , _fldArgumentsDefinition :: !ArgumentsDefinition
  , _fldType                :: !GType
  , _fldDirectives          :: ![Directive 'ValueConstant]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FieldDefinition

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition
  = InputValueDefinition
  { _ivdDescription  :: !(Maybe Description)
  , _ivdName         :: !Name
  , _ivdType         :: !GType
  , _ivdDefaultValue :: !(Maybe DefaultValue)
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputValueDefinition

data InterfaceTypeDefinition
  = InterfaceTypeDefinition
  { _itdDescription      :: !(Maybe Description)
  , _itdName             :: !Name
  -- TODO ImplementsInterfaces (in Draft version of the GraphQL spec)
  , _itdDirectives       :: ![Directive 'ValueConstant]
  , _itdFieldsDefinition :: ![FieldDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InterfaceTypeDefinition

data UnionTypeDefinition
  = UnionTypeDefinition
  { _utdDescription :: !(Maybe Description)
  , _utdName        :: !Name
  , _utdDirectives  :: ![Directive 'ValueConstant]
  , _utdMemberTypes :: ![NamedType]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable UnionTypeDefinition

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdDescription :: !(Maybe Description)
  , _stdName        :: !Name
  , _stdDirectives  :: ![Directive 'ValueConstant]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ScalarTypeDefinition

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdDescription      :: !(Maybe Description)
  , _etdName             :: !Name
  , _etdDirectives       :: ![Directive 'ValueConstant]
  , _etdValueDefinitions :: ![EnumValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumTypeDefinition

data EnumValueDefinition
  = EnumValueDefinition
  { _evdDescription :: !(Maybe Description)
  , _evdName        :: !EnumValue
  , _evdDirectives  :: ![Directive 'ValueConstant]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumValueDefinition

newtype EnumValue
  = EnumValue { unEnumValue :: Name }
  deriving (Show, Eq, Lift, Hashable, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdDescription      :: !(Maybe Description)
  , _iotdName             :: !Name
  , _iotdDirectives       :: ![Directive 'ValueConstant]
  , _iotdValueDefinitions :: ![InputValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputObjectTypeDefinition

data DirectiveDefinition
  = DirectiveDefinition
  { _ddDescription :: !(Maybe Description)
  , _ddName        :: !Name
  , _ddArguments   :: !ArgumentsDefinition
  , _ddLocations   :: ![DirectiveLocation]
  } deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveDefinition

data DirectiveLocation
  = DLExecutable !ExecutableDirectiveLocation
  | DLTypeSystem !TypeSystemDirectiveLocation
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveLocation

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDirectiveLocation

data TypeSystemDirectiveLocation
  = TSDLSCHEMA
  | TSDLSCALAR
  | TSDLOBJECT
  | TSDLFIELD_DEFINITION
  | TSDLARGUMENT_DEFINITION
  | TSDLINTERFACE
  | TSDLUNION
  | TSDLENUM
  | TSDLENUM_VALUE
  | TSDLINPUT_OBJECT
  | TSDLINPUT_FIELD_DEFINITION
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDirectiveLocation
$(genSingletons [''ValueVariability])
