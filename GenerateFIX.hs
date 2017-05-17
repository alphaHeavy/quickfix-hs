{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.Map (Map)
import Data.List (intercalate, partition, stripPrefix, sortBy)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)
import Text.XML.Expat.Proc
import Text.XML.Expat.Tree hiding (QName)
import qualified Data.ByteString as B
import Language.Haskell.Exts
import System.Environment (getArgs)

type Required = Bool

data FIXType
  = FIXAmount
  | FIXBoolean
  | FIXChar
  | FIXCountry
  | FIXCurrency
  | FIXData
  | FIXDayOfMonth
  | FIXExchange
  | FIXFloat
  | FIXGroup String
  | FIXInt
  | FIXLength
  | FIXLocalMktDate
  | FIXMonthYear
  | FIXMultiValueString
  | FIXNumInGroup
  | FIXPercentage
  | FIXPrice
  | FIXPriceOffset
  | FIXQuantity
  | FIXString
  | FIXUTCDate
  | FIXUTCDateOnly
  | FIXUTCTimeOnly
  | FIXUTCTimeStamp
  | FIXEnum String FIXType [(ValueEnum, Description)]
  deriving (Show,Eq)

instance Read FIXType where
  readsPrec _ s = case map toLower s of
    " char"               -> [(FIXChar, "")]
    "amt"                 -> [(FIXAmount, "")]
    "boolean"             -> [(FIXBoolean, "")]
    "char"                -> [(FIXChar, "")]
    "country"             -> [(FIXCountry, "")]
    "currency"            -> [(FIXCurrency, "")]
    "data"                -> [(FIXData, "")]
    "dayofmonth"          -> [(FIXDayOfMonth, "")]
    "exchange"            -> [(FIXExchange, "")]
    "float"               -> [(FIXFloat, "")]
    "int"                 -> [(FIXInt, "")]
    "length"              -> [(FIXLength, "")]
    "localmktdate"        -> [(FIXLocalMktDate, "")]
    "monthyear"           -> [(FIXMonthYear, "")]
    "multiplevaluestring" -> [(FIXMultiValueString, "")]
    "numingroup"          -> [(FIXNumInGroup, "")]
    "percentage"          -> [(FIXPercentage, "")]
    "price"               -> [(FIXPrice, "")]
    "priceoffset"         -> [(FIXPriceOffset, "")]
    "qty"                 -> [(FIXQuantity, "")]
    "quantity"            -> [(FIXQuantity, "")]
    "seqnum"              -> [(FIXInt, "")]
    "string"              -> [(FIXString, "")]
    "utcdate"             -> [(FIXUTCDate, "")]
    "utcdateonly"         -> [(FIXUTCDateOnly, "")]
    "utctimeonly"         -> [(FIXUTCTimeOnly, "")]
    "utctimestamp"        -> [(FIXUTCTimeStamp, "")]
    x -> trace ("unhandled: "++x) undefined

data ValueEnum
  = ValueChar Char
  | ValueBool Bool
  | ValueString String
  | ValueInt Int
  deriving (Read,Show,Eq)

type Description = String

data Field
  = Field {fieldID :: Int, fieldName :: String, requiredField :: Required, fieldType :: FIXType}
  | Group {fieldID :: Int, fieldName :: String, requiredField :: Required, fields :: [(Field, Required)]}
  deriving (Eq,Show)

isEnumField :: Field -> Bool
isEnumField Field{fieldType = FIXEnum{}} = True
isEnumField _ = False

fieldTyQName (field@Field{}, _)
  | isEnumField field = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Enumeration"
  | otherwise         = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Field"
fieldTyQName (field@Group{}, _) = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Group"

data Message = Message
  { messageType :: Char
  , messageName :: String
  , messageCategory :: String
  , messageFields :: [(Field, Required)]
  } deriving (Show)

data Header = Header [(Field, Required)] deriving (Show)
data Trailer = Trailer [(Field, Required)] deriving (Show)
data Components = Components deriving (Show)

type Messages = [Message]
type Fields = [Field]

data FIX = FIX Header Messages Trailer Components Fields deriving (Show)

parseReqdField :: (String -> Field) -> Node String String -> (Field, Required)
parseReqdField fieldMap (Element "field" attr _) = (field, reqd')
  where Just name = lookup "name" attr
        Just reqd = lookup "required" attr
        reqd' = case reqd of "Y" -> True; _ -> False
        field = (fieldMap name) {requiredField = reqd'}

parseReqdFields :: (String -> Field) -> Node String String -> [(Field, Required)]
parseReqdFields fieldMap node = map p'fld fields
  where fields = findChildren "field" node
        p'fld  = parseReqdField fieldMap

parseHeader :: (String -> Field) -> Node String String -> Header
parseHeader fieldMap header@(Element "header" _ _) =
  Header $ parseReqdFields fieldMap header

parseTrailer :: (String -> Field) -> Node String String -> Trailer
parseTrailer fieldMap trailer@(Element "trailer" _ _) =
  Trailer $ parseReqdFields fieldMap trailer

parseFIXEnum :: FIXType -> String -> ValueEnum
parseFIXEnum ty str = case ty of
  FIXBoolean          -> case str of "Y" -> ValueBool True; "N" -> ValueBool False
  FIXChar             -> ValueChar $ head str
  FIXInt              -> ValueInt $ read str
  FIXString           -> ValueString str
  FIXMultiValueString -> ValueString str
  FIXNumInGroup       -> ValueInt $ read str
  _                   -> ValueString $ "unknown: (" ++ show ty ++ ") " ++ show str

parseField :: Node String String -> Field
parseField node@(Element "field" attr _) = Field num name False (ty' children)
  where Just num  = liftM read $ lookup "number" attr
        Just name = lookup "name" attr
        Just ty   = liftM read $ lookup "type" attr
        children  = findChildren "value" node
        ty' []    = ty
        ty' _     = FIXEnum name ty values
        values    = map typedVals $ findChildren "value" node
        typedVals (Element "value" attr2 _) = (parseFIXEnum ty enum, description)
          where Just enum        = lookup "enum" attr2
                Just description = lookup "description" attr2

parseGroup :: (String -> Field) -> String -> Node String String -> (Field, Required)
parseGroup fieldMap msgName node@(Element "group" attr _) = (group, reqd')
  where group            = Group fieldID groupName reqd' fields
        fields           = parseReqdFields fieldMap node
        Field{fieldID}   = fieldMap name
        Just name        = lookup "name" attr
        name'            = case stripPrefix "No" name of Just str -> str; _ -> name
        groupName        = msgName ++ name'
        Just reqd        = lookup "required" attr
        reqd'            = case reqd of "Y" -> True; _ -> False

parseGroups :: (String -> Field) -> String -> Node String String -> [(Field, Required)]
parseGroups fieldMap msgName node = map (parseGroup fieldMap msgName) groups
  where groups = findChildren "group" node

parseMessage :: (String -> Field) -> Node String String -> Maybe Message
parseMessage fieldMap msg@(Element "message" attr _) = fmap (\ ty' -> Message ty' name cat merged) ty
  where ty = case lookup "msgtype" attr of
               Just [val] -> Just val
               Just val   -> trace ("unsupported multichar message type: "++name) Nothing
               Nothing    -> trace ("malformed record: "++show msg) Nothing
        Just name    = lookup "name" attr
        Just cat     = lookup "msgcat" attr
        fields       = sortBy fieldSort $ parseReqdFields fieldMap msg
        groups       = parseGroups fieldMap name msg
        merged       = fields ++ groups
        fieldSort (_, True) (_, False) = LT
        fieldSort (_, False) (_, True) = GT
        fieldSort (f1, _) (f2, _) = compare (fieldQName f1 True) (fieldQName f2 True)

parseFIXDocument :: Node String String -> FIX
parseFIXDocument root@(Element "fix" _ _) = FIX header messages' trailer components fields'
  where Just header      = liftM (parseHeader fieldLookup) $ findChild "header" root
        Just trailer     = liftM (parseTrailer fieldLookup) $ findChild "trailer" root
        Just messages    = findChild "messages" root
        messages'        = catMaybes $ map (parseMessage fieldLookup) $ findChildren "message" messages
        components       = Components
        Just fields      = findChild "fields" root
        fields'          = map parseField $ findChildren "field" fields
        fieldMap         = Map.fromList $ map fieldMapVal fields'
        fieldMapVal f    = (fieldName f, f)
        fieldLookup name
          | Just val <- Map.lookup name fieldMap = val
          | otherwise = error $ "Missing field for " ++ name

typeNat :: Int -> QName
typeNat = UnQual . Ident . show

strongFIXTyCon :: Field -> Type
strongFIXTyCon f@Field{fieldID, requiredField} =
  let fieldTy   = foldl1 TyApp [TyCon fieldQN, TyCon (typeNat fieldID), TyCon (strongFIXQName f)]
      fieldQN
        | isEnumField f = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Enumeration"
        | otherwise     = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Field"
      optField
        | requiredField = id
        | otherwise     = TyApp (TyCon (Qual (ModuleName "Prelude") (Ident "Maybe")))
  in optField fieldTy

strongFIXTyCon Group{fieldID, fieldName} =
  let fieldQN   = Qual (ModuleName "AlphaHeavy.FIX") $ Ident "Group"
  in foldl1 TyApp [TyCon fieldQN, TyCon (typeNat fieldID), TyCon (UnQual (Ident fieldName))]

strongFIXQName :: Field -> QName
strongFIXQName = UnQual . Ident . ty
  where ty field = case fieldType field of
          FIXGroup str     -> str
          -- FIXEnum name FIXMultiValueString _ -> "[" ++ name ++ "]"
          FIXEnum name _ _ -> name
          FIXPrice         -> "Price"
          FIXQuantity      -> "Quantity"
          _                -> fieldName field

weakFIXTyCon :: FIXType -> Type
weakFIXTyCon = TyCon . UnQual . baseFIXName

isPrimitiveTy :: FIXType -> Bool
isPrimitiveTy tyEnum = case tyEnum of
  FIXGroup _    -> False
  FIXEnum _ _ _ -> False
  _             -> True

baseFIXName :: FIXType -> Name
baseFIXName tyEnum = Ident $ case tyEnum of
  FIXAmount           -> "Decimal"
  FIXBoolean          -> "Bool"
  FIXChar             -> "Char"
  FIXCountry          -> "String"
  FIXCurrency         -> "Currency"
  FIXData             -> "Data.ByteString.ByteString"
  FIXDayOfMonth       -> "DayOfMonth"
  FIXEnum name _ _    -> name
  FIXExchange         -> "Exchange"
  FIXFloat            -> "Double"
  FIXGroup str        -> str
  FIXInt              -> "Int"
  FIXLength           -> "Int"
  FIXLocalMktDate     -> "MarketLocalTime"
  FIXMonthYear        -> "MonthYear"
  FIXMultiValueString -> "String"
  FIXNumInGroup       -> "Int"
  FIXPercentage       -> "Double"
  FIXPrice            -> "Price"
  FIXPriceOffset      -> "Decimal"
  FIXQuantity         -> "Quantity"
  FIXString           -> "String"
  FIXUTCDate          -> "UTCDate"
  FIXUTCDateOnly      -> "UTCDate"
  FIXUTCTimeOnly      -> "UTCTime"
  FIXUTCTimeStamp     -> "UTCTimeStamp"
  x                   -> trace ("undefined: "++show x) undefined

camel :: String -> String
camel (x:xs) = toLower x:xs
camel [] = []

maybeFIXTyCon :: Field -> Type
maybeFIXTyCon fty = (strongFIXTyCon fty)

{-unbangedMaybeFIXTyCon :: Field -> BangType
unbangedMaybeFIXTyCon = BangedTy . maybeFIXTyCon-}

fieldType' :: (Field, Required) -> (Name, Type)
fieldType' field = case field of
    (f@Field{}, required) -> (name, ty required f)
    ((Group _ groupName _ _), _) -> (name, grp $ FIXGroup groupName)
  where grp fty      = TyList $ weakFIXTyCon fty
        ty True fty  = TyCon $ strongFIXQName fty
        ty False fty = maybeFIXTyCon fty
        name         = uncurry fieldQName field

fieldQName :: Field -> Required -> Name
fieldQName field required = Ident qname
  where name = fieldName field
        qname = if required
                  then camel name
                  else "opt" ++ name

recordDecl :: Message -> ConDecl
recordDecl (Message _ name _ fields) = ConDecl ident args
  where ident        = Ident name
        args         = map x fields
        grp fty      = strongFIXTyCon fty
        ty True fty  = strongFIXTyCon fty
        ty False fty = maybeFIXTyCon fty
        x (g@Group{}, _)   = grp g
        x (f@Field{}, reqd)= ty reqd f

generateMessageConDecl :: Message -> QualConDecl
generateMessageConDecl msg = QualConDecl srcLoc tyVarBind context ctor
  where tyVarBind = []
        context   = []
        ctor      = recordDecl msg

generateMessageDecl :: Message -> Decl
generateMessageDecl msg@(Message _ name _ _) = decl where
  decl      = DataDecl srcLoc DataType context name' tyVarBind decls derived
  decls     = [generateMessageConDecl msg]
  name'     = Ident name
  context   = []
  tyVarBind = []
  derived   = map ((\v -> (v, [])) . UnQual . Ident) ["Generic","Show","Eq"]

generateLensInstDecl :: Message -> [Decl]
generateLensInstDecl msg@(Message _ name _ fields) = decls where
  decls = concatMap decl fields
  context   = []
  className (Field{fieldName}, reqd)  = UnQual (Ident (fieldName ++ (if reqd then "Lens" else "MaybeLens")))
  lensName  (Field{fieldName}, True)  = Ident (camel fieldName)
  lensName  (Field{fieldName}, False) = Ident ("opt" ++ fieldName)
  -- className (Group{fieldName}, _)    = UnQual (Ident (fieldName ++ "Lens"))
  decl field@(Field{}, _) = [InstDecl srcLoc context (className field) [TyCon (UnQual (Ident name))] [InsDecl (FunBind [lens field])]]
  decl (Group{}, _) = []
  lens field = Match srcLoc (lensName field) [] Nothing (rhs field) (binds field)
  unqual = Var . UnQual . Ident
  rhs _ = UnGuardedRhs (foldl1 App [Var (Qual (ModuleName "Control.Lens") (Ident "lens")), unqual "g", unqual "s"])
  binds field = BDecls [FunBind (getter field), FunBind [setter field]]
  getter field@(_, True) = [getterReqMatch field]
  getter field@(_, False) = [getterReqMatch field, getterOptMatch field]
  getterReqMatch field = Match srcLoc (Ident "g") [getterLhs field] Nothing (UnGuardedRhs (getterRhs field)) (BDecls [])
  getterOptMatch field = Match srcLoc (Ident "g") [PWildCard] Nothing (UnGuardedRhs (Con (UnQual (Ident ("Nothing"))))) (BDecls [])
  getterLhs field = PApp (UnQual (Ident name)) (fieldGetter field)
  fieldGetter field@(_, True)  = map (\ x -> if x /= field then PWildCard else (PApp (fieldTyQName field) [PVar (Ident "x")])) fields
  fieldGetter field@(_, False) = map (\ x -> if x /= field then PWildCard else (PApp (UnQual (Ident "Just")) [PApp (fieldTyQName field) [PVar (Ident "x")]])) fields
  getterRhs field@(_, True) = Var (UnQual (Ident ("x")))
  getterRhs field@(_, False) = App (Con (UnQual (Ident ("Just")))) (Var (UnQual (Ident ("x"))))

  setter field = Match srcLoc (Ident "s") [setterLhsFields field, setterLhs field] Nothing (UnGuardedRhs (setterRhs field)) (BDecls [])
  setterLhs field = PApp (UnQual (Ident "val")) []
  setterLhsFields field@(f, _) = PApp (UnQual . Ident $ name) $ zipWith (\ x y -> if x == field then PWildCard else PVar . Ident $ ("_x" ++ show y)) fields [1..]
  reqVal field = foldr1 App [Con (fieldTyQName field), Var . UnQual . Ident $ "val"]
  -- optVal field = App (Con . UnQual . Ident $ "fmap") (reqVal field)
  optVal field = foldl1 App [Var (UnQual (Ident "fmap")), Con (fieldTyQName field), Var . UnQual . Ident $ "val"]
  setterRhsFields field@(f, True) = foldl1 App ((Con . UnQual . Ident $ name):zipWith (\ x y -> if x == field then reqVal field else Var . UnQual . Ident $ ("_x" ++ show y)) fields [1..])
  setterRhsFields field@(f, False) = foldl1 App ((Con . UnQual . Ident $ name):zipWith (\ x y -> if x == field then optVal field else Var . UnQual . Ident $ ("_x" ++ show y)) fields [1..])
  -- setterRhs field@(_, True) = setterRhsFields field
  -- setterRhs field@(_, False) = App (Con . UnQual . Ident $ "Just") (setterRhsFields field)
  setterRhs field = setterRhsFields field

srcLoc :: SrcLoc
srcLoc = SrcLoc {srcFilename = "foo.hs", srcLine = 1, srcColumn = 1}

generateMessagesDecl :: Messages -> [Decl]
generateMessagesDecl msgs = concatMap decls msgs where
  decls f = generateMessageDecl f : generateLensInstDecl f

groupDecl :: Field -> ConDecl
groupDecl (Group _ name _ fields) = ConDecl ident args
  where ident = Ident name
        args         = map x fields
        ty           = BangedTy . strongFIXTyCon
        grp          = BangedTy . weakFIXTyCon
        groupPrefix fieldName = camel name ++ fieldName
        x (f@Field{fieldName}, _) = ty f
        x (Group{fieldName}, _) = grp $ FIXGroup fieldName

generateGroups :: Messages -> [Decl]
generateGroups msgs = map (\ (name, group) -> DataDecl srcLoc DataType context name tyVarBind [group] derived) groups
  where fields    = concat [map fst fieldList | Message _ _ _ fieldList <- msgs]
        groups    = [(Ident fieldName, decl $ groupDecl group) | group@Group{fieldName} <- fields]
        decl      = QualConDecl srcLoc tyVarBind context
        context   = []
        tyVarBind = []
        derived   = map ((\v -> (v, [])) . UnQual . Ident) ["Generic","Show","Eq"]

generateFieldEnum' :: Field -> [Decl]
generateFieldEnum' field = [generateFieldEnum field, generateFieldTag field]

generateFieldEnum :: Field -> Decl
generateFieldEnum (Field _ name _ (FIXEnum _ ty enums)) = DataDecl srcLoc DataType context (Ident name) tyVarBind decls derived
  where decls     = map decl enums ++ unspecified
        decl enum = QualConDecl srcLoc tyVarBind context $ ConDecl ctorName []
          where ctorName = Ident $ name ++ "_" ++ snd enum
        unspecified = []
        context   = []
        tyVarBind = []
        derived   = map ((\v -> (v, [])) . UnQual . Ident) ["Read","Show","Eq"]

valueEnumToPat :: ValueEnum -> Pat
valueEnumToPat ve = case ve of
  ValueChar char  -> PLit $ Char char
  ValueBool bool  -> PApp (UnQual . Ident $ show bool) []
  ValueString str -> PLit $ String str
  ValueInt int    -> PLit . Int $ fromIntegral int

valueEnumToLit :: ValueEnum -> Exp
valueEnumToLit ve = case ve of
  ValueChar char  -> Lit $ Char char
  ValueBool bool  -> Con (UnQual . Ident $ show bool)
  ValueString str -> Lit $ String str
  ValueInt int    -> Lit . Int $ fromIntegral int

{-
instance FieldTag ExecType where
  type FieldTagRep ExecType = Char
  toFieldTagRep :: a -> FieldTagRep a
  fromFieldTagRep :: FieldTagRep a -> Maybe a
-}

generateFieldTag :: Field -> Decl
generateFieldTag (Field _ name _ (FIXEnum _ ty enums)) = decl where
  decl = InstDecl srcLoc context className [tyCon] decls
  tyCon = TyCon . UnQual $ Ident name
  context = []
  className = Qual (ModuleName "AlphaHeavy.FIX") (Ident "FieldTag")
  decls = [repDecl, InsDecl toDecl, InsDecl fromDecl]
  repDecl = InsType srcLoc (TyApp (TyCon (UnQual (Ident "FieldTagRep"))) tyCon) (TyCon (UnQual (baseFIXName ty)))
  toDecl = FunBind $ map toMatch enums
  toMatch (e, d) = Match srcLoc (Ident "toFieldTagRep") [PApp (enumDesc d) []] Nothing (UnGuardedRhs (valueEnumToLit e)) (BDecls [])
  fromMatch (e, d) = Match srcLoc (Ident "fromFieldTagRep") [valueEnumToPat e] Nothing (UnGuardedRhs (App (Con (UnQual (Ident "Just"))) (Con (enumDesc d)))) (BDecls [])
  noMatch
    | FIXBoolean <- ty = []
    | otherwise = [Match srcLoc (Ident "fromFieldTagRep") [PWildCard] Nothing (UnGuardedRhs (Con (UnQual (Ident "Nothing")))) (BDecls [])]
  fromDecl = FunBind $ map fromMatch enums ++ noMatch
  enumDesc d = UnQual . Ident $ name ++ "_" ++ d

newImport :: String -> Bool -> ImportDecl
newImport mod qualified = ImportDecl{
  importLoc       = srcLoc,
  importModule    = ModuleName mod,
  importQualified = qualified,
  importSrc       = False,
  importPkg       = Nothing,
  importAs        = Nothing,
  importSpecs     = Nothing}

qualifiedImport :: String -> ImportDecl
qualifiedImport mod = newImport mod True

unqualifiedImport :: String -> ImportDecl
unqualifiedImport mod = newImport mod False

generateNewTypeDecl :: Field -> [Decl]
generateNewTypeDecl field = case (fieldType field) of
                              FIXPrice -> []
                              FIXQuantity -> []
                              _ -> [newTypeDecl]
  where newTypeDecl = DataDecl srcLoc NewType context name tyVarBind decls derived
        decls     = [QualConDecl srcLoc tyVarBind context (ConDecl name [BangedTy baseTyCon])]
        name      = Ident $ fieldName field
        context   = []
        tyVarBind = []
        baseTyCon = TyCon $ UnQual $ baseFIXName $ fieldType field
        derived   = map ((\v -> (v, [])) . UnQual . Ident) (defTyCls ++ baseTyCls)
        defTyCls  = ["Generic","Show","Eq"]
        intTyCls  = ["Num","Ord","Real","Enum","Integral"]
        decTyCls  = ["Num","Ord","Real"]
        baseTyCls = case fieldType field of
                      FIXInt      -> intTyCls
                      FIXAmount   -> decTyCls
                      FIXQuantity -> decTyCls
                      FIXFloat    -> ["Enum","Floating","Fractional","Num","Ord","Real","RealFloat","RealFrac"]
                      FIXPrice    -> decTyCls
                      FIXString   -> ["Ord"]
                      _ -> []

generateLensClassDecls :: [Field] -> [Decl]
generateLensClassDecls fields = regular ++ optional where
  regular  = map generateLensClassDecl fields
  optional = map generateMaybeLensClassDecl fields

generateLensClassDecl :: Field -> Decl
generateLensClassDecl field@Field{fieldName} = decl where
  decl    = ClassDecl srcLoc context (Ident (fieldName ++ "Lens")) [varA] [] [lens]
  lens    = ClsDecl $ TypeSig srcLoc [Ident (camel fieldName)] lensTy
  lensArr = TyCon (Qual (ModuleName "Control.Lens") (Ident "Lens'"))
  lensTy  = foldl1 TyApp [lensArr, tyVarA, TyCon (strongFIXQName field)] -- UnQual (Ident fieldName))]
  varA    = UnkindedVar (Ident "a")
  tyVarA  = TyVar (Ident "a")
  context = []

generateMaybeLensClassDecl :: Field -> Decl
generateMaybeLensClassDecl field@Field{fieldName} = decl where
  decl    = ClassDecl srcLoc context (Ident (fieldName ++ "MaybeLens")) [varA] [] [lens]
  lens    = ClsDecl $ TypeSig srcLoc [Ident ("opt" ++ fieldName)] lensTy
  lensArr = TyCon $ Qual (ModuleName "Control.Lens") (Ident "Lens'")
  lensTy  = foldl1 TyApp [lensArr, tyVarA, TyApp (TyCon (UnQual (Ident "Maybe"))) (TyCon (strongFIXQName field))] -- UnQual (Ident fieldName))]
  varA    = UnkindedVar (Ident "a")
  tyVarA  = TyVar (Ident "a")
  context = []

generateMessageModule :: FIX -> String -> Module
generateMessageModule (FIX _ messages _ _ fields) version = Module srcLoc modName pragmas warningText exports imports decls
  where modName       = ModuleName $ "AlphaHeavy.FIX.FIX" ++ version ++ ".Types"
        pragmas       = [LanguagePragma srcLoc $ Ident <$> ["DeriveGeneric", "DataKinds", "GeneralizedNewtypeDeriving", "TypeFamilies", "TypeOperators"]]
        warningText   = Nothing
        exports       = Nothing
        imports       = [ qualifiedImport "Data.ByteString", qualifiedImport "Control.Lens"
                        , unqualifiedImport "GHC.Generics", unqualifiedImport "AlphaHeavy.FIX"]
        groups        = generateGroups messages
        fieldEnums    = [i | i@(Field _ _ _ (FIXEnum _ _ (_:_))) <- fields]
        fields'       = concatMap generateFieldEnum' fieldEnums
        decls         = messagesDecl ++ groups ++ fields' ++ newTypes ++ lensDecl
        ignoreNames n = not (n `elem` ["Price", "Currency"])
        newTypes      = concat [generateNewTypeDecl f | f@Field{fieldName, fieldType} <- fields, isPrimitiveTy fieldType, ignoreNames fieldName]
        messagesDecl  = generateMessagesDecl messages
        lensDecl      = generateLensClassDecls fields

generateMessageCtorDecl :: Message -> [Decl]
generateMessageCtorDecl (Message _ msgName _ fields) = [TypeSig srcLoc [name] sig, FunBind [match]]
  where match                     = Match srcLoc name fieldVars Nothing msgCase binds
        sig                       = foldr1 TyFun $ requiredFieldTys ++ [unqMsgTy]
        unqTy                     = TyCon . UnQual . Ident
        unqMsgTy                  = unqTy msgName
        requiredFieldTys          = map (snd . fieldType') required
        (required, _)             = partition snd fields
        name                      = Ident $ camel msgName
        binds                     = BDecls []
        msgCase                   = UnGuardedRhs $ foldl1 App (Var (UnQual (Ident msgName)):updates)
        fieldVars                 = map (PVar . fieldName) required
        update f@(_, True)        = App (Con (fieldTyQName f)) (Var (fieldNameQ f))
        -- requiredUpdates           = map (requiredUpdate . fieldNameQ) required
        update (Group{}, False)   = App (Con (UnQual (Ident "Group"))) (Con $ UnQual $ Ident "[]")
        update (Field{}, False)   = (Con $ UnQual $ Ident "Nothing")
        updates                   = map update fields
        fieldName                 = fst . fieldType'
        fieldNameQ                = UnQual . fieldName

generateMessageFactories :: FIX -> String -> Module
generateMessageFactories (FIX _ messages _ _ _) version = Module srcLoc modName pragmas warningText exports imports decls
  where modName       = ModuleName $ "AlphaHeavy.FIX.FIX" ++ version ++ ".Factory"
        pragmas       = [LanguagePragma srcLoc [Ident "MultiParamTypeClasses", Ident "FlexibleInstances"]]
        warningText   = Nothing
        exports       = Nothing
        imports       = [ qualifiedImport "Data.ByteString"
                        , unqualifiedImport "AlphaHeavy.FIX"
                        , unqualifiedImport $ "AlphaHeavy.FIX.FIX" ++ version ++ ".Types"
                        ]
        decls         = concatMap generateMessageCtorDecl messages

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then do putStrLn "Usage: GenerateFix FixPath Version"
            return ()
    else do let path = args !! 0
                version = args !! 1
            source <- B.readFile path
            let Right root = parse' (defaultParseOptions :: ParseOptions String String) source
                tree = parseFIXDocument root
                types = generateMessageModule tree version
                factories = generateMessageFactories tree version
                ppr = prettyPrintStyleMode style defaultMode

            putStrLn (ppr types)
            writeFile ("./src/AlphaHeavy/FIX/FIX" ++ version ++ "/Types.hs") $ ppr types
            writeFile ("./src/AlphaHeavy/FIX/FIX" ++ version ++ "/Factory.hs") $ ppr factories
            return ()
