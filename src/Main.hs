module Main (main) where

import SimpleGetOpt
import Language.Haskell.Lexer
import Data.List(unfoldr)
import Data.Maybe(mapMaybe)
import Data.Char(isSpace)

data Settings = Settings
  { makeHtml :: Bool
  , useCSS   :: [String]
  , useJS    :: [String]
  , filters  :: Decl -> Maybe Decl
  , showHelp :: Bool
  }


options :: OptSpec Settings
options = OptSpec
  { progDefaults = Settings
      { makeHtml = False
      , useCSS   = []
      , useJS    = []
      , filters  = Just
      , showHelp = False
      }

  , progOptions =
      [ Option [] ["html"] "Generate HTML"
       $ NoArg $ \s -> Right s { makeHtml = True }

      , Option [] ["js"] "Include JavaScript"
        $ ReqArg "URL" $ \u s -> Right s { useJS = u : useJS s }

      , Option [] ["css"] "Include CSS"
        $ ReqArg "URL" $ \u s -> Right s { useCSS = u : useCSS s }

      , Option [] ["no-code"] "Hide code, including instance implementations."
        $ NoArg $ addFilter noCode

      , Option [] ["no-imports"] "Hide imports."
        $ NoArg $ addFilter noImports

      , Option [] ["no-types"] "Hide types."
        $ NoArg $ addFilter noTypes

      , Option [] ["no-ctrs"] "Hide types' constructors."
        $ NoArg $ addFilter noConstructors

      , Option [] ["no-classes"] "Hide classes and instances."
        $ NoArg $ addFilter noClasses

      , Option [] ["no-methods"] "Hide classes methods."
        $ NoArg $ addFilter noMethods

      , Option [] ["no-sigs"] "Hide type signatures."
        $ NoArg $ addFilter noSigs

      , Option [] ["no-comments"] "Hide top-level comments."
        $ NoArg $ addFilter noComments

      , Option ['h'] ["help"] "Show this help."
        $ NoArg $ \s -> Right s { showHelp = True }

      ]

  , progParamDocs = []
  , progParams    = \s _ -> Left ("Unexpected parameter: " ++ s)
  }


addFilter :: (Decl -> Maybe Decl) -> OptSetter Settings
addFilter p s = Right s { filters = \d -> p =<< filters s d }

noCode :: Decl -> Maybe Decl
noCode (Code {})       = Nothing
noCode (Instance as _) = Just (Instance as (nl as))
noCode d               = Just d

noImports :: Decl -> Maybe Decl
noImports (Import {}) = Nothing
noImports d           = Just d

noComments :: Decl -> Maybe Decl
noComments (LineComment {})  = Nothing
noComments (BlockComment {}) = Nothing
noComments d                 = Just d

noSigs :: Decl -> Maybe Decl
noSigs (TySig {}) = Nothing
noSigs d = Just d

noTypes :: Decl -> Maybe Decl
noTypes (Type {}) = Nothing
noTypes d = Just d

noConstructors :: Decl -> Maybe Decl
noConstructors (Type as _) = Just (Type as (nl as))
noConstructors d            = Just d

noClasses :: Decl -> Maybe Decl
noClasses (Class {})    = Nothing
noClasses (Instance {}) = Nothing
noClasses d             = Just d

noMethods :: Decl -> Maybe Decl
noMethods (Class as _)    = Just (Class as (nl as))
noMethods (Instance as _) = Just (Instance as (nl as))
noMethods d               = Just d



main :: IO ()
main =
  do settings <- getOpts options
     if showHelp settings
        then dumpUsage options
        else do let render
                      | makeHtml settings =
                            htmlDoc (reverse $ useCSS settings)
                                    (reverse $ useJS settings)
                                  . concatMap declToHtml
                      | otherwise = concatMap flatten
                interact (render . catDecls . map (filters settings) . decls)

decls :: String -> [Decl]
decls = foldr joinHaddocks [] . map classify . blocks . lexerPass0
  where
  isLineComment (LineComment Normal xs) = True
  isLineComment _ = False

  lineCommentToks (LineComment _ xs) = xs
  lineCommentToks _                  = []

  joinHaddocks (LineComment f@(Haddock _) xs) ys =
    let (as,bs) = span isLineComment ys
    in LineComment f (concat (xs : map lineCommentToks as)) : bs

  joinHaddocks x xs = x : xs

catDecls :: [Maybe Decl] -> [Maybe Decl]
catDecls = foldr cons []
  where cons Nothing (Just x : xs) = Nothing : Just x : xs
        cons Nothing xs            = xs
        cons (Just x) xs           = Just x : xs





nl :: [PosToken] -> [PosToken]
nl ((_,(p,_)) : _) = [(Whitespace, (p,"\n"))]
nl []              = []



data Decl =
    Import [PosToken]
  | Type [PosToken] [PosToken]
  | Class [PosToken] [PosToken]
  | Instance [PosToken] [PosToken]
  | TySig [PosToken]
  | LineComment CommentType [PosToken]
  | BlockComment CommentType [PosToken]
  | Code [PosToken]
  | CPP [PosToken]

data CommentType = Pragma | Haddock Dir | Normal
data Dir         = Up | Down

type Block = [ PosToken ]

startsBlock :: PosToken -> Bool
startsBlock (Whitespace,_) = False
startsBlock (_,(p,_))      = column p == 1


block :: [PosToken] -> Maybe (Block, [PosToken])
block []       = Nothing
block (l : ls) = Just (l : this, rest)
  where (this,rest) = break startsBlock ls

blocks :: [PosToken] -> [Block]
blocks = unfoldr block

classify :: Block -> Decl
classify b =
  case b of
    (Varsym, (_,"#")) : _ -> CPP b

    (Reservedid, (_,kw)) : _
      | kw == "import"   -> Import b
      | kw == "class"    -> splitOn isWhere Class
      | kw `elem` [ "deriving", "instance" ] -> splitOn isWhere Instance

      | kw == "data"
      , (as,bs@(_:_)) <- break isWhere b -> Type as bs

      | kw `elem` [ "data", "type", "newtype" ] -> splitOn isEq Type


    (NestedComment, (_,s)) : _ -> BlockComment (commentTy $ drop 2 s) b
    (Commentstart, _) : (Comment, (_,s)) : _ -> LineComment (commentTy s) b

    _ | isTySig b -> TySig b

    _ -> Code b

  where
  commentTy s =
    case take 1 (dropWhile isSpace s) of
      "|" -> Haddock Down
      "^" -> Haddock Up
      "#" -> Pragma
      _   -> Normal

  isEq (Reservedop, (_,"=")) = True
  isEq _ = False

  isWhere (Reservedid, (_,"where")) = True
  isWhere _ = False

  splitOn p f = let (as,bs) = break p b
                in f as bs

isTySig :: Block -> Bool
isTySig = go . rmSpace
  where
  go xs = case xs of
            (Varid, _) : as -> more as
            (Special,(_,"(")) : (Varsym,_) : (Special, (_,")")) : as -> more as
            _ -> False

  more ((Special, (_,",")) : ys)    = go ys
  more ((Reservedop, (_,"::")) : _) = True
  more _                            = False



flatten :: Maybe Decl -> String
flatten Nothing = "\n"
flatten (Just d) =
  case d of
    Import ps         -> flat ps
    Type as bs        -> flat as ++ flat bs
    Instance as bs    -> flat as ++ flat bs
    Class as bs       -> flat as ++ flat bs
    TySig a           -> flat a
    LineComment _ a   -> flat a
    BlockComment _ a  -> flat a
    Code a            -> flat a

  where txt (_,(_,t)) = t
        flat = concatMap txt

--------------------------------------------------------------------------------
type Html = String

toHtml :: String -> Html
toHtml = concatMap esc
  where esc '<' = "&lt;"
        esc '>' = "&gt;"
        esc '&' = "&amp;"
        esc c   = [c]

hspan :: String -> Html -> Html
hspan c h = "<span class=" ++ show c ++ ">" ++ h ++ "</span>"

hdiv :: String -> Html -> Html
hdiv c h = "<div class=" ++ show c ++ ">" ++ h ++ "</div>"

declToHtml :: Maybe Decl -> Html
declToHtml Nothing = "<br>"
declToHtml (Just d) =
  case d of
    Import as               -> atom "block import" as
    Code as                 -> atom "block code"  as
    TySig as                -> atom "block signature" as
    LineComment c as        -> atom ("block bcomment " ++ commentC c) as
    BlockComment c as       -> atom ("block bcomment " ++ commentC c) as
    Type as bs              -> sub  "block type" as bs
    Class as bs             -> sub  "block class" as bs
    Instance as bs          -> sub  "block instance" as bs
    CPP as  -> hdiv "block cpp" $ concat [ toHtml s | (_,(_,s)) <- as ]

  where
  atom c as   = hdiv c (tokensToHtml as)
  sub c as bs = hdiv c (tokensToHtml as ++ hspan "sub" (tokensToHtml bs))
  commentC c = case c of
                 Pragma       -> "pragma"
                 Haddock Down -> "haddock_next"
                 Haddock Up   -> "haddock_up"
                 Normal       -> ""

tokensToHtml :: [PosToken] -> Html
tokensToHtml [] = []
tokensToHtml (x : xs)
  | isTick x = case break isTick xs of
                 (as, b:bs) -> inf (html x ++ tokensToHtml as ++ html b) bs
                 _          -> html x ++ tokensToHtml xs
  | otherwise = case fst x of
                  Varid              -> simple "var"
                  Conid              -> simple "con"
                  Varsym             -> inf (hspan "var" (html x)) xs
                  Consym             -> inf (hspan "con" (html x)) xs
                  Reservedid         -> simple "kw"
                  Reservedop         -> inf (hspan "kw" (html x)) xs
                  IntLit             -> simple "lit"
                  FloatLit           -> simple "lit"
                  CharLit            -> simple "lit"
                  StringLit          -> simple "lit"
                  Qvarid             -> simple "var"
                  Qconid             -> simple "con"
                  Qvarsym            -> inf (hspan "var" (html x)) xs
                  Qconsym            -> inf (hspan "con" (html x)) xs
                  NestedComment      -> simple "comment"
                  LiterateComment    -> simple "comment"
                  Commentstart       -> simple "comment"
                  Comment            -> simple "comment"
                  _          -> html x ++ tokensToHtml xs

  where
  inf a b = hspan "infix" a ++ tokensToHtml b

  simple c = hspan c (html x) ++ tokensToHtml xs

  isTick (Special, (_,"`")) = True
  isTick _                  = False

  html (_,(_,s))            = toHtml s

htmlDoc :: [String] -> [String] -> Html -> Html
htmlDoc css jss h = unlines $
  [ "<!DOCTYPE HTML>"
  , "<head>"
  ] ++ map addCSS css ++ map addScript jss ++
  [ "</head>"
  , "<body>"
  , h
  , "</body>"
  , "</html>"
  ]
  where
  addScript a = "<script src=" ++ show a ++ "></script>"
  addCSS a    = "<link rel=\"stylesheet\" href=" ++ show a ++ ">"


