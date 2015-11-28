{-# Language OverloadedStrings #-}
{-# Language TypeOperators     #-}
{-# Language FlexibleContexts  #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Sharp
  ( csharp
  , csharpForAPI
  , writeCSharpForAPI
  ) where

import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Char (toUpper)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.PrettyPrint hiding ((<>))
import Data.Monoid ((<>))
import Servant.Foreign

----------------------------------------------------------------------

csharp :: HasForeign layout => Proxy layout -> Foreign layout
csharp p = foreignFor p defReq

csharpForAPI :: (HasForeign api, GenerateList (Foreign api))
    => Proxy api -> NamespaceName -> Text
csharpForAPI p n = generateCSharpNamespace n (listFromAPI p)

writeCSharpForAPI :: (HasForeign api, GenerateList (Foreign api))
    => Proxy api -> NamespaceName -> IO ()
writeCSharpForAPI p n = Text.writeFile (n ++ ".cs") $ csharpForAPI p n

----------------------------------------------------------------------
-- CSharp Types

instance HasForeignType Int where
    typeFor _ = "int"
instance HasForeignType Bool where
    typeFor _ = "bool"
instance HasForeignType Text where
    typeFor _ = "string"
instance HasForeignType a => HasForeignType [a] where
    typeFor _ = "IEnumerable<" <> (typeFor (Proxy :: Proxy a)) <> ">"

----------------------------------------------------------------------
-- CSharp Code Generator

type NamespaceName = String
generateCSharpNamespace :: NamespaceName -> [Req] -> Text
generateCSharpNamespace namespaceName reqs = Text.pack . render $ vcat 
    [ "using System;"
    , "using System.Net.Http;"
    , "using System.Net.Http.Headers;"
    , "using System.Threading.Tasks;"
    , "using System.Collections.Generic;"
    , ""
    , "namespace" <+> text namespaceName
    , cbraces $ vcat $ 
        [ "public static class" <+> txt className
        , cbraces $ vcat $ 
            [ "public static string ServerUrl;" 
            ] ++ map generateCSharpMethod reqs
        ]
    ]
    where
    className = "Api"

generateCSharpMethod :: Req -> Doc
generateCSharpMethod req = vcat
    [ ""
    , functionDeclaration
    , functionBody
    ]
    where
    functionDeclaration = hsep
        ["public", "static", "async", "Task<"<>returnType<>">", functionName, argumentList]
    returnType   = txt $ req^.reqReturnType
    functionName = toCSharpFunctionName $ req^.funcName
    argumentList = parens $ hsep $ punctuate comma arguments

    arguments 
        = captureArgs
       ++ map (fromArg . view argName) queryparams
       ++ body
       ++ headerArgs

    fromArg (aname, atype) = txt atype <+> txt aname

    captureArgs
        = map (fromArg . captureArg)
        . filter isCapture
        $ req^.reqUrl.path

    queryparams = req^.reqUrl.queryStr

    body = case req^.reqBody of
        Nothing -> []
        Just ty -> [txt ty <+> "requestBody"]

    headerArgs = map (fromArg . over _1 ("header" <>) . headerArg)
        $ req^.reqHeaders

    toCSharpFunctionName = text . over (ix 0) toUpper . Text.unpack . camelCase

    segments []     = ""
    segments (x:[]) = segmentToStr x True
    segments (x:xs) = segmentToStr x False <> segments xs

    segmentToStr (Segment (Static s)) _ = "/" <> txt s
    segmentToStr (Segment (Cap s)) end = 
        "/\" + Uri.EscapeDataString("
        <> txt (fst s) <> ".ToString())"
        <> " + \""

    headers = map headerToStr $ req^.reqHeaders
    headerToStr h = "client.DefaultRequestHeaders.Add("
        <> doubleQuotes hn <> ", header" <> hn <>");"
        where hn = txt . fst $ headerArg h

    queryMethod = case Text.unpack $ req^.reqMethod of
        "GET"    ->        "GetAsync(" <> params <> ");"
        "POST"   -> "PostAsJsonAsync(" <> params <> ", requestBody);"
        "PUT"    ->  "PutAsJsonAsync(" <> params <> ", requestBody);"
        "DELETE" ->     "DeleteAsync(" <> params <> ");"
        sm       -> error $ "UnknownMethod: " ++ sm

    urlSegments = segments $ req^.reqUrl.path
    url = if urlSegments /= "" then " + " <> doubleQuotes urlSegments else ""
    params = if null queryparams
        then doubleQuotes "" else "\"?" <> foldParams "&" queryparams

    foldParams _ []     = ""
    foldParams _ (x:[]) = paramToStr x True
    foldParams s (x:xs) = paramToStr x False <> s <> foldParams s xs

    paramToStr qarg end = case qarg^.argType of
        Normal -> argPack "="
        Flag   -> "\" + " <> name <> "?" <> doubleQuotes name <> ":\"\""
               <> properEnd end
        -- name <> "="
        List   -> argPack "[]="
        where
        name = txt $ qarg^.argName._1
        argPack i = name <> i <> "\" + Uri.EscapeDataString("
            <> name <> ".ToString())"
            <> properEnd end

    properEnd end = if end then "" else " + \"" 

    -- TODO: make exception handling configurable in options...
    handleExceptions = False
    functionBody = cbraces $ vcat
        [ (if handleExceptions then tryCatch else id) $
            jbraces "using (var client = new HttpClient())" $ vcat
            [ "client.BaseAddress = new Uri(Api.ServerUrl" <> url <> ");"
            , "client.DefaultRequestHeaders.Accept.Clear();"
            , "client.DefaultRequestHeaders.Accept.Add("
            , nest 2 "new MediaTypeWithQualityHeaderValue(\"application/json\"));"
            , vcat headers 
            , ""
            , "HttpResponseMessage resp = await client." <> queryMethod
            , "resp.EnsureSuccessStatusCode();"
            , "return await resp.Content.ReadAsAsync<"<> returnType <>">();"
            ]
        ]

    tryCatch tryBody = vcat
        [ jbraces "try" $ tryBody
        , jbraces "catch (Exception e)" $ vcat
            [ "//TODO: do something on exception..."
            , "//perhaps more specific exceptions, to handle connection problems" 
            , "//and decoding separately, anyway this needs work."
            ]
        ]

-- indentation size
indent = 2

cbraces :: Doc -> Doc
cbraces code = vcat
    [ lbrace
    , ""
    , nest indent code
    , rbrace
    ]

jbraces :: Doc -> Doc -> Doc
jbraces name code = vcat
    [ name <+> lbrace
    , ""
    , nest indent code
    , rbrace
    ]

txt :: Text -> Doc
txt = text . Text.unpack


----------------------------------------------------------------------
-- This should be in Servant.Foreign

class GenerateList reqs where
  generateList :: reqs -> [Req]

instance GenerateList Req where
    generateList r = [r]

instance (GenerateList r, GenerateList rs) => GenerateList (r :<|> rs) where
    generateList (r :<|> rs) = (generateList r) ++ (generateList rs)

listFromAPI :: (HasForeign api, GenerateList (Foreign api)) => Proxy api -> [Req]
listFromAPI p = generateList (csharp p)
