-- | This module is responsible for generating a XML representation of a Rpm.
module System.Rpm.Xml ( executeRpm2Xml ) where

import System.Exit
import System.Process

--------------------------------------------------------------------------------

type QueryTag = String
type QueryTags = [QueryTag]
type Xml = String

-- | 'executeRpm2Xml' takes a path to an RPM package and generates an
-- XML representation in the form of a string.
executeRpm2Xml :: FilePath      -- ^ Path to RPM Package
               -> IO (Maybe String)
executeRpm2Xml file = do
    (statusCode, out, _ ) <- readProcessWithExitCode "rpm" ["-qp", "--qf", queryString queryTags, file] ""
    case statusCode of
      ExitSuccess -> return (Just $ (wrapRpmXml .tail . init) out)
      _           -> return Nothing

--------------------------------------------------------------------------------

-- | 'queryTags' is list of the query tags we wish to extract from our
-- rpm files.
queryTags :: QueryTags
queryTags = [ "NAME"
            , "BUILDHOST"
            , "BUILDTIME"
            , "DESCRIPTION"
            , "RELEASE"
            , "VERSION"
            , "SIZE"
            , "SUMMARY"
            , "URL"
            , "ARCH"
            , "ARCHIVESIZE"]

--------------------------------------------------------------------------------

queryString :: QueryTags -> String
queryString = quote . concatMap (\x -> "[%{" ++ x ++ ":xml}]")
    where quote :: String -> String
          quote xs = "\"" ++ xs ++ "\""

--------------------------------------------------------------------------------

wrapRpmXml :: Xml -> Xml
wrapRpmXml xml = "<rpm>" ++ xml ++ "</rpm>"

--------------------------------------------------------------------------------




