{-| 

   This module is mainly for internal purposes and these functions
   should not be called directly.
   --  Remove 'testName'
-}

module System.Rpm.Info 
    ( RpmInfo(..)
    , rpmInfoFromFile
    , rpmInfoFromXml
    ) where

import Text.XML.HaXml hiding (version)
import Text.XML.HaXml.Posn (Posn)

import System.Rpm.Xml (executeRpm2Xml)

data RpmInfo = RpmI {
      name        :: String
    , buildHost   :: String
    , buildTime   :: Integer 
    , description :: String
    , summary     :: String
    , size        :: Integer
    , version     :: String
    , release     :: String
    } deriving(Show, Eq)

--------------------------------------------------------------------------------

runFilter :: CFilter i    -- ^ Function like 'getName'
          -> [Content i]  -- ^ Usually the children of the root element.
          -> [Maybe String] -- ^ The extracted text, if any.

runFilter f  = map extractText . concatMap f


{-| 'rpmFilter' is a function that creates a 'CFilter' that matches elements similar to:
  
  @\<rpmTag name=QueryTag>

     \<TagType>\</TagType>

   \</rpmTag>@
   -}
rpmFilter :: String  -- ^ Tag Name
          -> String  -- ^ Tag Type
          -> CFilter i
rpmFilter tagName tagType c = (txt `o` children `o` tag tagType `o` children `o` attrval att `o` tag "rpmTag") c
    where
      att :: Attribute
      att = ("name", AttValue [Left tagName])


{-| 
    getName is a filter for extracting string elements from tags with
    Name attributes.  The other functions are typically the same
    unless otherwise noted. -}
getName :: CFilter i
getName = rpmFilter "Name" "string" 


getBuildHost :: CFilter i
getBuildHost = rpmFilter "Buildhost" "string"


getBuildTime :: CFilter i
getBuildTime = rpmFilter "Buildtime" "integer"


getDescription :: CFilter i
getDescription = rpmFilter "Description" "string"


getSummary :: CFilter i
getSummary = rpmFilter "Summary" "string"


getSize :: CFilter i
getSize = rpmFilter "Size" "integer"


getVersion :: CFilter i
getVersion = rpmFilter "Version" "string"


getRelease :: CFilter i
getRelease = rpmFilter "Release" "string"


getURL :: CFilter i
getURL = rpmFilter "Url" "string"


getArch :: CFilter i
getArch = rpmFilter "Arch" "string"


getArchiveSize :: CFilter i
getArchiveSize = rpmFilter "Archivesize" "integer"

--------------------------------------------------------------------------------
getRpmInfo :: [Content i] 
           -> Maybe RpmInfo

getRpmInfo cs = do
    n  <- extract getName
    s  <- extract getSummary
    d  <- extract getDescription
    bh <- extract getBuildHost
    bt <- extract getBuildTime
    sz <- extract getSize
    v  <- extract getVersion
    r  <- extract getRelease
    return RpmI { name        = n
                , summary     = s
                , description = d
                , buildHost   = bh
                , size        = read sz :: Integer
                , buildTime   = read bt :: Integer
                , version     = v
                , release     = r }
    where
      extract f = let results = runFilter f cs in
                    if null results
                      then Nothing
                      else head results


rpmInfoFromFile :: FilePath     -- ^ Path to a Rpm
                -> IO (Maybe RpmInfo)

rpmInfoFromFile file = do
    contents <- executeRpm2Xml file
    case contents of
      Just xml -> return (rpmInfoFromXml xml)
      Nothing -> return Nothing


rpmInfoFromXml :: String  -- ^ A string of XML containing information about a package.
               -> Maybe RpmInfo

rpmInfoFromXml xml = getRpmInfo (content xml)
    where content file = extractContent ( getRootElement file file)                   


getRootElement :: String        -- ^ Filename to report parsing errors for.
               -> String        -- ^ String of XML Content
               -> Element Posn
getRootElement fileName xml = elt
    where Document _ _ elt _ =  xmlParse xml fileName 

extractContent :: Element i -> [Content i]
extractContent (Elem _ _ c )  = c

extractText :: Content i -> Maybe String
extractText e = case e of
                  CString _ n _ -> Just n
                  _             -> Nothing






