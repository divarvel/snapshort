{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (unpack, split)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Base64 as B64
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.UUID
import           Data.UUID.V4
import           Control.Monad.IO.Class
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application


creds :: (BS.ByteString, BS.ByteString)
creds = ("IbNPztMgTO", "wy1a3da1A4")

data Link = Link
    { link_id :: UUID
    , code :: T.Text
    , long_url :: T.Text
    , hits :: Integer
    } deriving (Show, Eq)

$(deriveToJSON defaultOptions ''Link)

instance FromJSON UUID where
    parseJSON (String t) = maybe mzero return (fromString (T.unpack t))
    parseJSON _ = mzero

instance ToJSON UUID where
    toJSON = String . T.pack . toString


instance FromRow Link where
    fromRow = Link <$> field <*> field <*> field <*> field


authenticatedAction :: Handler App App () -> Handler App App ()
authenticatedAction a = do
    authData <- getBasicAuth
    case authData of
        Just creds -> a
        Just _ -> do
            modifyResponse $ setResponseCode 403
        _ -> do
            modifyResponse $
                (setResponseCode 401) . (addHeader "WWW-Authenticate" "Basic realm=\"admin\"")


createNewLink :: Handler App App ()
createNewLink = authenticatedAction $ do
  code <- fmap (fmap TE.decodeUtf8) $ getPostParam "code"
  long_url <- fmap (fmap TE.decodeUtf8) $ getPostParam "long_url"
  uuid <- liftIO nextRandom
  newLink <- execute "INSERT INTO url VALUES (?, ?, ?, ?)" (uuid, code, long_url, (0 :: Integer))
  redirect "/"

getAllLinks :: Handler App App ()
getAllLinks = authenticatedAction $ do
  allLinks <- query_ "SELECT * FROM url"
  writeLBS $ encode (allLinks :: [Link])

fetchLinkByUUID :: UUID -> Handler App App (Maybe Link)
fetchLinkByUUID u = listToMaybe <$> query "select * from url where url_id = ?" (Only u)

fetchLinkByCode :: BS.ByteString -> Handler App App (Maybe Link)
fetchLinkByCode t = listToMaybe <$> query "select * from url where code = ?" (Only t)

uuidParam :: Handler App App (Maybe BS.ByteString) -> Handler App App (Maybe UUID)
uuidParam = fmap (fromASCIIBytes =<<)

getLink :: Handler App App ()
getLink = do
    id <- uuidParam $ getParam "id"
    link <- maybe (return Nothing) fetchLinkByUUID (id :: Maybe UUID)
    maybe
        (modifyResponse $ setResponseCode 404)
        (writeLBS . encode)
        (link :: Maybe Link)

redirectLink :: Handler App App ()
redirectLink = do
    code <- getParam "code"
    link <- maybe (return Nothing) fetchLinkByCode (code :: Maybe BS.ByteString)
    maybe
        (modifyResponse $ setResponseCode 404)
        (redirect . TE.encodeUtf8 . long_url)
        (link :: Maybe Link)

deleteLink :: Handler App App ()
deleteLink = authenticatedAction $ do
    id <- getParam "id"
    _ <- execute "DELETE FROM url WHERE url_id = ?" (Only id)
    return ()


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [ ("/urls",       method GET getAllLinks)
         , ("/urls",       method POST createNewLink)
         , ("/urls/:id",   method GET getLink)
         , ("/urls/:id",   method DELETE deleteLink)
         , ("/:code",      method GET redirectLink)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    pg <- nestSnaplet "pg" pg pgsInit
    addRoutes routes
    return $ App pg


parseBasicAuth :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseBasicAuth h = let
    (authScheme, encoded) = BS.splitAt 6 h
    in
        if authScheme == "Basic " then
            let
                decoded = B64.decode encoded
                components = fmap (split ':') decoded
                tup c = case c of
                    (x:y:_) -> Just (x, y)
                    _ -> Nothing
            in (either (const Nothing) Just components) >>= tup
        else
            Nothing

getBasicAuth :: MonadSnap m => m (Maybe (BS.ByteString, BS.ByteString))
getBasicAuth = withRequest (\r -> return $ getHeader "Authorization" r >>= parseBasicAuth)
