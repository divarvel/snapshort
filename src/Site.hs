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
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy (fromStrict)
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

data Link = Link
    { link_id :: UUID
    , code :: ByteString
    , long_url :: ByteString
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


createNewLink :: Handler App App ()
createNewLink = do
  code <- getPostParam "code"
  long_url <- getPostParam "long_url"
  uuid <- liftIO nextRandom
  newLink <- execute "INSERT INTO url VALUES (?, ?, ?, ?)" (uuid, code, long_url, (0 :: Integer))
  redirect "/"

getAllLinks :: Handler App App ()
getAllLinks = do
  allLinks <- query_ "SELECT * FROM url"
  writeLBS $ encode (allLinks :: [Link])


fetchLinkByUUID :: UUID -> Handler App App (Maybe Link)
fetchLinkByUUID u = listToMaybe <$> query "select * from url where url_id = ?" (Only u)

fetchLinkByCode :: ByteString -> Handler App App (Maybe Link)
fetchLinkByCode t = listToMaybe <$> query "select * from url where code = ?" (Only t)

uuidParam :: Handler App App (Maybe ByteString) -> Handler App App (Maybe UUID)
uuidParam = fmap (fromByteString . fromStrict =<<)

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
    link <- maybe (return Nothing) fetchLinkByCode (code :: Maybe ByteString)
    maybe
        (modifyResponse $ setResponseCode 404)
        (redirect . long_url)
        (link :: Maybe Link)

deleteLink :: Handler App App ()
deleteLink = do
    id <- getParam "id"
    _ <- execute "DELETE FROM url WHERE url_id = ?" (Only id)
    return ()

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",      with auth handleLoginSubmit)
         , ("/logout",     with auth handleLogout)
         , ("/new_user",   with auth handleNewUser)
         , ("/urls",       method GET getAllLinks)
         , ("/urls",       method POST createNewLink)
         , ("/urls/:id",   method GET getLink)
         , ("/urls/:id",   method DELETE deleteLink)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    pg <- nestSnaplet "pg" pg pgsInit
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App pg h s a

