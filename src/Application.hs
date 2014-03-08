{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap (get)
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
data App = App
    { _pg :: Snaplet Postgres
    }

makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get

------------------------------------------------------------------------------
type AppHandler = Handler App App

