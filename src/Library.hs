{-# LANGUAGE ExistentialQuantification, RankNTypes, GADTs, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- Copyright 2017, GRACeFUL project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alexg@chalmers.se
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- A small data type for expressing components.
--
-----------------------------------------------------------------------------

module Library 
    ( module Types
    , Library(..)
    , Item(..)
    ) where

import GL hiding (Item)
import Types
import Utils

import Data.Aeson

type Id  = String
type URL = String

data Library = Library
    { libraryId :: Id
    , items     :: [Item]
    } deriving Show

instance ToJSON Library where
    toJSON (Library n is) = object
        [ "library" .= toJSONList is]

data Item = Item
    { itemId :: Id
    , f      :: TypedValue 
    } deriving Show 

instance ToJSON Item where
    toJSON (Item n (f ::: t)) = object 
        [ "name"       .= n
        , "parameters" .= parameters t
        , "interface"  .= ports t ]

parameters :: Type a -> Value
parameters = toJSONList . rec 
  where
    rec :: Type a -> [Value]
    rec tp = case tp of
        t@(Tag n t1) :-> t2 -> tag t : rec t2
        _                   -> []

tag :: Type a -> Value
tag (Tag n t) = object ["name" .= n, "type" .= show t]
tag _         = Null

ports :: Type a -> [Value]
ports tp = case tp of
    -- base
    Port' t@(Tag _ _) -> [tag t]
    -- recurse
    Tag _ t    -> ports t
    GCM t      -> ports t
    List t     -> ports t 
    Pair t1 t2 -> ports t1 ++ ports t2
    _ :-> t2   -> ports t2
    _          -> []
