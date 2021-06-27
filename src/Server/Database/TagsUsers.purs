module Server.Database.TagsUsers where

import Type.Proxy (Proxy(..))
import Droplet.Language

type TagsUsers = (
      id :: Auto Int,
      creator :: Int,
      tag :: Int
)

tags_users :: Table "tags_users" TagsUsers
tags_users = Table

_creator :: Proxy "creator"
_creator = Proxy

_tag :: Proxy "tag"
_tag = Proxy