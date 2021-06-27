module Shared.IM.Unread where

import Prelude
import Shared.Types
import Shared.IM.Types
import Data.Foldable as DF
import Shared.Options.File (imageBasePath)

title :: Int -> String
title =
      case _ of
            0 -> defaultTitle
            n -> "(" <> show n <> ") " <> defaultTitle
      where defaultTitle = "MelanChat - Friendly Random Chat"

favicon :: Int -> String
favicon =
      case _ of
            0 -> name <> extension
            n | n <= 10 -> name <> "-" <> show n <> extension
            n -> name <> "-10-plus" <> extension
      where name = imageBasePath <> "favicon"
            extension = ".ico"

countUnreadChats :: Int -> Array Contact -> Int
countUnreadChats id = DF.foldl count 0
      where count total { history }
                  | DF.any isUnread history = total + 1
                  | otherwise = total
            isUnread { sender, status } = status < Read && sender /= id
