module Types.Issue where

import Import

data IssueContent = IssueContent{title, body :: Text}
    deriving (Show)
