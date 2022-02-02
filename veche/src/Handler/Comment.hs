module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    -- requireCheckJsonBody will parse the request body into the appropriate
    -- type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file)
    comment <- requireCheckJsonBody

    -- The YesodAuth instance in Foundation.hs defines the UserId to be
    -- the type used for authentication.
    currentUserId <- requireAuthId
    let comment' = comment{commentUserId = currentUserId}

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment
