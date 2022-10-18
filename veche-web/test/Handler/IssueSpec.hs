{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.IssueSpec (spec) where

import TestImport

-- project
import Stellar.Horizon.Types qualified as Stellar

-- package
import Genesis (mtlAsset)
import Model.Forum (ForumId (ForumKey))
import Model.Issue (Issue (Issue, contacts), Key (IssueKey, IssueVersionKey))
import Model.Issue qualified as Issue
import Model.StellarHolder (StellarHolder (StellarHolder))
import Model.StellarHolder qualified
import Model.Types (Poll (ByMtlAmount))
import Model.User (Key (UserKey))

spec :: Spec
spec =
    withApp $
        it "posts an issue" do
            user <- createUser userKey Nothing
            runDB $ insert_ StellarHolder{asset = mtlAsset, key = Stellar.Address userKey, amount = 7457.92517}
            authenticateAs user

            get $ ForumIssueNewR forumId -- get CSRF token
            statusIs 200

            request do
                setMethod "POST"
                setUrl $ ForumIssuesR forumId
                addRequestHeader ("Accept", "text/plain")
                addTokenFromCookie
                addPostParam "title" "Flight ate"
                addPostParam "body" "Slide weight graph. Such bowl baby."
                addPostParam "poll" "2"
            statusIs 303

            issues <- runDB $ selectList [] []
            let issues' =
                    [ issueE{entityVal = entityVal{Issue.created = nullTime}}
                    | issueE <- issues, let Entity{entityVal} = issueE
                    ]
            issues'
                === [ Entity
                        (IssueKey 1)
                        Issue
                            { author            = UserKey 1
                            , approval          = 0
                            , commentNum        = 0
                            , contacts          = Nothing
                            , created           = nullTime
                            , curVersion        = Just $ IssueVersionKey 1
                            , eventDelivered    = False
                            , forum             = forumId
                            , open              = True
                            , poll              = Just ByMtlAmount
                            , priceOffer        = Nothing
                            , title             = "Flight ate"
                            }
                    ]
  where
    forumId = ForumKey "MTL-HOLDERS"
    userKey = "6b5c3a20-ead1-5157-b5a9-c6de2156c7b2"

nullTime :: UTCTime
nullTime = UTCTime (toEnum 0) 0
