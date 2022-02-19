{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Comment (insert_) where

import Import hiding (insert_)

import Database.Persist qualified as Persist

import Genesis (mtlFund)

insert_ :: User -> Comment -> Handler ()
insert_ User{userStellarAddress} comment =
    runDB do
        Entity signerId _ <- getBy403 $ UniqueMember mtlFund userStellarAddress
        requireAuthz $ AddIssueComment signerId
        Persist.insert_ comment
