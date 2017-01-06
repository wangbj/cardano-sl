{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Major logic of Update System (US).

module Pos.Update.Logic
       ( usApplyBlocks
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Control.Lens         ((^.))
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Formatting           (sformat, (%))
import           Universum

import           Pos.Constants        (updateProposalThreshold, updateVoteThreshold)
import qualified Pos.DB               as DB
import           Pos.Types            (Block, Coin, NEBlocks, UpdateProposal (..),
                                       UpdateVote (..), addressHash, applyCoinPortion,
                                       coinF, gbExtra, mebUpdate, mebUpdateVotes, mkCoin,
                                       prevBlockL, unsafeAddCoin)
import           Pos.Update.Error     (USError (..))
import           Pos.Util             (inAssertMode, _neHead)
import           Pos.WorkMode         (WorkMode)

-- | Apply chain of /definitely/ valid blocks to US part of GState
-- DB and to US local data. Head must be the __oldest__ block.
usApplyBlocks :: WorkMode ssc m => NEBlocks ssc -> m [DB.SomeBatchOp]
usApplyBlocks blocks = do
    tip <- DB.getTip
    when (tip /= blocks ^. _neHead . prevBlockL) $ throwM $
        USCantApplyBlocks "oldest block in NEBlocks is not based on tip"
    inAssertMode $
        do verdict <- usVerifyBlocks blocks
           case verdict of
               Right _ -> pass
               Left errors ->
                   panic $ "usVerifyBlocks failed: " <> errors
    concat <$> mapM usApplyBlock (toList blocks)

usApplyBlock :: WorkMode ssc m => Block ssc -> m [DB.SomeBatchOp]
usApplyBlock (Left _) = pure []
usApplyBlock (Right blk) = do
    let meb = blk ^. gbExtra
    let votes = meb ^. mebUpdateVotes
    let proposal = meb ^. mebUpdate
    undefined

-- | Revert application of given blocks to US part of GState DB
-- and US local data. Head must be the __youngest__ block. Caller must
-- ensure that tip stored in DB is 'headerHash' of head.
--
-- FIXME: return Batch.
usRollbackBlocks :: WorkMode ssc m => NEBlocks ssc -> m ()
usRollbackBlocks _ = pass

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks,
-- they are assumed to be done earlier.
--
-- TODO: add more checks! Most likely it should be stateful!
usVerifyBlocks :: WorkMode ssc m => NEBlocks ssc -> m (Either Text ())
usVerifyBlocks = runExceptT . mapM_ verifyBlock

verifyBlock :: WorkMode ssc m => Block ssc -> ExceptT Text m ()
verifyBlock (Left _)    = pass
verifyBlock (Right blk) = do
    let meb = blk ^. gbExtra
    verifyEnoughStake (meb ^. mebUpdateVotes) (meb ^. mebUpdate)

verifyEnoughStake
    :: forall ssc m.
       WorkMode ssc m
    => [UpdateVote] -> Maybe UpdateProposal -> ExceptT Text m ()
verifyEnoughStake votes mProposal = do
    -- [CSL-314] Snapshot must be used here.
    totalStake <- maybe (pure zero) (const DB.getTotalFtsStake) mProposal
    let proposalThreshold = applyCoinPortion totalStake updateProposalThreshold
    let voteThreshold = applyCoinPortion totalStake updateVoteThreshold
    totalVotedStake <- verifyUpdProposalDo voteThreshold votes
    when (totalVotedStake < proposalThreshold) $
        throwError (msgProposal totalVotedStake proposalThreshold)
  where
    zero = mkCoin 0
    msgProposal =
        sformat
            ("update proposal doesn't have votes from enough stake ("
             %coinF%" < "%coinF%
             ")")
    msgVote =
        sformat
            ("update vote issuer doesn't have enough stake ("
             %coinF%" < "%coinF%
             ")")
    isVoteForProposal UpdateVote {..} =
        case mProposal of
            Nothing                    -> True
            Just (UpdateProposal {..}) -> uvDecision && uvSoftware == upSoftwareVersion
    verifyUpdProposalDo :: Coin -> [UpdateVote] -> ExceptT Text m Coin
    verifyUpdProposalDo _ [] = pure zero
    verifyUpdProposalDo voteThreshold (v@UpdateVote {..}:vs) = do
        let id = addressHash uvKey
        -- FIXME: use stake corresponding to state right before block
        -- corresponding to UpdateProposal for which vote is given is
        -- applied.
        stake <- fromMaybe zero <$> DB.getFtsStake id
        when (stake < voteThreshold) $ throwError $ msgVote stake voteThreshold
        let addedStake = if isVoteForProposal v then stake else zero
        unsafeAddCoin addedStake <$> verifyUpdProposalDo voteThreshold vs
