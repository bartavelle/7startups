{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Backends.XMPP where

import Startups.Base
import Startups.Cards
import Startups.PrettyPrint
import Backends.Hub
import Backends.IMLike

import Network
import Network.Xmpp as XMPP
import Control.Monad
import Data.XML.Types
import Data.Monoid
import Data.Maybe (fromJust)
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP

toNode :: PrettyDoc -> [Node]
toNode = concatMap toNode' . splitLines

withBold :: [Node] -> Node
withBold = NodeElement . Element (Name "span" (Just "http://www.w3.org/1999/xhtml") Nothing) [(Name "style" Nothing Nothing,[ContentText "font-weight: bold;"])]

toColor :: PColor -> T.Text
toColor (PColorCard c) = case c of
    BaseResource        -> "#8A5736"
    AdvancedResource    -> "#9C9C9C"
    Infrastructure      -> "#0F07ED"
    ResearchDevelopment -> "#2fa34a"
    Commercial          -> "#C9C90C"
    HeadHunting         -> "#E30000"
    Community           -> "#C429BF"
toColor (PColorVictory v) = case v of
    PoachingVictory       -> "#FA2020"
    FundingVictory        -> "#FFA600"
    CompanyVictory        -> "#000000"
    InfrastructureVictory -> "#2600FF"
    RnDVictory            -> "#00B506"
    CommercialVictory     -> "#B5A900"
    CommunityVictory      -> "#B5009D"

toNode' :: PrettyDoc -> [Node]
toNode' PEmpty = []
toNode' (PCat a b) = toNode' a ++ toNode' b
toNode' (RawText s) = [NodeContent (ContentText s)]
toNode' NewLine = [NodeElement $ Element (Name "br" (Just "http://www.w3.org/1999/xhtml") Nothing) [] []]
toNode' Space = [NodeContent (ContentText " ")]
toNode' (Emph n) = [withBold (toNode n)]
toNode' (Colorize c n) = [addColor (toColor c) n]
toNode' (Indent n d) = concatMap toNode' (replicate n Space) <> toNode' d
toNode' (PDirection (Neighboring n)) = toNode' (PNeighbor n)
toNode' (PNeighbor NRight) = toNode' "▶"
toNode' (PNeighbor NLeft) = toNode' "◀"
toNode' (PDirection Own) = toNode' "⇓"
toNode' (PFund (Funding 0)) = []
toNode' (PFund (Funding n)) = [addColor "#C9C90C" (numerical n <> "$")]
toNode' (PPoach (Poacher 0)) = []
toNode' (PPoach (Poacher s)) = return $ addColor "#c43131" $ if s > 5
                                                        then numerical s <> "⚔"
                                                        else mconcat (replicate (fromIntegral s) "⚔")
toNode' (PVictory vp) = toNode' (numerical vp)
toNode' (PPlayerCount vp) = toNode' (numerical vp)
toNode' (PTurn vp) = toNode' (numerical vp)
toNode' (PResource Marketing)     = [addColor "#008F0C" "M"]
toNode' (PResource Operations)   = [addColor "#BABABA" "O"]
toNode' (PResource Finance) = [addColor "#373737" "F"]
toNode' (PResource Development)   = [addColor "#FF893D" "D"]
toNode' (PResource Adoption)  = [addColor "#D69469" "A"]
toNode' (PResource Vision)   = [addColor "#F700FF" "V"]
toNode' (PResource Youthfulness)    = [addColor "#2028FA" "Y"]
toNode' (PAge Age1) = toNode' "Ⅰ"
toNode' (PAge Age2) = toNode' "Ⅱ"
toNode' (PAge Age3) = toNode' "Ⅲ"
toNode' (PCompanyStage Project) = toNode' "."
toNode' (PCompanyStage Stage1) = toNode' "_"
toNode' (PCompanyStage Stage2) = toNode' "="
toNode' (PCompanyStage Stage3) = toNode' "Δ"
toNode' (PCompanyStage Stage4) = toNode' "☥"
toNode' (PConflict Defeat) = [addColor "#c43131" "-1"]
toNode' (PConflict (Victory Age1)) = [addColor "#c43131" "+1"]
toNode' (PConflict (Victory Age2)) = [addColor "#c43131" "+3"]
toNode' (PConflict (Victory Age3)) = [addColor "#c43131" "+5"]
toNode' (PCardType t) = [addColor (toColor (PColorCard t)) "🃏"]
toNode' (PResearch s) = [addColor "#00B506" c]
    where
        c = case s of
                CustomSolution    -> "⚡"
                Programming -> "λ"
                Scaling   -> "⚖"
toNode' (PCompany (CompanyProfile c s)) = toNode' (RawText (T.pack (show c <> show s)))

addColor :: T.Text -> PrettyDoc -> Node
addColor c = NodeElement . Element (Name "span" (Just "http://www.w3.org/1999/xhtml") Nothing) [(Name "style" Nothing Nothing,[ContentText ("color: " <> c <> ";")])] . toNode


withFont :: T.Text -> [Node] -> Node
withFont f = NodeElement . Element (Name "span" (Just "http://www.w3.org/1999/xhtml") Nothing) [(Name "style" Nothing Nothing,[ContentText ("font-family: " <> f <> ";")])]


toText :: PrettyDoc -> T.Text
toText = T.pack . flip PP.displayS "" . PP.renderCompact . PP.pretty

mkParagraph :: PrettyDoc -> [Element]
mkParagraph x = [ Element (Name "active" (Just "http://jabber.org/protocol/chatstates") Nothing) [] []
                , Element (Name "body" (Just "jabber:client") Nothing) [] [NodeContent (ContentText (toText x) )]
                , Element (Name "html" (Just "http://jabber.org/protocol/xhtml-im") Nothing) []
                    [NodeElement (Element (Name "body" (Just "http://www.w3.org/1999/xhtml") Nothing) []
                                 [ NodeElement (Element (Name "p" (Just "http://www.w3.org/1999/xhtml") Nothing) [] (toNode x))])]]

data MRecipient = RUser Jid
                | RChat Jid

sendTextContent :: Session -> MRecipient -> PrettyDoc -> IO ()
sendTextContent sess rec cnt = do
    let (mjid, msgt) = case rec of
                           RUser j -> (j, Chat)
                           RChat j -> (j, GroupChat)
    print (PP.pretty cnt)
    o <- sendMessage (message { messagePayload = mkParagraph cnt, messageTo = Just mjid, messageType = msgt }) sess
    case o of
        Left rr -> print rr
        Right () -> return ()

answerTextContent :: Session -> XMPP.Message -> PrettyDoc -> IO ()
answerTextContent sess msg cnt =
    case answerMessage msg (mkParagraph cnt) of
        Just m -> do
            o <- sendMessage m sess
            case o of
                Left rr -> print rr
                Right () -> return ()
        Nothing -> error ("Error: could not answer to " ++ show msg ++ ": \n" ++ show (PP.pretty (pe cnt)))

extractName :: Jid -> T.Text
extractName j = case jidToTexts j of
                    (Just _,_, Just t) -> t
                    _ -> T.pack (show j)

-- | This will create and register an XMPP backend. There is no error
-- handling whatsoever.
runXmpp :: HS
        -> HostName     -- ^ Domain
        -> String       -- ^ Server name
        -> PortNumber   -- ^ Port number
        -> T.Text       -- ^ username
        -> T.Text       -- ^ password
        -> T.Text       -- ^ Name of the conference room
        -> IO ()
runXmpp hs domain servername port username password confroom = do
    let auth Secured = [scramSha1 username Nothing password, plain username Nothing password]
        auth x = error (show x)
        streamConf = def { connectionDetails = UseHost servername (PortNumber port) }
        sessconf = def { sessionStreamConfiguration = streamConf, enableRoster = True }
        rconfroom = RChat (fromJust (jidFromText confroom))
    sess <- session domain (Just (auth, Nothing)) sessconf >>= \x -> case x of
        Left rr -> error (show rr)
        Right s -> return s
    void $ sendPresence (presenceOnline { presenceTo = jidFromText (confroom <> "/" <> username) }) sess
    sendTextContent sess rconfroom ("Bot ready, type" <+> emph "!ready" <+> "to start playing.")
    input <- newTChanIO
    output <- newTChanIO
    let backendid = "XMPP " <> T.pack servername <> "/" <> username
    -- run our IM backend
    backend <- runIMLike hs (pe backendid) backendid input output
    atomically (registerBackend hs backend)
    -- and run the loop that listens to messages, forever
    forever $ getMessage sess >>= \msg -> do
        let isBody e = nameLocalName (elementName e) == "body"
            content = case filter isBody (messagePayload msg) of
                          (Element _ _ [NodeContent (ContentText t)] : _) -> Just t
                          _ -> Nothing
        case (content, messageFrom msg) of
            (Just cnt, Just pjid) -> atomically (writeTChan input (extractName pjid, cnt))
            _ -> return ()
