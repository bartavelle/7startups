{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Xmpp.Backend where

import Startups.Base
import Startups.Cards
import Startups.PrettyPrint
import Startups.GameTypes (showPlayerId,PlayerId)
import Backends.Hub

import Network
import Network.Xmpp as XMPP
import qualified Network.Xmpp.Internal as XMPP
import qualified Network.TLS as TLS
import Control.Monad
import Control.Applicative
import Data.XML.Types
import Data.Monoid
import Data.Maybe (fromJust,fromMaybe)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Attoparsec.Text as A
import Data.Char (isSpace,isAlphaNum)
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Y
import System.Directory (getAppUserDataDirectory,createDirectoryIfMissing)

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

toNode :: PrettyDoc -> [Node]
toNode PEmpty = []
toNode (PCat a b) = toNode a <> toNode b
toNode (RawText s) = [NodeContent (ContentText s)]
toNode NewLine = [NodeElement $ Element (Name "br" (Just "http://www.w3.org/1999/xhtml") Nothing) [] []]
toNode Space = [NodeContent (ContentText " ")]
toNode (Emph n) = [withBold (toNode n)]
toNode (Colorize c n) = [addColor (toColor c) n]
toNode (Indent n d) = concatMap toNode (replicate n Space) <> toNode d
toNode (PDirection (Neighboring n)) = toNode (PNeighbor n)
toNode (PNeighbor NRight) = toNode "▶"
toNode (PNeighbor NLeft) = toNode "◀"
toNode (PDirection Own) = toNode "⇓"
toNode (PFund (Funding 0)) = []
toNode (PFund (Funding n)) = [addColor "#C9C90C" (numerical n <> "$")]
toNode (PPoach (Poacher 0)) = []
toNode (PPoach (Poacher s)) = return $ addColor "#c43131" $ if s > 5
                                                        then numerical s <> "⚔"
                                                        else mconcat (replicate (fromIntegral s) "⚔")
toNode (PVictory vp) = toNode (numerical vp)
toNode (PPlayerCount vp) = toNode (numerical vp)
toNode (PTurn vp) = toNode (numerical vp)
toNode (PResource Marketing)     = [addColor "#008F0C" "M"]
toNode (PResource Operations)   = [addColor "#BABABA" "O"]
toNode (PResource Finance) = [addColor "#373737" "F"]
toNode (PResource Development)   = [addColor "#FF893D" "D"]
toNode (PResource Adoption)  = [addColor "#D69469" "A"]
toNode (PResource Vision)   = [addColor "#F700FF" "V"]
toNode (PResource Hype)    = [addColor "#2028FA" "H"]
toNode (PAge Age1) = toNode "Ⅰ"
toNode (PAge Age2) = toNode "Ⅱ"
toNode (PAge Age3) = toNode "Ⅲ"
toNode (PCompanyStage Project) = toNode "."
toNode (PCompanyStage Stage1) = toNode "_"
toNode (PCompanyStage Stage2) = toNode "="
toNode (PCompanyStage Stage3) = toNode "Δ"
toNode (PCompanyStage Stage4) = toNode "☥"
toNode (PConflict Defeat) = [addColor "#c43131" "-1"]
toNode (PConflict (Victory Age1)) = [addColor "#c43131" "+1"]
toNode (PConflict (Victory Age2)) = [addColor "#c43131" "+3"]
toNode (PConflict (Victory Age3)) = [addColor "#c43131" "+5"]
toNode (PCardType t) = [addColor (toColor (PColorCard t)) "🃏"]
toNode (PResearch s) = [addColor "#00B506" c]
    where
        c = case s of
                CustomSolution    -> "⚡"
                Programming -> "λ"
                Scaling   -> "⚖"
toNode (PCompany (CompanyProfile c s)) = toNode (RawText (T.pack (show c <> show s)))

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
    forM_ (splitLines cnt) $ \ln -> sendMessage (message { messagePayload = mkParagraph ln, messageTo = Just mjid, messageType = msgt }) sess

data LocalCommand = Register T.Text
                  | Help
                  deriving Show

parseContent :: T.Text -> Either LocalCommand PlayerInput
parseContent t = case parseOnly ((Left <$> cmdparser <|> Right <$> cntparser) <* endOfInput) t of
                     Right x -> x
                     _ -> Right (CustomCommand t)
    where
    lexer p = p <* A.skipWhile isSpace
    cmdparser :: Parser LocalCommand
    cmdparser = register <|> help
    register  = Register <$> (lexer (string "!register") *> A.takeWhile isAlphaNum)
    help      = string "!help" *> pure Help
    cntparser :: Parser PlayerInput
    cntparser = start <|> stop <|> joing <|> go <|> notgo <|> leave <|> numchoice <|> mystart <|> details <|> detail
    start     = Start         <$> (lexer (string "!start") *> decimal)
    stop      = Stop          <$> (lexer (string "!stop")  *> decimal)
    joing     = Join          <$> (lexer (string "!ready") *> optional decimal)
    numchoice = NumericChoice <$> decimal
    go        = string "!go"      *> pure Go
    notgo     = string "!notgo"   *> pure NotGo
    leave     = string "!leave"   *> pure Leave
    mystart   = string "!info"    *> pure MyStartup
    detail    = string "!detail"  *> pure ShortSituation
    details   = string "!details" *> pure DetailedSituation

loadAssoc :: IO (M.Map Jid PlayerId)
loadAssoc = do
    d <- getAppUserDataDirectory "7startups"
    mlist <- Y.decodeFileEither (d <> "/xmppassoc") :: IO (Either Y.ParseException (M.Map T.Text PlayerId))
    let wjid :: Either Y.ParseException [(Maybe Jid, PlayerId)]
        wjid = mlist    & _Right %~ M.toList
                        & _Right . traverse . _1 %~ jidFromText
        flst :: Maybe [(Jid, PlayerId)]
        flst = sequenceOf (traverse . _1) . toListOf (_Right . traverse) $ wjid
    return $ M.fromList (fromMaybe mempty flst)

saveAssoc :: M.Map Jid PlayerId -> IO ()
saveAssoc m = do
    d <- getAppUserDataDirectory "7startups"
    createDirectoryIfMissing False d
    Y.encodeFile (d <> "/xmppassoc") (M.mapKeys jidToText m)

-- | This will create and register an XMPP backend. There is no error
-- handling whatsoever.
runXmpp :: HostName     -- ^ Domain
        -> String       -- ^ Server name
        -> PortNumber   -- ^ Port number
        -> T.Text       -- ^ username
        -> T.Text       -- ^ password
        -> T.Text       -- ^ Name of the conference room
        -> IO Backend
runXmpp domain servername port username password confroom = do
    let auth Secured = [scramSha1 username Nothing password, plain username Nothing password]
        auth x = error (show x)
        streamConf' = def { connectionDetails = UseHost servername port, tlsBehaviour = XMPP.PreferPlain }
        streamConf = streamConf' { tlsParams = (tlsParams streamConf') { TLS.clientHooks = def { TLS.onServerCertificate = \_ _ _ _ -> return [] }  } }
        sessconf = def { sessionStreamConfiguration = streamConf, enableRoster = True }
        rconfroom = RChat (fromJust (jidFromText confroom))
    sess <- session domain (Just (auth, Nothing)) sessconf >>= \x -> case x of
        Left rr -> error (show rr)
        Right s -> return s
    let myjid = fromJust $ jidFromText (confroom <> "/" <> username)
    void $ sendPresence (presenceOnline { presenceTo = Just myjid }) sess
    sendTextContent sess rconfroom ("Bot ready, type" <+> emph "!ready" <+> "to start playing.")
    input  <- newTChanIO
    playerAssoc <- loadAssoc >>= newTVarIO
    -- run the loop that listens to messages, forever
    void $ forkIO $ forever $ getMessage sess >>= \msg ->
        let isBody e = nameLocalName (elementName e) == "body"
            content = case filter isBody (messagePayload msg) of
                          (Element _ _ [NodeContent (ContentText t)] : _) -> Just (parseContent t)
                          _ -> Nothing
        in  case (,) <$> content <*> messageFrom msg of
                Nothing -> return ()
                Just (m, pjid) -> unless (pjid == myjid) $ do
                    print (pjid, m)
                    case m of
                        Right cnt -> atomically $ do
                            pa <- readTVar playerAssoc
                            case pa ^. at pjid of
                                Just name -> writeTChan input (name, cnt)
                                Nothing -> return ()
                        Left Help -> sendTextContent sess (RUser pjid) "Available commands: !help !register !start !stop !ready !go !notgo !leave !info !detail !details !bot"
                        Left (Register ename) -> join $ atomically $ do
                            let domsg = sendTextContent sess (RUser pjid)
                                name = if T.null ename
                                         then case jidToTexts pjid of
                                                (_,_,Just n) -> n
                                                _ -> jidToText pjid
                                         else ename
                            pa <- readTVar playerAssoc
                            case pa ^. at pjid of
                                Just oldname -> return $ domsg (showPlayerId oldname <+> "is already registered to this Jid")
                                Nothing -> do
                                    let npa = pa & at pjid ?~ name
                                    writeTVar playerAssoc npa
                                    return (saveAssoc npa >> domsg "You are now registered")
    let tell pid msg = readTVarIO playerAssoc >>= \pa -> case M.keys (M.filter (==pid) pa) of
                                                             [] -> broadcast ("Can't find" <+> showPlayerId pid <+> "to tell him" <+> msg)
                                                             (pjid:_) -> sendTextContent sess (RUser pjid) msg
        broadcast = sendTextContent sess rconfroom
    return (Backend (readTChan input) tell broadcast (const (return ())))
