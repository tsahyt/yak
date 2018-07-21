-- | All response codes as types, as defined by the current Ircv3 spec
-- <https://modern.ircdocs.horse/>. Please note that the argument parsing
-- in many of these is crude and the text is parsed only as a catch-all
-- 'Message' type, since there may be difference between implementations on the
-- server side, or even per network! In some places, responses have been
-- hard-coded as defined in the Irc in order to extract necessary information.
-- If you disagree with these choices, pull requests are welcome.
--
-- Also note that you probably want to import this module qualified, or import
-- just what you need, as it exports 93 type synonyms and many more lenses.
--
-- In case your use case allows more precise parsing, or you need server
-- specific response codes not listed, you can always hide the relevant types
-- and define your own by simply building the appropriate type synonym.
--
-- **NOTE:** Because of size and a lack of test cases, this module is largely
-- untested. Here be dragons!
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Network.Yak.Responses
where

import Control.Lens
import Data.Text (Text)
import Data.Void (Void)
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock.POSIX
import Network.Yak.Types
import Network.Yak.Modes
import Network.Yak.TH

type Client = Username

-- | > "<client> :Welcome to the <networkname> Network, <nick>[!<user>@<host>]"
type RplWelcome = Msg "001" '[Client, Message]
makeMsgLenses ''RplWelcome ["client", "message"]

-- | > "<client> :Your host is <servername>, running version <version>"
type RplYourhost = Msg "002" '[Client, Message]
makeMsgLenses ''RplYourhost ["client", "message"]

-- | > "<client> :This server was created <datetime>"
type RplCreated = Msg "003" '[Client, Message]
makeMsgLenses ''RplCreated ["client", "message"]

-- | > "<client> <servername> <version> <available user modes> 
--   > <available channel modes> [<channel modes with a parameter>]"
type RplMyinfo = Msg "004" '[Client, Hostname, Text, Modes, Modes, Maybe Modes]
makeMsgLenses ''RplMyinfo ["client", "server", "version", "umodes", "cmodes"
                          ,"cmodesParam"]

-- | > "<client> <1-13 tokens> :are supported by this server"
type RplIsupport = Msg "005" '[Client, SList Token, Message]
makeMsgLenses ''RplIsupport ["client", "tokens", "message"]

-- | > "<client> <hostname> <port> :<info>"
type RplBounce = Msg "010" '[Client, Hostname, Int, Message]
makeMsgLenses ''RplBounce ["client", "hostname", "port", "message"]

-- | > "<client> <user modes>"
type RplUmodeis = Msg "221" '[Client, Modes]
makeMsgLenses ''RplUmodeis ["client", "umodes"]

-- | > "<client> :There are <u> users and <i> invisible on <s> servers"
type RplLuserclient = Msg "251" '[Client, Message]
makeMsgLenses ''RplLuserclient ["client", "message"]

-- | > "<client> <ops> :operator(s) online"
type RplLuserop = Msg "252" '[Client, Int, Message]
makeMsgLenses ''RplLuserop ["client", "ops", "message"]

-- | > "<client> <connections> :unknown connection(s)"
type RplLuserunknown = Msg "253" '[Client, Int, Message]
makeMsgLenses ''RplLuserunknown ["client", "connections", "message"]

-- | > "<client> <channels> :channels formed"
type RplLuserchannels = Msg "254" '[Client, Int, Message]
makeMsgLenses ''RplLuserchannels ["client", "channels", "message"]

-- | > "<client> :I have <c> clients and <s> servers"
type RplLuserme = Msg "255" '[Client, Message]
makeMsgLenses ''RplLuserme ["client", "message"]

-- | > "<client> [<server>] :Administrative info"
type RplAdminme = Msg "256" '[Client, Maybe Hostname, Message]
makeMsgLenses ''RplAdminme ["client", "server", "message"]

-- | > "<client> :<info>"
type RplAdminemail = Msg "259" '[Client, Message]
makeMsgLenses ''RplAdminemail ["client", "message"]

-- | > "<client> <command> :Please wait a while and try again."
type RplTryagain = Msg "263" '[Client, Text, Message]
makeMsgLenses ''RplTryagain ["client", "command", "message"]

-- | > "<client> [<u> <m>] :Current local users <u>, max <m>"
type RplLocalusers = Msg "265" '[Client, Maybe (Int, Int), Message]
makeMsgLenses ''RplLocalusers ["client", "users", "message"]

rplLocalusersCurrent :: Traversal' RplLocalusers Int
rplLocalusersCurrent = rplLocalusersUsers . _Just . _1

rplLocalusersMaximum :: Traversal' RplLocalusers Int
rplLocalusersMaximum = rplLocalusersUsers . _Just . _2

-- | > "<client> [<u> <m>] :Current global users <u>, max <m>"
type RplGlobalusers = Msg "266" '[Client, Maybe (Int, Int), Message]
makeMsgLenses ''RplGlobalusers ["client", "users", "message"]

rplGlobalusersCurrent :: Traversal' RplGlobalusers Int
rplGlobalusersCurrent = rplGlobalusersUsers . _Just . _1

rplGlobalusersMaximum :: Traversal' RplGlobalusers Int
rplGlobalusersMaximum = rplGlobalusersUsers . _Just . _2

-- | > "<client> <nick> :has client certificate fingerprint <fingerprint>"
type RplWhoiscertfp = Msg "276" '[Client, Nickname, Message]
makeMsgLenses ''RplWhoiscertfp ["client", "nick", "message"]

-- | Undefined format
type RplNone = Msg "300" '[Void]

-- | > "<client> <nick> :<message>"
type RplAway = Msg "301" '[Client, Nickname, Message]
makeMsgLenses ''RplAway ["client", "nick", "message"]

-- | > "<client> :[<reply>{ <reply>}]"
type RplUserhost = Msg "302" '[Client, CList UReply]
makeMsgLenses ''RplUserhost ["client", "replies"]

-- | > "<client> :[<nickname>{ <nickname>}]"
type RplIson = Msg "303" '[Client, CList Nickname]
makeMsgLenses ''RplIson ["client", "nicks"]

-- | > "<client> :You are no longer marked as being away"
type RplUnaway = Msg "305" '[Client, Message]
makeMsgLenses ''RplUnaway ["client", "message"]

-- | > "<client> :You have been marked as being away"
type RplNowaway = Msg "306" '[Client, Message]
makeMsgLenses ''RplNowaway ["client", "message"]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhoisuser = Msg "311" '[Client, Nickname, Username, Hostname
                               ,Unused "*", Message]
makeMsgLenses ''RplWhoisuser ["client", "nick", "username", "host"
                             ,"unused", "message"]

-- | > "<client> <nick> <server> :<server info>"
type RplWhoisserver = Msg "312" '[Client, Nickname, Hostname, Message]
makeMsgLenses ''RplWhoisserver ["client", "nick", "server", "message"]

-- | > "<client> <nick> :is an Irc operator"
type RplWhoisoperator = Msg "313" '[Client, Nickname, Message]
makeMsgLenses ''RplWhoisoperator ["client", "nick", "message"]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhowasuser = Msg "314" '[Client, Nickname, Username, Hostname
                                ,Unused "*", Message]
makeMsgLenses ''RplWhowasuser ["client", "nick", "username", "host"
                              ,"unused", "message"]

-- | > "<client> <nick> <secs> [<signon>] :seconds idle, signon time"
type RplWhoisidle = Msg "317" '[Client, Nickname, Int, Maybe POSIXTime, Message]
makeMsgLenses ''RplWhoisidle ["client", "nick", "secs", "signon", "message"]

-- | > "<client> <nick> :End of /Whois list"
type RplEndofwhois = Msg "318" '[Client, Nickname, Message]
makeMsgLenses ''RplEndofwhois ["client", "nick", "message"]

-- | > "<client> <nick> :[prefix]<channel>{ [prefix]<channel>}"
type RplWhoischannels = Msg "319" '[Client, Nickname, CList (Member Channel)]
makeMsgLenses ''RplWhoischannels ["client", "nick", "channels"]

-- | > "<client> Channel :Users  Name"
type RplListstart = Msg "321" '[Client]
makeMsgLenses ''RplListstart ["client"]

-- | > "<client> <channel> <visible clients> :<topic>"
type RplList = Msg "322" '[Client, Channel, Int, Message]
makeMsgLenses ''RplList ["client", "channel", "visibleClients", "message"]

-- | > "<client> :End of /List"
type RplListend = Msg "323" '[Client, Message]
makeMsgLenses ''RplListend ["client", "message"]

-- | > "<client> <channel> <modestring> <mode arguments>..."
type RplChannelmodeis = Msg "324" '[Client, Channel, ByteString]
makeMsgLenses ''RplChannelmodeis ["client", "channel", "rawmode" ]

rplChannelmodeisMode :: ServerModes -> Fold RplChannelmodeis ModeStr
rplChannelmodeisMode m = rplChannelmodeisRawmode . to (fetchModeStr m) . _Just

-- | > "<client> <channel> :No topic is set"
type RplNotopic = Msg "331" '[Client, Channel, Message]
makeMsgLenses ''RplNotopic ["client", "channel", "message"]

-- | > "<client> <channel> :<topic>"
type RplTopic = Msg "332" '[Client, Channel, Message]
makeMsgLenses ''RplTopic ["client", "channel", "message"]

-- | > "<client> <channel> <nick> <setat>"
type RplTopictime = Msg "333" '[Client, Channel, Nickname, POSIXTime]
makeMsgLenses ''RplTopictime ["client", "channel", "nick", "setAt"]

-- | > "<client> <channel> <nick>"
type RplInviting = Msg "341" '[Client, Channel, Nickname]
makeMsgLenses ''RplInviting ["client", "channel", "nick"]

-- | > "<client> <channel> <mask>"
type RplInvitelist = Msg "346" '[Client, Channel, Mask]
makeMsgLenses ''RplInvitelist ["client", "channel", "mask"]

-- | > "<client> <channel> :End of channel invite list"
type RplEndofinvitelist = Msg "349" '[Client, Channel, Message]
makeMsgLenses ''RplEndofinvitelist ["client", "channel", "message"]

-- | > "<client> <channel> <mask>"
type RplExceptlist = Msg "348" '[Client, Channel, Mask]
makeMsgLenses ''RplExceptlist ["client", "channel", "mask"]

-- | > "<client> <channel> :End of channel exception list"
type RplEndofexceptlist = Msg "349" '[Client, Channel, Message]
makeMsgLenses ''RplEndofexceptlist ["client", "channel", "message"]

-- | > "<client> <version> <server> :<comments>"
type RplVersion = Msg "351" '[Client, Text, Hostname, Message]
makeMsgLenses ''RplVersion ["client", "version", "server", "message"]

-- | > "<client> <symbol> <channel> :[prefix]<nick>{ [prefix]<nick>}"
type RplNamreply = Msg "353" '[Client, Char, Channel, CList (Member Nickname)]
makeMsgLenses ''RplNamreply ["client", "symbol", "channel", "nicks"]

-- | > "<client> <channel> :End of /Names list"
type RplEndofnames = Msg "366" '[Client, Channel, Message]
makeMsgLenses ''RplEndofnames ["client", "channel", "message"]

-- | > "<client> <channel> <mask>"
type RplBanlist = Msg "367" '[Client, Channel, Mask]
makeMsgLenses ''RplBanlist ["client", "channel", "mask"]

-- | > "<client> <channel> :End of channel ban list"
type RplEndofbanlist = Msg "368" '[Client, Channel, Message]
makeMsgLenses ''RplEndofbanlist ["client", "channel", "message"]

-- | > "<client> <nick> :End of Whowas"
type RplEndofwhowas = Msg "369" '[Client, Nickname, Message]
makeMsgLenses ''RplEndofwhowas ["client", "nick", "message"]

-- | > "<client> :- <server> Message of the day - "
type RplMotdstart = Msg "375" '[Client, Unused ":-", Hostname]
makeMsgLenses ''RplMotdstart ["client", "unused", "server"]

-- | > "<client> :<line of the motd>"
type RplMotd = Msg "372" '[Client, Message]
makeMsgLenses ''RplMotd ["client", "message"]

-- | > "<client> :End of /Motd command."
type RplEndofmotd = Msg "376" '[Client, Message]
makeMsgLenses ''RplEndofmotd ["client", "message"]

-- | > "<client> :You are now an Irc operator"
type RplYoureoper = Msg "381" '[Client, Message]
makeMsgLenses ''RplYoureoper ["client", "message"]

-- | > "<client> <config file> :Rehashing"
type RplRehashing = Msg "382" '[Client, Text, Message]
makeMsgLenses ''RplRehashing ["client", "configFile", "message"]

-- | > "<client> <command>{ <subcommand>} :<info>"
type ErrUnknownerror = Msg "400" '[Client, SList Text, Message]
makeMsgLenses ''ErrUnknownerror ["client", "commands", "message"]

-- | > "<client> <nickname> :No such nick/channel"
type ErrNosuchnick = Msg "401" '[Client, Nickname, Message]
makeMsgLenses ''ErrNosuchnick ["client", "nickname", "message"]

-- | > "<client> <server name> :No such server"
type ErrNosuchserver = Msg "402" '[Client, Hostname, Message]
makeMsgLenses ''ErrNosuchserver ["client", "server", "message"]

-- | > "<client> <channel> :No such channel"
type ErrNosuchchannel = Msg "403" '[Client, Channel, Message]
makeMsgLenses ''ErrNosuchchannel ["client", "channel", "message"]

-- | > "<client> <channel> :Cannot send to channel"
type ErrCannotsendtochan = Msg "404" '[Client, Channel, Message]
makeMsgLenses ''ErrCannotsendtochan ["client", "channel", "message"]

-- | > "<client> <channel> :You have joined too many channels"
type ErrToomanychannels = Msg "405" '[Client, Channel, Message]
makeMsgLenses ''ErrToomanychannels ["client", "channel", "message"]

-- | > "<client> <command> :Unknown command"
type ErrUnknowncommand = Msg "421" '[Client, Text, Message]
makeMsgLenses ''ErrUnknowncommand ["client", "command", "message"]

-- | > "<client> :Motd File is missing"
type ErrNomotd = Msg "422" '[Client, Message]
makeMsgLenses ''ErrNomotd ["client", "message"]

-- | > "<client> <nick> :Erroneus nickname"
type ErrErroneusnickname = Msg "432" '[Client, Nickname, Message]
makeMsgLenses ''ErrErroneusnickname ["client", "nick", "message"]

-- | > "<client> <nick> :Nickname is already in use"
type ErrNicknameinuse = Msg "433" '[Client, Nickname, Message]
makeMsgLenses ''ErrNicknameinuse ["client", "nick", "message"]

-- | > "<client> :You have not registered"
type ErrNotregistered = Msg "451" '[Client, Message]
makeMsgLenses ''ErrNotregistered ["client", "message"]

-- | > "<client> <command> :Not enough parameters"
type ErrNeedmoreparams = Msg "461" '[Client, Text, Message]
makeMsgLenses ''ErrNeedmoreparams ["client", "command", "message"]

-- | > "<client> :You may not reregister"
type ErrAlreadyregistered = Msg "462" '[Client, Message]
makeMsgLenses ''ErrAlreadyregistered ["client", "message"]

-- | > "<client> :Password incorrect"
type ErrPasswdmismatch = Msg "464" '[Client, Message]
makeMsgLenses ''ErrPasswdmismatch ["client", "message"]

-- | > "<client> :You are banned from this server."
type ErrYourebannedcreep = Msg "465" '[Client, Message]
makeMsgLenses ''ErrYourebannedcreep ["client", "message"]

-- | > "<client> <channel> :Cannot join channel (+l)"
type ErrChannelisfull = Msg "471" '[Client, Channel, Message]
makeMsgLenses ''ErrChannelisfull ["client", "channel", "message"]

-- | > "<client> <modechar> :is unknown mode char to me"
type ErrUnknownmode = Msg "472" '[Client, Char, Message]
makeMsgLenses ''ErrUnknownmode ["client", "modeChar", "message"]

-- | > "<client> <channel> :Cannot join channel (+i)"
type ErrInviteonlychan = Msg "473" '[Client, Channel, Message]
makeMsgLenses ''ErrInviteonlychan ["client", "channel", "message"]

-- | > "<client> <channel> :Cannot join channel (+b)"
type ErrBannedfromchan = Msg "474" '[Client, Channel, Message]
makeMsgLenses ''ErrBannedfromchan ["client", "channel", "message"]

-- | > "<client> <channel> :Cannot join channel (+k)"
type ErrBadchannelkey = Msg "475" '[Client, Channel, Message]
makeMsgLenses ''ErrBadchannelkey ["client", "channel", "message"]

-- | > "<client> :Permission Denied- You're not an Irc operator"
type ErrNoprivileges = Msg "481" '[Client, Message]
makeMsgLenses ''ErrNoprivileges ["client", "message"]

-- | > "<client> <channel> :You're not channel operator"
type ErrChanoprivsneeded = Msg "482" '[Client, Channel, Message]
makeMsgLenses ''ErrChanoprivsneeded ["client", "channel", "message"]

-- | > "<client> :You cant kill a server!"
type ErrCantkillserver = Msg "483" '[Client, Message]
makeMsgLenses ''ErrCantkillserver ["client", "message"]

-- | > "<client> :No O-lines for your host"
type ErrNooperhost = Msg "491" '[Client, Message]
makeMsgLenses ''ErrNooperhost ["client", "message"]

-- | > "<client> :Unknown Mode flag"
type ErrUmodeunknownflag = Msg "501" '[Client, Message]
makeMsgLenses ''ErrUmodeunknownflag ["client", "message"]

-- | > "<client> :Cant change mode for other users"
type ErrUsersdontmatch = Msg "502" '[Client, Message]
makeMsgLenses ''ErrUsersdontmatch ["client", "message"]

-- | > "<client> :Starttls successful, proceed with Tls handshake"
type RplStarttls = Msg "670" '[Client, Message]
makeMsgLenses ''RplStarttls ["client", "message"]

-- | > "<client> :Starttls failed (Wrong moon phase)"
type ErrStarttls = Msg "691" '[Client, Message]
makeMsgLenses ''ErrStarttls ["client", "message"]

-- | > "<client> <priv> :Insufficient oper privileges."
type ErrNoprivs = Msg "723" '[Client, Text, Message]
makeMsgLenses ''ErrNoprivs ["client", "priv", "message"]

-- | > "<client> <nick>!<user>@<host> <account> 
--   > :You are now logged in as <username>"
type RplLoggedin = Msg "900" '[Client, Host, Text, Message]
makeMsgLenses ''RplLoggedin ["client", "host", "account", "message"]

-- | > "<client> <nick>!<user>@<host> :You are now logged out"
type RplLoggedout = Msg "901" '[Client, Host, Message]
makeMsgLenses ''RplLoggedout ["client", "host", "message"]

-- | > "<client> :You must use a nick assigned to you"
type ErrNicklocked = Msg "902" '[Client, Message]
makeMsgLenses ''ErrNicklocked ["client", "message"]

-- | > "<client> :Sasl authentication successful"
type RplSaslsuccess = Msg "903" '[Client, Message]
makeMsgLenses ''RplSaslsuccess ["client", "message"]

-- | > "<client> :Sasl authentication failed"
type ErrSaslfail = Msg "904" '[Client, Message]
makeMsgLenses ''ErrSaslfail ["client", "message"]

-- | > "<client> :Sasl message too long"
type ErrSasltoolong = Msg "905" '[Client, Message]
makeMsgLenses ''ErrSasltoolong ["client", "message"]

-- | > "<client> :Sasl authentication aborted"
type ErrSaslaborted = Msg "906" '[Client, Message]
makeMsgLenses ''ErrSaslaborted ["client", "message"]

-- | > "<client> :You have already authenticated using Sasl"
type ErrSaslalready = Msg "907" '[Client, Message]
makeMsgLenses ''ErrSaslalready ["client", "message"]

-- | > "<client> <mechanisms> :are available Sasl mechanisms"
type RplSaslmechs = Msg "908" '[Client, [Text], Message]
makeMsgLenses ''RplSaslmechs ["client", "mechanisms", "message"]
