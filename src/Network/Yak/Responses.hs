-- | All response codes as types, as defined by the current Ircv3 spec
-- <https://modern.ircdocs.horse/>. Please note that the argument parsing
-- in many of these is crude and the text is parsed only as a catch-all
-- 'Message' type, since there may be difference between implementations on the
-- server side, or even per network! In some places, responses have been
-- hard-coded as defined in the Irc in order to extract necessary information.
-- If you disagree with these choices, pull requests are welcome.
--
-- Also note that you probably want to import this module qualified, or import
-- just what you need, as it exports 135 type synonyms.
--
-- In case your use case allows more precise parsing, or you need server
-- specific response codes not listed, you can always hide the relevant types
-- and define your own by simply building the appropriate type synonym.
{-# Language DataKinds #-}
module Network.Yak.Responses
where

import Data.Text (Text)
import Data.Void (Void)
import Data.Time.Clock.POSIX
import Network.Yak.Types

type TODO = Void
type Client = Username

-- | > "<client> :Welcome to the <networkname> Network, <nick>[!<user>@<host>]"
type RplWelcome = Msg "001" '[Client, Message]

-- | > "<client> :Your host is <servername>, running version <version>"
type RplYourhost = Msg "002" '[Client, Message]

-- | > "<client> :This server was created <datetime>"
type RplCreated = Msg "003" '[Client, Message]

-- | > "<client> <servername> <version> <available user modes> <available channel modes> [<channel modes with a parameter>]"
type RplMyinfo = Msg "004" '[Client, Hostname, Modes, Modes, Modes, Maybe Modes]

-- | > "<client> <1-13 tokens> :are supported by this server"
type RplIsupport = Msg "005" '[Client, TODO]

-- | > "<client> <hostname> <port> :<info>"
type RplBounce = Msg "010" '[Client, Hostname, Int, Message]

-- | > "<client> <user modes>"
type RplUmodeis = Msg "221" '[Client, Modes]

-- | > "<client> :There are <u> users and <i> invisible on <s> servers"
type RplLuserclient = Msg "251" '[Client, Message]

-- | > "<client> <ops> :operator(s) online"
type RplLuserop = Msg "252" '[Client, Int, Message]

-- | > "<client> <connections> :unknown connection(s)"
type RplLuserunknown = Msg "253" '[Client, Int, Message]

-- | > "<client> <channels> :channels formed"
type RplLuserchannels = Msg "254" '[Client, Int, Message]

-- | > "<client> :I have <c> clients and <s> servers"
type RplLuserme = Msg "255" '[Client, Message]

-- | > "<client> [<server>] :Administrative info"
type RplAdminme = Msg "256" '[Client, Maybe Hostname, Message]

-- | > "<client> :<info>"
type RplAdminemail = Msg "259" '[Client, Message]

-- | > "<client> <command> :Please wait a while and try again."
type RplTryagain = Msg "263" '[Client, Text, Message]

-- | > "<client> [<u> <m>] :Current local users <u>, max <m>"
type RplLocalusers = Msg "265" '[Client, Maybe (Int, Int), Message]

-- | > "<client> [<u> <m>] :Current global users <u>, max <m>"
type RplGlobalusers = Msg "266" '[Client, Maybe (Int, Int), Message]

-- | > "<client> <nick> :has client certificate fingerprint <fingerprint>"
type RplWhoiscertfp = Msg "276" '[Client, Nickname, Message]

-- | Undefined format
type RplNone = Msg "300" '[Void]

-- | > "<client> <nick> :<message>"
type RplAway = Msg "301" '[Client, Nickname, Message]

-- | > "<client> :[<reply>{ <reply>}]"
type RplUserhost = Msg "302" '[Client, TODO]

-- | > "<client> :[<nickname>{ <nickname>}]"
type RplIson = Msg "303" '[Client, TODO]

-- | > "<client> :You are no longer marked as being away"
type RplUnaway = Msg "305" '[Client, Message]

-- | > "<client> :You have been marked as being away"
type RplNowaway = Msg "306" '[Client, Message]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhoisuser = Msg "311" '[Client, Nickname, Username, Hostname, Unused "*", Message]

-- | > "<client> <nick> <server> :<server info>"
type RplWhoisserver = Msg "312" '[Client, Nickname, Hostname, Message]

-- | > "<client> <nick> :is an Irc operator"
type RplWhoisoperator = Msg "313" '[Client, Nickname, Message]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhowasuser = Msg "314" '[Client, Nickname, Username, Hostname, Unused "*", Message]

-- | > "<client> <nick> <secs> [<signon>] :seconds idle, signon time"
type RplWhoisidle = Msg "317" '[Client, Nickname, Int, Maybe POSIXTime, Message]

-- | > "<client> <nick> :End of /Whois list"
type RplEndofwhois = Msg "318" '[Client, Nickname, Message]

-- | > "<client> <nick> :[prefix]<channel>{ [prefix]<channel>}"
type RplWhoischannels = Msg "319" '[Client, Nickname, TODO]

-- | > "<client> Channel :Users  Name"
type RplListstart = Msg "321" '[Client]

-- | > "<client> <channel> <visible clients> :<topic>"
type RplList = Msg "322" '[Client, Channel, Int, Message]

-- | > "<client> :End of /List"
type RplListend = Msg "323" '[Client, Message]

-- | > "<client> <channel> <modestring> <mode arguments>..."
type RplChannelmodeis = Msg "324" '[Client, Channel, ModeString, SList Text]

-- | > "<client> <channel> :No topic is set"
type RplNotopic = Msg "331" '[Client, Channel, Message]

-- | > "<client> <channel> :<topic>"
type RplTopic = Msg "332" '[Client, Channel, Message]

-- | > "<client> <channel> <nick> <setat>"
type RplTopictime = Msg "333" '[Client, Channel, Nickname, POSIXTime]

-- | > "<client> <channel> <nick>"
type RplInviting = Msg "341" '[Client, Channel, Nickname]

-- | > "<client> <channel> <mask>"
type RplInvitelist = Msg "346" '[Client, Channel, Mask]

-- | > "<client> <channel> :End of channel invite list"
type RplEndofinvitelist = Msg "349" '[Client, Channel, Message]

-- | > "<client> <channel> <mask>"
type RplExceptlist = Msg "348" '[Client, Channel, Mask]

-- | > "<client> <channel> :End of channel exception list"
type RplEndofexceptlist = Msg "349" '[Client, Channel, Message]

-- | > "<client> <version> <server> :<comments>"
type RplVersion = Msg "351" '[Client, Text, Hostname, Message]

-- | > "<client> <symbol> <channel> :[prefix]<nick>{ [prefix]<nick>}"
type RplNamreply = Msg "353" '[Client, Char, Channel, TODO]

-- | > "<client> <channel> :End of /Names list"
type RplEndofnames = Msg "366" '[Client, Channel, Message]

-- | > "<client> <channel> <mask>"
type RplBanlist = Msg "367" '[Client, Channel, Mask]

-- | > "<client> <channel> :End of channel ban list"
type RplEndofbanlist = Msg "368" '[Client, Channel, Message]

-- | > "<client> <nick> :End of Whowas"
type RplEndofwhowas = Msg "369" '[Client, Nickname, Message]

-- | > "<client> :- <server> Message of the day - "
type RplMotdstart = Msg "375" '[Client, Unused ":-", Hostname, Unused "Message of the day -"]

-- | > "<client> :<line of the motd>"
type RplMotd = Msg "372" '[Client, Message]

-- | > "<client> :End of /Motd command."
type RplEndofmotd = Msg "376" '[Client, Message]

-- | > "<client> :You are now an Irc operator"
type RplYoureoper = Msg "381" '[Client, Message]

-- | > "<client> <config file> :Rehashing"
type RplRehashing = Msg "382" '[Client, TODO, Message]

-- | > "<client> <command>{ <subcommand>} :<info>"
type ErrUnknownerror = Msg "400" '[Client, SList Text, Message]

-- | > "<client> <nickname> :No such nick/channel"
type ErrNosuchnick = Msg "401" '[Client, Nickname, Message]

-- | > "<client> <server name> :No such server"
type ErrNosuchserver = Msg "402" '[Client, Hostname, Message]

-- | > "<client> <channel> :No such channel"
type ErrNosuchchannel = Msg "403" '[Client, Channel, Message]

-- | > "<client> <channel> :Cannot send to channel"
type ErrCannotsendtochan = Msg "404" '[Client, Channel, Message]

-- | > "<client> <channel> :You have joined too many channels"
type ErrToomanychannels = Msg "405" '[Client, Channel, Message]

-- | > "<client> <command> :Unknown command"
type ErrUnknowncommand = Msg "421" '[Client, Text, Message]

-- | > "<client> :Motd File is missing"
type ErrNomotd = Msg "422" '[Client, Message]

-- | > "<client> <nick> :Erroneus nickname"
type ErrErroneusnickname = Msg "432" '[Client, Nickname, Message]

-- | > "<client> <nick> :Nickname is already in use"
type ErrNicknameinuse = Msg "433" '[Client, Nickname, Message]

-- | > "<client> :You have not registered"
type ErrNotregistered = Msg "451" '[Client, Message]

-- | > "<client> <command> :Not enough parameters"
type ErrNeedmoreparams = Msg "461" '[Client, Text, Message]

-- | > "<client> :You may not reregister"
type ErrAlreadyregistered = Msg "462" '[Client, Message]

-- | > "<client> :Password incorrect"
type ErrPasswdmismatch = Msg "464" '[Client, Message]

-- | > "<client> :You are banned from this server."
type ErrYourebannedcreep = Msg "465" '[Client, Message]

-- | > "<client> <channel> :Cannot join channel (+l)"
type ErrChannelisfull = Msg "471" '[Client, Channel, Message]

-- | > "<client> <modechar> :is unknown mode char to me"
type ErrUnknownmode = Msg "472" '[Client, Char, Message]

-- | > "<client> <channel> :Cannot join channel (+i)"
type ErrInviteonlychan = Msg "473" '[Client, Channel, Message]

-- | > "<client> <channel> :Cannot join channel (+b)"
type ErrBannedfromchan = Msg "474" '[Client, Channel, Message]

-- | > "<client> <channel> :Cannot join channel (+k)"
type ErrBadchannelkey = Msg "475" '[Client, Channel, Message]

-- | > "<client> :Permission Denied- You're not an Irc operator"
type ErrNoprivileges = Msg "481" '[Client, Message]

-- | > "<client> <channel> :You're not channel operator"
type ErrChanoprivsneeded = Msg "482" '[Client, Channel, Message]

-- | > "<client> :You cant kill a server!"
type ErrCantkillserver = Msg "483" '[Client, Message]

-- | > "<client> :No O-lines for your host"
type ErrNooperhost = Msg "491" '[Client, Message]

-- | > "<client> :Unknown Mode flag"
type ErrUmodeunknownflag = Msg "501" '[Client, Message]

-- | > "<client> :Cant change mode for other users"
type ErrUsersdontmatch = Msg "502" '[Client, Message]

-- | > "<client> :Starttls successful, proceed with Tls handshake"
type RplStarttls = Msg "670" '[Client, Message]

-- | > "<client> :Starttls failed (Wrong moon phase)"
type ErrStarttls = Msg "691" '[Client, Message]

-- | > "<client> <priv> :Insufficient oper privileges."
type ErrNoprivs = Msg "723" '[Client, Text, Message]

-- | > "<client> <nick>!<user>@<host> <account> :You are now logged in as <username>"
type RplLoggedin = Msg "900" '[Client, Host, Text, Message]

-- | > "<client> <nick>!<user>@<host> :You are now logged out"
type RplLoggedout = Msg "901" '[Client, Host, Message]

-- | > "<client> :You must use a nick assigned to you"
type ErrNicklocked = Msg "902" '[Client, Message]

-- | > "<client> :Sasl authentication successful"
type RplSaslsuccess = Msg "903" '[Client, Message]

-- | > "<client> :Sasl authentication failed"
type ErrSaslfail = Msg "904" '[Client, Message]

-- | > "<client> :Sasl message too long"
type ErrSasltoolong = Msg "905" '[Client, Message]

-- | > "<client> :Sasl authentication aborted"
type ErrSaslaborted = Msg "906" '[Client, Message]

-- | > "<client> :You have already authenticated using Sasl"
type ErrSaslalready = Msg "907" '[Client, Message]

-- | > "<client> <mechanisms> :are available Sasl mechanisms"
type RplSaslmechs = Msg "908" '[Client, [Text], Message]
