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

import Network.Yak.Types

-- | > "<client> :Welcome to the <networkname> Network, <nick>[!<user>@<host>]"
type RplWelcome = Msg "001" '[]

-- | > "<client> :Your host is <servername>, running version <version>"
type RplYourhost = Msg "002" '[]

-- | > "<client> :This server was created <datetime>"
type RplCreated = Msg "003" '[]

-- | > "<client> <servername> <version> <available user modes> <available channel modes> [<channel modes with a parameter>]"
type RplMyinfo = Msg "004" '[]

-- | > "<client> <1-13 tokens> :are supported by this server"
type RplIsupport = Msg "005" '[]

-- | > "<client> <hostname> <port> :<info>"
type RplBounce = Msg "010" '[]

-- | > "<client> <user modes>"
type RplUmodeis = Msg "221" '[]

-- | > "<client> :There are <u> users and <i> invisible on <s> servers"
type RplLuserclient = Msg "251" '[]

-- | > "<client> <ops> :operator(s) online"
type RplLuserop = Msg "252" '[]

-- | > "<client> <connections> :unknown connection(s)"
type RplLuserunknown = Msg "253" '[]

-- | > "<client> <channels> :channels formed"
type RplLuserchannels = Msg "254" '[]

-- | > "<client> :I have <c> clients and <s> servers"
type RplLuserme = Msg "255" '[]

-- | > "<client> [<server>] :Administrative info"
type RplAdminme = Msg "256" '[]

-- | > "<client> :<info>"
type RplAdminemail = Msg "259" '[]

-- | > "<client> <command> :Please wait a while and try again."
type RplTryagain = Msg "263" '[]

-- | > "<client> [<u> <m>] :Current local users <u>, max <m>"
type RplLocalusers = Msg "265" '[]

-- | > "<client> [<u> <m>] :Current global users <u>, max <m>"
type RplGlobalusers = Msg "266" '[]

-- | > "<client> <nick> :has client certificate fingerprint <fingerprint>"
type RplWhoiscertfp = Msg "276" '[]

-- | Undefined format
type RplNone = Msg "300" '[]

-- | > "<client> <nick> :<message>"
type RplAway = Msg "301" '[]

-- | > "<client> :[<reply>{ <reply>}]"
type RplUserhost = Msg "302" '[]

-- | > "<client> :[<nickname>{ <nickname>}]"
type RplIson = Msg "303" '[]

-- | > "<client> :You are no longer marked as being away"
type RplUnaway = Msg "305" '[]

-- | > "<client> :You have been marked as being away"
type RplNowaway = Msg "306" '[]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhoisuser = Msg "311" '[]

-- | > "<client> <nick> <server> :<server info>"
type RplWhoisserver = Msg "312" '[]

-- | > "<client> <nick> :is an Irc operator"
type RplWhoisoperator = Msg "313" '[]

-- | > "<client> <nick> <username> <host> * :<realname>"
type RplWhowasuser = Msg "314" '[]

-- | > "<client> <nick> <secs> [<signon>] :seconds idle, signon time"
type RplWhoisidle = Msg "317" '[]

-- | > "<client> <nick> :End of /Whois list"
type RplEndofwhois = Msg "318" '[]

-- | > "<client> <nick> :[prefix]<channel>{ [prefix]<channel>}"
type RplWhoischannels = Msg "319" '[]

-- | > "<client> Channel :Users  Name"
type RplListstart = Msg "321" '[]

-- | > "<client> <channel> <visible clients> :<topic>"
type RplList = Msg "322" '[]

-- | > "<client> :End of /List"
type RplListend = Msg "323" '[]

-- | > "<client> <channel> <modestring> <mode arguments>..."
type RplChannelmodeis = Msg "324" '[]

-- | > "<client> <channel> :No topic is set"
type RplNotopic = Msg "331" '[]

-- | > "<client> <channel> :<topic>"
type RplTopic = Msg "332" '[]

-- | > "<client> <channel> <nick> <setat>"
type RplTopictime = Msg "333" '[]

-- | > "<client> <channel> <nick>"
type RplInviting = Msg "341" '[]

-- | > "<client> <channel> <mask>"
type RplInvitelist = Msg "346" '[]

-- | > "<client> <channel> :End of channel invite list"
type RplEndofinvitelist = Msg "349" '[]

-- | > "<client> <channel> <mask>"
type RplExceptlist = Msg "348" '[]

-- | > "<client> <channel> :End of channel exception list"
type RplEndofexceptlist = Msg "349" '[]

-- | > "<client> <version> <server> :<comments>"
type RplVersion = Msg "351" '[]

-- | > "<client> <symbol> <channel> :[prefix]<nick>{ [prefix]<nick>}"
type RplNamreply = Msg "353" '[]

-- | > "<client> <channel> :End of /Names list"
type RplEndofnames = Msg "366" '[]

-- | > "<client> <channel> <mask>"
type RplBanlist = Msg "367" '[]

-- | > "<client> <channel> :End of channel ban list"
type RplEndofbanlist = Msg "368" '[]

-- | > "<client> <nick> :End of Whowas"
type RplEndofwhowas = Msg "369" '[]

-- | > "<client> :- <server> Message of the day - "
type RplMotdstart = Msg "375" '[]

-- | > "<client> :<line of the motd>"
type RplMotd = Msg "372" '[]

-- | > "<client> :End of /Motd command."
type RplEndofmotd = Msg "376" '[]

-- | > "<client> :You are now an Irc operator"
type RplYoureoper = Msg "381" '[]

-- | > "<client> <config file> :Rehashing"
type RplRehashing = Msg "382" '[]

-- | > "<client> <command>{ <subcommand>} :<info>"
type ErrUnknownerror = Msg "400" '[]

-- | > "<client> <nickname> :No such nick/channel"
type ErrNosuchnick = Msg "401" '[]

-- | > "<client> <server name> :No such server"
type ErrNosuchserver = Msg "402" '[]

-- | > "<client> <channel> :No such channel"
type ErrNosuchchannel = Msg "403" '[]

-- | > "<client> <channel> :Cannot send to channel"
type ErrCannotsendtochan = Msg "404" '[]

-- | > "<client> <channel> :You have joined too many channels"
type ErrToomanychannels = Msg "405" '[]

-- | > "<client> <command> :Unknown command"
type ErrUnknowncommand = Msg "421" '[]

-- | > "<client> :Motd File is missing"
type ErrNomotd = Msg "422" '[]

-- | > "<client> <nick> :Erroneus nickname"
type ErrErroneusnickname = Msg "432" '[]

-- | > "<client> <nick> :Nickname is already in use"
type ErrNicknameinuse = Msg "433" '[]

-- | > "<client> :You have not registered"
type ErrNotregistered = Msg "451" '[]

-- | > "<client> <command> :Not enough parameters"
type ErrNeedmoreparams = Msg "461" '[]

-- | > "<client> :You may not reregister"
type ErrAlreadyregistered = Msg "462" '[]

-- | > "<client> :Password incorrect"
type ErrPasswdmismatch = Msg "464" '[]

-- | > "<client> :You are banned from this server."
type ErrYourebannedcreep = Msg "465" '[]

-- | > "<client> <channel> :Cannot join channel (+l)"
type ErrChannelisfull = Msg "471" '[]

-- | > "<client> <modechar> :is unknown mode char to me"
type ErrUnknownmode = Msg "472" '[]

-- | > "<client> <channel> :Cannot join channel (+i)"
type ErrInviteonlychan = Msg "473" '[]

-- | > "<client> <channel> :Cannot join channel (+b)"
type ErrBannedfromchan = Msg "474" '[]

-- | > "<client> <channel> :Cannot join channel (+k)"
type ErrBadchannelkey = Msg "475" '[]

-- | > "<client> :Permission Denied- You're not an Irc operator"
type ErrNoprivileges = Msg "481" '[]

-- | > "<client> <channel> :You're not channel operator"
type ErrChanoprivsneeded = Msg "482" '[]

-- | > "<client> :You cant kill a server!"
type ErrCantkillserver = Msg "483" '[]

-- | > "<client> :No O-lines for your host"
type ErrNooperhost = Msg "491" '[]

-- | > "<client> :Unknown Mode flag"
type ErrUmodeunknownflag = Msg "501" '[]

-- | > "<client> :Cant change mode for other users"
type ErrUsersdontmatch = Msg "502" '[]

-- | > "<client> :Starttls successful, proceed with Tls handshake"
type RplStarttls = Msg "670" '[]

-- | > "<client> :Starttls failed (Wrong moon phase)"
type ErrStarttls = Msg "691" '[]

-- | > "<client> <priv> :Insufficient oper privileges."
type ErrNoprivs = Msg "723" '[]

-- | > "<client> <nick>!<user>@<host> <account> :You are now logged in as <username>"
type RplLoggedin = Msg "900" '[]

-- | > "<client> <nick>!<user>@<host> :You are now logged out"
type RplLoggedout = Msg "901" '[]

-- | > "<client> :You must use a nick assigned to you"
type ErrNicklocked = Msg "902" '[]

-- | > "<client> :Sasl authentication successful"
type RplSaslsuccess = Msg "903" '[]

-- | > "<client> :Sasl authentication failed"
type ErrSaslfail = Msg "904" '[]

-- | > "<client> :Sasl message too long"
type ErrSasltoolong = Msg "905" '[]

-- | > "<client> :Sasl authentication aborted"
type ErrSaslaborted = Msg "906" '[]

-- | > "<client> :You have already authenticated using Sasl"
type ErrSaslalready = Msg "907" '[]

-- | > "<client> <mechanisms> :are available Sasl mechanisms"
type RplSaslmechs = Msg "908" '[]
