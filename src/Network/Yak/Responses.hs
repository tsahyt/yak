-- | All response codes as types, as defined by RFC 2812
-- <https://tools.ietf.org/html/rfc2812>. Please note that the argument parsing
-- in many of these is crude and the text is parsed only as a catch-all
-- 'Message' type, since there may be difference between implementations on the
-- server side, or even per network! In some places, responses have been
-- hard-coded as defined in the IRC in order to extract necessary information.
-- If you disagree with these choices, pull requests are welcome.
--
-- Also note that you probably want to import this module qualified, or import
-- just what you need, as it exports 135 type synonyms.
--
-- In case your use case allows more precise parsing, or you need response codes
-- not listed in the RFC, you can always hide the relevant types and define your
-- own by simply building the appropriate type synonym.
{-# LANGUAGE DataKinds #-}
module Network.Yak.Responses
where

import Data.Text (Text)
import Network.Yak.Types

-- | > "Welcome to the Internet Relay Network <nick>!<user>@<host>"
type RplWelcome           = Msg "001" '[Message]

-- | > "Your host is <servername>, running version <ver>"
type RplYourhost          = Msg "002" '[Message]

-- | > "This server was created <date>"
type RplCreated           = Msg "003" '[Message]

-- | > "<servername> <version> <available user modes> <available channel modes>"
type RplMyinfo            = Msg "004" '[Message]

-- | > "Try server <server name>, port <port number>"
type RplBounce            = Msg "005" '[Message]

-- | > ":*1<reply> *( " " <reply> )"
type RplUserhost          = Msg "302" '[]

-- | > ":*1<nick> *( " " <nick> )"
type RplIson              = Msg "303" '[]

-- | > "<nick> :<away message>"
type RplAway              = Msg "301" '[Nickname, Message]

-- | > ":You are no longer marked as being away"
type RplUnaway            = Msg "305" '[Message]

-- | > ":You have been marked as being away"
type RplNowaway           = Msg "306" '[Message]

-- | > "<nick> <user> <host> * :<real name>"
type RplWhoisuser         = Msg "311" '[Nickname, Username, Target, 
                                        Unused "*", Message]

-- | > "<nick> <server> :<server info>"
type RplWhoisserver       = Msg "312" '[Nickname, Target, Message]

-- | > "<nick> :is an Irc operator"
type RplWhoisoperator     = Msg "313" '[Nickname, Message]

-- | > "<nick> <integer> :seconds idle"
type RplWhoisidle         = Msg "317" '[Nickname, Int, Message]

-- | > "<nick> :End of Whois list"
type RplEndofwhois        = Msg "318" '[Nickname, Message]

-- | > "<nick> :*( ( "@" / "+" ) <channel> " " )"
type RplWhoischannels     = Msg "319" '[]

-- | > "<nick> <user> <host> * :<real name>"
type RplWhowasuser        = Msg "314" '[Nickname, Username, Target, 
                                        Unused "*", Message]

-- | > "<nick> :End of Whowas"
type RplEndofwhowas       = Msg "369" '[Nickname, Message]

-- | > "<channel> <# visible> :<topic>"
type RplList              = Msg "322" '[Channel, Int, Message]

-- | > ":End of List"
type RplListend           = Msg "323" '[Message]

-- | > "<channel> <nickname>"
type RplUniqopis          = Msg "325" '[Channel, Nickname]

-- | > "<channel> <mode> <mode params>"
type RplChannelmodeis     = Msg "324" '[]

-- | > "<channel> :No topic is set"
type RplNotopic           = Msg "331" '[Channel, Message]

-- | > "<channel> :<topic>"
type RplTopic             = Msg "332" '[Channel, Message]

-- | > "<channel> <nick>"
type RplInviting          = Msg "341" '[Channel, Nickname]

-- | > "<user> :Summoning user to Irc"
type RplSummoning         = Msg "342" '[Username, Message]

-- | > "<channel> <invitemask>"
type RplInvitelist        = Msg "346" '[Channel, Mask]

-- | > "<channel> :End of channel invite list"
type RplEndofinvitelist   = Msg "347" '[Channel, Message]

-- | > "<channel> <exceptionmask>"
type RplExceptlist        = Msg "348" '[Channel, Mask]

-- | > "<channel> :End of channel exception list"
type RplEndofexceptlist   = Msg "349" '[Channel, Message]

-- | > "<version>.<debuglevel> <server> :<comments>"
type RplVersion           = Msg "351" '[Text, Target, Message]

-- | > "<channel> <user> <host> <server> <nick> ( "H" / "G" > ["*"] 
--   > [ ( "@" / "+" ) ] :<hopcount> <real name>"
type RplWhoreply          = Msg "352" '[]

-- | > "<name> :End of Who list"
type RplEndofwho          = Msg "315" '[Channel, Message]

-- | > "( " = " / "*" / "@" ) <channel> :[ "@" / "+" ] <nick> *( " " 
--   > [ "@" / "+" ] <nick> )
type RplNamreply          = Msg "353" '[]

-- | > "<channel> :End of Names list"
type RplEndofnames        = Msg "366" '[Channel, Message]

-- | > "<mask> <server> :<hopcount> <server info>"
type RplLinks             = Msg "364" '[Mask, Target, Message]

-- | > "<mask> :End of Links list"
type RplEndoflinks        = Msg "365" '[Mask, Message]

-- | > "<channel> <banmask>"
type RplBanlist           = Msg "367" '[Channel, Mask]

-- | > "<channel> :End of channel ban list"
type RplEndofbanlist      = Msg "368" '[Channel, Message]

-- | > ":<string>"
type RplInfo              = Msg "371" '[Message]

-- | > ":End of Info list"
type RplEndofinfo         = Msg "374" '[Message]

-- | > ":- <server> Message of the day - "
type RplMotdstart         = Msg "375" '[Message]

-- | > ":- <text>"
type RplMotd              = Msg "372" '[Message]

-- | > ":End of Motd command"
type RplEndofmotd         = Msg "376" '[Message]

-- | > ":You are now an Irc operator"
type RplYoureoper         = Msg "381" '[Message]

-- | > "<config file> :Rehashing"
type RplRehashing         = Msg "382" '[Text, Message]

-- | > "You are service <servicename>"
type RplYoureservice      = Msg "383" '[Message]

-- | The time format used here can differ server to server, hence it is returned
-- as a 'Message' rather than as some suitable time type. May be subject to
-- change in the future.
-- 
-- > "<server> :<string showing server's local time>"
type RplTime              = Msg "391" '[Target, Message]

-- | > ":UserId   Terminal  Host"
type RplUsersstart        = Msg "392" '[Message]

-- | > ":<username> <ttyline> <hostname>"
type RplUsers             = Msg "393" '[Message]

-- | > ":End of users"
type RplEndofusers        = Msg "394" '[Message]

-- | > ":Nobody logged in"
type RplNousers           = Msg "395" '[Message]

-- | > "Link <version & debug level> <destination> <next server> 
--   > V<protocol> version> <link uptime in seconds> <backstream sendq> 
--   > <upstream sendq>"
type RplTracelink         = Msg "200" '[Message]

-- | > "Try. <class> <server>"
type RplTraceconnecting   = Msg "201" '[Message]

-- | > "H.S. <class> <server>"
type RplTracehandshake    = Msg "202" '[Message]

-- | > "???? <class> [<client Ip address in dot form>]"
type RplTraceunknown      = Msg "203" '[Message]

-- | > "Oper <class> <nick>"
type RplTraceoperator     = Msg "204" '[Message]

-- | > "User <class> <nick>"
type RplTraceuser         = Msg "205" '[Message]

-- | > "Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server>
--   > V<protocol version>"
type RplTraceserver       = Msg "206" '[Message]

-- | > "Service <class> <name> <type> <active type>"
type RplTraceservice      = Msg "207" '[Message]

-- | > "<newtype> 0 <client name>"
type RplTracenewtype      = Msg "208" '[Message]

-- | > "Class <class> <count>"
type RplTraceclass        = Msg "209" '[Message]

-- | > "File <logfile> <debug level>"
type RplTracelog          = Msg "261" '[Message]

-- | > "<server name> <version & debug level> :End of Trace"
type RplTraceend          = Msg "262" '[Text, Text, Message]

-- | > "<linkname> <sendq> <sent messages> <sent Kbytes> <received messages>
--   > <received Kbytes> <time open>"
type RplStatslinkinfo     = Msg "211" '[]

-- | > "<command> <count> <byte count> <remote count>"
type RplStatscommands     = Msg "212" '[]

-- | > "<stats letter> :End of Stats report"
type RplEndofstats        = Msg "219" '[]

-- | > ":Server Up %d days %d:%02d:%02d"
type RplStatsuptime       = Msg "242" '[]

-- | > "O <hostmask> * <name>"
type RplStatsoline        = Msg "243" '[]

-- | > "<user mode string>"
type RplUmodeis           = Msg "221" '[Text]

-- | > "<name> <server> <mask> <type> <hopcount> <info>"
type RplServlist          = Msg "234" '[Text, Target, Mask, Text, Int, Text]

-- | > "<mask> <type> :End of service listing"
type RplServlistend       = Msg "235" '[Mask, Text, Message]

-- | > ":There are <integer> users and <integer> services on <integer> servers"
type RplLuserclient       = Msg "251" '[Message]

-- | > "<integer> :operator(s) online"
type RplLuserop           = Msg "252" '[Int, Message]

-- | > "<integer> :unknown connection(s)"
type RplLuserunknown      = Msg "253" '[Int, Message]

-- | > "<integer> :channels formed"
type RplLuserchannels     = Msg "254" '[Int, Message]

-- | > ":I have <integer> clients and <integer> servers"
type RplLuserme           = Msg "255" '[Message]

-- | > "<server> :Administrative info"
type RplAdminme           = Msg "256" '[Target, Message]

-- | > ":<admin info>"
type RplAdminloc1         = Msg "257" '[Message]

-- | > ":<admin info>"
type RplAdminloc2         = Msg "258" '[Message]

-- | > ":<admin info>"
type RplAdminemail        = Msg "259" '[Message]

-- | > "<command> :Please wait a while and try again."
type RplTryagain          = Msg "263" '[Text, Message]

-- | > "<nickname> :No such nick/channel"
type ErrNosuchnick        = Msg "401" '[Nickname, Message]

-- | > "<server name> :No such server"
type ErrNosuchserver      = Msg "402" '[Target, Message]

-- | > "<channel name> :No such channel"
type ErrNosuchchannel     = Msg "403" '[Channel, Message]

-- | > "<channel name> :Cannot send to channel"
type ErrCannotsendtochan  = Msg "404" '[Channel, Message]

-- | > "<channel name> :You have joined too many channels"
type ErrToomanychannels   = Msg "405" '[Channel, Message]

-- | > "<nickname> :There was no such nickname"
type ErrWasnosuchnick     = Msg "406" '[Nickname, Message]

-- | > "<target> :<error code> recipients. <abort message>"
type ErrToomanytargets    = Msg "407" '[Target, Message]

-- | > "<service name> :No such service"
type ErrNosuchservice     = Msg "408" '[Text, Message]

-- | > ":No origin specified"
type ErrNoorigin          = Msg "409" '[Message]

-- | > ":No recipient given (<command>)"
type ErrNorecipient       = Msg "411" '[Message]

-- | > ":No text to send"
type ErrNotexttosend      = Msg "412" '[Message]

-- | > "<mask> :No toplevel domain specified"
type ErrNotoplevel        = Msg "413" '[Mask, Message]

-- | > "<mask> :Wildcard in toplevel domain"
type ErrWildtoplevel      = Msg "414" '[Mask, Message]

-- | > "<mask> :Bad Server/host mask"
type ErrBadmask           = Msg "415" '[Mask, Message]

-- | > "<command> :Unknown command"
type ErrUnknowncommand    = Msg "421" '[Text, Message]

-- | > ":Motd File is missing"
type ErrNomotd            = Msg "422" '[Message]

-- | > "<server> :No administrative info available"
type ErrNoadmininfo       = Msg "423" '[Target]

-- | > ":File error doing <file op> on <file>"
type ErrFileerror         = Msg "424" '[Message]

-- | > ":No nickname given"
type ErrNonicknamegiven   = Msg "431" '[Message]

-- | > "<nick> :Erroneous nickname"
type ErrErroneusnickname  = Msg "432" '[Nickname, Message]

-- | > "<nick> :Nickname is already in use"
type ErrNicknameinuse     = Msg "433" '[Nickname, Message]

-- | > "<nick> :Nickname collision Kill from <user>@<host>"
type ErrNickcollision     = Msg "436" '[Nickname, Message]

-- | > "<nick/channel> :Nick/channel is temporarily unavailable"
type ErrUnavailresource   = Msg "437" '[Either Nickname Channel, Message]

-- | > "<nick> <channel> :They aren't on that channel"
type ErrUsernotinchannel  = Msg "441" '[Nickname, Channel]

-- | > "<channel> :You're not on that channel"
type ErrNotonchannel      = Msg "442" '[Channel, Message]

-- | > "<user> <channel> :is already on channel"
type ErrUseronchannel     = Msg "443" '[Username, Channel, Message]

-- | > "<user> :User not logged in"
type ErrNologin           = Msg "444" '[Username, Message]

-- | > ":Summon has been disabled"
type ErrSummondisabled    = Msg "445" '[Message]

-- | > ":Users has been disabled"
type ErrUsersdisabled     = Msg "446" '[Message]

-- | > ":You have not registered"
type ErrNotregistered     = Msg "451" '[Message]

-- | > "<command> :Not enough parameters"
type ErrNeedmoreparams    = Msg "461" '[Text, Message]

-- | > ":Unauthorized command (already registered)"
type ErrAlreadyregistred  = Msg "462" '[Message]

-- | > ":Your host isn't among the privileged"
type ErrNopermforhost     = Msg "463" '[Message]

-- | > ":Password incorrect"
type ErrPasswdmismatch    = Msg "464" '[Message]

-- | > ":You are banned from this server"
type ErrYourebannedcreep  = Msg "465" '[Message]

type ErrYouwillbebanned   = Msg "466" '[]

-- | > "<channel> :Channel key already set"
type ErrKeyset            = Msg "467" '[Channel, Message]

-- | > "<channel> :Cannot join channel (+l)"
type ErrChannelisfull     = Msg "471" '[Channel, Message]

-- | > "<char> :is unknown mode char to me for <channel>"
type ErrUnknownmode       = Msg "472" '[Char, Message]

-- | > "<channel> :Cannot join channel (+i)"
type ErrInviteonlychan    = Msg "473" '[Channel, Message]

-- | > "<channel> :Cannot join channel (+b)"
type ErrBannedfromchan    = Msg "474" '[Channel, Message]

-- | > "<channel> :Cannot join channel (+k)"
type ErrBadchannelkey     = Msg "475" '[Channel, Message]

-- | > "<channel> :Bad Channel Mask"
type ErrBadchanmask       = Msg "476" '[Channel, Message]

-- | > "<channel> :Channel doesn't support modes"
type ErrNochanmodes       = Msg "477" '[Channel, Message]

-- | > "<channel> <char> :Channel list is full"
type ErrBanlistfull       = Msg "478" '[Channel, Char, Message]

-- | > ":Permission Denied- You're not an Irc operator"
type ErrNoprivileges      = Msg "481" '[Message]

-- | > "<channel> :You're not channel operator"
type ErrChanoprivsneeded  = Msg "482" '[Channel, Message]

-- | > ":You can't kill a server!"
type ErrCantkillserver    = Msg "483" '[Message]

-- | > ":Your connection is restricted!"
type ErrRestricted        = Msg "484" '[Message]

-- | > ":You're not the original channel operator"
type ErrUniqopprivsneeded = Msg "485" '[Message]

-- | > ":No O-lines for your host"
type ErrNooperhost        = Msg "491" '[Message]

-- | > ":Unknown Mode flag"
type ErrUmodeunknownflag  = Msg "501" '[Message]

-- | > ":Cannot change mode for other users"
type ErrUsersdontmatch    = Msg "502" '[Message]
