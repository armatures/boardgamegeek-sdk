{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UtilSpec (spec) where

import           Import
import           ResponseParser
import qualified RIO.ByteString.Lazy   as BL
import           RIO.Lens              (_Right)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Util

spec :: Spec
spec = do
  describe "plus2" $ do
    prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i
  describe "parseMarketplace" $ do
    it "parses" $ isRight (parseMarketplace xmlBody) `shouldBe` True
    it "parses a name" $  marketplaceName <$> (parseMarketplace xmlBody ^? _Right) `shouldBe` Just "\"Paper Tales\""
  where
    xmlBody :: BL.ByteString = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
     \ <items termsofuse=\"https://boardgamegeek.com/xmlapi/termsofuse\">\
     \ <item type=\"boardgame\" id=\"217861\">\
         \<thumbnail>https://cf.geekdo-images.com/V62ulI0ifY0LIGocVuKCIQ__thumb/img/-aesB6Vzn2VfsN619Ellkc8Z0UE=/fit-in/200x150/filters:strip_icc()/pic4043983.jpg</thumbnail>\
\      <image>https://cf.geekdo-images.com/V62ulI0ifY0LIGocVuKCIQ__original/img/XGKDfeYi6S3jBz4eE4cly-Nc5Us=/0x0/filters:format(jpeg)/pic4043983.jpg</image>\
\                                     \t\t\t\t\
\\t\t\t\t<name type=\"primary\" sortindex=\"1\" value=\"Paper Tales\" />\
\\t\t\t\
\\t\t\t\t\t\t                               \t\t\t\t\
\\t\t\t\t<name type=\"alternate\" sortindex=\"1\" value=\"Reinos de Papel\" />\
\\t\t\t    \t\t\t\t\
\\t\t\t\t<name type=\"alternate\" sortindex=\"1\" value=\"\231\142\139\229\155\189\232\189\182\233\151\187\229\189\149\" />\
\\t\t\t\
\\t\t\t\t\t\t               \t\t\t\t\t\t\t\t\t\t\t\t\t<description>Experience two fantastical centuries of expansions and combat in Paper Tales. Remodel your assorted assembly of characters, units, and buildings in each period based on your developments and the age of your heroes. Write a new legend of the rightful ruler who brought unity to the rival kingdoms.&amp;#10;&amp;#10;In more detail, Paper Tales is a simultaneous drafting card game. Each turn, players draft five units that they then recruit into their kingdom &amp;mdash; assuming that they can pay. These choices determine the players ability to shine in battle, generate great income, construct dominant buildings, and earn legend points. There are only four hiring positions available during the four rounds of the play, but your units grow older with each turn until time takes them away.&amp;#10;&amp;#10;Build a comprehensive strategy and adapt the shape of your realm according  to opportunities and restrictions  and you will make history!&amp;#10;&amp;#10;(Espa&amp;ntilde;ol)&amp;#10;Experimenta dos siglos de expansi&amp;oacute;n y batalla en Reinos de Papel. Remodela y adapta tus personajes, unidades y edificios en cada periodo y en la &amp;eacute;ra de cada h&amp;eacute;roe. Forma parte de la leyenda del mandatario justo que trajo la unidad a los reinos rivales.&amp;#10;&amp;#10;En mayor deatlle, Reinos de Papel es un juego de cartas de draft simult&amp;aacute;neo. Cada turno, los jugadores eligen 5 unidades que pueden reclutar en sus reinos, siempre que los puedan pagar. Esta selecci&amp;oacute;n determina la habilidad del jugador de destacar en la batalla, generar grandes ingresos, construir edificioes imponentes o ganar puntos de leyenda. S&amp;oacute;lo hay 4 posiciopones para reclutar durante las cuatro rondas de juego, tus unidades envejecer&amp;aacute;n cada turno hasta que el tiempo se las lleve.&amp;#10;&amp;#10;Desarrolla una estrategia coherente y adapta el perfil de tu reino de acuerdo a las oportunidades y resctricciones que surjan y &amp;iexcl;Har&amp;aacute;s historia!&amp;#10;&amp;#10;</description>\
\\t\t\t\t\t\t\t\t\t\t      \t               \t\t\t\t<yearpublished value=\"2017\" />\
\\t\t\t\t\t\t               \t\t\t\t<minplayers value=\"2\" />\
\\t\t\t\t\t\t               \t\t\t\t<maxplayers value=\"5\" />\
\\t\t\t\t\t\t      \t\t\t<poll name=\"suggested_numplayers\" title=\"User Suggested Number of Players\" totalvotes=\"49\">\
\\t\t\t\
\\t\t<results numplayers=\"1\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"1\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"6\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"24\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t\t\t\
\\t\t<results numplayers=\"2\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"3\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"22\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"16\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t\t\t\
\\t\t<results numplayers=\"3\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"11\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"26\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"0\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t\t\t\
\\t\t<results numplayers=\"4\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"24\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"9\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"1\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t\t\t\
\\t\t<results numplayers=\"5\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"5\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"23\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"5\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t\t\t\
\\t\t<results numplayers=\"5+\">\t\t\
\\t\t\t\t\t<result value=\"Best\" numvotes=\"1\" />\
\\t\t\t\t\t<result value=\"Recommended\" numvotes=\"4\" />\
\\t\t\t\t\t<result value=\"Not Recommended\" numvotes=\"20\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t</poll> \t\t\t               \t\t\t\t<playingtime value=\"30\" />\
\\t\t\t\t\t\t               \t\t\t\t<minplaytime value=\"30\" />\
\\t\t\t\t\t\t               \t\t\t\t<maxplaytime value=\"30\" />\
\\t\t\t\t\t\t               \t\t\t\t<minage value=\"12\" />\
\\t\t\t\t\t\t      \t\t\t<poll name=\"suggested_playerage\" title=\"User Suggested Player Age\" totalvotes=\"9\">\
\\t\t\t<results>\t\t\
\\t\t\t\t\t<result value=\"2\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"3\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"4\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"5\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"6\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"8\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"10\" numvotes=\"7\" />\
\\t\t\t\t\t<result value=\"12\" numvotes=\"2\" />\
\\t\t\t\t\t<result value=\"14\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"16\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"18\" numvotes=\"0\" />\
\\t\t\t\t\t<result value=\"21 and up\" numvotes=\"0\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t</poll> \t\t\t      \t\t\t<poll name=\"language_dependence\" title=\"Language Dependence\" totalvotes=\"12\">\
\\t\t\t\
\\t\t<results>\t\t\
\\t\t\t\t\t<result level=\"1\" value=\"No necessary in-game text\" numvotes=\"0\" />\
\\t\t\t\t\t<result level=\"2\" value=\"Some necessary text - easily memorized or small crib sheet\" numvotes=\"5\" />\
\\t\t\t\t\t<result level=\"3\" value=\"Moderate in-game text - needs crib sheet or paste ups\" numvotes=\"7\" />\
\\t\t\t\t\t<result level=\"4\" value=\"Extensive use of text - massive conversion needed to be playable\" numvotes=\"0\" />\
\\t\t\t\t\t<result level=\"5\" value=\"Unplayable in another language\" numvotes=\"0\" />\
\\t\t\t\t</results>\t\t\t\t\t\
\\t</poll> \t\t\t      \t\t\t \t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamecategory\" id=\"1002\" value=\"Card Game\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamecategory\" id=\"1010\" value=\"Fantasy\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2041\" value=\"Card Drafting\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2857\" value=\"Card Play Conflict Resolution\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2984\" value=\"Drafting\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2875\" value=\"End Game Bonuses\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2040\" value=\"Hand Management\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2016\" value=\"Secret Unit Deployment\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2020\" value=\"Simultaneous Action Selection\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamemechanic\" id=\"2849\" value=\"Tech Trees / Tech Tracks\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamefamily\" id=\"27646\" value=\"Mechanism: Tableau Building\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamefamily\" id=\"65851\" value=\"Players: Games with expansions that add solo play\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgameexpansion\" id=\"245473\" value=\"Paper Tales: Beyond the Gates\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t\t\t\
\\
\\t\t\t      \t\t\t\
\\
\\t\t\t      \t\t\t\
\\
\\t\t\t      \t\t\t\
\\
\\t\t\t      \t\t\t\
\\
\\t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgameimplementation\" id=\"130548\" value=\"Vorpals\" inbound=\"true\"/>\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t      \t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamedesigner\" id=\"76303\" value=\"Masato Uesugi\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgameartist\" id=\"98418\" value=\"Christine Alcouffe\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t      \t      \t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"28651\" value=\"Catch Up Games\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"39390\" value=\"Engames\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"38574\" value=\"Evrikus\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"29895\" value=\"Frosted Games\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"40171\" value=\"Funiverse\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"12540\" value=\"Game Harbor\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"7772\" value=\"Mercurio\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"37959\" value=\"MS Edizioni\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"39\" value=\"Pegasus Spiele\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\t\
\\t\t \t\t\t\
\\
\\t\t\t\
\\t\t\
\\t\t\t\t\t<link type=\"boardgamepublisher\" id=\"11652\" value=\"Stronghold Games\" />\
\\t\t\
\\t\t\t\t\t\t\t\t\t\
\\t\t\t\
\\
\\t\t\t\
\\t\
\\
\\t\
\\
\\t\
\\t\
\\t\
\   \
\\t\
\          <marketplacelistings>\
\              <listing>\
\         <listdate value=\"Tue, 06 Mar 2018 14:15:40 +0000\" />\
\         <price currency=\"USD\" value=\"89.95\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"free USA shipping&amp;#10;&amp;#10;ask about international shipping\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1439362\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Mon, 26 Mar 2018 09:59:39 +0000\" />\
\         <price currency=\"USD\" value=\"20.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Brand new in shrink wrap.&amp;#10;&amp;#10;NOT a Pre-Order!!! Ships TODAY !!! No Waiting !!!&amp;#10;&amp;#10;&amp;#10;Get $4 DISCOUNT or MORE!!  See below!!&amp;#10;&amp;#10;I will ship immediately and will ship worldwide.&amp;#10;&amp;#10;&amp;#10;DISCOUNT OFFER_____________________________________________&amp;#10;&amp;#10;Buy ANY 2 games from me --&gt; get $ 4 discount.&amp;#10;Buy MORE than 2 games --&gt; get $ 4 on first 2 + $1 OFF for the 3rd, 4th, 5th, etc. game.&amp;#10;&amp;#10;Combined shipping for the whole purchase anywhere in the WORLD!&amp;#10;____________________________________________________________&amp;#10;&amp;#10;,~~ooO~~~~~~~~~~~~~~~,&amp;#10;| __  MYSTERY ________|  See details here&amp;#10;| ______ GAMES ______|  [url=https://www.boardgamegeek.com/geekmarket/product/2545399]MYSTERY GAMES BOX[/url]&amp;#10;| ___________ BOX ___|&amp;#10;'~~~~~~~~~~~~~~~~OOo~'&amp;#10;&amp;#10;See the rest of my games in [url=http://www.boardgamegeek.com/geekmarket/user/greengow][COLOR=#009933][b][u]my marketplace[/u][/b][/COLOR][/url]&amp;#10;&amp;#10;&amp;#10;----- 0-9 -----&amp;#10;&amp;#10;1655 Habemus Papam&amp;#10;1920 Wall Street&amp;#10;3x8&amp;#10;3 sind eine zu viel&amp;#10;6 Nimmt!&amp;#10;8*28&amp;#10;&amp;#10;----- A -----&amp;#10;&amp;#10;Age of Industry&amp;#10;Amber Route&amp;#10;AVES&amp;#10;&amp;#10;----- B -----&amp;#10;&amp;#10;Bali&amp;#10;Basilica&amp;#10;Bastille&amp;#10;Batavia&amp;#10;Baumeister des Colosseums&amp;#10;Bl&amp;#195;&amp;#182;de Kuh&amp;#10;Bookmaker&amp;#10;Bon Voyage&amp;#10;Brewin' USA&amp;#10;Burano&amp;#10;&amp;#10;----- C -----&amp;#10;&amp;#10;Cargotrain&amp;#10;Castles of Burgundy - Card Game&amp;#10;Castles of Burgundy - Dice Game&amp;#10;Catan Geographies: The Carolinas&amp;#10;Catan Geographies: Georgia&amp;#10;Chickwood Forest&amp;#10;Chronicler&amp;#10;City Explorer: Kyoto&amp;#10;City Tycoon&amp;#10;Concordia Base + Aegyptus + Balearica + Creta + Cyprus + Mini Exp.&amp;#10;Concordia Balearica / Cyprus&amp;#10;Concordia Gallia / Corsica&amp;#10;Concordia Venus + Balearica / Italy&amp;#10;Crown of Emara&amp;#10;Crows Overkill&amp;#10;&amp;#10;----- D -----&amp;#10;&amp;#10;Dadaocheng 2nd Edition&amp;#10;Da Yunhe&amp;#10;Demeter&amp;#10;Der Isses!&amp;#10;De Vulgari Eloquentia - Deluxe KS Edition&amp;#10;Dice Brewing + Extras&amp;#10;Dice Settlers&amp;#10;Dokmus&amp;#10;Double Mission: Beyond the Object&amp;#10;&amp;#10;----- F -----&amp;#10;&amp;#10;Faiyum&amp;#10;Farlight&amp;#10;Favelas&amp;#10;Feierabend&amp;#10;Fertility&amp;#10;Feuville&amp;#10;Florenza Dice&amp;#10;Florenza X Anniversary Edition&amp;#10;Fortune City&amp;#10;Forum Trajanum&amp;#10;Franchise&amp;#10;Frankenstein&amp;#10;Freaky&amp;#10;Fuji&amp;#10;&amp;#10;----- G -----&amp;#10;&amp;#10;Ganz Sch&amp;#195;&amp;#182;n Clever&amp;#10;Ganymede&amp;#10;Ganymede: Moon&amp;#10;Garum&amp;#10;Get Off My Land!&amp;#10;Grand Austria Hotel&amp;#10;Great Western Trail&amp;#10;The Great Fire of London 1666&amp;#10;The Great City of Rome&amp;#10;&amp;#10;----- H -----&amp;#10;&amp;#10;Hadara&amp;#10;Hacienda 2nd Edition&amp;#10;Hakenschlagen&amp;#10;Hansa Teutonica&amp;#10;Hansa Teutonica - East expansion&amp;#10;Hansa Teutonica - Britannia expansion&amp;#10;Haru Ichiban&amp;#10;Harvest Island&amp;#10;Hawaii&amp;#10;Hengist&amp;#10;Hokito&amp;#10;Hot Tin Roof&amp;#10;&amp;#10;----- I -----&amp;#10;&amp;#10;Ilos&amp;#10;Imhotep The Duell&amp;#10;Imperial 2030&amp;#10;Indian Summer&amp;#10;Inseln im Nebel&amp;#10;In the Year of the Dragon - 10th Anniversary&amp;#10;Into the Black Forest&amp;#10;Isaribi&amp;#10;&amp;#10;----- K -----&amp;#10;&amp;#10;K2&amp;#10;K2 Broad Peak&amp;#10;K2 Lhotse&amp;#10;Kamisado Pocket&amp;#10;Keyper&amp;#10;Kings of Mithril&amp;#10;King's Pouch&amp;#10;Konja&amp;#10;&amp;#10;----- L -----&amp;#10;&amp;#10;Star Wars IX Labyrinth&amp;#10;LAMA&amp;#10;La Vina&amp;#10;Libraria&amp;#10;Lisbon, The Gate to the World&amp;#10;Lobo 77&amp;#10;Long Live the Queen&amp;#10;Lost Ruins of Arnak&amp;#10;&amp;#10;----- M -----&amp;#10;&amp;#10;Mamma Mia! Plus&amp;#10;Mana&amp;#10;Manitoba&amp;#10;Mercatores&amp;#10;Metro&amp;#10;Michael Strogoff&amp;#10;Migrato&amp;#10;Mini Rails&amp;#10;Montana&amp;#10;Moorea&amp;#10;Mystery of the Temples&amp;#10;MYSTERY GAMES BOX - see link above!!&amp;#10;&amp;#10;----- N -----&amp;#10;&amp;#10;Neta-Tanka&amp;#10;New Haven&amp;#10;New York - Alhambra&amp;#10;Niagara&amp;#10;Nord&amp;#10;Noria&amp;#10;&amp;#10;----- O -----&amp;#10;&amp;#10;Okanagan&amp;#10;On the Origin of Species&amp;#10;Origami&amp;#10;&amp;#10;----- P -----&amp;#10;&amp;#10;Palm Island&amp;#10;Pandoria&amp;#10;Pantarei&amp;#10;Paper Tales&amp;#10;Papua&amp;#10;Passing Through Petra&amp;#10;Pedzace Jeze&amp;#10;Philadelphia 1777&amp;#10;Piepmatz&amp;#10;Pillars of the Earth - Card game&amp;#10;Porta Nigra&amp;#10;Portale von Molthar&amp;#10;Princes of Machu of Picchu&amp;#10;Precious Cargo&amp;#10;Prehistory&amp;#10;&amp;#10;----- Q -----&amp;#10;&amp;#10;Quatro Flash&amp;#10;Queen's Architect&amp;#10;Queenz&amp;#10;&amp;#10;----- R -----&amp;#10;&amp;#10;Race to the New Found Land&amp;#10;Raiatea + Expansion&amp;#10;Rajas of the Ganges Dice Charmers&amp;#10;Rajas of the Ganges Goodie Box 1&amp;#10;Rajas of the Ganges Goodie Box 2&amp;#10;Rattus Pied Piper&amp;#10;Ratzzia&amp;#10;Rhodes&amp;#10;Riverboat&amp;#10;Robber Knights&amp;#10;Rocketmen&amp;#10;Romolo o Remo&amp;#10;Royals&amp;#10;&amp;#10;----- S -----&amp;#10;&amp;#10;Sagrada&amp;#10;Sailing Towards Osiris&amp;#10;Seidenstrasse&amp;#10;Semiramis&amp;#10;Sencha&amp;#10;Shadows in Kyoto&amp;#10;Ships&amp;#10;Sierra West&amp;#10;Small Islands&amp;#10;Snowdonia: The Siege of Petersburg / The Channel Tunnel 1881&amp;#10;Songbirds&amp;#10;&amp;#10;----- T -----&amp;#10;&amp;#10;TA-KE&amp;#10;Takenoko&amp;#10;Taiwan&amp;#10;TAXI&amp;#10;Tekhenu Obelisk of the Sun&amp;#10;The Game: Face To Face Duel&amp;#10;The Great Fire of London 1666&amp;#10;The Quest for El Dorado: The Golden Temples&amp;#10;Ticket to Ride: Italy - Japan&amp;#10;Ticket to Ride: London&amp;#10;Ticket To Ride: New York&amp;#10;Ticket To Ride: Poland&amp;#10;Tobago + Volcano&amp;#10;Tokyo Metro + Coins&amp;#10;Torres&amp;#10;Town Builder: Coevorden&amp;#10;Trideo&amp;#10;Transatlantic&amp;#10;Triovision&amp;#10;Tsukiji&amp;#10;TurfMaster + Map collections I + II&amp;#10;Turn The Tide&amp;#10;&amp;#10;----- U -----&amp;#10;&amp;#10;U.S. Telegraph&amp;#10;&amp;#10;----- V -----&amp;#10;&amp;#10;Valparaiso&amp;#10;Venetia&amp;#10;Versailles&amp;#10;Vinyl&amp;#10;The Voyages of Marco Polo&amp;#10;Voll Verladen&amp;#10;&amp;#10;----- W -----&amp;#10;&amp;#10;Watergate&amp;#10;Welcome To... plus 2 expansions&amp;#10;Wendake&amp;#10;West of Africa&amp;#10;Whistle Stop&amp;#10;Wir sind das Volk&amp;#10;Wir sind das Volk 2+2&amp;#10;W&amp;#195;&amp;#188;rfelland&amp;#10;Wurst Case Scenario&amp;#10;&amp;#10;----- X -----&amp;#10;&amp;#10;Xe Queo!&amp;#10;Xi'an&amp;#10;&amp;#10;----- Y -----&amp;#10;&amp;#10;Yangtze&amp;#10;Yedo&amp;#10;Ys with Ys+ expansion&amp;#10;Yukon\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1455058\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Wed, 25 Apr 2018 14:08:06 +0000\" />\
\         <price currency=\"EUR\" value=\"30.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1474769\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Wed, 08 Aug 2018 08:11:42 +0000\" />\
\         <price currency=\"EUR\" value=\"44.99\" />\
\           <condition value=\"new\" />\
\         <notes value=\"ENGLISH EDITION\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1564966\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Thu, 09 Aug 2018 21:59:21 +0000\" />\
\         <price currency=\"USD\" value=\"45.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"SHRINKED.\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1566359\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Tue, 04 Sep 2018 18:18:42 +0000\" />\
\         <price currency=\"USD\" value=\"40.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"SHRINKED.\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1585691\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Wed, 10 Oct 2018 19:43:22 +0000\" />\
\         <price currency=\"USD\" value=\"30.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Brand new in shrink (English edition 2018) $9.95 flat shipping for the contiguous United States\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1615417\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Tue, 13 Nov 2018 18:14:49 +0000\" />\
\         <price currency=\"EUR\" value=\"38.99\" />\
\           <condition value=\"new\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1668695\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Fri, 23 Nov 2018 16:17:16 +0000\" />\
\         <price currency=\"EUR\" value=\"29.90\" />\
\           <condition value=\"new\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1677348\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Fri, 08 Nov 2019 14:50:08 +0000\" />\
\         <price currency=\"EUR\" value=\"29.99\" />\
\           <condition value=\"new\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1698391\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Thu, 09 May 2019 17:39:09 +0000\" />\
\         <price currency=\"USD\" value=\"25.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/1841360\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Fri, 06 Mar 2020 15:11:10 +0000\" />\
\         <price currency=\"EUR\" value=\"36.65\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Game: Paper Tales&amp;#10;Languages: IT&amp;#10;Condition: New&amp;#10;&amp;#10;&amp;#10;SHIPPING COSTS&amp;#10;&amp;#10;[AUSTRALIA]&amp;#10;o &lt; 500g: 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 21,99&amp;#226;&amp;#130;&amp;#172;[Express], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 26,99&amp;#226;&amp;#130;&amp;#172;[Express], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[AUSTRIA]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[BELGIUM]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[BULGARIA]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[CANADA]&amp;#10;o &lt; 500g: 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 17,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 19,99&amp;#226;&amp;#130;&amp;#172;[Express], 22,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[CHINA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 19,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 17,99&amp;#226;&amp;#130;&amp;#172;[Express], 22,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 22,99&amp;#226;&amp;#130;&amp;#172;[Express], 25,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 27,99&amp;#226;&amp;#130;&amp;#172;[Express], 28,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[CROATIA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 17,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 21,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 26,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[CYPRUS]&amp;#10;o &lt; 500g: 11,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 14,99&amp;#226;&amp;#130;&amp;#172;[Standard], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 15,99&amp;#226;&amp;#130;&amp;#172;[Standard], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 17,99&amp;#226;&amp;#130;&amp;#172;[Standard], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[CZECH REPUBLIC]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[DENMARK]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[ESTONIA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[FINLAND]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[FRANCE]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[GERMANY]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[GREECE]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[HUNGARY]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[IRELAND]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[ISRAEL]&amp;#10;o &lt; 500g: 15,99&amp;#226;&amp;#130;&amp;#172;[Express], 22,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 20,99&amp;#226;&amp;#130;&amp;#172;[Express], 25,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 25,99&amp;#226;&amp;#130;&amp;#172;[Express], 28,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 30,99&amp;#226;&amp;#130;&amp;#172;[Express], 32,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[ITALY]&amp;#10;o &lt; 500g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere]&amp;#10;o 500-999g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere]&amp;#10;o 1000-1499g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere]&amp;#10;o 1500-2000g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere]&amp;#10;&amp;#10;[JAPAN]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 21,99&amp;#226;&amp;#130;&amp;#172;[Express], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 26,99&amp;#226;&amp;#130;&amp;#172;[Express], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[KOREA, SOUTH]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard], 41,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard], 47,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard], 54,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard], 60,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[LITHUANIA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[LUXEMBOURG]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[NETHERLANDS]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 17,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 20,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 24,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 27,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[NORWAY]&amp;#10;o &lt; 500g: 11,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Express], 14,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 14,99&amp;#226;&amp;#130;&amp;#172;[Express], 17,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[PERU]&amp;#10;o &lt; 500g: 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 21,99&amp;#226;&amp;#130;&amp;#172;[Express], 31,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 26,99&amp;#226;&amp;#130;&amp;#172;[Express], 34,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[POLAND]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[PORTUGAL]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[ROMANIA]&amp;#10;o &lt; 500g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 11,99&amp;#226;&amp;#130;&amp;#172;[Standard], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[SAN MARINO]&amp;#10;o &lt; 500g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere], 7,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 500-999g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1000-1499g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 4,99&amp;#226;&amp;#130;&amp;#172;[Corriere], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[SLOVAKIA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[SLOVENIA]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 19,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 23,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[SPAIN]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[SWEDEN]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[SWITZERLAND]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express], 14,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 14,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 17,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[TURKEY]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[UNITED KINGDOM]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 10,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 500-999g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 11,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;o 1000-1499g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 12,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;o 1500-2000g: 8,99&amp;#226;&amp;#130;&amp;#172;[Standard], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 15,99&amp;#226;&amp;#130;&amp;#172;[Economy]&amp;#10;&amp;#10;[UNITED STATES]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 12,99&amp;#226;&amp;#130;&amp;#172;[Express], 14,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 14,99&amp;#226;&amp;#130;&amp;#172;[Express], 16,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 18,99&amp;#226;&amp;#130;&amp;#172;[Express], 19,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard], 21,99&amp;#226;&amp;#130;&amp;#172;[Express]&amp;#10;&amp;#10;[TAIWAN]&amp;#10;o &lt; 500g: 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 21,99&amp;#226;&amp;#130;&amp;#172;[Express], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 26,99&amp;#226;&amp;#130;&amp;#172;[Express], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;[HONG KONG]&amp;#10;o &lt; 500g: 7,99&amp;#226;&amp;#130;&amp;#172;[Economy], 13,99&amp;#226;&amp;#130;&amp;#172;[Express], 20,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 500-999g: 11,99&amp;#226;&amp;#130;&amp;#172;[Economy], 16,99&amp;#226;&amp;#130;&amp;#172;[Express], 21,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1000-1499g: 12,99&amp;#226;&amp;#130;&amp;#172;[Economy], 21,99&amp;#226;&amp;#130;&amp;#172;[Express], 24,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;o 1500-2000g: 15,99&amp;#226;&amp;#130;&amp;#172;[Economy], 26,99&amp;#226;&amp;#130;&amp;#172;[Express], 27,99&amp;#226;&amp;#130;&amp;#172;[Standard]&amp;#10;&amp;#10;We do not ship to: Abkhazia - Afghanistan - Aland - Albania - Algeria - American Samoa - Andorra - Angola - Anguilla - Antigua and Barbuda - Argentina - Armenia - Aruba - Ascension - Ashmore and Cartier Islands - Australian Antarctic Territory - Azerbaijan - Bahamas, The - Bahrain - Baker Island - Bangladesh - Barbados - Belarus - Belize - Benin - Bermuda - Bhutan - Bolivia - Bosnia and Herzegovina - Botswana - Bouvet Island - Brazil - British Antarctic Territory - British Indian Ocean Territory - British Sovereign Base Areas - British Virgin Islands - Brunei - Burkina Faso - Burundi - Cambodia - Cameroon - Cape Verde - Cayman Islands - Central African Republic - Chad - Chile - Christmas Island - Clipperton Island - Cocos (Keeling) Islands - Colombia - Comoros - Congo, (Congo ? Brazzaville) - Congo, (Congo ? Kinshasa) - Cook Islands - Coral Sea Islands - Costa Rica - Cote d'Ivoire (Ivory Coast) - Cuba - Djibouti - Dominica - Dominican Republic - Ecuador - Egypt - El Salvador - Equatorial Guinea - Eritrea - Ethiopia - Falkland Islands (Islas Malvinas) - Faroe Islands - Fiji - French Guiana - French Polynesia - French Southern and Antarctic Lands - Gabon - Gambia, The - Georgia - Ghana - Gibraltar - Greenland - Grenada - Guadeloupe - Guam - Guatemala - Guernsey - Guinea - Guinea-Bissau - Guyana - Haiti - Heard Island and McDonald Islands - Honduras - Howland Island - Iceland - India - Indonesia - Iran - Iraq - Isle of Man - Jamaica - Jarvis Island - Jersey - Johnston Atoll - Jordan - Kazakhstan - Kenya - Kingman Reef - Kiribati - Korea, North - Kuwait - Kyrgyzstan - Laos - Latvia - Lebanon - Lesotho - Liberia - Libya - Liechtenstein - Macau - Macedonia - Madagascar - Malawi - Malaysia - Maldives - Mali - Malta - Marshall Islands - Martinique - Mauritania - Mauritius - Mayotte - Mexico - Micronesia - Midway Islands - Moldova - Monaco - Mongolia - Montenegro - Montserrat - Morocco - Mozambique - Myanmar (Burma) - Nagorno-Karabakh - Namibia - Nauru - Navassa Island - Nepal - Netherlands Antilles - New Caledonia - New Zealand - Nicaragua - Niger - Nigeria - Niue - Norfolk Island - Northern Cyprus - Northern Mariana Islands - Oman - Pakistan - Palau - Palmyra Atoll - Panama - Papua New Guinea - Paraguay - Peter I Island - Philippines - Pitcairn Islands - Pridnestrovie (Transnistria) - Puerto Rico - Qatar - Queen Maud Land - Reunion - Ross Dependency - Russia - Rwanda - Saint Barthelemy - Saint Helena - Saint Kitts and Nevis - Saint Lucia - Saint Martin - Saint Pierre and Miquelon - Saint Vincent and the Grenadines - Samoa - Sao Tome and Principe - Saudi Arabia - Senegal - Serbia - Seychelles - Sierra Leone - Singapore - Solomon Islands - Somalia - Somaliland - South Africa - South Georgia &amp; South Sandwich Islands - South Ossetia - Sri Lanka - Sudan - Suriname - Svalbard - Swaziland - Syria - Tajikistan - Tanzania - Thailand - Timor-Leste (East Timor) - Togo - Tokelau - Tonga - Trinidad and Tobago - Tristan da Cunha - Tunisia - Turkmenistan - Turks and Caicos Islands - Tuvalu - U.S. Virgin Islands - Uganda - Ukraine - United Arab Emirates - Uruguay - Uzbekistan - Vanuatu - Venezuela - Vietnam - Wake Island - Wallis and Futuna - Yemen - Zambia - Zimbabwe&amp;#10;&amp;#10;&amp;#10;[#]1HXP4+8A44IrTm111Jkju5UdfYy2ZCW5P4oeIOjaGiQ=[#]\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2095820\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Sat, 16 May 2020 21:40:44 +0000\" />\
\         <price currency=\"EUR\" value=\"25.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"Played once. Top condition.&amp;#10;&amp;#10;Message me for an shipping estimate\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2149554\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Wed, 01 Jul 2020 09:12:47 +0000\" />\
\         <price currency=\"EUR\" value=\"24.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"traduzione regole in italiano\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2202355\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Thu, 30 Jul 2020 18:32:39 +0000\" />\
\         <price currency=\"USD\" value=\"22.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"Played once. Comes from a smoke-free home.&amp;#10;&amp;#10;Local pickup available in Buffalo Niagara region&amp;#10;&amp;#10;I can take Paypal, Venmo or Zelle transfers. Please message to get shipping cost.\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2247459\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Fri, 04 Dec 2020 12:26:44 +0000\" />\
\         <price currency=\"EUR\" value=\"15.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"Premium sleeved\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2390744\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Sat, 26 Dec 2020 09:52:13 +0000\" />\
\         <price currency=\"EUR\" value=\"32.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2413674\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Thu, 31 Dec 2020 07:57:04 +0000\" />\
\         <price currency=\"EUR\" value=\"23.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"- English Edition&amp;#10;- Only played in sleeves&amp;#10;- No markings, wear or tear&amp;#10;- Pickup in Zurich Area possible\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2417904\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Wed, 06 Jan 2021 18:57:18 +0000\" />\
\         <price currency=\"EUR\" value=\"18.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2423519\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Tue, 16 Feb 2021 13:52:23 +0000\" />\
\         <price currency=\"EUR\" value=\"29.85\" />\
\           <condition value=\"new\" />\
\         <notes value=\"In it's original packaging\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2465473\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Sun, 14 Mar 2021 15:23:09 +0000\" />\
\         <price currency=\"GBP\" value=\"25.00\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Shrinkwrapped.&amp;#10;German language edition.\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2494796\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Thu, 08 Apr 2021 22:36:07 +0000\" />\
\         <price currency=\"USD\" value=\"20.00\" />\
\           <condition value=\"likenew\" />\
\         <notes value=\"Sleeved and only played twice!\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2519159\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Mon, 31 May 2021 15:21:09 +0000\" />\
\         <price currency=\"EUR\" value=\"46.90\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Language: italian&amp;#10;Publisher: Magic Store Srl&amp;#10;Game Condition: Mint&amp;#10;Box Condition: Mint&amp;#10;PREORDER - The current availability of this product needs to be checked.&amp;#10;The order could be deleted after the availability check.&amp;#10;We will inform you anyway.&amp;#10;ID Gis: 32267 (for reference)\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2565200\" title=\"marketlisting\" />\
\       </listing>\
\              <listing>\
\         <listdate value=\"Mon, 31 May 2021 15:22:05 +0000\" />\
\         <price currency=\"EUR\" value=\"46.90\" />\
\           <condition value=\"new\" />\
\         <notes value=\"Language: english&amp;#10;Publisher: Stronghold Games&amp;#10;Game Condition: Mint&amp;#10;Box Condition: Mint&amp;#10;PREORDER - The current availability of this product needs to be checked.&amp;#10;The order could be deleted after the availability check.&amp;#10;We will inform you anyway.&amp;#10;ID Gis: 32527 (for reference)\" />\
\                  <link href=\"https://boardgamegeek.com/geekmarket/product/2565201\" title=\"marketlisting\" />\
\       </listing>\
\            </marketplacelistings>\
\</item>\
\</items>"
