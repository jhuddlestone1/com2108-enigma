{-
  COM2108  Assignment 2a
  2019  Jamie Huddlestone, Dr Emma Norling (where annotated)
-}

-- Module imports ----------------------------------------------------------------------------------


module Enigma where

  import Data.Char
  import Data.List


-- Data types --------------------------------------------------------------------------------------


  type Cipher = String
  type Substitution = (Char, Char)

  type Rotor = Cipher
  type Reflector = [Substitution]
  type Offsets = (Int, Int, Int)
  type SteckerPair = Substitution
  type Steckerboard = [Substitution]
  type Crib = (String, String)
  type Menu = [Int]

  data Enigma
    = SimpleEnigma {
        rotorL :: Rotor,
        rotorM :: Rotor,
        rotorR :: Rotor,
        reflector :: Reflector,
        offsets :: Offsets
      }
    | SteckeredEnigma {
        rotorL :: Rotor,
        rotorM :: Rotor,
        rotorR :: Rotor,
        reflector :: Reflector,
        offsets :: Offsets,
        stecker :: Steckerboard
      }
    deriving (Show, Eq)


-- Definitions from AssignmentHelp, as provided by Emma --------------------------------------------


  rotor1 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
  rotor2 = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
  rotor3 = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
  rotor4 = "ESOVPZJAYQUIRHXLNFTGKDCMWB"
  rotor5 = "VZBRGITYUPSDNHLXAWMJQOFECK"

  reflectorB = [('A','Y'),
                ('B','R'),
                ('C','U'),
                ('D','H'),
                ('E','Q'),
                ('F','S'),
                ('G','L'),
                ('I','P'),
                ('J','X'),
                ('K','N'),
                ('M','O'),
                ('T','Z'),
                ('V','W')]

  fromMaybe :: Maybe a -> a
  fromMaybe (Just x) = x

  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'


-- Functions ---------------------------------------------------------------------------------------


  alphabet = ['A'..'Z']

  alphaLen = length alphabet

  -- Comparator for use when sorting by list length
  compLen :: [a] -> [b] -> Ordering
  compLen a b = compare (length a) (length b)

  -- Sorts lists of 2-tuples by comparing the second element
  sortBySnd :: Ord o => [(e, o)] -> [(e, o)]
  sortBySnd = sortBy (\(_,a) (_,b) -> compare a b)

  -- Creates a list of 2-tuples corresponding to (plainChar, encodedChar)
  zipCipher :: Cipher -> Int -> [Substitution]
  zipCipher cipher offset = zip alphabet $ drop (offset `mod` length cipher) (cycle cipher)

  encode :: Cipher -> Int -> Char -> Char
  encode cipher offset char = snd (zipCipher cipher offset !! alphaPos char)

  reverseEncode :: Cipher -> Int -> Char -> Char
  reverseEncode cipher offset char = fst (sortBySnd (zipCipher cipher offset) !! alphaPos char)

  substitute :: [Substitution] -> String -> String
  substitute [] message = message
  substitute ((x,y):z) message = substitute z $ map sub message
    where sub char | char == y = x
                   | char == x = y
                   | otherwise = char

  -- Performs substitutions on a single char only; prefer the substitute function above where poss.
  subChar :: [Substitution] -> Char -> Char
  subChar subs char = head $ substitute subs [char]

  -- Function provided by Emma - it's a lot cleaner than what I came up with (see below)!
  advanceOffsets :: Offsets -> Offsets
  advanceOffsets (l, m, r)
    | r < 25    = (l, m, r+1)
    | m < 25    = (l, m+1, 0)
    | otherwise = (l+1 `mod` alphaLen, 0, 0)

  -- advanceOffsets :: Offsets -> Offsets
  -- advanceOffsets (l, m, r) = (wrap(l + cm), wrap(m + cr), wrap(r + 1))
    -- where wrap n = n `mod` alphaLen
          -- cr     = max 0 (r + 1 - alphaLen + 1)  -- carry bit from right
          -- cm     = max 0 (m + cr - alphaLen + 1) -- carry bit from middle

  enigmaEncode :: Char -> Enigma -> Char
  enigmaEncode char (SimpleEnigma lr mr rr reflector offsets) =
    let (ol, om, or) = advanceOffsets offsets
        rtl r o c    = reverseEncode alphabet o $ encode r o c
        ltr r o c    = reverseEncode r o $ encode alphabet o c
        rightToLeft  = rtl lr ol . rtl mr om . rtl rr or
        leftToRight  = ltr rr or . ltr mr om . ltr lr ol
    in leftToRight . subChar reflector . rightToLeft $ char
  enigmaEncode char (SteckeredEnigma lr mr rr reflector offsets stecker) =
    subChar stecker (enigmaEncode (subChar stecker char) simpleEnigma)
    where simpleEnigma = SimpleEnigma lr mr rr reflector offsets

  enigmaEncodeMessage :: String -> Enigma -> String
  enigmaEncodeMessage [] _ = []
  enigmaEncodeMessage (x:xs) enigmaState@(SimpleEnigma lr mr rr reflector offsets) =
    enigmaEncode x enigmaState : enigmaEncodeMessage xs nextEnigmaState
    where nextEnigmaState = SimpleEnigma lr mr rr reflector (advanceOffsets offsets)
  enigmaEncodeMessage message (SteckeredEnigma lr mr rr reflector offsets stecker) =
    substitute stecker (enigmaEncodeMessage (substitute stecker message) simpleEnigma)
    where simpleEnigma = SimpleEnigma lr mr rr reflector offsets

  longestMenu :: Crib -> [Int]
  longestMenu (plaintext, ciphertext) = nub . maximumBy compLen $ [] : menus
    where subs  = zip3 [0..] plaintext ciphertext
          edges = [(i,j) | (i,_,a) <- subs, (j,b,_) <- subs, a == b]
          menus = [chain start edges | start <- edges]

  -- Creates a list of pairs, where the second item in a pair matches the first item in the next
  chain :: Ord o => (o, o) -> [(o, o)] -> [o]
  chain (x, y) links = x : (maximumBy compLen $ [y] : [chain z unvisited | z <- linked])
    where unvisited = filter (\(n,_) -> n /= x) links
          linked    = filter (\(n,_) -> n == y) unvisited


-- Test harness ------------------------------------------------------------------------------------


  assert :: Eq e => Bool -> e -> e -> Bool
  assert bool input output = bool == (input == output)

  -- Report enumerated results for all assertions
  testAll = zip [0..] testSuite

  -- Report if test suite passed (True/False), and list indices of failures if applicable
  test = (all (== True) testSuite, [fst result | result <- testAll, snd result == False])


  -- List of tests for this module:
  testSuite = [ -- START TESTS

    -- Test for correct encoding of chars, as per examples in the brief and assignment FAQs
    assert True  (enigmaEncode 'A' simple321_25) 'N',
    assert True  (enigmaEncode 'N' simple321_25) 'A',
    assert True  (enigmaEncode 'I' simple123_0) 'H',
    assert False (enigmaEncode 'I' simple123_1) 'H',
    assert False (enigmaEncode 'I' simple123_25) 'H',
    assert False (enigmaEncode 'I' simple321_25) 'H',
    assert False (enigmaEncode 'I' simple321_1) 'H',
    assert False (enigmaEncode 'I' simple321_0) 'H',

    -- Test for correct encoding of messages, as per examples in the brief and assignment FAQs
    assert True  (enigmaEncodeMessage "A" simple321_25) "N",
    assert True  (enigmaEncodeMessage "N" simple321_25) "A",
    assert False (enigmaEncodeMessage "A" simple321_0) "N",
    assert False (enigmaEncodeMessage "N" simple321_0) "A",
    assert True  (enigmaEncodeMessage testMessage simple123_0) testEncodedMessage,
    assert False (enigmaEncodeMessage testMessage simple123_1) testEncodedMessage,
    assert False (enigmaEncodeMessage testMessage simple123_25) testEncodedMessage,
    assert False (enigmaEncodeMessage testMessage simple321_0) testEncodedMessage,
    assert False (enigmaEncodeMessage testMessage simple321_1) testEncodedMessage,
    assert False (enigmaEncodeMessage testMessage simple321_25) testEncodedMessage,

    -- Test the symmetric encoding/decoding property
    assert True  (enigmaEncodeMessage (enigmaEncodeMessage testMessage simple123_0) simple123_0) testMessage,
    assert False (enigmaEncodeMessage (enigmaEncodeMessage testMessage simple123_0) simple123_1) testMessage,
    assert False (enigmaEncodeMessage (enigmaEncodeMessage testMessage simple123_1) simple123_0) testMessage,
    assert True  (enigmaEncodeMessage (enigmaEncodeMessage testMessage simple123_1) simple123_1) testMessage,

    assert True  (enigmaEncodeMessage (enigmaEncodeMessage testMessage steckered123_0) steckered123_0) testMessage,
    assert False (enigmaEncodeMessage (enigmaEncodeMessage testMessage steckered123_0) steckered123_1) testMessage,
    assert False (enigmaEncodeMessage (enigmaEncodeMessage testMessage steckered123_1) steckered123_0) testMessage,
    assert True  (enigmaEncodeMessage (enigmaEncodeMessage testMessage steckered123_1) steckered123_1) testMessage,

    -- Test properties of longestMenu
    assert True  (longestMenu ("","")) [],
    assert True  (longestMenu ("A","A")) [0],
    assert True  (longestMenu ("AB","BC")) [0,1],
    assert True  (longestMenu testCrib) testMenu

    ] -- END TESTS


  -- Test artifacts for use above
  simple123_0 = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0)
  simple123_1 = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,1)
  simple123_25 = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (25,25,25)
  simple321_0 = SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,0)
  simple321_1 = SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,1)
  simple321_25 = SimpleEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25)

  steckered123_0 = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0) [('E','X'),('S','W')]
  steckered123_1 = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,1) [('E','X'),('S','W')]
  steckered123_25 = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (25,25,25) [('E','X'),('S','W')]
  steckered321_0 = SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,0) [('E','X'),('S','W')]
  steckered321_1 = SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,1) [('E','X'),('S','W')]
  steckered321_25 = SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25) [('E','X'),('S','W')]

  testCrib = ("WETTERVORHERSAGEBISKAYA",
              "RWIVTYRESXBFOGKUHQBAISE")

  testMenu = [13,14,19,22,4,3,6,8,12,7,1,0,5,21,18,16,9]

  testMessage = "INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"

  testEncodedMessage = "HQRFMNYYVUUNBHACFQDZYSABBUEXJJGJPSFQGNTAJNLNZEIEPUSAXSYEKUBAHXLJZEUCGRFLYHUCDKDMKLZRPCQFMAHTGVYSSEYKUTWRXFHFMZRUWNNKCRTBNHSIOUWODBTAZXPRSJALISVOTAFSFXETWMZRVFRLJNYCWYNMKVBGJTUJKDQBZTNBRSXUGDJRRBUWJBKVCAAWMSSFELVIPOHZTDGOXIZDQGHNLADFAXHVFKGQYASKCZEFAWFABPIITZQPUWXJRHDFLLSMKIMVCIWEJCYSULAAWVQLOVHGJOKYFHIWVFBATADWVYARQBFEAWHLCKGDRXDRMSMNNBSKHFYIRSYHLQGCEQKIDQEXGIMHTUGHISMWQBWERWLGLEATJIJPRWZJISCGDIVXJCRWJTCJNOFDEBXBGSRRICMQXUZHDVYQVFTNXQVCLOBCNZGKSQUFTAOZUHXURSKLKZFHBBYPQTDILBLXCOSAMFHNEGJPXXBCGAXVSRIVSWSRSQOWUAGZSYVOAEMQHUOFJYKOGRFAXUQLYCPGCFMCOPIBIYGJJJZAFSJVSLRBAJZVWITZKJMFWSBGKTLVOCSWHTDSYVWYNHYZMNISJHSPLXTJGIQVNJHGYWLOCTXGCGKHAURIKBNSAMKLPJWQVAVZOHYNUBEPNAXILRQWDIQDYYPZVXBHLTLSSFXBJGJVVNHZHMWKLCWENMLOYDITLQCERPYNODYZLAPLYPLCEWOMJCEKSKRSAQKCLUMNBYWWWJAHHVEOYKXHOYYNUREFGGTVMJYMJLYUNQKMMWYJQMZXDFVFSIEKYVFTMMFAJSLBQBCKWBDUGKCJSJLRYHGADWCWMTSTKRGGPYRBOLPGZUVVKPRKCFAEJWWVVPWAHEGHKDAVPMXVHBLPWIVYILHKDSKWCSDWLVHRLOSUHCSKUDTAVIIFRXUFBWFYZLAQWBQJADGJOFDEFWGVXSKEYQCKCFTZWMBIQNWLRAXJOONXTNJQMZCREOIQZYYPIVIQEXFSHAZIOKYXJHJCHIWGWWZSIAYJPJVBKWDFKZUOUBYIGVMLCIZWIFKDELOULELFBUBUEJMUTMGTQUDIGIKZLZKNDGYQUAHODPSHEBEEOSNHUBTNPNAUQKJIZFYXHDQOQBXSCRWICRMGBETZKZBURJCHITCUBFJJHSXOLXUQRGKWGJBPKNNODIBHFOCKYDEVRVZITAMPVZPZLEKFLZKHBVLYTWBFCCUWMXGLSRQALPJQPTISHPWDBQAMBMKSKIZCQCHLGPDUVRWYWW"
