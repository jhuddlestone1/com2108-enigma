{-
  COM2108  Assignment 3
  2019  Jamie Huddlestone
-}

module Bombe where

  import Debug.Trace
  import Data.Char
  import Data.List
  import Enigma


  rotors = [rotor1, rotor2, rotor3, rotor4, rotor5]
  rotorList = [(l,m,r) | l <- rotors, m <- rotors, r <- rotors, m /= r, l/= r, l /= m]
  reflectorList = [reflectorB]
  offsetList = [(l,m,r) | l <- [0..25], m <- [0..25], r <- [0..25]]


  breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
  breakEnigma crib
    | null solution = Nothing
    | otherwise     = Just (offsets (fromMaybe solution), stecker (fromMaybe solution))
    where solution  = breakEnigmaFull crib


  breakEnigmaFull :: Crib -> Maybe Enigma
  breakEnigmaFull crib@(plaintext, ciphertext)
    | null solutions = Nothing
    | otherwise      = head solutions
    where subs       = zip plaintext ciphertext
          menu       = longestMenu crib
          startChar  = fst (subs !! head menu)
          settings   = [--trace (show (lr, mr, rr, offsets)) -- show current rotor settings for debugging
                        SteckeredEnigma lr mr rr reflector offsets [(startChar, startChar)] |
                        (lr, mr, rr) <- rotorList,
                        reflector <- reflectorList,
                        offsets <- offsetList ]
          attempts   = [findStecker subs menu enigma | enigma <- settings]
          solutions  = [result | result <- attempts, result /= Nothing]


  findStecker :: [Substitution] -> Menu -> Enigma -> Maybe Enigma
  findStecker subs menu (SteckeredEnigma lr mr rr reflector offsets [(p,_)])
    | null solutions = Nothing
    | otherwise      = head solutions
    where pairs      = [(p, x) | x <- alphabet, x /= p]
          firstGuess = SteckeredEnigma lr mr rr reflector offsets [] -- <==> [(startChar, startChar)]
          guesses    = firstGuess : [SteckeredEnigma lr mr rr reflector offsets [pair] | pair <- pairs]
          attempts   = [followMenu subs menu enigma | enigma <- guesses]
          solutions  = [result | result <- attempts, result /= Nothing]


  followMenu :: [Substitution] -> Menu -> Enigma -> Maybe Enigma
  followMenu subs [] enigma = Just enigma
  followMenu subs (i:is) (SteckeredEnigma lr mr rr reflector offsets stecker)
    | null nextStecker = Nothing
    | otherwise = followMenu subs is (SteckeredEnigma lr mr rr reflector offsets $ fromMaybe nextStecker)
    where positions   = iterate advanceOffsets offsets !! i
          (p, c)      = subs !! i
          q           = subChar stecker p
          r           = enigmaEncode q (SimpleEnigma lr mr rr reflector positions)
          nextStecker = steckerAdd (r, c) stecker


  steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
  steckerAdd (r, c) stecker
    | duplicate      = Just stecker
    | eitherChar     = Nothing
    | r == c         = Just stecker
    | otherwise      = Just ((r, c):stecker)
    where chars      = foldr (\(x, y) z -> x:y:z) [] stecker
          eitherChar = r `elem` chars || c `elem` chars
          duplicate  = (r, c) `elem` stecker || (c, r) `elem` stecker


-- Test code ---------------------------------------------------------------------------------------


  testBombe = [enigmaEncodeMessage m (fromMaybe $ breakEnigmaFull crib) | crib@(_,m) <- testCribs]

  testCribs = [

    ("COMPUTERSCIENCECALIBRATIONSTRINGTESTINGONETWOTHREE", -- partial solution
     "QWAVMZPNGFQVGWGYCKCXXHMEXTCGWPFOCWCSYXAEFUNXQFIZJW"),

    ("AFJEQTMC",
     "FJEQTMCF"),

    ("ZGXWAUTS",
     "XKGZWAUT"),

    ("TURINGBOMBEHASKELLSIMULATIONSTOP",
     "FDLQIYHKFXSYEEXAYTWJBNNMFCHUACVMERSLXIXVWCCOBSVUESKCQGKSCXSQUMCWLWXCWNDEKHCGRKAUWLSCNUUROQVOTZCWUICNEXDCQPKQKHSTFTCQJXEFKDLKOTH"),

    ("COMPUTERSCIENCESHEFFIELDUNIVERSITYSTOP",
     "YZCSDCVUFVJAAEMVILWRVSQZFCBPJFVYHUUPHLAPJMTMFNLURRADJFCBRBXBCUSSXVYWAPQIRCUVVNODKELDMNNQHYFEFOZPBUIPWKPXIYPKQHMVOAVXFVDCKMZOULMTQNUFBVHFUSXYCYPWFKBYW")

    ]