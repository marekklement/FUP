import SedmaDatatypes

-- Test data cards
c = [Card Heart R10,Card Heart R7,Card Diamond RA,Card Diamond R10]

-- Equal of rank of given cards
instance Eq Rank where
    (==) R7  R7  = True
    (==) R8  R8  = True
    (==) R9  R9  = True
    (==) R10 R10 = True
    (==) RJ  RJ  = True
    (==) RQ  RQ  = True
    (==) RK  RK  = True
    (==) RA  RA  = True
    (==) _ _     = False

-- Equal of suid of given cards
instance Eq Suit where
    (==) Club    Club    = True
    (==) Heart   Heart   = True
    (==) Spade   Spade   = True
    (==) Diamond Diamond = True 
    (==) _ _             = False 

-- Equal of cards - rank and suit must be specified too
instance Eq Card where
    (==) (Card col1 type1) (Card col2 type2) = (col1 == col2) && (type1 == type2)

-- Checks for unique cards
notUniqueKarty :: Eq a => [a] -> Bool
notUniqueKarty [] = False
notUniqueKarty karty = 
        let firstElem = head karty
            zbytek    = tail karty
        in if (elem firstElem zbytek) then True else notUniqueKarty zbytek

type Pair = (Team, Int)

roundX :: Cards -> Rank -> Team -> Team -> Int -> Pair
roundX [] _ _ winning cost =  (winning, cost)
roundX karty first playingTeam winning cost = 
    let karta  = head karty
        others = tail karty
        actualRank = giveRank karta
        nextFirst = if ((length karty) == 4) then actualRank else first
        nextTeam = switchTeams playingTeam
        nextWinning = if ((length karty) == 4 || isSameRankOrSeven actualRank nextFirst) then playingTeam else winning
        nextCost = cost + (getCostForCard actualRank)
    in roundX others nextFirst nextTeam nextWinning nextCost


giveRank :: Card -> Rank
giveRank (Card _ rank) = rank

switchTeams :: Team -> Team
switchTeams AC = BD
switchTeams BD = AC

isSameRankOrSeven :: Rank -> Rank -> Bool
isSameRankOrSeven R7 _ = True
isSameRankOrSeven curr first = curr == first

getCostForCard :: Rank -> Int
getCostForCard R10 = 10
getCostForCard RA = 10
getCostForCard _ = 0

getWinnerForPair :: Pair -> Team
getWinnerForPair (a,_) = a


getScoreForPair :: Pair -> Int
getScoreForPair (_,b) = b

cover :: Cards -> Team -> Int -> Int -> Bool -> Bool -> Maybe Winner
cover [] _ accounter bdcounter acwon bdwon = getFinalResult accounter bdcounter acwon bdwon
cover karty playing accounter bdcounter acwon bdwon = 
    let actRes = roundX (take 4 karty) R8 playing playing 0
        nextKarty = drop 4 karty
        nextPlaying = getWinnerForPair actRes
        sc = getScoreForPair actRes
        score = if (length nextKarty) == 0 then sc + 10 else sc
        nextacCounter = if nextPlaying == AC then accounter + score else accounter
        nextbdCounter = if nextPlaying == BD then bdcounter + score else bdcounter
        nextAcWon = if nextPlaying == AC then True else acwon
        nextBdWon = if nextPlaying == BD then True else bdwon
    in cover nextKarty nextPlaying nextacCounter nextbdCounter nextAcWon nextBdWon


getFinalResult :: Int -> Int -> Bool -> Bool -> Maybe Winner
getFinalResult accounter bdcounter acwon bdwon 
    | acwon == False = Just (BD, Three)
    | bdwon == False = Just (AC, Three)
    | accounter == 0 = Just (BD, Two)
    | bdcounter == 0 = Just (AC, Two)
    | bdcounter > accounter = Just (BD, One)
    | otherwise = Just (AC, One)

-- start eval function
replay :: Cards -> Maybe Winner
replay [] = Nothing
replay karty | (length karty) /= 32 = Nothing
             | (notUniqueKarty karty) = Nothing
             | otherwise = cover karty AC 0 0 False False

b = [Card Club RA    ,Card Club RK      ,Card Club RQ       ,Card Club RJ,
     Card Club R10   ,Card Club R9      ,Card Club R7       ,Card Club R8,
     Card Spade RA   ,Card Spade RK     ,Card Spade RQ      ,Card Spade RJ,
     Card Spade R10  ,Card Spade R9     ,Card Spade R7      ,Card Spade R8,
     Card Diamond RA ,Card Diamond RK   ,Card Diamond RQ    ,Card Diamond RJ,
     Card Diamond R7 ,Card Diamond R9   ,Card Diamond R8    ,Card Diamond R10,
     Card Heart RJ   ,Card Heart RK     ,Card Heart RQ      ,Card Heart RA,
     Card Heart R7   ,Card Heart R9     ,Card Heart R8      ,Card Heart R10]