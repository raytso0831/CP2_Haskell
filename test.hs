funcPlusTwo f n = f (n+2)
doubleIt n = n * 2 
allLowerCaseLetters = ['a'..'z']
allIntegersInclusive a b = if a< b
                        then [a..b]
                           else [a,a-1..b]
                           
allIntegersExcludingFirst a b = if a< b
                            then tail [a..b]
                                else tail [a,a-1..b]
allThousands = [1000 .. 9999]

integersUpDownUp a b c d = allIntegersInclusive a b ++ allIntegersExcludingFirst b c ++ allIntegersExcludingFirst c d

isDivisor n d = n `mod` d == 0

divisors n = [x | x <- [1..n-1], isDivisor n x ]

isPerfect n = n == sum (divisors n)

allPerfects n = [x | x <- [1..n], isPerfect x ]


stringlist n = [[c] | c <- show  n]
numConverter n = [(read :: String -> Int) s | s <- stringlist n]
isRigid n = sum(numConverter n) == product(numConverter n)

allRigids n = [x | x <- [1..n], isRigid x ]

numList n = [x | x <- [1..n]]
numTostring n = (show (numList n ) )
fizzBuzz n f b = [if n`mod`(f*b)== 0 then "FizzBuzz!" else if n `mod`(f) == 0 then "Fizz!" else "Buzz!" | n <- [1..n], f <- [f], b <- [b]]
 

    
windChillRisk :: (Floating a, Ord a) => a -> a -> String
windChillRisk t v
    | twc >= low = "Low Risk"  
    | twc >= moderate = "Moderate Risk"  
    | twc >= high = "High Risk"  
    | twc >= veryhigh = "Very High Risk"
    | twc >= severe = "Severe Risk"
    | otherwise = "Extreme Risk"  
    
    where twc = 35.74 + 0.6215*t - 35.75*v**0.16 + 0.4275*t*v**0.16  
          low = 16
          moderate = -17
          high = -38
          veryhigh = -53
          severe = -65

oneStr = '9'
manyStrList = ['8','7','6']
moreNum = [10..100]
recips = [1/x | x <- moreNum]
nums = [1,2,3,4,5,6,7,8]

yoCount :: String -> Integer
-- replace the "-99" bits with your own code below
yoCount [] = 0 -- empty string passed in? How many yo is that?
yoCount (_:[]) = 0 -- a single character passed in? How many yo is that?
yoCount ('y':'o':s) = 1 + yoCount s -- a string starting with "yo". What to do here?
yoCount ('Y':'o':s) = 1 + yoCount s -- a string starting with "Yo". What to do here?
yoCount s = yoCount (tail s ) -- a string that doesn't start with "yo" or "Yo". What to do here?


interestTable p r 0 = [round p]
interestTable p r n = [round p] ++ interestTable (p * (1 +0.01*r)) r (n-1)

charToPhoneDigit :: Char -> Int
charToPhoneDigit char
    | (char `elem` "ABC") || (char `elem` "abc") = 2
    | (char `elem` "DEF") || (char `elem` "def") = 3
    | (char `elem` "GHI") || (char `elem` "ghi") = 4
    | (char `elem` "JKL") || (char `elem` "jkl") = 5
    | (char `elem` "MNO") || (char `elem` "mno") = 6
    | (char `elem` "PQRS") || (char `elem`"pqrs") = 7
    | (char `elem` "TUV") || (char `elem` "tuv") = 8
    | (char `elem` "WXYZ") || (char `elem`"wxyz") = 9

a = [5,10..20]
b = [x | x <- a , 0 == mod x 15]

numToNumList []= 0
numToNumList l = last l + (numToNumList (init l)) * 10


transForm :: String -> String
transForm x = blah ++ blah
    where blah = 'i':x
    
recurzhun ::
    (Num a , Eq a) => a -> a 
recurzhun 0 = 1
recurzhun x = x * recurzhun (x-1)

lFunc = \a -> a:a:[]
strs= ["lol", "whet", "what", "wat"]


strs3 = ["when","arrive","three"]
nums1 = [1,2,3,4]

lFunc2 :: Char -> Char
lFunc2 = \x -> succ x 

mF :: Num a => a -> a -> a -> a 
mF a b c = a*b + c

oF :: Num a => a -> a -> a 
oF x y = x*y

xF :: Floating a => a -> a 
xF a = a**2

yF :: Floating a => a -> a 
yF a = a/2

letItBe :: Num a => a -> a -> a 
letItBe x y = 
    let b = x + y
    in b*2

recurStr :: String -> String
recurStr [] = ""
recurStr s =
    head s : recurStr (tail s )
    


