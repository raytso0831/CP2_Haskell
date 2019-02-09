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