{- 
    Write an interactive program that does the FizzBuzz problem that was in
    an earlier homework assignment. Here is an example of how the program should work:

    How many numbers shall we print? 25
    For multiples of what number shall we print 'Fizz'? 3
    For multiples of what number shall we print 'Buzz'? 5
    1
    2
    Fizz
    4
    Buzz
    Fizz
    7
    8
    Fizz
    Buzz
    11
    Fizz
    13
    14
    FizzBuzz
    16
    17
    Fizz
    19
    Buzz
    Fizz
    22
    23
    Fizz
    Buzz
    
    Tips:
    
    * Example of getting an Integer (as opposed to a String) from the user: 
        numVariable  <- readLn
      (Clarification: It gets whatever data type is required.)
    
    * Use your fizzBuzz function from the lists homework assignment. You can
      copy that code into this file.
      
    * Use mapM_ with putStrLn to print the result of calling fizzBuzz.

-} 

numList n = [x | x <- [1..n]]
numTostring n = (show (numList n ) )
fizzBuzz n f b = [if n`mod` (f)== 0 && n`mod` (b)== 0
                    then "FizzBuzz!" 
                    else if n `mod`(f) == 0 
                            then "Fizz!" 
                            else if n `mod`(b) == 0  
                              then "Buzz!" 
                              else show n   | n <- [1..n]]

-- Copy your fizzBuzz function and supporting functions here


main = do
    putStrLn "How many numbers shall we print ? "
    nums  <- readLn
    putStrLn "For multiples of what number shall we print 'Fizz'?"
    fnum  <- readLn
    putStrLn "For multiples of what number shall we print 'Buzz'?"
    bnum  <- readLn
    mapM_ putStrLn $ fizzBuzz nums fnum bnum
    


    
