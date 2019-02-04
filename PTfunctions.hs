-- module PTfunctions
quadraticVertex a b c = (-b)/ (2*a)
quadraticDiscriminant a b c = b*b - 4*a*c

quadraticRealSolutionQty a b c = if quadraticDiscriminant a b c< 0
                        then 0
                        
                        else if quadraticDiscriminant a b c == 0
                        then 1
                        
                        else 2
                       
convertFtoC x = (x-32) / 5/9
convertCtoF x = ( x*9/5 ) + 32
convertTemp x u = if u == "f"
                    then convertFtoC x
                    else convertCtoF x
