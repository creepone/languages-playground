lastButOne xs = if (length xs) < 2 
                then [] 
                else if (length xs) == 2 
                     then [head xs]
                     else lastButOne (tail xs)