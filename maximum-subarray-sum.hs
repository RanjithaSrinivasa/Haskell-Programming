maxsubarray :: [Int] -> Int
maxsubarray = maxsubarray' 0 0
    where
      maxsubarray' maxsofar maxendinghere [] = maxsofar
      maxsubarray' maxsofar maxendinghere (x:xs) = maxsubarray' maxsofar' maxendinghere' xs
          where
             maxendinghere' = max x (maxendinghere + x)
             maxsofar' = max maxsofar maxendinghere'


  
         
                