pointcosts <- c(Phasma = list(c(12,15)),
                FOS = list(c(7,14,21,28)),
                GG = list(c(13,18)),
                Veers = list(c(11,14)),
                Dooku = list(c(11,15)),
                Vader = list(c(16,21)),
                Kylo = list(c(10,13)),
                NS = list(c(8,16,24)),
                Bala = list(c(8,11)),
                Jabba = list(c(11,14)),
                Jango = list(c(12,16)),
                TR = list(c(9,18,27)))

#list of min to max possible amounts per character
possiblecount <- c(Phasma = list(0:2),
                   FOS = list(0:4),
                   GG = list(0:2),
                   Veers = list(0:2),
                   Dooku = list(0:2),
                   Vader = list(0:2),
                   Kylo = list(0:2),
                   NS = list(0:3),
                   Bala = list(0:2),
                   Jabba = list(0:2),
                   Jango = list(0:2),
                   TR = list(0:3))

#generate lower bound based off of the lowest cost character
lowerbound <- 31-min(unlist(pointcosts))

#generate all possible combinations, including illegal ones
possibilities <- do.call(expand.grid,possiblecount)

#because we know the maximum number of characters is 4, 
#we cut everything higher than that for computational help
#then remove the character sum column
possibilities$sum <- rowSums(possibilities)
possibilities <- subset(possibilities,possibilities$sum<=4)
possibilities <- possibilities[,!names(possibilities) %in% ("sum")]

#create new columns in the dataframe with default values
possibilities$include <- FALSE
possibilities$pointsum <- 0

#loop through the reasonable possibilities
for(i in 1:nrow(possibilities)) {
   rowsum <- 0
   for(j in 1:length(pointcosts)) {
      if(possibilities[i,j]==0)
         next #we skip this, because there is no 0th item in the pointcosts
      
      #this is the point value at the particular point in the loop
      value <- pointcosts[[j]][possibilities[i,j]]
      rowsum <- value + rowsum
   }
   #indicate the row is to be included in the final product if it's between the upper
   #and lower bounds
   possibilities[i,]$include <- ifelse((rowsum >= lowerbound) & (rowsum <= 30),TRUE,FALSE)
   possibilities[i,]$pointsum <- rowsum #stored for ease of access
}

#subsets the possibilities with those found to match the rules
output <- possibilities[possibilities$include==T,]

#sort by each column, left to right
output <- output[with(output,order(-Phasma,-FOS,-GG,-Veers,-Dooku,-Vader,-Kylo,-NS,-Bala,-Jabba,-Jango,-TR)),]
