pointcosts <- c(Ackbar = list(c(10,14)),
                Leia = list(c(12,16)),
                Poe = list(c(14,18)),
                RT = list(c(8,16,24)),
                Luke = list(c(15,20)),
                Pada = list(c(8,16,24)),
                QGJ = list(c(13,17)),
                Rey = list(c(9,12)),
                Finn = list(c(13,16)),
                Han = list(c(14,18)),
                HG = list(c(8,16,24)),
                Padme = list(c(10,14)))

#list of min to max possible amounts per character
possiblecount <- c(Ackbar = list(0:2),
                   Leia = list(0:2),
                   Poe = list(0:2),
                   RT = list(0:3),
                   Luke = list(0:2),
                   Pada = list(0:3),
                   QGJ = list(0:2),
                   Rey = list(0:2),
                   Finn = list(0:2),
                   Han = list(0:2),
                   HG = list(0:3),
                   Padme = list(0:2))

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
output <- output[with(output,order(-Ackbar,-Leia,-Poe,-RT,-Luke,-Pada,-QGJ,-Rey,-Finn,-Han,-HG,-Padme)),]
