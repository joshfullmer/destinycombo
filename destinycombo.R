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

lowerbound <- 31-min(unlist(pointcosts))

possibilities <- do.call(expand.grid,possiblecount)

possibilities$sum <- rowSums(possibilities)

possibilities <- subset(possibilities,possibilities$sum<=4)

possibilities <- possibilities[,!names(possibilities) %in% ("sum")]

possibilities$include <- FALSE
possibilities$pointsum <- 0

for(i in 1:nrow(possibilities)) {
   rowsum <- 0
   for(j in 1:length(pointcosts)) {
      if(possibilities[i,j]==0)
         next
      value <- pointcosts[[j]][possibilities[i,j]]
      rowsum <- value + rowsum
   }
   possibilities[i,]$include <- ifelse((rowsum >= lowerbound) & (rowsum <= 30),TRUE,FALSE)
   possibilities[i,]$pointsum <- rowsum
}

output <- possibilities[possibilities$include==T,]

output <- output[with(output,order(-Phasma,-FOS,-GG,-Veers,-Dooku,-Vader,-Kylo,-NS,-Bala,-Jabba,-Jango,-TR)),]
