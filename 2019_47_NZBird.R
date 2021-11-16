library(data.table)
library(magrittr)
library(ggplot2)
bird_o <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")
bird_o <- as.data.table(nz_bird)
bird_o <- bird_o[!is.na(bird_breed)]

bird <- copy(bird_o)
loserTotal <- character()
loserTotalRank <- integer()
n <- 0
repeat{
  currentRound<-bird[vote_rank == "vote_1",.N ,by = bird_breed]#[order(-N)]
  # is it 50%?
  if(currentRound[N == max(N),N]/currentRound[,sum(N)]>0.5) break
  loser <- currentRound[N == min(N), bird_breed]
  loserTotalRank <- c(loserTotalRank,currentRound[N == min(N), N])
  loserTotal <- c(loserTotal, loser)
  loserRows <- bird[vote_rank == "vote_1" & bird_breed %in% loser,which = TRUE]
  loserRows <- ifelse(max(loserRows) != nrow(bird), loserRows + 1 , loserRows[-max(loserRows)] + 1)
  bird[loserRows + 1, vote_rank := "vote_1"]
  bird <- bird[!(bird_breed %in% loser)]
  n<-n+1
}

# add the losers to the current Round
final <- data.table(bird_breed  = loserTotal, N = loserTotalRank) %>% rbind(currentRound)

ggplot(final, aes(x= bird_breed, y = N))



