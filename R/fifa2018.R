# See 
# https://www.kaggle.com/agostontorok/soccer-world-cup-2018-winner?utm_medium=email&utm_source=mailchimp&utm_campaign=datanotes-20160614
# for inspiration and data sets

library(data.table)

# Parameters ####

# For evaluation the strength of each team, existing results are used up to the following date
# Forecast of future results is computed started from the day after this date
opt.date <- as.Date("2018-06-22")  # ok until 2018-06-20

# Number of monte-carlo rounds
opt.rounds <- 100000

# how many years of past matches should be used for strength estimation
opt.years <- 30

# set weight for oldest matches at opt.years. Newest matches have weight 1.0
opt.fulldecay <- 0.01


if (file.exists("data/results.RData")) {
  load("data/results.RData")
} else {
  results_by_day <- list()
}


# Download datasets ####

# kaggle datasets download -d martj42/international-football-results-from-1872-to-2017
history<-read.csv("data/international-football-results-from-1872-to-2017.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")
setDT(history)
history$date<-as.Date(history$date, "%Y-%m-%d")
history<-history[, c("date", "home_team", "away_team", "home_score", "away_score")]
colnames(history)<-c("date", "team1", "team2", "score1", "score2")

# get current results (will be updated during the fifa world cup 2018)
dummy <- readLines("https://fixturedownload.com/results/fifa-world-cup-2018") # need to do this first
worldcup<-read.csv("https://fixturedownload.com/download/fifa-world-cup-2018-RussianStandardTime.csv", header=T, stringsAsFactors = F)
write.csv(worldcup, "data/FIFA_Worldcup_2018_results.csv", row.names = F)
# worldcup <- read.csv("data/FIFA_Worldcup_2018_results.csv", header=T, stringsAsFactors=F, encoding="UTF-8")
setDT(worldcup)
rm(dummy)


# Prepare datasets ####

worldcup$Home.Team<-gsub("\t","",worldcup$Home.Team)
worldcup$Away.Team<-gsub("\t","",worldcup$Away.Team)
worldcup<-worldcup[, -3]
colnames(worldcup)<-c("round","date","team1", "team2", "group", "result")
worldcup$date<-as.Date(worldcup$date, "%d/%m/%Y")
worldcup[
  !is.na(result) & result!=""
  , `:=`(
    score1=as.numeric(sub("-.*$","", result))
    , score2=as.numeric(sub("^.*-","",result))
  )]

# remove existing results after the given date to allow forecasting
worldcup[date>opt.date, c("score1", "score2"):=NA]

worldcup[
  !is.na(result) & result!=""
  , win:=ifelse(score1>score2,1,ifelse(score1<score2,-1,0))
]
worldcup[, result:=NULL]

history<-rbind(
  history,
  worldcup[!is.na(score1) & !is.na(score2), c("date","team1", "team2", "score1", "score2")]
)

# add symmetric copy to history table (A vs B => B vs A), each match is now twice in history
history<-rbind(
  history
  ,history[, list(date, "team1"=team2, "team2"=team1, "score1"=score2, "score2"=score1)]
)

# Prepare final results table with the participating teams
results<-unique(rbind(
  worldcup[worldcup$round %in% c("1","2","3"), list(group, team=team1)]
  ,worldcup[worldcup$round %in% c("1","2","3"), list(group, team=team2)]
))
results[, c("P1","P2","P3","P4","G1","G2","F8","F4","F2","F1"):=0]
# P1-4:   Place 1-4
# G1/G2:  winner/runner-up in group
# F8:     reached round of 16 (G1+G2)
# F4:     reached quarter finals
# F2:     reached semi finals
# F1:     reached finals


# Strength estimation per team ####

# For strength computation only use matches from participating teams
history<-history[(team1 %in% results$team) & (team2 %in% results$team)]

history$TimePassed<-as.numeric(max(history$date)-history$date)
history<-history[TimePassed<=365*opt.years] 
decay.factor <- -365*opt.years/log(opt.fulldecay) 
history$decay <- exp(-history$TimePassed/decay.factor)
rm(decay.factor)

history[, "win":=ifelse(score1>score2,1,ifelse(score1<score2,-1,0))]
history[, winpoints:=0]
history[win==1, winpoints:=3]
history[win==0, winpoints:=1]

scores <- unique(history[, list("team"=team1, "level"=1)])
for (i in 1:50) {
  res_scores <- merge(merge(history, scores[, list("team1"=team, "level1"=level)], by="team1"), scores[, list("team2"=team, "level2"=level)], by="team2")
  scores<-res_scores[
    , list(
      "level"=sum(winpoints * (level2/level1) * decay) / sum((level2/level1) * decay)
      , "even"=sum((win==0) * (level2/level1) * decay) / sum((level2/level1) * decay)
      , "goals_mean"=sum(score1 * exp(-TimePassed/decay)) / sum(exp(-TimePassed/decay))
      , "goals_sd"=sd(score1)
    )
    , by=list("team"=team1)
    ]
}

# helper data sets to simplify join operations
scores1<-scores[, c("team","level","even", "goals_mean")]
scores2<-scores[, c("team","level","even", "goals_mean")]
colnames(scores1)<-paste(colnames(scores1),"1",sep="")
colnames(scores2)<-paste(colnames(scores2),"2",sep="")

worldcup[, NoEven:=TRUE]
worldcup[which(worldcup$round %in% c("1","2","3")),NoEven:=FALSE]


# Simulation (monte carlo) ####

sim <- function(data) {
  d<-data
  d$idx<-1:nrow(d)
  d<-merge(merge(d,scores1,by="team1"),scores2,by="team2")
  d<-d[order(d$idx)]
  total <- (d$level1+d$level2) * (1+ifelse(d$NoEven,0,d$even1+d$even2)/2)
  rnd <- runif(nrow(d))*total
  w<-ifelse(rnd<=d$level1,1,ifelse(rnd>=total-d$level2,-1,0))
  ifelse(is.na(d$win), w, d$win)
}
# sim(worldcup[worldcup$round %in% c("1","2","3"), ])



set.seed(12345)
time.start <- Sys.time()

for (i in 1:opt.rounds) {
  
  # einzelner Simulationsschritt
  mat <- copy(worldcup)
  
  # Gruppenrunde
  round1 <- which(mat$round %in% c("1","2","3"))
  mat$win[round1]<-sim(mat[round1, ])
  
  rp <- rbind(mat[round1, list("team"=team1, group, win)], mat[round1, list("team"=team2, group, "win"=-win)])
  rp[, winpoints:=0]
  rp[win==1, winpoints:=3]
  rp[win==0, winpoints:=1]
  
  rp<-rp[
    , list("winpoints"=as.numeric(sum(winpoints)))
    , by=list(group, team)
  ]
  
  rp<-rp[, "rank":=frank(-winpoints, ties.method = "random"), by=list(group)]
  
  rp[rank==1.0, pos:=paste("Winner", group)]
  rp[rank==2.0, pos:=paste("Runner-up", group)]
  rp<-rp[!is.na(rp$pos)]
  
  results[team %in% rp$team[rp$rank==1.0], G1:=G1+1]
  results[team %in% rp$team[rp$rank==2.0], G2:=G2+1]
  results[team %in% rp$team[rp$rank %in% c(1.0,2.0)], F8:=G1+G2]
  
  # Round of 16 ("Achtelfinale/KO-Runde")
  round2 <- which(mat$round =="Round of 16")
  map<-setNames(rp$team, rp$pos)
  mat$team1[round2] <- map[mat$team1[round2]]
  mat$team2[round2] <- map[mat$team2[round2]]
  mat$win[round2]<-sim(mat[round2, ])
  
  mat[round2, "winner":=ifelse(win>0,team1,ifelse(win<0,team2,"-"))]
  results[team %in% mat$winner[round2], F4:=F4+1]
  
  # Quarter finals
  round3 <- which(mat$round =="Quarter Finals")
  mat$team1[round3[1]] <- mat$winner[round2[1]]
  mat$team2[round3[1]] <- mat$winner[round2[2]]
  mat$team1[round3[2]] <- mat$winner[round2[3]]
  mat$team2[round3[2]] <- mat$winner[round2[4]]
  mat$team1[round3[3]] <- mat$winner[round2[5]]
  mat$team2[round3[3]] <- mat$winner[round2[6]]
  mat$team1[round3[4]] <- mat$winner[round2[7]]
  mat$team2[round3[4]] <- mat$winner[round2[8]]
  
  mat$win[round3]<-sim(mat[round3, ])
  mat[round3, "winner":=ifelse(win>0,team1,ifelse(win<0,team2,"-"))]
  
  results[team %in% mat$winner[round3], F2:=F2+1]
  
  # Semi finals
  round4 <- which(mat$round=="Semi Finals")
  mat$team1[round4[1]] <- mat$winner[round3[3]]
  mat$team2[round4[1]] <- mat$winner[round3[1]]
  mat$team1[round4[2]] <- mat$winner[round3[2]]
  mat$team2[round4[2]] <- mat$winner[round3[4]]
  
  mat$win[round4]<-sim(mat[round4, ])
  mat[round4, "winner":=ifelse(win>0,team1,ifelse(win<0,team2,"-"))]
  mat[round4, "looser":=ifelse(win<0,team1,ifelse(win>0,team2,"-"))]
  
  results[team %in% mat$winner[round4], F1:=F1+1]
  
  # Finals
  round5 <- which(mat$round=="Finals")
  mat$team1[round5[1]] <- mat$looser[round4[1]]
  mat$team2[round5[1]] <- mat$looser[round4[2]]
  mat$team1[round5[2]] <- mat$winner[round4[1]]
  mat$team2[round5[2]] <- mat$winner[round4[2]]
  
  mat$win[round5]<-sim(mat[round5, ])
  mat[round5, "winner":=ifelse(win>0,team1,ifelse(win<0,team2,"-"))]
  mat[round5, "looser":=ifelse(win<0,team1,ifelse(win>0,team2,"-"))]
  
  results[team == mat$winner[round5[2]]]$P1 <- results[team == mat$winner[round5[2]]]$P1 + 1
  results[team == mat$looser[round5[2]]]$P2 <- results[team == mat$looser[round5[2]]]$P2 + 1
  results[team == mat$winner[round5[1]]]$P3 <- results[team == mat$winner[round5[1]]]$P3 + 1
  results[team == mat$looser[round5[1]]]$P4 <- results[team == mat$looser[round5[1]]]$P4 + 1

  if (i %% 100==0 | i == opt.rounds) {
    cat(
      "Iteration: ",i
      , ", Zeit: ", round(difftime(Sys.time(),time.start, unit="min"),1), " min."
      , ", Platzierung 1-3: ", paste(head(paste(results$team[order(results$P1, decreasing=T)]," (",round(results$P1[order(results$P1, decreasing=T)]/sum(results$P1),2),")", sep=""),3), collapse=", ")
      , "\n"
      , sep="")
  }
}

results_perc<-cbind(results[, c(1,2)], results[, -c(1,2)]/sum(results$P1))

results_by_day[[as.character(opt.date)]]<-list(
  "results"=results
  ,"results_perc"=results_perc
  ,"scores"=scores
)
save(results_by_day, file="data/results.RData", compress = T)
