#!/usr/bin/env Rscript

library(reshape)
library(data.table)
library(plotly)

load("data/results.RData")
names(results_by_day)

fullres <- NULL

for (i in 1:length(results_by_day)) {
  d<-results_by_day[[i]]  
  r<-d$results_perc
  r$date <- as.Date(names(results_by_day)[i])
  r$group<-NULL
  fullres <- rbind(fullres, melt(r, id.vars=c("team","date"), variable.name = "measure"))
}

worldcup <- read.csv("data/FIFA_Worldcup_2018_results.csv", header=T, stringsAsFactors=F, encoding="UTF-8")
setDT(worldcup)

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
wc<-rbind(
  worldcup[!is.na(score1) & !is.na(score2), list(date,"team"=team1,score1,score2)],
  worldcup[!is.na(score1) & !is.na(score2), list(date,"team"=team2,"score1"=score2,"score2"=score1)]
)
wc[, result:=paste(score1,":",score2)]
wc[, win:=ifelse(score1>score2,1,ifelse(score1<score2,-1,0))]

wc[, match:=1]
fullres <- merge(fullres, wc[, list(date,team,match,result,win)], by=c("date", "team"), all.x=T)
fullres[is.na(fullres$match), match:=0]

d<-results_by_day[[1]]$results_perc
d<-d[order(P1, decreasing=T)]
teams <- head(d$team,5)
d<-results_by_day[[length(results_by_day)]]$results_perc
d<-d[order(P1, decreasing=T)]
teams <- sort(unique(c(teams, head(d$team,5))))


# chart ####

# teams is populated above
dta <- fullres[team %in% teams & measure=="P1"]
dta$date <- as.POSIXct(paste(as.character(dta$date), "00:00"))

sortorder <- dta[date==max(date),list(team, sortorder=frank(-value))]
dta <- merge(dta, sortorder, by=c("team"))    
dta<-dta[order(date, sortorder)]

space.factor <- 1.05

dta[, value.prev:=shift(value, fill = 0), by=list(date)]
dta[, value.max:=space.factor*max((value.prev+value)/2), by=list(team)]
dta[, value.shift:=cumsum(value.max), by=list(date)]

# stepped visualization:
dta2<-copy(dta)[, date:=shift(date, type = "lead"), by=list(team)]
dta2<-dta2[!is.na(date)]
dta2[, date:=date-0.001]
dta2[, match:=0]
dta3<-dta[date==max(date)]
dta3[,date:=date+60*60*24]
dta3[, match:=0]
dta<-rbind(dta,dta2,dta3)


color_map = setNames(c("green","red","gray"), c(1,-1,0))

ggplot(dta, aes(x=date, ymin=value.shift-.5*value, ymax=value.shift + .5* value, fill=team)) +
  guides(fill=FALSE) +
  geom_ribbon(alpha=1) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_text(data=dta[date==min(date)], aes(x=date, y=value.shift, label=team, hjust="left")) +
  geom_text(data=dta[date==max(date)], aes(x=date, y=value.shift, label=paste(round(100*value,1),"%",sep=""), hjust="right")) +
  geom_label(data = dta[match==1], aes(x=date,y=value.shift, label=result), label.r = unit(0.45, "lines"), label.size = 0.25, size=3, color="black", fill=color_map[as.character(dta[match==1]$win)]) 
  

# + geom_vline(xintercept = ...)[1])
# + geom_hline(yintercept=20, linetype="dashed", color = "red", size=2)
# + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
# + theme_void()

#ggsave("images/chart1.png")

dta[date==max(date)]
View(results_by_day[[length(results_by_day)]]$results_perc)
