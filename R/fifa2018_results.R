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
colnames(worldcup)<-c("round","date","team1", "team2", "group", "result", "origresult")
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
teams <- c(head(d$team,5), "Russia")
d<-results_by_day[[length(results_by_day)]]$results_perc
d<-d[P1>0]
d<-d[order(P1, decreasing=T)]
teams <- sort(unique(c(teams, head(d$team,5))))


# chart ####

worldcup[, round2:=round]
worldcup[round2 %in% c("1","2","3"), round2:="1-3"]
rounds <- worldcup[
  , list("StartDate"=min(date), "EndDate"=max(date))
  , by=list(round2)
]


# teams is populated above
dta <- fullres[team %in% teams & measure=="P1"]
dta$date <- as.POSIXct(paste(as.character(dta$date), "00:00"))

# old sort/packing
# sortorder <- dta[date==max(date),list(team, sortorder=frank(-value))]
# # dta[, dateout:=min(ifelse(value==0,date, na.rm=T), na.rm=T), by=list(team)]
# 
# dta <- merge(dta, sortorder, by=c("team"))   
# 
# dta[
#   , DateOut:=sum(value==0)
#   , by=list(team)
#   ]
# 
# dta<-dta[order(date, sortorder, -DateOut, team)] 

# new sort/packing
teams.val<- dta[
  , list("maxval"=max(value))
  , by=list(team)
  ]
teams.val<- teams.val[order(maxval, decreasing=T)]
dta$teamorder <- setNames(1:length(teams), teams.val$team)[dta$team]

for (tm in 3:length(teams.val)) {
  ord.best.val<-Inf
  ord.best<-NULL
  
  for (i in 1:tm) {
    d<-dta[teamorder<=tm]
    
    ord<-c(
      if (i>1) 1:(i-1) else NULL
      , tm
      , if (i<tm) i:(tm-1) else NULL
    )
    
    ord<-setNames(1:tm, ord)
    
    unique(d[, c("team", "teamorder")])
    d$teamorder <- as.numeric(ord[as.character(d$teamorder)])
    unique(d[, c("team", "teamorder")])
    
    d<-d[order(d$date, d$teamorder)]
    
    d[, value.prev:=shift(value, fill = 0), by=list(date)]
    d[, value.max:=max((value.prev+value)/2), by=list(team)]
    d[, value.shift:=cumsum(value.max), by=list(date)]
    
    val <- max(d$value.shift)
    if (val < ord.best.val) {
      ord.best.val <- val
      ord.best <- ord
    }
  }
  order.new <- as.numeric(ord.best[as.character(dta$teamorder)])
  order.new[is.na(order.new)] <- dta$teamorder[is.na(order.new)]
  dta$teamorder<-order.new
}
dta<-dta[order(date, teamorder)]




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

date_range <- as.POSIXct(round(dta$date))

ggplot(dta, aes(x=date, ymin=value.shift-.5*value, ymax=value.shift + .5* value, fill=team)) +
  guides(fill=FALSE) +
  scale_x_datetime(name="time", breaks=date_range, labels=format(date_range, "%m/%d")) +
  geom_vline(xintercept= 12*60*60+as.POSIXct(rounds$EndDate), color="black") +
  geom_ribbon(alpha=1) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background = element_blank()) +
  geom_text(data=dta[date==min(date)], aes(x=date, y=value.shift, label=team, hjust="left")) +
  geom_text(data=dta[date==max(date) & value>0], aes(x=date, y=value.shift, label=paste(round(100*value,1),"%",sep=""), hjust="right")) +
  geom_label(data = dta[match==1], aes(x=date,y=value.shift, label=result), label.r = unit(0.45, "lines"), label.size = 0.25, size=3, color="black", fill=color_map[as.character(dta[match==1]$win)])


#ggsave("images/chart1.png")

dta[date==max(date)]
View(results_by_day[[length(results_by_day)]]$results_perc)
