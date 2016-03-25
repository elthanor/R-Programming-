best <- function(state, outcome) {
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!state %in% data[,7])stop("invalid state")
    if(outcome=="heart attack") data<-data[c(2,7,11)]
    else if(outcome=="heart failure") data<-data[c(2,7,17)]
    else if(outcome=="pneumonia") data<-data[c(2,7,23)]
    else stop("invalid outcome")
    data<-data[data[,2]==state & data[,3]!="Not Available",]
    data<-data[which.min(data[,3]),1]
    min(data)
}