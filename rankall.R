rankall<-function(outcome,num="best"){
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!state %in% data[,7])stop("invalid state")
    if(outcome=="heart attack") data<-data[c(2,7,11)]
    else if(outcome=="heart failure") data<-data[c(2,7,17)]
    else if(outcome=="pneumonia") data<-data[c(2,7,23)]
    else stop("invalid outcome")
    found=NULL
    data<-data[data[,3]!="Not Available",]
    data<-data[order(data[,1]),]
    data<-data[order(as.numeric(data[,3])),]
    for(state in unique(data[,2])){
        temp<-data[data[,2]==state,]
        if(num=="best")found<-rbind(found,temp[1,1:2])
        else if(num=="worst")found<-rbind(found,temp[nrow(temp),1:2])
        else if(num<=nrow(temp))found<-rbind(found,temp[num,1:2])
        else found<-rbind(found,c(NA,state))
    }
    found[order(found[,2]),]
}