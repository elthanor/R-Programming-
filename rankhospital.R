rankhospital <- function(state, outcome,num="best") {
    data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!state %in% data[,7])stop("invalid state")
    if(outcome=="heart attack") data<-data[c(2,7,11)]
    else if(outcome=="heart failure") data<-data[c(2,7,17)]
    else if(outcome=="pneumonia") data<-data[c(2,7,23)]
    else stop("invalid outcome")
    data<-data[data[,2]==state & data[,3]!="Not Available",]
    data<-data[order(data[,1]),]
    data<-data[order(as.numeric(data[,3])),]
    ##first approach creating a new frame, ordering it and then returning each value to main frame
    ##row.names(data)<-1:nrow(data)
    ##for(i in unique(data[,3])){
        ##found<-data[data[,3]==i,]
        ##if (nrow(found)==1)next()
        ##place<-which(data[,1]==found[1,1])
        ##found<-found[order(found[,1]),]
        ##for(y in place:(place+nrow(found)-1)){
            ##data[y,]<-found[y-place+1,]
        ##}
    ##}
    if(num=="best")return(data[1,1])
    else if(num=="worst")return(data[nrow(data),1])
    else if(num<=nrow(data))return(data[num,1])
    return(NA)
}