pollutantmean<-function(directory,pollutant,id=1:332){
    for(i in id){
        if(i==id[1]){
            if(i<10) res<-read.csv(paste(directory,"/","00",i,".csv",sep = ""))
            else if(i<100)
                res<-read.csv(paste(directory,"/","0",i,".csv",sep = ""))
            else res<-read.csv(paste(directory,"/",i,".csv",sep = ""))
        } 
        else if (i<10)
            res<-rbind(res,read.csv(paste(directory,"/","00",i,".csv",sep = "")))
        else if (i<100)
            res<-rbind(res,read.csv(paste(directory,"/","0",i,".csv",sep = "")))
        else res<-rbind(res,read.csv(paste(directory,"/",i,".csv",sep = "")))
    }
    if (pollutant=="sulfate")res1=mean(res[,2],na.rm = TRUE)
    else if (pollutant=="nitrate")res1=mean(res[,3],na.rm = TRUE)
    res1
}