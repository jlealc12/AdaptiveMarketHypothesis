library(lmtest)
library(readxl)
library(scales)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(beepr)
library(mice)
rm(list=ls())
#save.image("TesisPaperSpain")

#save.image("Paper RRRR Results")
{
  print(Sys.time())
  #paises=c("INDIA","INDONESIA","KOREA","MALAYSIA","PAKISTAN","PHILIPPINES","TAIWAN","THAILAND","CHINA",
  #         "Brazil","Mexico","Chile","Colombia","Peru","Czech Republic","Hungary","Poland","Russia","Greece")
  prueba=c("Monthly", "Daily")
  # estudios=c("Europa/America","Asia")
  
  data1=seq(1,9,1)  ##1:8 son todas las graficas; 9 es comparando rolling vs accumulative de lo mismo
  if(any(data1==9))tipo="Compa" else tipo="Indiv"
  #vas.o.novas.movil=matrix(ncol = 6, nrow = length(paises))
  #vas.o.no.vas.act=matrix(ncol = 6, nrow = length(paises))
  #rownames(vas.o.novas.movil)=paises
  #rownames(vas.o.no.vas.act)=paises
  #colnames(vas.o.novas.movil)=c("Monthly 24", "Monthly 30","Monthly 36", "Daily 100","Daily 200","Daily 300")
  #colnames(vas.o.no.vas.act)=c("Monthly 24", "Monthly 30","Monthly 36", "Daily 100","Daily 200","Daily 300")
  for(v in 1:length(prueba)){
    if(prueba[v]=="Monthly"){
      paises=c("London","EmergingMarkets","LATAM","Brazil","Mexico",
               "Peru","Colombia","Chile","Argentina" )
      ventana=c(24,30,36)
      range1="a2:b265"
      path1=("C:\\Users\\XXXX\\OneDrive\\Escritorio\\school\\PaperRRRR\\Data\\MonthlyData.xlsx")
      color.grafica="blue"
      
      vas.o.novas.movil.monthly=matrix(ncol = 3, nrow = length(paises))
      vas.o.no.vas.act.monthly=matrix(ncol = 3, nrow = length(paises))
      rownames(vas.o.novas.movil.monthly)=paises
      rownames(vas.o.no.vas.act.monthly)=paises
      colnames(vas.o.novas.movil.monthly)=c("Monthly 24", "Monthly 30","Monthly 36")
      colnames(vas.o.no.vas.act.monthly)=c("Monthly 24", "Monthly 30","Monthly 36")
      
    }
    
    if(prueba[v]=="Daily"){
      paises=c("LATAM","Brazil","Mexico","Peru","Colombia","Chile","Argentina","Portugal","Spain")
      ventana=c(100,200,300)
      range1="a2:b5741"
      path1=("C:\\Users\\XXXX\\OneDrive\\Escritorio\\school\\PaperRRRR\\Data\\DailyData.xlsx")
      color.grafica="red"
      
      vas.o.novas.movil.daily=matrix(ncol = 3, nrow = length(paises))
      vas.o.no.vas.act.daily=matrix(ncol = 3, nrow = length(paises))
      rownames(vas.o.novas.movil.daily)=paises
      rownames(vas.o.no.vas.act.daily)=paises
      colnames(vas.o.novas.movil.daily)=c("Daily 100","Daily 200","Daily 300")
      colnames(vas.o.no.vas.act.daily)=c("Daily 100","Daily 200","Daily 300")
      
      
    }
    
    
    #############
    
    for(s in 1:length(ventana)){
      pdf(paste("C:\\Users\\XXXX\\OneDrive\\Escritorio\\school\\PaperRRRR\\MyPlots\\",tipo,prueba[v],ventana[s],".pdf"), width = 10,height = 10)
      retornos.pais=NA
      datos.pais.1=NA
      for(i in 1:length(paises)){
        datos.pais <- read_excel( path1, sheet = paises[i], range = range1, col_names = FALSE)
        if(any(is.na(datos.pais))){ impute_rf = mice(datos.pais, method = 'rf')
        datos.pais = complete(impute_rf)
        }
        datos.pais=data.frame(datos.pais[,2])
        if(is.na(datos.pais.1)==FALSE)datos.pais.1=cbind(datos.pais.1,datos.pais)
        if(is.na(datos.pais.1)==TRUE)datos.pais.1=datos.pais
        retornos.pais.2=NULL
        for(u in 1:length(datos.pais.1[,i])-1){
          retornos.pais.1=(datos.pais.1[u+1,i]-datos.pais.1[u,i])/datos.pais.1[u,i]
          
          if(is.null(retornos.pais.2)==FALSE)retornos.pais.2=rbind(retornos.pais.2,retornos.pais.1)
          if(is.null(retornos.pais.2)==TRUE)retornos.pais.2=retornos.pais.1
          
        }
        
        if(is.na(retornos.pais)==FALSE)retornos.pais=cbind(retornos.pais,retornos.pais.2)
        if(is.na(retornos.pais)==TRUE)retornos.pais=retornos.pais.2
      }
      
      if(prueba[v]=="Monthly")fecha= read_excel("C:\\Users\\XXXX\\OneDrive\\Escritorio\\school\\PaperRRRR\\Data\\MonthlyData.xlsx",
                                                sheet = paises[i], range = "A2:A265", col_names = FALSE) else fecha= read_excel("C:\\Users\\XXXX\\OneDrive\\Escritorio\\school\\PaperRRRR\\Data\\DailyData.xlsx",
                                                                                                                                sheet = paises[i], range = "A2:A5741", col_names = FALSE)
      
      
      todas.fechas=as.matrix(fecha)
      fecha=as.matrix(fecha)[-1]
      rownames(retornos.pais)=fecha
      colnames(retornos.pais)=paises
      ## retornos.pais   ya tengo los retornos de los paises
      list.p.values=list()
      list.p.values.ancla=list()
      
      for(i in 1:length(paises)){
        ##### para las ventanas moviles
        q=1
        q1=ventana[s]
        q2=q+1
        q3=q1+1
        skip=ventana[s]+1
        print(i)
        
        ### variables para guardar la info
        
        #u=1 ## BORRAAAAAARRRR
        list.p.values[[i]]=NA
        list.p.values.ancla[[i]]=NA
        pvalor1=matrix(ncol=4)
        pvalor2=matrix(ncol=4)
        while(q3<=length(retornos.pais[,i])){
          
          e_level<-as.numeric(retornos.pais[q2:q3,i])
          e_lags <- as.numeric(retornos.pais[q:q1,i])
          armod<-lm(e_level~ e_lags)
          
          pvalor1[,1]=summary(armod)$coefficients[1,4]
          pvalor1[,2]=summary(armod)$coefficients[1,1]  
          pvalor1[,3]=summary(armod)$coefficients[2,4]  
          pvalor1[,4]=summary(armod)$coefficients[2,1]
          
          if(is.na(list.p.values[[i]])!=TRUE) list.p.values[[i]]=rbind(list.p.values[[i]],pvalor1)
          if(is.na(list.p.values[[i]])==TRUE)list.p.values[[i]]=pvalor1
          
          e_level1<-as.numeric(retornos.pais[2:q3,i])
          e_lags1 <- as.numeric(retornos.pais[1:q1,i])
          armod1<-lm(e_level1~ e_lags1)
          
          pvalor2[,1]=summary(armod1)$coefficients[1,4]
          pvalor2[,2]=summary(armod1)$coefficients[1,1]  
          pvalor2[,3]=summary(armod1)$coefficients[2,4]  
          pvalor2[,4]=summary(armod1)$coefficients[2,1]
          
          if(is.na(list.p.values.ancla[[i]])!=TRUE) list.p.values.ancla[[i]]=rbind(list.p.values.ancla[[i]],pvalor2)
          if(is.na(list.p.values.ancla[[i]])==TRUE)list.p.values.ancla[[i]]=pvalor2
          
          q=q+1
          q1=q1+1
          q2=q2+1
          q3=q3+1
          
        }
      }
      
      fecha.ventanas=fecha[-(c(1:ventana[s]))]
      names(list.p.values)=paises
      names(list.p.values.ancla)=paises
      list.p.values=lapply(list.p.values,"rownames<-",fecha.ventanas)
      list.p.values=lapply(list.p.values,"colnames<-",c("Pval B0", "B0","Pval B1", "B1"))
      list.p.values.ancla=lapply(list.p.values.ancla,"rownames<-",fecha.ventanas)
      list.p.values.ancla=lapply(list.p.values.ancla,"colnames<-",c("Pval B0", "B0","Pval B1", "B1"))
      #### pronosticos
      resultados.AR1.m=list()
      resultados.AR1.a=list()
      diferencias.pronosticado.real=list()
      for(k in 1:length(paises)){
        resultados.AR1.m[[k]]=matrix(ncol=2,nrow = length(list.p.values[[k]][,2]))
        resultados.AR1.a[[k]]=matrix(ncol=2,nrow = length(list.p.values.ancla[[k]][,2]))
        diferencias.pronosticado.real[[k]]=matrix(ncol=4,nrow = length(list.p.values[[k]][,2])-1)
        for(x in 1:length(list.p.values[[k]][,2])){
          if(list.p.values[[k]][x,1]<=.05)resultados.AR1.m[[k]][x,1]=list.p.values[[k]][x,1]+list.p.values[[k]][x,3]*retornos.pais[x,k]
          if(list.p.values[[k]][x,1]>.05)resultados.AR1.m[[k]][x,2]=list.p.values[[k]][x,1]+list.p.values[[k]][x,3]*retornos.pais[x,k]
        }
        for(x in 1:length(list.p.values.ancla[[k]][,2])){
          if(list.p.values.ancla[[k]][x,1]<=.05)resultados.AR1.a[[k]][x,1]=list.p.values.ancla[[k]][x,1]+list.p.values.ancla[[k]][x,3]*retornos.pais[x,k]
          if(list.p.values.ancla[[k]][x,1]>.05)resultados.AR1.a[[k]][x,2]=list.p.values.ancla[[k]][x,1]+list.p.values.ancla[[k]][x,3]*retornos.pais[x,k]
        }
        
        for(x in 1:(length(resultados.AR1.a[[k]][,1])-1)){
          #moviles 1=conductual, 2=EMH
          if(is.na(resultados.AR1.m[[k]][x,1])==FALSE)diferencias.pronosticado.real[[k]][x,1]=retornos.pais[x+skip,k]-resultados.AR1.m[[k]][x,1]
          if(is.na(resultados.AR1.m[[k]][x,2])==FALSE)diferencias.pronosticado.real[[k]][x,2]=retornos.pais[x+skip,k]-resultados.AR1.m[[k]][x,2]
          #acumulativo 3=conducutal, 4=EMH
          if(is.na(resultados.AR1.a[[k]][x,1])==FALSE)diferencias.pronosticado.real[[k]][x,3]=retornos.pais[x+skip,k]-resultados.AR1.a[[k]][x,1]
          if(is.na(resultados.AR1.a[[k]][x,2])==FALSE)diferencias.pronosticado.real[[k]][x,4]=retornos.pais[x+skip,k]-resultados.AR1.a[[k]][x,2]
          
        }
        diferencias.pronosticado.real[[k]]=cbind(retornos.pais[-c(1:skip),k],resultados.AR1.m[[k]][-1,],
                                                 diferencias.pronosticado.real[[k]],resultados.AR1.a[[k]][-1,])
      }
      
      names( diferencias.pronosticado.real)=paises
      diferencias.pronosticado.real=lapply(diferencias.pronosticado.real,"colnames<-",c("RetReal","RetBFMov","RetEMHMov","DifBFMov","DifEMHMov","RetBFAnc","RetEMHAnc","DifBFAnc","DifEMHAnc"))
      diferencias.pronosticado.real=lapply(diferencias.pronosticado.real,"rownames<-",fecha[-c(1:skip)])
      head(diferencias.pronosticado.real[[6]])
      
      # labels and breaks for X axis text
      lbls=NA
      for(m in 1:nrow(todas.fechas)){
        if(substr(todas.fechas[m],5,10)=="-01-31") lbls=rbind(lbls,(todas.fechas[m]))
      }
      lbls <- lbls[-1]
      lbls=as.Date(lbls)
      if(prueba[v]=="Daily")lbls=c((year(lbls[1])):(year(lbls[length(lbls)])+1)) else lbls=c((year(lbls[1])):(year(lbls[length(lbls)])+1))
      graphs.p.values=list()
      graphs.p.values.ancla=list()
      {
        ###queremos el 1, 3, 5, 7
        for(l in data1){
          ###rolling
          if(l==1|l==2){
            
            if(l==1)par(mfrow=c(3,3))
            for(k in 1:length(paises)){
              
              graficar=NA
              graficar=c(rep(" ", skip), list.p.values[[k]][,1])
              if(prueba[v]=="Monthly"){length.x=length(graficar)+12
              cuts=12}else {length.x=length(graficar)+261
              cuts=261}
              if(l==2){par(mfrow=c(1,1))
                par(fig=c(.1,.9,.1,.95))}
              {plot(graficar, type="l",xlim = c(0,length.x), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
                if(l==2)title(main=paste(ventana[s], "Period Rolling Window:", paises[k],"Index" ), sub=paste(prueba[v],"data"),col.main="Black", font.main=4)
                if(l==1)title(main=paises[k])
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                axis(1,at=seq77, labels =lbls, las=3)
                axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
                if(l==2)axis(2,at=0.05,col = "black", las=1)
                abline(h=0.05,col="black", lty="dotted")
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
                lines(graficar, col=color.grafica)
                legend("topleft", legend=c("P Value"),
                       col=(color.grafica), lty=1:1, cex=0.8,bg="white")
                box()
                if(l==1)mtext(paste(ventana[s],"Period Rolling Window Analysis"), side = 3, line = -1.5, outer = TRUE)
                graphs.p.values[[k]]=recordPlot()
                
                
              }
              length(seq(1, length.x, cuts))
              length(lbls)
            }
          }
          ###acumulativa
          if(l==3|l==4){
            
            if(l==3)par(mfrow=c(3,3))
            for(k in 1:length(paises)){  
              graficar=NA
              graficar=c(rep(" ", skip), list.p.values.ancla[[k]][,1])
              if(prueba[v]=="Monthly"){length.x=length(graficar)+12
              cuts=12}else {length.x=length(graficar)+261
              cuts=261}
              if(l==4){par(mfrow=c(1,1))
                par(fig=c(.1,.9,.1,.95))}
              
              
              #if(l==4)dev.new()
              {plot(graficar, type="l",xlim = c(0,length.x), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
                if(l==4)title(main=paste(ventana[s], "Period Acumulative Window:", paises[k],"Index" ), sub=paste(prueba[v],"data"),col.main="Black", font.main=4)
                if(l==3)title(main=paises[k])
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                axis(1,at=seq77, labels =lbls, las=3)
                axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
                if(l==4)axis(2,at=0.05,col = "black", las=1)
                abline(h=0.05,col="black", lty="dotted")
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
                lines(graficar, col=color.grafica)
                legend("topleft", legend=c("P Value"),
                       col=(color.grafica), lty=1:1, cex=0.8,bg="white")
                box()
                if(l==3)mtext(paste(ventana[s],"Period Acumulative Window Analysis"), side = 3, line = -1.5, outer = TRUE)
                graphs.p.values.ancla[[k]]=recordPlot()
                
                
              }
            }
          }
          ###rolling todo
          if(l==5|l==6){
            
            if(l==5)par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
            for(k in 1:length(paises)){
              
              graficar=NA
              graficar=c(rep(" ", skip), list.p.values[[k]][,1])
              if(prueba[v]=="Monthly"){length.x=length(graficar)+12
              cuts=12}else {length.x=length(graficar)+261
              cuts=261}
              ###grafica linea
              if(l==5){
                par(fig=c(0.05,0.7,0.35,.95))
                plot(graficar, type="l",xlim = c(0,length(graficar)), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
                if(l==6)title(main=paste(ventana[s], "Period Rolling Window:", paises[k],"Index" ), sub=paste(prueba[v],"data"),col.main="Black", font.main=4)
                if(l==5)title(main=paste("Rolling Window:", paises[k],"Index" ))
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                axis(1,at=seq77, labels =lbls, las=3)
                axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
                if(l==6)axis(2,at=0.05,col = "black", las=1)
                abline(h=0.05,col="black", lty="dotted")
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
                lines(graficar, col=color.grafica)
                legend("topleft", legend=c("P Value"),
                       col=(color.grafica), lty=1:1, cex=0.8,bg="white")
                box()
                mtext(paste(ventana[s],"Period Rolling Window Analysis",paises[k],"Index"), side = 3, line = -1.5, outer = TRUE)}
              # graphs.p.values[[k]]=recordPlot()
              
              
              limite.bajo=min(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,2]),(diferencias.pronosticado.real[[k]][,3]))))
              limite.alto=max(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,2]),(diferencias.pronosticado.real[[k]][,3]))))
              ### diferencias
              if(l==6){par(mfrow=c(1,1))
                par(fig=c(.05,.9,.1,.95))}
              if(l==5) par(fig=c(0.05,0.7,0.1,0.3), new=TRUE)
              plot(c(rep("",skip),diferencias.pronosticado.real[[k]][,1]),type = "l", xlim=c(0,length(fecha)),
                   ylim=c(limite.bajo+-.05,limite.alto+.05),axes=FALSE, ylab="Returns",xlab="",col="purple4")
              if(l==6){ title(main=paste(paises[k], "Comparison Between Real and Pronosticated"), col.main="Black",font.main=4 )
                title(sub = paste(ventana[s],"Period Rolling Window Analysis"),adj=0,line=-1)}
              if(l==5)title(main="Comparison Between Real and Pronosticated")
              seq77=seq(1, length.x, cuts)
              seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
              axis(1,at=seq77, labels =lbls, las=3)
              if(l==6)axis(2,at=seq(limite.bajo+-.05,limite.alto+.05,.2),labels=round(seq(limite.bajo+-.05,limite.alto+.05,.2),2),las=1)
              if(l==5)axis(2,at=c(limite.bajo,0,limite.alto),labels=round(c(limite.bajo,0,limite.alto),2), las=1)
              lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,2]),col= "Orange") #BF
              lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,3]),col= "limegreen") #EMH
              abline(v=skip+1,col="red",lty="dotted")
              legend("topleft", legend=c("Real","BF","EMH"),
                     col=c("Purple","Orange","Green"), lty=1:1, cex=0.8,bg="white")
              box()
              ###boxplot
              if(l==5)par(fig=c(0.75,1,0.1,.95), new=TRUE)
              if(l==6){par(mfrow=c(1,1))
                par(fig=c(.05,.9,.1,.95))}
              boxplot(diferencias.pronosticado.real[[k]][,4],diferencias.pronosticado.real[[k]][,5], names = c("BF","EMH"),
                      ylab="Differences",xlab="",axes=FALSE)
              axis(2,las=1)
              axis(1,labels =c("BF","EMH"), at=c(1:2))
              if(l==6)title(main=paste(paises[k],"Differences Real and Pronosticated Returns"),font.main=4)
              if(l==6)title(sub = paste(ventana[s],"Period Rolling Window Analysis"),line = -1,adj=0)
              
              if(l==5)title("Diff Real vs Pronosticated")
              box()
              
            }
          }
          ##acumulativo todo
          if(l==7|l==8){
            
            if(l==7)par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
            for(k in 1:length(paises)){
              
              graficar=NA
              graficar=c(rep(" ", skip), list.p.values.ancla[[k]][,1])
              if(prueba[v]=="Monthly"){length.x=length(graficar)+12
              cuts=12}else {length.x=length(graficar)+261
              cuts=261}
              if(l==8){par(mfrow=c(1,1))
                par(fig=c(.1,.9,.1,.95))}
              ###grafica linea
              if(l==7){
                par(fig=c(0.05,0.7,0.35,.95))
                plot(graficar, type="l",xlim = c(0,length(graficar)), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
                if(l==8)title(main=paste(ventana[s], "Period Acumulative Window:", paises[k],"Index" ), sub=paste(prueba[v],"data"),col.main="Black", font.main=4)
                if(l==7)title(main=paste("Acumulative Window:", paises[k],"Index" ))
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                axis(1,at=seq77, labels =lbls, las=3)
                axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
                if(l==8)axis(2,at=0.05,col = "black", las=1)
                abline(h=0.05,col="black", lty="dotted")
                seq77=seq(1, length.x, cuts)
                seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
                abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
                lines(graficar, col=color.grafica)
                legend("topleft", legend=c("P Value"),
                       col=(color.grafica), lty=1:1, cex=0.8,bg="white")
                box()
                mtext(paste(ventana[s],"Period Acumulative Window Analysis",paises[k],"Index"), side = 3, line = -1.5, outer = TRUE)}
              # graphs.p.values[[k]]=recordPlot()
              
              
              limite.bajo=min(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,6]),(diferencias.pronosticado.real[[k]][,7]))))
              limite.alto=max(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,6]),(diferencias.pronosticado.real[[k]][,7]))))
              ### diferencias
              if(l==7) par(fig=c(0.05,0.7,0.1,0.3), new=TRUE)
              plot(c(rep("",skip),diferencias.pronosticado.real[[k]][,1]),type = "l", xlim=c(0,length(fecha)),
                   ylim=c(limite.bajo+-.05,limite.alto+.05),axes=FALSE, ylab="Returns",xlab="",col="purple4")
              if(l==8){ title(main=paste(paises[k], "Comparison Between Real and Pronosticated"), col.main="Black",font.main=4)
                title(sub = paste(ventana[s],"Period Acumulative Window Analysis"),adj=0,line=-1)}
              if(l==7)title(main="Comparison Between Real and Pronosticated")
              seq77=seq(1, length.x, cuts)
              seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
              axis(1,at=seq77, labels =lbls, las=3)
              if(l==8)axis(2,at=seq(limite.bajo+-.05,limite.alto+.05,.20),labels=round(seq(limite.bajo+-.05,limite.alto+.05,.20),2),las=1)
              if(l==7)axis(2,at=c(limite.bajo,0,limite.alto),labels=round(c(limite.bajo,0,limite.alto),2), las=1)
              lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,6]),col= "Orange") #BF
              lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,7]),col= "limegreen") #EMH
              abline(v=skip+1,col="red",lty="dotted")
              legend("topleft", legend=c("Real","BF","EMH"),
                     col=c("Purple","Orange","Green"), lty=1:1, cex=0.8,bg="white")
              box()
              ###boxplot
              
              if(l==7)par(fig=c(0.75,1,0.1,.95), new=TRUE)
              if(l==8){par(mfrow=c(1,1))
                par(fig=c(.1,.9,.1,.95))}
              boxplot(diferencias.pronosticado.real[[k]][,8],diferencias.pronosticado.real[[k]][,9], names = c("BF","EMH"),
                      ylab="Differences",xlab="",axes=FALSE,sub = paste(ventana[s],"Period Acumulative Window Analysis") )
              axis(2,las=1)
              axis(1,labels =c("BF","EMH"), at=c(1:2))
              if(l==8)title(main=paste(paises[k],"Differences Real and Pronosticated Returns"),font.main=4)
              if(l==8)title(sub = paste(ventana[s],"Period Acumulative Window Analysis"),line = -1,adj=0)
              
              if(l==7)title("Diff Real vs Pronosticated")
              
              box()
              
            }
            
          }
          ########
          if(l==9){
            par(mfrow=c(1,2))
            
            graficar=NA
            graficar=c(rep(" ", skip), list.p.values[[k]][,1])
            if(prueba[v]=="Monthly"){length.x=length(graficar)+12
            cuts=12}else {length.x=length(graficar)+261
            cuts=261}
            ###grafica linea
            ##rolling
            {
              plot(graficar, type="l",xlim = c(0,length(graficar)), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
              title(main=paste("Rolling Window"))
              seq77=seq(1, length.x, cuts)
              seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
              axis(1,at=seq77, labels =lbls, las=3)
              axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
              abline(h=0.05,col="black", lty="dotted")
              seq77=seq(1, length.x, cuts)
              seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
              abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
              lines(graficar, col=color.grafica)
              legend("topleft", legend=c("P Value"),
                     col=(color.grafica), lty=1:1, cex=0.8,bg="white")
              box()
              mtext(paste(ventana[s],"Period Rolling Window Analysis",paises[k],"Index"), side = 3, line = -1.5, outer = TRUE)}
            # graphs.p.values[[k]]=recordPlot()
            
            
            
            
            graficar=NA
            graficar=c(rep(" ", skip), list.p.values.ancla[[k]][,1])
            if(prueba[v]=="Monthly"){length.x=length(graficar)+12
            cuts=12}else {length.x=length(graficar)+261
            cuts=261}
            ###grafica linea
            
            ###acumulative
            
            plot(graficar, type="l",xlim = c(0,length(graficar)), xlab="", ylab = "Grade of the efficiency of the Market", ylim =c(0,1.1),axes=FALSE)
            title(main=paste("Acumulative Window"))
            seq77=seq(1, length.x, cuts)
            seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
            axis(1,at=seq77, labels =lbls, las=3)
            axis(2, at=seq(0,1,.20), labels=seq(0,1,0.20),las=1)
            abline(h=0.05,col="black", lty="dotted")
            seq77=seq(1, length.x, cuts)
            seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
            abline(h=seq(0,1,.20),v=seq77, col="grey91",lty="dotted")
            lines(graficar, col=color.grafica)
            legend("topleft", legend=c("P Value"),
                   col=(color.grafica), lty=1:1, cex=0.8,bg="white")
            box()
            
            mtext(paste(ventana[s],"Period Analysis:",paises[k],"Index"), side = 3, line = -1.5, outer = TRUE)
            
            
            ####diferencias
            par(mfrow=c(1,2))
            limite.bajo=min(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,2]),(diferencias.pronosticado.real[[k]][,3]))))
            limite.alto=max(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,2]),(diferencias.pronosticado.real[[k]][,3]))))
            
            
            plot(c(rep("",skip),diferencias.pronosticado.real[[k]][,1]),type = "l", xlim=c(0,length(fecha)),
                 ylim=c(limite.bajo+-.05,limite.alto+.05),axes=FALSE, ylab="Returns",xlab="",col="purple4")
            title(main="Rolling Window Data")
            seq77=seq(1, length.x, cuts)
            seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
            axis(1,at=seq77, labels =lbls, las=3)
            axis(2,at=c(limite.bajo,0,limite.alto),labels=round(c(limite.bajo,0,limite.alto),2), las=1)
            lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,2]),col= "Orange") #BF
            lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,3]),col= "limegreen") #EMH
            abline(v=skip+1,col="red",lty="dotted")
            legend("topleft", legend=c("Real","BF","EMH"),
                   col=c("Purple","Orange","Green"), lty=1:1, cex=0.8,bg="white")
            box()
            
            
            ##acumulative
            limite.bajo=min(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,6]),(diferencias.pronosticado.real[[k]][,7]))))
            limite.alto=max(na.omit(c((diferencias.pronosticado.real[[k]][,1]),(diferencias.pronosticado.real[[k]][,6]),(diferencias.pronosticado.real[[k]][,7]))))
            
            
            plot(c(rep("",skip),diferencias.pronosticado.real[[k]][,1]),type = "l", xlim=c(0,length(fecha)),
                 ylim=c(limite.bajo+-.05,limite.alto+.05),axes=FALSE, ylab="Returns",xlab="",col="purple4")
            title(main="Accumulative Window Data")
            seq77=seq(1, length.x, cuts)
            seq77=seq77[seq(1, length.x, cuts)<=length(graficar)]
            axis(1,at=seq77, labels =lbls, las=3)
            axis(2,at=c(limite.bajo,0,limite.alto),labels=round(c(limite.bajo,0,limite.alto),2), las=1)
            lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,6]),col= "Orange") #BF
            lines(c(rep("",skip),diferencias.pronosticado.real[[k]][,7]),col= "limegreen") #EMH
            abline(v=skip+1,col="red",lty="dotted")
            legend("topleft", legend=c("Real","BF","EMH"),
                   col=c("Purple","Orange","Green"), lty=1:1, cex=0.8,bg="white")
            box()
            mtext(paste(ventana[s],"Period", paises[k],"Forecast in Returns"), side = 3, line = -1.5, outer = TRUE)
            
            
            ###boxplot
            par(mfrow=c(1,2))
            
            boxplot(diferencias.pronosticado.real[[k]][,4],diferencias.pronosticado.real[[k]][,5], names = c("BF","EMH"),
                    ylab="Differences",xlab="",axes=FALSE)
            axis(2,las=1)
            axis(1,labels =c("BF","EMH"), at=c(1:2))
            
            title("Rolling Window Data")
            box()
            
            
            
            
            
            ##acumulative
            boxplot(diferencias.pronosticado.real[[k]][,8],diferencias.pronosticado.real[[k]][,9], names = c("BF","EMH"),
                    ylab="Differences",xlab="",axes=FALSE,sub = paste(ventana[s],"Period Acumulative Window Analysis") )
            axis(2,las=1)
            axis(1,labels =c("BF","EMH"), at=c(1:2))
            title("Accumulative Window Data")
            
            mtext(paste(ventana[s],"Period:",paises[k],"Difference Real vs Pronosticated"), side = 3, line = -1.5, outer = TRUE)
            
            box()
            
          }
          #########
        }###este cierra tooodooo el loop de graficas
        
        
        
      }
      dev.off()
      print(s)
      for(ll in 1:length(paises)){
        es.o.no.es=0
        empezo=NA
        es.o.no.es1=0
        empezo1=NA
        for(ol in 1: length(resultados.AR1.m[[ll]][,1])){
          if(is.na(resultados.AR1.m[[ll]][1,1])!= TRUE && es.o.no.es%%2==0&&is.na(empezo)==TRUE){empezo=1
          es.o.no.es=1}else empezo=0
          if(is.na(resultados.AR1.m[[ll]][1,1])!= TRUE && es.o.no.es%%2==0&&is.na(empezo)==TRUE){empezo1=1
          es.o.no.es1=1}else empezo1=0
          if(is.na(resultados.AR1.m[[ll]][ol,2])!= TRUE && es.o.no.es%%2==0)es.o.no.es=es.o.no.es+1
          if(is.na(resultados.AR1.m[[ll]][ol,1])!= TRUE && es.o.no.es%%2!=0)es.o.no.es=es.o.no.es+1
          if(is.na(resultados.AR1.a[[ll]][ol,2])!= TRUE && es.o.no.es1%%2==0)es.o.no.es1=es.o.no.es1+1
          if(is.na(resultados.AR1.a[[ll]][ol,1])!= TRUE && es.o.no.es1%%2!=0)es.o.no.es1=es.o.no.es1+1
          
        }
        if(prueba[v]=="Monthly"){
          vas.o.novas.movil.monthly[ll,s]= if(es.o.no.es>=(6+empezo))"Reject H0" else "Dont Reject H0"
          vas.o.no.vas.act.monthly[ll,s]= if(es.o.no.es1>=(6+empezo1))"Reject H0" else "Dont Reject H0"} else {
            vas.o.novas.movil.daily[ll,s]= if(es.o.no.es>=(6+empezo))"Reject H0" else "Dont Reject H0"
            vas.o.no.vas.act.daily[ll,s]= if(es.o.no.es1>=(6+empezo1))"Reject H0" else "Dont Reject H0"
          }
      }
      
    }
  }#cierra prueba mothly y daily
  beep(sound=4)
}#cierra todo
Sys.time()

