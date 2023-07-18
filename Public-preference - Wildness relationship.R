library(vegan)
library(ggpubr)
# define functions ####
{

  # calculate SE for a matrix
  # x is a matrix
  SE <- function(x){
    
    colnames(x)
    result <- NULL;  i=1
    repeat{
      if(i > ncol(x))break
      se <- sd(x[,i])/sqrt(length(x[,i]))
      i=i+1
      result <- c(result,se)
    }
    result <- data.frame(name = colnames(x),se=result)
    result
  }
  
  # Matrix is site-variable matrix, with row names are site, col names are variables
  # grp is a vector
  Mean.SE.grp <- function(matrix,grp){
    title <- Element.grp(grp)
    n <- length(title)
    i=0;result.F <- NULL
    repeat{
      i=i+1
      data <- matrix[grp==title[i],]
      result <- Mean.SE(data)
      result.F <- cbind(result.F,result)
      if(i==n)break
    }
    return(result.F)
  }
  Mean.SE <- function(data){
    # Mean
    nrow <- nrow(data)
    mean <- colSums(data)/nrow
    
    # SD
    ncol <- ncol(data)
    i=0;result.F <- NULL
    repeat{
      i=i+1
      result <- sd(data[,i])
      result.F <- c(result.F,result)
      if (i == ncol) break
    }
    
    # SE
    SE <- result.F/(nrow^(1/2))
    
    result <- cbind(mean,SE)
    return(result)
  }
  Element.grp <- function(data){
    
    element.F <- NULL
    repeat{
      
      element <- data[1]
      
      # create a index from 1 to n
      n <- length(data);vector <- 1:n
      
      # get the index of the elements that are same as the first one
      index <- vector[data==element]
      
      # remove the same elements, and formed a new data list
      data <- data[-index]
      
      # record the elements
      element.F <- c(element.F,element)
      
      if (length(data)==0)break
    }
    return(element.F)
  }
  # Result, row names are variables, col names are the groups
  
  # data in the list.of.data need to be vector or distance
  Boxplot.grp <- function(list.of.data,grp){
    n = length(list.of.data)
    
    i=0;group <- NULL;data.F <- NULL
    repeat{
      i=i+1
      data <- list.of.data[[i]]
      
      # transform the data into vactors
      data.F <- c(data.F,as.vector(data))
      
      # enlarge the grp factor
      group <- c(group,rep(grp[i],length(data)))
      if(i==n)break
    }
    
    # sort the group based on what you want
    group <- factor(group,levels = grp)
    boxplot(data.F~group)
  }
  
  # the data and grp is a vector
  Wilcox.test.grp <- function(data,grp){
    
    # See how many levels of the grp
    level <- Element.grp(grp) 
    
    n <- length(level)
    # built two matrix to store the statistics of 'W' and 'P value'
    
    i=0;result.F.F <- NULL
    repeat{
      i=i+1
      
      # use Data[i] to compare with all the Data set behind
      j=i;result.F <- NULL
      repeat{
        j=j+1
        
        test <- wilcox.test(data[grp==level[i]],data[grp==level[j]])
        t.statistic <- test$statistic
        p.value <- test$p.value
        result <- c(t.statistic,p.value)
        result.F <- c(result.F,result)
        if(j==n)break
      }
      result.F <- c(rep(0,(i-1)*2),result.F)
      result.F.F <- rbind(result.F.F,result.F)
      if(i==n-1)break
    }
    
    # give row name to the result
    rownames(result.F.F) <- level[-n]
    
    # give 'w' and 'p' to the result
    result.F.F <- rbind(
      (rep(c('w','p'),n-1))
      ,result.F.F)
    
    # give col name to the result
    i=0;colname <- NULL
    repeat{
      i=i+1
      result <- c(level[i+1],'')
      colname <- c(colname,result)
      if(i+1 == n) break
    }
    
    colnames(result.F.F) <- colname
    
    return(result.F.F)
  }
  
  # to replace a vector with another vector with the same number
  # 'original' is a vector of data 
  # 'replace' is a vector of level factor
  Replace <- function(original,replace){
    old <- Element.grp(original)
    new <- NULL
    # replace the grp var to color 
    i=0
    repeat{
      i=i+1
      
      # For each element in original
      # Check which they are
      # And then replace then accordingly
      j=0
      repeat{
        j=j+1
        if(original[i] == old[j]) new[i] <- replace[j]
        if(j==length(old))break
      }
      
      if(i == length(original) ) break
    }
    
    # record which replaced which
    i=0; alter.F <- NULL
    repeat{
      i=i+1
      alteration <- c(replace[i],'-->',old[i])
      alter.F <- rbind(alter.F,alteration)
      if(i==length(replace))break
    }
    alter.F
    
    result <- list(alter.F,new)
    return(result)
  }

  #Merge()
  # A vector have only 4 kind of strings or numbers, but every letter occurred number of times
  # We need to count how many of each string or number
  # by the way to order the result by the parameter 'order'
  Merge <- function(vector,order){
    level <- Element.grp(vector)
    result <- NULL
    i=0
    repeat{
      i=i+1
      count <- length(vector[vector==order[i]])
      #if(isTRUE(level==order[i])==FALSE) count=0
      result <- c(result,count)
      if(i==length(order))break
    }
    result <- data.frame(level=order,count=result)
    n1 = length(order); n2 = length(level)
    if(isTRUE(n1==n2)==FALSE) print('the length of the order you gave is not equal to the length of the level')
    
    return(result)
  }
  
  #Order.number
  
  # When the vector are numbers, 
  # We sometimes need to order the vector from small to large, or otherwise
  Order.number <- function(vector, order='decrease'){
    n=length(vector)
    if(is.numeric(vector)==TRUE){
      new.vector <- NULL
      i=0
      repeat{
        i=i+1
        # the highest value
        max <- max(vector)
        # create a vector to find the index
        array = 1:length(vector)
        # find out the index of the highest value
        index <- array[vector==max]
        # put the highest value in the new vector
        if(order=='decrease') new.vector <- c(new.vector,vector[index])
        if(order=='increase') new.vector <- c(number,new.vector)
        # remove the highest value from the vector
        vector<-vector[vector!=max]
        if(length(vector)==0)break
      }
      return(new.vector)
    } 
    if(is.numeric(vector)==FALSE) return('the vector is not numeric')
  }

  #Order()
# A vector have only several kind of strings or numbers, but every letter occured number of times
# We need to count how many of each string or number

Order <- function(vector,order){
  
  level <- Element.grp(vector)
  
  # Reorder the vectors
  result <- NULL
  i=0
  repeat{
    i=i+1
    reorder <- vector[vector==order[i]]
    result <- c(result,reorder)
    if(i==length(order))break
  }
  
  n1 = length(order); n2 = length(level)
  if(isTRUE(n1==n2)==FALSE) print('the length of the order you gave is not equal to the length of the level')
  
  return(result)
}

}

# Data input ####
setwd('D:\\PhD\\20210607 Human preference and biodiversity. A mamagement issue\\220708 Results of questionnaire')

#data in France
data.fr <- read.csv('230410 B-P relationship + motivation + demograph -fr.csv');nrow(data.fr)
data.cn <- read.csv('230410 B-P relation. + motiv. + demo. - Wuhan.csv');nrow(data.cn)
#data.cn.off <- data.cn.off.online[1:130,]

data <- data.fr;title<- 'fr'
data <- data.cn;title<- 'cn'
#data <- data.cn.off

#labels
label.col <- c('order','distance','frq','HLC','preference','aesthetic','visit'
               ,rep('density',4),rep('SR',4)
               ,rep('motive',44)
               ,rep('demographic',4))

label.motiv <- c(rep('Aesthethic',10),rep('Functionality',8)
                 ,rep('Safety',5),rep('Ecology conditions',6)
                 ,rep('Particular vegetation',2)
                 ,rep('Human intervention',2)
                 ,rep('Imaginary context',11)
                 )

# biodiversity from low to high: b < c < d < a
label.row <- data$Preference

#1.Counting different scenarios ####
{
  num.a = nrow(data[label.row=='a',])
  num.b = nrow(data[label.row=='b',])
  num.c = nrow(data[label.row=='c',])
  num.d = nrow(data[label.row=='d',])
}
total <- nrow(data)
num <- c(num.b,num.c,num.d,num.a); preference <- (num/total)*100
num;preference

# re-sample from the existing data set
i=0;result.F <- NULL
repeat{
  i=i+1
  sample <- sample(data$Preference,size = 0.8*nrow(data),replace = FALSE)
  Merge(sample,order=c('b','c','d','a')) # Count how many of the people choose a,b,c and d
  result <- Merge(sample,order=c('b','c','d','a'))$count
  result.F <- rbind(result.F,result)
  if(i==1000)break
}

colnames(result.F) <- c('b','c','d','a');rownames(result.F) <- 1:nrow(result.F)
result.F
Pref.per <- result.F/(0.8*nrow(data))
Mean.SE(Pref.per)
data.Pref.per <- data.frame(Preference.Percent = c(Pref.per[,1],Pref.per[,2],Pref.per[,3],Pref.per[,4])
                            ,Wildness = c(rep(1,nrow(Pref.per)),rep(2,nrow(Pref.per))
                                          ,rep(3,nrow(Pref.per)),rep(4,nrow(Pref.per)))
                            )
p.pref <- ggerrorplot(data.Pref.per,x='Wildness',y='Preference.Percent'
                      ,desc_stat = 'mean_sd'
                      ,add = c("jitter"),add.params = list(color='gray',size=0.8,shape=1)
                      ,ylim=c(0,0.7))
p.pref
compare_means(Preference.Percent~Wildness,data=data.Pref.per,method = 'kruskal.test') # method:'kruskal.test'(Kruskal-Wallis test),'wilcox.test'(Wilcoxon test)
compare_means(Preference.Percent~Wildness,data=data.Pref.per,method = 'wilcox.test',paired = TRUE) # method:'kruskal.test','wilcox.test'
# plot the significance
p.pref + stat_compare_means(method = 'kruskal.test'
                          ,label.y=6)


# # aesthetic and visit
# aesthetic.label <- data$Aesthetic
# visit.label <- data$Visit
# 
# {
#   aesthetic.a = nrow(data[aesthetic.label=='a',]);visit.a <- nrow(data[visit.label=='a',])
#   aesthetic.b = nrow(data[aesthetic.label=='b',]);visit.b <- nrow(data[visit.label=='b',])
#   aesthetic.c = nrow(data[aesthetic.label=='c',]);visit.c <- nrow(data[visit.label=='c',])
#   aesthetic.d = nrow(data[aesthetic.label=='d',]);visit.d <- nrow(data[visit.label=='d',])
#   
# }
# 
# aesthetic <- c(aesthetic.b,aesthetic.c,aesthetic.d,aesthetic.a)
# visit <- c(visit.b,visit.c,visit.d,visit.a)

#2.Calculate SE####
density <- data[,label.col=='density'];density <- density[is.na(rowSums(density))==FALSE,] # remove empty value
SR <- data[,label.col=='SR']; SR <- SR[is.na(rowSums(SR))==FALSE,] # remove empty value

colSums(density)
colSums(SR)

mean.density <- colSums(density)/nrow(density);mean.density
se.density <- SE(density);se.density <- se.density$se;se.density

mean.SR <- colSums(SR)/nrow(SR);mean.SR
se.SR <- SE(SR);se.SR <- se.SR$se;se.SR

# mean<- c(mean.SR,mean.density)
# se <- c(se.SR$se,se.density$se)

#3.Human-lake connection among different diversity gradient####
mean.HLC <- mean(data$HLC.sum);mean.HLC
n <- length(data$HLC.sum)
se.HLC <- sd(data$HLC.sum)/n^(1/2);se.HLC

HLC.a <- data$HLC.sum[label.row=='a'];mean.hlc.a <- mean(HLC.a);se.hlc.a <- sd(HLC.a)/sqrt(length(HLC.a))
HLC.b <- data$HLC.sum[label.row=='b'];mean.hlc.b <- mean(HLC.b);se.hlc.b <- sd(HLC.b)/sqrt(length(HLC.b))
HLC.c <- data$HLC.sum[label.row=='c'];mean.hlc.c <- mean(HLC.c);se.hlc.c <- sd(HLC.c)/sqrt(length(HLC.c))
HLC.d <- data$HLC.sum[label.row=='d'];mean.hlc.d <- mean(HLC.d);se.hlc.d <- sd(HLC.d)/sqrt(length(HLC.d))

level.HLC <- c(rep(1,length(HLC.b)),
           rep(2,length(HLC.c)),
           rep(3,length(HLC.d)),
           rep(4,length(HLC.a))
           )
HLC <- c(HLC.b,HLC.c,HLC.d,HLC.a)
boxplot(HLC~level.HLC,ylab='Huamn-nature connection',xlab='',ylim=c(2,5))
plot(HLC~level.HLC,ylab='Huamn-nature connection',xlab='',ylim=c(2,5))

# difference between groups
Wilcox.test.grp(HLC,level.HLC)

Mean.HLC <- c(mean.hlc.b,mean.hlc.c,mean.hlc.d,mean.hlc.a)
SE.HLC <- c(se.hlc.b,se.hlc.c,se.hlc.d,se.hlc.a)
Mean.HLC;SE.HLC

#4.Plot preference, mean±SE####
  #par(mfrow=c(1,3))
  # data.preference <- data.frame(Preference = num, Wildness = c(1,2,3,4))
  # ggline(data.preference,x='Wildness',y='Preference',ylim=c(0,100))
  #plot(aesthetic,ylim=c(0,85))
  #plot(visit,ylim=c(0,85))
  
  # perceived SR: mean and error bar
  data.SR <- data.frame(Perceived.Richness = c(SR[,1],SR[,2],SR[,3],SR[,4])
                        ,Wildness = c(rep(1,length(SR[,1])),rep(2,length(SR[,1]))
                                      ,rep(3,length(SR[,1])),rep(4,length(SR[,1])))
                        )
  #plot(Perceived.Richness~Wildness, data=data.SR)
  p.SR <- ggerrorplot(data.SR,x='Wildness',y='Perceived.Richness',color='black'
              ,desc_stat = 'mean_sd',add='jitter',add.params = list(color='gray',size=0.8,shape=1)
              ,ylim=c(0,6)
              )
  # significant test among groups
  compare_means(Perceived.Richness~Wildness,data=data.SR,method = 'kruskal.test') # method:'kruskal.test','wilcox.test'
  compare_means(Perceived.Richness~Wildness,data=data.SR,method = 'wilcox.test') # method:'kruskal.test','wilcox.test'
  # plot the significance
  p.SR + stat_compare_means(method = 'kruskal.test'
                            ,label.y=6)
  # perceived density
  data.density <- data.frame(Perceived.Density = c(density[,1],density[,2],density[,3],density[,4])
                             ,Wildness = c(rep(1,length(density[,1])),rep(2,length(density[,1]))
                                          ,rep(3,length(density[,1])),rep(4,length(density[,1])))
                            )
  p.Density <- ggerrorplot(data.density,x='Wildness',y='Perceived.Density',color='black'
              ,desc_stat = 'mean_sd',add='jitter',add.params = list(color='gray',size=0.8,shape=1)
              ,ylim=c(0,6)
  )
  compare_means(Perceived.Density~Wildness,data=data.density,method = 'kruskal.test') # method:'kruskal.test','wilcox.test'
  compare_means(Perceived.Density~Wildness,data=data.density,method = 'wilcox.test') # method:'kruskal.test','wilcox.test'
  # plot the significance
  p.Density + stat_compare_means(method = 'kruskal.test'
                            ,label.y=6)
  #par(mfrow=c(1,1))
  
  # HLC
  data.HLC <- data.frame(HLC=HLC,level.HLC=level.HLC)
  p.HLC <- ggerrorplot(data.HLC,x='level.HLC',y='HLC',color='black'
                           ,desc_stat = 'mean_sd',add='jitter',add.params = list(color='gray')
                           ,ylim=c(2,5),xlab = 'Wildness',ylab='Human-lake relatedness'
  )
  p.HLC
  compare_means(HLC~level.HLC,data=data.HLC,method = 'kruskal.test') # method:'kruskal.test','wilcox.test'
  compare_means(HLC~level.HLC,data=data.HLC,method = 'wilcox.test') # method:'kruskal.test','wilcox.test'
  # plot the significance
  p.HLC + stat_compare_means(method = 'kruskal.test'
                                 ,label.y=5)
  
  # plot(HLC~level.HLC,ylab='Huamn-nature connection',xlab='',ylim=c(3,5))
  # plot(Mean.HLC,xlim=c(1,4),ylim=c(3,5),col='blue',type = 'p',pch=17
  #      ,ylab = 'Human-lake connection',xlab='')
  # arrowup(Mean.HLC,SE.HLC)

#4.2 3D plot between preference, wildeness and perceived density ####
  library(plot3D)
  # data China
  {z <- c(0.057,0.255,0.361,0.325)#data.Pref.per$Preference.Percent
  x <- c(1,2,3,4)#data.Pref.per$Wildness
  y <- c(1.802,2.819,3.896,4.789)#data.density$Perceived.Density
  title.3d <- 'China'}
  
  # data France  
  {z <- c(0.025,0.125,0.591,0.259)#data.Pref.per$Preference.Percent
    x <- c(1,2,3,4)#data.Pref.per$Wildness
    y <- c(2.328,2.897,4.129,4.810)#data.density$Perceived.Density
    title.3d <- 'France'}
  
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y,
                  z = 0, pmat = pmat)
    scatter2D(XY$x, XY$y,  pch = 1,
              cex = 1, add = TRUE, colkey = FALSE)

    XY <- trans3D(x = x, y = 6  # y=0, to put the plot on one side of the square
                  ,z = z, pmat = pmat)
    scatter2D(XY$x, XY$y, pch = 1,
              cex = 1, add = TRUE, colkey = FALSE)
  
    XY <- trans3D(x = 0, y = y  # x=6, to put the plot on one side of the square
                  ,z = z, pmat = pmat)
    scatter2D(XY$x, XY$y, pch = 1,
              cex = 1, add = TRUE, colkey = FALSE)
    }

  scatter3D(x,y,z, col='black'
            #,theta=105
            ,n=length(z),xlim=c(0,5),ylim=c(0,6),zlim=c(0,max(z)+0.1),ticktype='detailed'
            ,pch=16
            ,xlab='Wildness',ylab='Perceived density',zlab='Frequency of response'
            ,panel.first = panelfirst, main=title.3d)

#5.Motivation####
motivations <- data[,label.col=='motive']
head(motivations)

#5.1 Categorize the motivation into groups (Merge)####
level.motiv <- Element.grp(label.motiv)
i=0;result<-NULL
repeat{
  i=i+1
  label <- level.motiv[i]
  matrix <- motivations[,label.motiv==label]
  rowsum <- rowSums(matrix)
  binary <- rowsum
  binary[binary>0]=1
  result <- cbind(result,binary)
  if(i==length(level.motiv))break
}
colnames(result) <- level.motiv
result
motivations.category <- result
head(motivations.category)

Motiv.in.general <- Order.number(colSums(motivations.category),order = 'decrease')
# the most frequent mentioned motivation in general
barplot(Motiv.in.general,ylim=c(0,max(Motiv.in.general)+10))

#5.2 Count the motivation by wildness ####
{
  moti.a <- motivations.category[label.row=='a',];colsum.moti.a <- colSums(moti.a)
  moti.b <- motivations.category[label.row=='b',];colsum.moti.b <- colSums(moti.b)
  moti.c <- motivations.category[label.row=='c',];colsum.moti.c <- colSums(moti.c)
  moti.d <- motivations.category[label.row=='d',];colsum.moti.d <- colSums(moti.d)

  ylim <- max(c(max(colsum.moti.a),max(colsum.moti.b),max(colsum.moti.c),max(colsum.moti.d)))+10
  
# The ratio of occurrence
  colsum.moti.a.percent <- colsum.moti.a/sum(colsum.moti.a)
  colsum.moti.b.percent <- colsum.moti.b/sum(colsum.moti.b)
  colsum.moti.c.percent <- colsum.moti.c/sum(colsum.moti.c)
  colsum.moti.d.percent <- colsum.moti.d/sum(colsum.moti.d)
}

# #5.3 Plot the motivation count of the preference####
# {
# par(mfrow=c(2,4))
#   barplot(colsum.moti.b,ylab = '(a)',xlim = c(0,ylim),horiz = TRUE)
#   barplot(colsum.moti.c,ylab = '(b)',xlim = c(0,ylim),horiz = TRUE)
#   barplot(colsum.moti.d,ylab = '(c)',xlim = c(0,ylim),horiz = TRUE)
#   barplot(colsum.moti.a,ylab = '(d)',xlim = c(0,ylim),horiz = TRUE)
# 
# # Plot the motivation % of the preference
# 
#   barplot(colsum.moti.b.percent,ylab = '(a)',xlim = c(0,1),horiz = TRUE)
#   barplot(colsum.moti.c.percent,ylab = '(b)',xlim = c(0,1),horiz = TRUE)
#   barplot(colsum.moti.d.percent,ylab = '(c)',xlim = c(0,1),horiz = TRUE)
#   barplot(colsum.moti.a.percent,ylab = '(d)',xlim = c(0,1),horiz = TRUE)
# par(mfrow=c(1,1))
# }

# Barplot the ratio by four group in general
moti <- rbind(colsum.moti.b, colsum.moti.c, colsum.moti.d, colsum.moti.a);rownames(moti) <- c('1','2','3','4')
moti.percent <- rbind(colsum.moti.b.percent, colsum.moti.c.percent, colsum.moti.d.percent, colsum.moti.a.percent)
rownames(moti.percent) <- c('1','2','3','4')
#write.csv(rbind(moti,moti.percent),'230415 motivation - cn.csv')
#write.csv(rbind(moti,moti.percent),'230415 motivation - fr.csv')
color <- c('#F0E442','#0072B2','#BA0C2F','#009E73','#CC79A7','gray','#AA0D91','#E69F00')
barplot(t(moti.percent),ylim=c(0,1)
        ,col=color)
legend('topright',rev(colnames(moti.percent)),col = rev(color),pch=15)

# Barplot of every single specific categories are too many...
{
# #5.4 Barplot the ratio be four group in specific categories
# level.motiv <- Element.grp(label.motiv)
# label.row <- data$Preference
# 
# par(mfrow=c(2,4))
# i=0;#result.F.F <- NULL
# repeat{
#   i=i+1
#   label <- level.motiv[i]
#   specific.categories <- motivations[,label.motiv==label]
#   
#   # for each specific categories, such as Ecology
#     m=0;result.F <- NULL
#     level.pref <- c('b','c','d','a')
#     repeat{
#       m=m+1
#       moti.pref <- specific.categories[label.row==level.pref[m],]
#       
#       # colname: specific categories; row: wildness
#       result <- colSums(moti.pref)
#       result.F <- rbind(result.F,result)
#       
#       if(m==length(level.pref))break
#     }
#     rownames(result.F) <- c('1','2','3','4')
#     
#     result.F
#     result.F.freq <- result.F/rowSums(result.F)
#     # barplot the specific categories
#     density <- rev(c(10,20,30,40,50,60,70,80,90,100,110))[1:length(colnames(result.F))]
#     angle <- c(45,90,135,180,225,270,315,360,45,90,135)[1:length(colnames(result.F))]
#     {
#       legend <- colnames(result.F)[colSums(result.F)>0]
#       # Just want the legend, so I assign the data to 0, in order to copy the legend more easily
#       result.F[result.F>0]=0
#       barplot(t(result.F),col=color[i],main=label
#               ,density=density,angle = angle
#               ,ylim=c(0,1)
#               ,legend=FALSE)
#       legend('topright',legend = rev(legend),fill = color[i]
#              ,density = rev(density),angle = rev(angle))
#       
#       barplot(t(result.F.freq),col=color[i],main=label
#               ,density=density,angle = angle
#               ,legend=FALSE)
#       #legend('topright',legend = rev(colnames(result.F)),fill = color[i],density = rev(density),angle = rev(angle))
#     }
#   #result.F.F <- cbind(result.F.F,result.F)
#   if (i==length(level.motiv))break
# }
# par(mfrow=c(1,1))
# result.F.F
}

#5.5 A table to show the every single specific categories####
motivations <- data[,label.col=='motive']
label.row <- data$Preference

# matrix sum by row, row was categorized by grp factors
Sum.matrix.grp <-function(matrix,grp){
  
  i=0; result.F <- NULL
  level <- Element.grp(grp)
  level <- Order(level,c('b','c','d','a'))
  n <- length(level)
  repeat{
    i=i+1
    data <- matrix[grp==level[i],]
    result <- colSums(data)
    result.F <- rbind(result.F,result)
    if(i==n)break
  }
  rownames(result.F) <- level
  return(result.F)
}
moti.specific.cate <- Sum.matrix.grp(motivations,grp = label.row)
moti.specific.cate <- t(moti.specific.cate)
moti.specific.cate

if(title == 'cn') moti.specific.cate.cn <- moti.specific.cate
if(title == 'fr') moti.specific.cate.fr <- moti.specific.cate

#write.csv(moti.specific.cate,'230417 Motivations of specific categories-fr.csv')
#write.csv(Sum.matrix.grp(motivations,grp = label.row),'230417 Motivations of specific categories-cn.csv')

# #5.6 Order the code by them and their occurrence.
#     # P.S: this is done by 
# occurence.rate.fr <- moti.specific.cate.fr/sum(rowSums(moti.specific.cate.fr))
# occurence.rate.cn <- moti.specific.cate.cn/sum(rowSums(moti.specific.cate.cn))
# 
# occurence.rate <- rowSums(occurence.rate.fr + occurence.rate.cn)
# 
# # this is ordered by the occurrence
# level <- c("Aesthethic", "Ecology conditions", "Functionality", "Particular vegetation"
#            ,"Human intervention", "Imaginary context", "Safety") 
# 
# i=0; result.F <- NULL
# repeat{
#   i=i+1
#   data <- occurence.rate[label.motiv==level[i]] # take out one of the theme
# 
#   Motiv.general.order <- Order.number(data,order = 'decrease')
# 
#   result.F <- c(result.F, Motiv.general.order)
#   if(i==length(level))break
# }
#  write.csv(result.F,'the importance of each code.csv')
# code.general.order <- names(result.F)
# code.general.order
# # [1] "Green"              "Neatness"           "Beautiful"          "Not.messy"         
# # [5] "Openness"           "Water.is.clear"     "Colorful"           "Order"             
# # [9] "Bare.land"          "Not.bare.land"      "Wild"               "Natural"           
# # [13] "Not.too.wild"       "Healthier"          "Vitality"           "High.biodiversity" 
# # [17] "Easy.access"        "Recreational"       "Fishing"            "Sitting"           
# # [21] "Picnicing"          "Stroll"             "Swimming"           "Grass.to.feed.fish"
# # [25] "Lotus"              "Less.lotus"         "Human.intervention" "Less.intervention" 
# # [29] "Comfortable"        "Summer"             "Calm"               "Good.weather"      
# # [33] "Pleasure"           "Quiet"              "Peaceful"           "Autumn"            
# # [37] "Winter"             "Intimacy"           "Cool"               "No.bugs"           
# # [41] "Safe"               "No.snake"           "Good.water.quality" "Less.mosquitos"

# Theme ordered by the occurrence
level <- c("Aesthethic", "Ecology conditions", "Functionality", "Particular vegetation"
           ,"Human intervention", "Imaginary context", "Safety")

# Ordered by the occurrence of theme and the occurence
code.general.order <- c(
  "Green", "Neatness", "Beautiful", "Not.messy", 
  "Openness", "Water.is.clear", "Colorful", "Order", 
  "Bare.land", "Not.bare.land", "Wild", "Natural", 
  "Not.too.wild", "Healthier", "Vitality", "High.biodiversity", 
  "Easy.access", "Recreational", "Fishing", "Sitting", 
  "Picnicing", "Stroll", "Swimming", "Grass.to.feed.fish",
  "Lotus", "Less.lotus", "Human.intervention", "Less.intervention", 
  "Comfortable", "Summer", "Calm", "Good.weather", 
  "Pleasure", "Quiet", "Peaceful", "Autumn", 
  "Winter", "Intimacy", "Cool", "No.bugs", 
  "Safe", "No.snake", "Good.water.quality", "Less.mosquitos"
)

# the ordered theme for the code.general.order
label.motiv.ordered <- c(rep('Aesthethic',10)
                         ,rep('Ecology conditions',6)
                         ,rep('Functionality',8)
                         ,rep('Particular vegetation',2)
                         ,rep('Human intervention',2)
                         ,rep('Imaginary context',11)
                         ,rep('Safety',5)
)

#5.7 The ratio of every single specific categories, calculated by group ####
#   The ratio within the one theme

# Order the code by the code.general.order
i=0; matrix <- NULL
repeat{
  i=i+1
  moti.specific.cate
  
  row <- moti.specific.cate[rownames(moti.specific.cate) == code.general.order[i],]
  matrix <- rbind(matrix,row)
  if(i==length(code.general.order))break
}
rownames(matrix) <- code.general.order
moti.specific.cate.ordered <- matrix

# Calculate the ratio by theme
i=0; result.F <- NULL
repeat{
  i=i+1
  data <- moti.specific.cate.ordered[label.motiv.ordered==level[i],] # take out one of the theme
  total <- colSums(data)
  ratio <- t(data)/total
  ratio <- t(ratio)
  Empty <- c(0,0,0,0)
  result.F <- rbind(result.F,empty,empty,ratio)
  if(i==length(level))break
}
moti.specific.cate
ratio.specific.cate <- result.F
ratio.specific.cate[is.na(ratio.specific.cate)==TRUE]=0
ratio.specific.cate

#file.name = paste(Sys.time(),"Occurrence rate of code by theme.",title,'.csv')
#write.csv(ratio.specific.cate,file.name)

#5.7 Heatmap####

  # Orange: "#E69F00"
  # reverse the matrix
  ratio.specific.cate.re <- apply(t(ratio.specific.cate), 1, rev) # apply function rev on the 2nd margin (row)
  
  col<- colorRampPalette(c("white", "#0072B2"))(100) # created a gradually fading color
  heatmap(ratio.specific.cate.re, scale = 'none', col=col  # scale sets the color are scale by row, not by the exact value
          ,Rowv = NA,Colv = NA   # no cluster results
          )
  # Draw legends in PPT
  
#6.Demographic ####
demographic <- data[,label.col=='demographic']
general.stat <- function(matrix){
  i=0;result.F <- NULL
  repeat{
    i=i+1
    column <- matrix[,i]
    #remove empty
    column <- column[is.na(column)==FALSE]
    max <- max(column)
    min <- min(column)
    n <- length(column)
    mean <- mean(column)
    result <- data.frame(max=max,min=min,mean=mean,n=n)
    result.F <- rbind(result.F,result)
    if(i==ncol(matrix))break
  }
  rownames(result.F) <- colnames(matrix)
  return(result.F)
}

general.stat(demographic)

# data from Excel "220720 B-P relationship - field investigation - questionnaire results - streamlined.xlxs"
{
  # demographic <- read.table('clipboard',header=TRUE) # Excel C2:N4
  # demographic
  # label.demo <- c(rep('gender',2),rep('income',6),rep('education',4))
  # 
  # demo.plot <- function()
  # {
  # par(mfrow=c(1,3))
  # barplot(as.matrix(demographic[1,label.demo=='gender']),ylim=c(0,70),col='gray')
  # barplot(as.matrix(demographic[1,label.demo=='income']),ylim=c(0,40),col='gray')
  # barplot(as.matrix(demographic[1,label.demo=='education']),ylim=c(0,70),col='gray')
  # par(mfrow=c(1,1))
  # }
  # demo.plot()
  # 
  # # f.plot.demo <- 'plot.demo.pptx'
  # # p.plot.demo <- as.ggplot(~demo.plot())
  # # topptx(p.plot.demo,f.plot.demo)
  # 
  # age <- read.table('clipboard',header=FALSE); age <- age[age>0] # Excel B5:B124
  # mean.age <- mean(age,rm.na=TRUE);mean.age
  # sd.age <- sd(age);sd.age
  # hist(age,col='gray')
}
# data from Excel "220720 B-P relationship - field investigation - questionnaire results - streamlined.xlxs"

#7.(intimacy & Demo) - preference analysis ####
target.col <- c('Distance','Frequency','Age','Gender','Income','Education')
Na.judge <- rowSums(data[target.col])

# # remove NA
# data.rm.na <- data[is.na(Na.judge)==FALSE,]
# predictor <- data.rm.na[target.col] # take out the demographic data
# label.pref <- data.rm.na$Preference

# not rm the NA
predictor <- data[target.col] # take out the demographic data
label.pref <- data$Preference

# transform the letter to number a,b,c,d => 3,0,1,2
if(title=='fr'){
  pref.numero <- Replace(label.pref,c(4,3,2,1))[[2]]  # 4,3,2,1
  #pref.numero <- pref.numero
}

if(title=='cn'){
  pref.numero <- Replace(label.pref,c(3,4,2,1))[[2]] # 3,4,2,1
  #pref.numero <- pref.numero
}

hist(pref.numero)
# #Loop
# list <- c('age','gender','income','education')


#boxplot each variable along the gradient of abcd
par(mfrow=c(2,4))
i=0;result <- NULL;matrix.dif.test <- NULL
repeat{
  i=i+1
  main <- colnames(predictor)[i]
  #boxplot(predictor[,i]~pref.numero, main=main)
  
  ggerrorplot(data=cbind(pref.numero,predictor),x='pref.numero',y=main
              ,desc_stat = 'mean_sd'
              ,add = c("jitter"),add.params = list(color='gray'),main=main
              ,xlim=c(1,4)
  )

  dif.test <- Wilcox.test.grp(predictor[,i],pref.numero)
  matrix.dif.test <- rbind(matrix.dif.test,dif.test)
  result <- list(result,list(main,dif.test))
  if(i==length(colnames(predictor)))break
}
par(mfrow=c(1,1))
result
hist(pref.numero) # check what distribution of the data is.
write.csv(matrix.dif.test,'pairwise wilcox test.csv')

# Correlation plot
library(corrplot)
add.cor <- function(x,y){
  usr <- par('usr'); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  cor.test <- cor.test(x,y,method='spearman')
  cor.effi <- round(cor.test$estimate,2)
  p <-  round(cor.test$p.value,3)    
  text(0.5,0.5,c(cor.effi),cex=1.5)
  text(0.5,0.3,p,cex=1.5)
}

pairs(predictor[,-1]
      ,upper.panel= panel.smooth
      , lower.panel = add.cor
      )

# Poisson distribution: count data, Y>=0.

# select regression model

# pref ~ demographic factors# 
lm1.0 <- gamlss(pref.numero~.
                #,sigma.formula = ~.
                #,nu.formula = ~.
                ,data=predictor
                ,family=BI) # Binominal
lm1.1 <- gamlss(pref.numero~.
                # ,sigma.formula = ~.
                # ,nu.formula = ~.
                # ,tau.formula = ~.
                ,data=predictor
                ,family=MN3) # poisson
lm1.2 <- gamlss(pref.numero~.,data=predictor
                ,family=PIG) # Poisson inverse Gaussian
lm1.3 <- gamlss(pref.numero~.,data=predictor
                ,family=ZAP) # zero altered Poisson
lm1.4 <- gamlss(pref.numero~.,data=predictor
                ,family=ZIP) # zero inflated Poisson
lm1.5 <- gamlss(pref.numero~.,data=predictor
                ,family=ZIP2) # zero inflated Poisson ( µ the mean)
lm1.6 <- gamlss(pref.numero~.,data=predictor
                ,family=ZIPIG) #  zero inflated Poisson inv. Gaussian 
lm1.7 <- gamlss(pref.numero~Distance+Frequency+HLC.sum+Age+Gender+Income+Education,data=predictor
                ,family=ZIPF) # poisson

# which model is the best
AIC(lm1.0,lm1.1,lm1.2,lm1.3,lm1.4,lm1.5,lm1.6)

plot(lm1.1)

par(mfrow=c(2,4))
wp(lm1.0);wp(lm1.1);wp(lm1.2);wp(lm1.3)
wp(lm1.4);wp(lm1.5);wp(lm1.6);wp(lm1.7)
par(mfrow=c(1,1))

summary(lm1.0)
Rsq(lm1.3)

lm1.0 <- NULL;lm1.1 <- NULL;lm1.2 <- NULL;lm1.3 <- NULL
lm1.4 <- NULL;lm1.5 <- NULL;lm1.6 <- NULL;lm1.7 <- NULL

# pref ~ HLC
lm2.1 <- gamlss(pref.numero~HLC.lm
              ,family=PO)
lm2.2 <- gamlss(pref.numero~HLC.lm
                ,family=PIG)
lm2.3 <- gamlss(pref.numero~HLC.lm
                ,family=ZAP)
lm2.4 <- gamlss(pref.numero~HLC.lm
                ,family=ZIP)
lm2.5 <- gamlss(pref.numero~HLC.lm
                ,family=ZIP2)
lm2.6 <- gamlss(pref.numero~HLC.lm
                ,family=ZIPIG)
AIC(lm2.1,lm2.2,lm2.3,lm2.4,lm2.5,lm2.6)

plot(lm2.2);plot(lm2.3)
wp(lm2.2)
summary(lm2.2)
Rsq(lm2.2)

# HLC ~ demographic 
lm3.1 <- gamlss(HLC.lm~age+gender+income+education
                ,family=GA) #zero altered Poisson
plot(lm3.1)
summary(lm3.1)
wp(lm3.1)
Rsq(lm3.1)

# pref ~ HLC + demo 
lm4.1 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=PO) # Poisson
lm4.2 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=PIG) 
lm4.3 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=ZAP) 
lm4.4 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=ZIP) 
lm4.5 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=ZIP2) 
lm4.6 <- gamlss(pref.numero~HLC.lm+age+gender+income+education
                ,family=ZIPIG) 
AIC(lm4.1,lm4.2,lm4.3,lm4.4,lm4.5,lm4.6)

plot(lm4.3)
summary(lm4.3)
wp(lm4.3)
Rsq(lm4.3)

# End. Results in the end ####
num
preference
#aesthetic
#visit
mean.density
se.density

mean.SR
se.SR

Mean.HLC;SE.HLC

wilcox.test(HLC.a,HLC.d)# c,d
wilcox.test(HLC.a,HLC.c)# b,d
wilcox.test(HLC.c,HLC.d)# b,c

moti
