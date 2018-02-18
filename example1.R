library(foreach)
library(doParallel)
hosts <- as.vector(unique(read.table(Sys.getenv("PBS_NODEFILE"),stringsAsFactors=F))[,1])
nh<-length(hosts)
nc<-100
cl<-makeCluster(rep(hosts,each=nc/nh))
registerDoParallel(cl)
example1<-read.csv("/work/xwang35/data/example1.csv",header=TRUE)
ex1_500 = foreach(i=1:1000, 
                  .combine="cbind", 
                  .packages=c("oem","dplyr","dtplyr")) %dopar% {
                    d<-sample_n(example1,2000) 
                    matrix<-xtabs(~d$TR+d$outcome)
                    Diff <- (matrix[1,1]/(matrix[1,1]+matrix[1,2]))-(matrix[2,1]/(matrix[2,1]+matrix[2,2]))
                    ratio1 <- (matrix[1,1]/(matrix[1,1]+matrix[1,2]))/(matrix[2,1]/(matrix[2,1]+matrix[2,2]))
                    ratio2 <- (1-(matrix[1,1]/(matrix[1,1]+matrix[1,2])))/(1-(matrix[2,1]/(matrix[2,1]+matrix[2,2])))
                    ratio3 <- (matrix[2,1]/(matrix[2,1]+matrix[2,2]))/(matrix[1,1]/(matrix[1,1]+matrix[1,2]))
                    ratio4 <- (1-(matrix[2,1]/(matrix[2,1]+matrix[2,2])))/(1-(matrix[1,1]/(matrix[1,1]+matrix[1,2])))
                    n_odds_ratio <- ratio1/ratio2
                    loglm <- glm (outcome~TR+X2+X1+X3+X4+X5+X6+X7+X8,data=d,family=binomial)
                    log_odds_ratio <- exp(loglm$coefficients[2])
                    loglm1 <- glm(outcome~TR+X2+X1+X3+X4+X5+X6,data=d,family=binomial)
                    log_odds_ratio1 <- exp(loglm1$coefficients[2])
                    x <- cbind(d$TR,d$X1,d$X2,d$X3,d$X4,d$X5,d$X6,d$X7,d$X8)
                    z <- cbind(d$TR,d$X1,d$X2,d$X3,d$X4,d$X5,d$X6)
                    y <- d$outcome
                    oem <- oem(x=x,y=y,family="binomial",penalty="ols")
                    oem1<- oem$beta[1]
                    oem_coff<- oem1$ols[2]
                    oem_odds <- exp(oem_coff)
                    oem_z<-oem(x=z,y=y,family="binomial",penalty="ols")
                    oem2<-oem_z$beta[1]
                    oem_odds1<-exp(oem2$ols[2])
                    log <- glm (TR~X1+X2+X3+X4+X5+X6,data=d,family=binomial)
                    d$ps <- predict(log,type="response")
                    loglm2 <- glm(outcome~TR+X1+X2+X3+X4+X5+X6,weights=ps,data=d,family=quasibinomial)
                    log_odds_ratio2 <- exp(loglm2$coefficients[2])
                    d1 <- count(d,X1,X2,X3,X4,X5,X6,ps)
                    d2 <- count(d,X1,X2,X3,X4,X5,X6,ps,TR)
                    d3 <- count(d,X1,X2,X3,X4,X5,X6,ps,TR,outcome)
                    d4 <- merge(d1,d2, by=c("X1","X2","X3","X4","X5","X6","ps"))
                    d5 <- merge (d4,d3,by=c("X1","X2","X3","X4","X5","X6","ps","TR"))
                    d5$pst <- ifelse(d5$TR==1,d5$ps,1-d5$ps)
                    n <-sum(d5$n)
                    cper<-(d5$n.x)/n
                    bper<-d5$n/(d5$n.y)
                    d5$cb<-cper*bper
                    stand<-aggregate(cb~TR+outcome,data=d5,FUN=sum)
                    stand1<-data.frame(stand)
                    stand_Diff <- stand1$cb[1]-stand1$cb[2]
                    stand_risk1 <- stand1$cb[1]/stand1$cb[2]
                    stand_risk2 <- stand1$cb[3]/stand1$cb[4]
                    stand_risk3 <- stand1$cb[2]/stand1$cb[1]
                    stand_risk4 <- stand1$cb[4]/stand1$cb[3]
                    stand_odds <- stand_risk1/stand_risk2
                    tper<-(d5$n.y)/(d5$n.x)
                    d5$weighting <- d5$n/tper
                    IPWt<-aggregate(weighting~TR+outcome,data=d5,FUN=sum)
                    IPW1 <- data.frame(IPWt)
                    IPW_Diff <- (IPW1$weighting[1]/(IPW1$weighting[1]+IPW1$weighting[3]))-(IPW1$weighting[2]/(IPW1$weighting[2]+IPW1$weighting[4]))
                    IPW_risk1 <- (IPW1$weighting[1]/(IPW1$weighting[1]+IPW1$weighting[3]))/(IPW1$weighting[2]/(IPW1$weighting[2]+IPW1$weighting[4]))
                    IPW_risk2 <- (1-(IPW1$weighting[1]/(IPW1$weighting[1]+IPW1$weighting[3])))/(1-(IPW1$weighting[2]/(IPW1$weighting[2]+IPW1$weighting[4])))
                    IPW_risk3 <- (IPW1$weighting[2]/(IPW1$weighting[2]+IPW1$weighting[4]))/(IPW1$weighting[1]/(IPW1$weighting[1]+IPW1$weighting[3]))
                    IPW_risk4 <- (1-(IPW1$weighting[2]/(IPW1$weighting[2]+IPW1$weighting[4])))/(1-(IPW1$weighting[1]/(IPW1$weighting[1]+IPW1$weighting[3])))
                    IPW_odds <- IPW_risk1/IPW_risk2
                    d5$weighting1 <- d5$n/d5$pst
                    IPWt1<-aggregate(weighting1~TR+outcome,data=d5,FUN=sum)
                    IPW11 <- data.frame(IPWt1)
                    IPW1_Diff <- (IPW11$weighting1[1]/(IPW11$weighting1[1]+IPW11$weighting1[3]))-(IPW11$weighting1[2]/(IPW11$weighting1[2]+IPW11$weighting1[4]))
                    IPW1_risk1 <- (IPW11$weighting1[1]/(IPW11$weighting1[1]+IPW11$weighting1[3]))/(IPW11$weighting1[2]/(IPW11$weighting1[2]+IPW11$weighting1[4]))
                    IPW1_risk2 <- (1-(IPW11$weighting1[1]/(IPW11$weighting1[1]+IPW11$weighting1[3])))/(1-(IPW11$weighting1[2]/(IPW11$weighting1[2]+IPW11$weighting1[4])))
                    IPW1_risk3 <- (IPW11$weighting1[2]/(IPW11$weighting1[2]+IPW11$weighting1[4]))/(IPW11$weighting1[1]/(IPW11$weighting1[1]+IPW11$weighting1[3]))
                    IPW1_risk4 <- (1-(IPW11$weighting1[2]/(IPW11$weighting1[2]+IPW11$weighting1[4])))/(1-(IPW11$weighting1[1]/(IPW11$weighting1[1]+IPW11$weighting1[3])))
                    IPW1_odds <- IPW1_risk1/IPW1_risk2
                    e<-d[sample(nrow(d)),]
                    log1 <- glm (TR~X1+X2+X3+X4+X5+X6,data=e,family=binomial)
                    e$ps <- predict(log1,type="response")
                    e$c <- log(e$ps/(1-e$ps))
                    n1 <- nrow(d[which(e$TR==1),])
                    n2 <- nrow(d[which(e$TR==0),])
                    Treatment <- e[which(e$TR==1),]
                    Control <- e[which(e$TR==0),]
                    sd1<-sd(Treatment$c)
                    sd2<-sd(Control$c)
                    caliper <- 0.2* sqrt(((n1-1)*sd1*sd1+(n2-1)*sd2*sd2)/(n1+n2-2))
                    x1 <- rep(NA)
                    for (i in 1:nrow(Treatment)) {
                      g1 <- c(0:14)
                      row <- Treatment[i,]
                      g <- Control
                      g3<-0.0
                      for ( j in 1:nrow(g)) {
                        row1 <- g[j,]
                        if (abs(row1$c-row$c)<=caliper) {
                          g1<-rbind(g1,row1)
                          g2<-(data.frame(g1))[-1,]
                          g3<-mean(g2$outcome)
                        }
                      }
                      x1<-rbind(x1,g3)
                    }
                    x2 <- (data.frame(x1))[-1,]
                    psm_diff_m <- mean(Treatment$outcome)-mean(x2)
                    Match1 <- replicate(500, {
                      e1 <- c(0:14)
                      for (i in 1:nrow(Treatment)) {
                        row <- Treatment[i,]
                        f <- Control[sample(nrow(Control)),]
                        for ( j in 1:nrow(f)) {
                          row1 <- f[j,]
                          if (abs(row1$c-row$c)<=caliper) {
                            e1 <- rbind(e1,row1)
                            break
                          }
                        }
                      }
                      e2 <- data.frame(e1)
                      e3 <- e2[-1,]
                      e4 <- subset(Treatment,select=c(TR,outcome,X1,X2,X3,X4,X5,X6,X7,X8,ps))
                      e5 <- subset(e3,select=c(TR,outcome,X1,X2,X3,X4,X5,X6,X7,X8,ps))
                      m_data<- rbind(e4,e5)
                      matrix<-xtabs(~m_data$TR+m_data$outcome)
                      m_glm <- glm (outcome~TR+X2+X1+X3+X4+X5+X6+X7+X8,data=m_data,family=binomial)
                      m_glm1<-glm(outcome~TR+X2+X1+X3+X4+X5+X6,weights=ps,data=m_data,family=quasibinomial)
                      m_glm2<-glm(outcome~TR+X2+X1+X3+X4+X5+X6,data=m_data,family=binomial)
                      odds_c<-m_glm$coefficients
                      odds_c1<-m_glm1$coefficients
                      odds_c2 <- m_glm2$coefficients
                      odds_coeff <- data.frame(odds_c)
                      odds_coeff1<-data.frame(odds_c1)
                      odds_coeff2<-data.frame(odds_c2)
                      odds_m <- exp(odds_coeff$odds_c[2])
                      odds_m1 <-exp(odds_coeff1$odds_c1[2])
                      odds_m2<-exp(odds_coeff2$odds_c2[2])
                      Diff <- (matrix[1,1]/(matrix[1,1]+matrix[1,2]))-(matrix[2,1]/(matrix[2,1]+matrix[2,2]))
                      ratio1 <- (matrix[1,1]/(matrix[1,1]+matrix[1,2]))/(matrix[2,1]/(matrix[2,1]+matrix[2,2]))
                      ratio2 <- (1-(matrix[1,1]/(matrix[1,1]+matrix[1,2])))/(1-(matrix[2,1]/(matrix[2,1]+matrix[2,2])))
                      ratio3 <- (matrix[2,1]/(matrix[2,1]+matrix[2,2]))/(matrix[1,1]/(matrix[1,1]+matrix[1,2]))
                      ratio4 <- (1-(matrix[2,1]/(matrix[2,1]+matrix[2,2])))/(1-(matrix[1,1]/(matrix[1,1]+matrix[1,2])))
                      n_odds_ratio <- ratio1/ratio2
                      rbind(odds_m,odds_m1,Diff,ratio1,ratio2,ratio3,ratio4,n_odds_ratio,odds_m2)
                    })
                    Match <- rowMeans(as.data.frame(Match1))
                    PSM_odds <- Match[1]
                    PSM_odds1<-Match[2]
                    PSM_Diff <- Match[3]
                    PSM_ratio1 <- Match[4]
                    PSM_ratio2 <- Match[5]
                    PSM_ratio3 <- Match[6]
                    PSM_ratio4 <- Match[7]
                    PSM_HM <- Match[8]
                    PSM_odds2<-Match[9]
                    rbind(Diff,ratio1,ratio2,ratio3,ratio4, n_odds_ratio,log_odds_ratio,log_odds_ratio1,log_odds_ratio2, oem_odds,oem_odds1, stand_Diff,stand_risk1,stand_risk2,stand_risk3,stand_risk4,stand_odds,IPW_Diff,IPW_risk1,
                          IPW_risk2,IPW_risk3,IPW_risk4,IPW_odds,IPW1_Diff,IPW1_risk1,IPW1_risk2,IPW1_risk3,IPW1_risk4,IPW1_odds,PSM_odds,PSM_odds1,psm_diff_m, PSM_Diff, PSM_ratio1,PSM_ratio2,PSM_ratio3,PSM_ratio4,PSM_HM,PSM_odds2)
                  }

write.csv(x=ex1_500,file="example1_2000.csv")
