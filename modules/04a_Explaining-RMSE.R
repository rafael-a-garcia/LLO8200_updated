#unconditional mean is just sqrt((Y-Y-bar)^2/N) or the stdev
var.p<-function(x){
  var(x, na.rm = TRUE)*
    (length(x[!is.na(x)])-1)/length(x[!is.na(x)])
} # var() by default uses (n-1), we want N, so we define var.p


# sqrt(variance) = Standard Deviation
sqrt(var.p(df$grad_debt_mdn))

#Look familiar?


## Conditional mean is breaking up/partioning the data into groups. Kinda like you did with t-tests and ANOVAs. The difference is: We didn't design these groups like you would for an experiment. We selected them from the data. 

# Entire sample
gg<-df%>%
  ggplot(aes(x=grad_debt_mdn))+
  geom_histogram(color="darkgrey",fill="white")
gg

df.pri<-df%>%
  filter(control=="Private")

df.pub<-df%>%
  filter(control=="Public")

# Just the Private Schools
gg1<-gg+ geom_histogram(data=df.pri, color="red",fill="red",alpha=.3)

# Just the Public Schools
gg1<-gg1+ geom_histogram(data=df.pub, color="blue",fill="blue",alpha=.3)
gg1


# Not all obs were used, so what are our ACTUAL Ns
N.pri<-length(df.pri$grad_debt_mdn[!is.na(df.pri$grad_debt_mdn)])
N.pub<-length(df.pub$grad_debt_mdn[!is.na(df.pub$grad_debt_mdn)])
N.tot<-length(df$grad_debt_mdn[!is.na(df$grad_debt_mdn)])

# Variances for each grouping
var.pri<-var.p(df.pri$grad_debt_mdn)
var.pub<-var.p(df.pub$grad_debt_mdn)

# The Squareroot of the weighted average of the variances, is equal to the RMSE!
sqrt((N.pri*var.pri + N.pub*var.pub)/N.tot)

######################################################

sc0<-sc%>%
  mutate(mean_earn_uncon=mean(md_earn_wne_p6, na.rm=T))

rmse_sc_uncon<-sc0%>%
  rmse(md_earn_wne_p6, mean_earn_uncon)

rmse_sc_uncon

### finding the amount of change

pRMSEChange<-function(x,y){
  (x$.estimate-y$.estimate)/x$.estimate*100
}

pRMSEChange(rmse_sc_uncon,rmse_adm)

