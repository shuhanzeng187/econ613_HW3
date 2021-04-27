#####Exercise 1 Links to the datasets
crime_long <- read_csv("Downloads/crime_long.csv")
officers <- read_csv("Downloads/officers.csv")
population <- read_csv("Downloads/population.csv")


#####Exercise 2 Data Manipulation
##Calculate total crime per month and plot the time series of crime.
library(dplyr)
crime_sum_per<-crime_long %>% 
  group_by(crime_month) %>% 
  summarise(total_number = sum(crimes))

plot1 <- ggplot(crime_sum_per, aes(x=crime_month, y=total_number)) +
  geom_line() + 
  xlab("month") +ylab("total number of crimes")

plot1


###Merge the two datasets by districts-units and period.
crime_pop<-merge(crime_long, population, by.x=c("crime_month","district"), by.y=c("month","district"))

###contruct a panel data 
crime_panel<-crime_pop %>% 
  group_by(crime_month,district) %>% 
  summarise(total_crime = sum(crimes))
crime_panel_vc<-crime_pop[crime_pop$crime_type=="violent",] %>% 
  group_by(crime_month,district) %>% 
  summarise(vc_sum=sum(crimes)) 
crime_panel_pc<-crime_pop %>% filter(crime_type=="property") %>% 
  group_by(crime_month,district) %>% 
  summarise(pc_sum=sum(crimes)) 
crime_panel_other<- population %>% 
  group_by(month,district) %>% 
  summarise(share_white=tot_white/tot_pop,share_black=tot_black/tot_pop, share_hisp=tot_hisp/tot_pop, tot_pop=tot_pop, median_income=p50_inc) 
crime_panel<-merge(crime_panel,crime_panel_vc,by=c("crime_month","district"))
crime_panel<-merge(crime_panel,crime_panel_pc,by=c("crime_month","district"))
crime_panel<-merge(crime_panel,crime_panel_other,by.x=c("crime_month","district"),by.y=c("month","district"))
crime_panel$tot_crime_per<-crime_panel$total_crime/crime_panel$tot_pop
crime_panel$vc_crime_per<-crime_panel$vc_sum/crime_panel$tot_pop
crime_panel$pc_crime_per<-crime_panel$pc_sum/crime_panel$tot_pop

kable(crime_panel[1:20,c(1:2,6:9)], format = "latex", caption = "panel data")
crime_panel


#####Exercise 3 Panel Data: Introduction
crime_officer<-merge(officers,crime_panel,by.x=c("month","unit"),by.y=c("crime_month","district"))
crime_officer1<-crime_officer
E3 <- lm(arrest ~ tenure + total_crime + median_income + share_black +
            share_white+share_hisp -1, data = crime_officer1)
summary(E3)
texreg(E3,digit=8)
#####Exercise 4 Panel Data: More controls

E4 <- lm(arrest ~ tenure + total_crime + median_income + share_black +
           share_white+share_hisp+ factor(unit)+factor(month)-1, data = crime_officer1)
summary(E4)
texreg(E4,digit=8,longtable = TRUE)

#####Exercise 5 Panel Data: Individual fixed effects

####average by NUID
off_id<-crime_officer %>% 
  group_by(NUID) %>% 
  summarise(arrest_id = mean(arrest),tenure_id = mean(tenure))
####average by district
off_unit<-crime_officer %>% 
  group_by(unit) %>% 
  summarise(arrest_unit = mean(arrest),tot_crime_d = mean(total_crime),income_d=mean(median_income),share_black_d=mean(share_black),share_white_d=mean(share_white),share_hisp_d=mean(share_hisp))
####average by month
off_month<-crime_officer %>% 
  group_by(month) %>% 
  summarise(arrest_t = mean(arrest),tot_crime_t = mean(total_crime),income_t=mean(median_income),share_black_t=mean(share_black),share_white_t=mean(share_white),share_hisp_t=mean(share_hisp),tenure_t = mean(tenure))
####average
off<-crime_officer %>% select(arrest, tenure, total_crime, median_income, share_black,share_white,share_hisp) %>% colMeans()
####
off_d_month<-crime_officer %>% 
  group_by(unit,NUID) %>% 
  summarise(arrest_d_m = mean(arrest),)
####
off_d_id<-crime_officer %>% 
  group_by(unit,month) %>% 
  summarise(arrest_d_id = mean(arrest),)

###between
###get cross section data of individuals
between_demeaned <- with(crime_officer1,
                         data.frame(arrest = ave(arrest, NUID),
                                    tenure = ave(tenure, NUID),
                                    NUID = NUID,
                                    unit = unit,
                                    month=month))
summary(lm(arrest ~ tenure , data = between_demeaned))

###within
####method1
crime_officer<-merge(crime_officer,off_id,by=c("NUID"))
crime_officer<-merge(crime_officer,off_unit,by=c("unit"))
crime_officer<-merge(crime_officer,off_month,by=c("month"))
crime_officer<-merge(crime_officer,off_d_month,by=c("NUID","unit"))
crime_officer<-merge(crime_officer,off_d_id,by=c("month","unit"))

off_rep<-sapply(off, rep.int, times=nrow(crime_officer))
crime_officer<-cbind(crime_officer,off_rep)
colnames(crime_officer)[32:38]<-paste(colnames(crime_officer)[32:38],"_m",sep = "")
###extract mean values
within<-data.frame(matrix(ncol = 0, nrow = nrow(crime_officer)))
within$month<-crime_officer$month
within$unit<-crime_officer$unit
within$NUID<-crime_officer$NUID
within$arrest<-crime_officer$arrest-crime_officer$arrest_id-crime_officer$arrest_unit-crime_officer$arrest_t+2*crime_officer$arrest_m
within$tenure<-crime_officer$tenure-crime_officer$tenure_id-crime_officer$tenure_t+crime_officer$tenure_m
within$total_crime<-crime_officer$total_crime-crime_officer$tot_crime_d-crime_officer$tot_crime_t+crime_officer$total_crime_m
within$income<-crime_officer$median_income-crime_officer$income_d-crime_officer$income_t+crime_officer$median_income_m
within$share_black<-crime_officer$share_black-crime_officer$share_black_d-crime_officer$share_black_t+crime_officer$share_black_m
within$share_white<-crime_officer$share_white-crime_officer$share_white_d-crime_officer$share_white_t+crime_officer$share_white_m
within$share_hisp<-crime_officer$share_hisp-crime_officer$share_hisp_d-crime_officer$share_hisp_t+crime_officer$share_hisp_m
within_reg<-lm(arrest ~ tenure + total_crime + income + share_black +
                 share_white+share_hisp-1,data=within)
summary(within_reg)
####method 2
# obtain demeaned data
within_demeaned <- with(crime_officer,
                            data.frame(arrest = arrest - ave(arrest, NUID)-ave(arrest, unit)-ave(arrest, month)+2*ave(arrest),
                                       tenure = tenure - ave(tenure, NUID)-ave(tenure, month)+ave(tenure),
                                       total_crime = total_crime - ave(total_crime,unit)-ave(total_crime,month)+ave(total_crime),
                                       median_income = median_income - ave(median_income,unit)-ave(median_income,month)+ave(median_income),
                                       share_black = share_black - ave(share_black,unit)-ave(share_black,month)+ave(share_black),
                                       share_white = share_white - ave(share_white,unit)-ave(share_white,month)+ave(share_white),
                                       share_hisp = share_hisp - ave(share_hisp,unit)-ave(share_hisp,month)+ave(share_hisp),
                                       NUID = NUID,
                                       unit = unit,
                                       month=month))

# estimate the regression
summary(lm(arrest ~ tenure + total_crime + median_income + share_black +
             share_white+share_hisp-1 , data = within_demeaned))
####method 3
within_2<-data.frame(matrix(ncol = 0, nrow = nrow(crime_officer)))
within_2$month<-crime_officer$month
within_2$unit<-crime_officer$unit
within_2$NUID<-crime_officer$NUID
within_2$arrest<-crime_officer$arrest-crime_officer$arrest_id
within_2$tenure<-crime_officer$tenure-crime_officer$tenure_id
within_2$total_crime<-crime_officer$total_crime
within_2$income<-crime_officer$median_income
within_2$share_black<-crime_officer$share_black
within_2$share_white<-crime_officer$share_white
within_2$share_hisp<-crime_officer$share_hisp
within_reg_2<-lm(arrest ~ tenure + total_crime + income + share_black +
                 share_white+share_hisp+factor(month)+factor(unit),data=within_2)
summary(within_reg_2)

###first difference
fd<-with(crime_officer1,data.frame(arrest_d = arrest - ave(arrest, month),
                                       tenure_d = tenure - ave(tenure, month),
                                       total_crime_d = total_crime - ave(total_crime, month),
                                       median_income_d = median_income - ave(median_income, month),
                                       share_black_d = share_black - ave(share_black, month),
                                       share_white_d = share_white - ave(share_white, month),
                                       share_hisp_d = share_hisp - ave(share_hisp, month),
                                       NUID = NUID,
                                       unit = unit,
                                       month=month))
fd_diff<-fd %>% group_by(NUID,unit) %>% arrange(month) %>% summarise(arrest = arrest_d-dplyr::lag(arrest_d,order_by = month),month=month,unit=unit)
fd_diff_2<-fd %>% group_by(NUID) %>%  arrange(month)%>%summarise(month=month,NUID=NUID,unit=unit,tenure= tenure_d- dplyr::lag(tenure_d,order_by = month))
fd_diff_3<-fd %>% group_by(unit) %>% arrange(month) %>% summarise(month=month,unit=unit,NUID=NUID,total_crime = total_crime_d-dplyr::lag(total_crime_d),median_income = median_income_d-dplyr::lag(median_income_d),share_black = share_black_d-dplyr::lag(share_black_d),share_white = share_white_d-dplyr::lag(share_white_d),share_hisp =share_hisp_d-dplyr::lag(share_hisp_d))
fd_diff<-merge(fd_diff,fd_diff_2,by=c("month","NUID","unit"))
fd_diff<-merge(fd_diff,fd_diff_3,by=c("month","unit","NUID"))
fd_reg<-lm(arrest ~ tenure + total_crime + median_income + share_black +
             share_white+share_hisp-1, data = fd_diff)
summary(fd_reg)

####plm
plm_fd<-plm(arrest ~ tenure + total_crime + median_income + share_black +
      share_white+share_hisp+ factor(month)+factor(unit)-1,index="NUID",effect="individual", model = "fd", data = crime_officer1)
summary(plm_fd)
plm_wi<-plm(arrest ~ tenure + total_crime + median_income + share_black +
              share_white+share_hisp+ factor(month)+factor(unit)-1,index="NUID",effect="individual", model = "within", data = crime_officer1)
summary(plm_wi)
plm_bt<-plm(arrest ~ tenure + total_crime + median_income + share_black +
              share_white+share_hisp+factor(month)+factor(unit)-1,index=c("NUID"),effect="individual", model = "between", data = crime_officer1)
summary(plm_bt)


####GMM
gmm(arrest ~ tenure + total_crime + median_income + 
       share_black + share_white + share_hisp+factor(month)+factor(unit)+factor("NUID"),data=crime_officer1)


