library(rlang)
library(plyr)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)

#read data
contact <- read.csv("\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\contact.csv")
links<-c(as.character(tags$a(href = "https://www.Americo.com", target = "_blank", "Americo.com" )),
         as.character(tags$a(href= "https://www.assuredlife.org", target = "_blank", "assuredlife.org")),
         as.character(tags$a(href= "https://www.bflic.com", target = "_blank", "bflic.com")),
         as.character(tags$a(href= "https://www.bcbsmt.com", target = "_blank", "bcbsmt.com")),
         as.character(tags$a(href= "https://www.cso.com", target = "_blank", "cso.com")),
         as.character(tags$a(href= "https://www.bankerslife.com/products/medicare-supplement-insurance", target = "_blank", "bankerslife.com")),
         as.character(tags$a(href= "https://www.aetnaseniorproducts.com", target = "_blank", "aetnaseniorproducts.com")),
         as.character(tags$a(href= "https://www.everence.com", target = "_blank", "everence.com")),
         as.character(tags$a(href= "https://www.federallife.com", target = "_blank", "federallife.com")),
         as.character(tags$a(href= "https://www.slaico.com", target = "_blank", "slaico.com")),
         as.character(tags$a(href= "https://www.gpmhealthandlife.com", target = "_blank", "gpmhealthandlife.com")),
         as.character(tags$a(href= "https://www.gtlic.com", target = "_blank", "gtlic.com")),
         as.character(tags$a(href= "https://www.humana.com", target = "_blank", "humana.com")),
         as.character(tags$a(href= "https://www.lumico.com", target = "_blank", "lumico.com")),
         as.character(tags$a(href= "https://www.MHC.COOP", target = "_blank", "MHC.COOP")),
         as.character(tags$a(href= "https://www.mutualofomaha.com/states", target = "_blank", "mutualofomaha.com")),
         as.character(tags$a(href= "https://www.prosperitylife.com", target = "_blank", "prosperitylife.com")),
         as.character(tags$a(href= "https://www.statefarm.com", target = "_blank", "statefarm.com")),
         as.character(tags$a(href= "https://www.uct.org", target = "_blank", "uct.org")),
         as.character(tags$a(href= "https://www.aarpmedicaresupplement.com", target = "_blank", "aarpmedicaresupplement.com")),
         as.character(tags$a(href= "https://www.wmimutual.com/medigap", target = "_blank", "wmimutual.com"))
)
web<-data.frame(links = links)
contact<-cbind(contact,web)

discount <- read.csv("\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\discount.csv")
history <- read.csv("\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\history.csv", check.names = FALSE)
PAC <- read.csv("\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\PAC.csv")
rates <- read.csv("\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\rates.csv")

#Change first column header
names(contact)[1]<- "Company"
names(discount)[1]<- "Company"
names(history)[1]<- "Company"
names(PAC)[1]<- "Company"
names(rates)[1]<- "Company"

#merge rates and PAC to get monthly yearly tob/nontob rates
ratefactors<-left_join(rates, PAC, by = "Company")
ratefactors<-left_join(ratefactors, discount, by = "Company")

ann_nontob_nhh<-ratefactors[,c(1:4,6:12)]
ann_nontob_nhh$Rate<-ratefactors$Rate*ratefactors$PAC_yearly
ann_nontob_nhh$Time<-"Yearly"
ann_nontob_nhh$Tobacco<-"No"
ann_nontob_nhh$HH<-"No"

ann_tob_nhh<-ann_nontob_nhh[,c(1:13,15)]
ann_tob_nhh$Tobacco<-"Yes"
ann_tob_nhh$Rate<-ann_tob_nhh$Rate*ann_tob_nhh$tob_factor

ann_nontob_hh<-ann_nontob_nhh[,1:15]
ann_nontob_hh$Rate<-ann_nontob_hh$Rate*ann_nontob_hh$Factor
ann_nontob_hh$HH<-"Yes"

ann_tob_hh<-ann_tob_nhh[,c(1:13,15)]
ann_tob_hh$Rate<-ann_tob_hh$Rate*ann_tob_hh$Factor
ann_tob_hh$HH<-"Yes"


###Monthly
mon_nontob_nhh<-ann_nontob_nhh[,c(1:12,14,15)]
mon_nontob_nhh$Rate<-mon_nontob_nhh$Rate*mon_nontob_nhh$PAC_mthly
mon_nontob_nhh$Time<-"Monthly"

#Colonial Penn & state farm have different monthly calculations that need to be corrected
#pulling out the 2 companies then removing them from the data frame
colpenmon<-mon_nontob_nhh%>%filter(Company == "Colonial Penn Life Insurance Company")
statefarmmon<-mon_nontob_nhh%>%filter(Company == "State Farm Mutual Automobile Insurance Company")
mon_nontob_nhh<-mon_nontob_nhh%>%filter(Company != "Colonial Penn Life Insurance Company")
mon_nontob_nhh<-mon_nontob_nhh%>%filter(Company != "State Farm Mutual Automobile Insurance Company")

#rate adjustment
colpenmon$Rate<-colpenmon$Rate+1
statefarmmon$Rate<-statefarmmon$Rate/6
#bring companies back into dataframe
mon_nontob_nhh<-merge(mon_nontob_nhh,colpenmon, all = TRUE)
mon_nontob_nhh<-merge(mon_nontob_nhh,statefarmmon, all = TRUE)

#check number of companies - should be 21
length(unique(mon_nontob_nhh$Company))
rm(colpenmon,statefarmmon)

mon_tob_nhh<-ann_tob_nhh[,c(1:12,14,15)]
mon_tob_nhh$Rate<-mon_tob_nhh$Rate*mon_tob_nhh$PAC_mthly
mon_tob_nhh$Time<-"Monthly"

#Colonial Penn & state farm have different monthly calculations that need to be corrected
#pulling out the 2 companies then removing them from the data frame
colpenmon<-mon_tob_nhh%>%filter(Company == "Colonial Penn Life Insurance Company")
statefarmmon<-mon_tob_nhh%>%filter(Company == "State Farm Mutual Automobile Insurance Company")
mon_tob_nhh<-mon_tob_nhh%>%filter(Company != "Colonial Penn Life Insurance Company")
mon_tob_nhh<-mon_tob_nhh%>%filter(Company != "State Farm Mutual Automobile Insurance Company")

#rate adjustment
colpenmon$Rate<-colpenmon$Rate+1
statefarmmon$Rate<-statefarmmon$Rate/6
#bring companies back into dataframe
mon_tob_nhh<-merge(mon_tob_nhh,colpenmon, all = TRUE)
mon_tob_nhh<-merge(mon_tob_nhh,statefarmmon, all = TRUE)

#check number of companies - should be 20
length(unique(mon_nontob_nhh$Company))
rm(colpenmon,statefarmmon)

mon_nontob_hh<-mon_nontob_nhh[,c(1:13,15)]
mon_nontob_hh$Rate<-mon_nontob_hh$Rate*mon_nontob_hh$Factor
mon_nontob_hh$HH<-"Yes"

mon_tob_hh<-mon_tob_nhh[,c(1:12,14,15)]
mon_tob_hh$Rate<-mon_tob_hh$Rate*mon_tob_hh$Factor
mon_tob_hh$HH<-"Yes"

#Merge rates
an<-merge(ann_nontob_hh,ann_nontob_nhh, all = TRUE)
at<-merge(ann_tob_hh, ann_tob_nhh, all = TRUE)
a<-merge(an,at,all = TRUE)

mn<-merge(mon_nontob_hh,mon_nontob_nhh,all = TRUE)
mt<-merge(mon_tob_hh, mon_tob_nhh,all = TRUE)
m<-merge(mt,mn,all = TRUE)

final_rates<-merge(m,a,all = TRUE)

#contact, discount, history
#final_rates<-left_join(final_rates,discount, by = "Company")
final_rates<-left_join(final_rates,contact, by = "Company")
final_rates<-left_join(final_rates,history, by = c("Company" ,"Plan"))


# library(reshape2)
# testing <- dcast(final_rates, Rate ~ HH)
final_rates<-final_rates %>% mutate(
  "disRate" = ifelse(final_rates$HH == "No", floor(final_rates$Rate -final_rates$Rate*final_rates$Percent), NA)
)

# final_rates <- final_rates[final_rates$disRate is.NA, ]

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

final_rates <- completeFun(final_rates, "disRate")
# discount<-left_join(discount,final_rates, by = "Company")
# colnames(discount)
# discount <- discount %>% select("Company", "Rate", "Plan","Percent.x","Description.x")

# discount <- drop_na(discount)

final_rates$Rate<-round(final_rates$Rate,0)
final_rates <- final_rates %>% select(-"2016",-"2017")
save(final_rates, file ="\\\\saohlnfs003\\Desktop$\\cya066\\Desktop\\Rscript\\Project\\final_rates.Rdata" )
#csilogo<-readPNG("\\\\saohlnfs003\\Desktop$\\cya028\\Documents\\med-supp-shiny\\2021\\data\\www\\CSI Auditor Logo.png")

