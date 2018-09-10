
# setting working directory where my data files exist

#load libraries
library(stringr)
library(tidyr)
library(dplyr)

#open companies and round2 file
companies <- read.delim("companies.txt", sep="\t",header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
rounds2  <- read.csv("rounds2.csv",header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)

#solution:How many unique companies are present in rounds2? 
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink, locale = "en")
distinct_companies1 <- distinct(rounds2, company_permalink)
summary(distinct_companies1)

#solution:How many unique companies are present in the companies file?
companies$permalink <- str_to_lower(companies$permalink, locale = "en")
distinct_companies3 <- distinct(companies, permalink)
summary(distinct_companies3)

#solution:Are there any companies in the rounds2 file which are not  present in companies ?
diff_link <- ifelse(is.na(rounds2$company_permalink==companies$permalink),1,0)
sum(diff_link)

# create master frame from companies and round2
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

#creating new column raised_amount_usd_million in master frame
master_frame <- mutate(master_frame, 
                       raised_amount_usd_million = raised_amount_usd/1000000)

#solution: Average Values of Investments for venture,seed, angel, private equity
#Funding Types
fundingr_groups <- group_by(master_frame, funding_round_type)
summarise(fundingr_groups, 
          mean(raised_amount_usd, na.rm = T))

#finding top9 english speaking country sum of investment in usd_million wise
#Analysing the Top 3 English-Speaking Countries
master_frame$funding_round_type <- str_to_lower(master_frame$funding_round_type, locale = "en")
fund_slice <-subset(master_frame,master_frame$funding_round_type =="venture")
inv_groups <- group_by(fund_slice, country_code)
inv <- summarise(inv_groups, 
          sum(raised_amount_usd_million, na.rm = T))
colnames(inv) <- c("country","inv_million")
b<-arrange(inv, desc(inv_million))
top9 <- head(b,9)
top9

#reading mapping file.
mapping <- read.csv("mapping.csv", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
#converting wrong data into correct one i.e. "0" with "na"
mapping[,1]<-str_replace_all(mapping[,1],"0","na")
mapping[,1]<-str_replace_all(mapping[,1],"2.na","2.0")

#compressing columns in mapping using gather command
sectordata <- gather(mapping, sector, sector_val, Automotive...Sports:Social..Finance..Analytics..Advertising
)
sectordata <- sectordata[!(sectordata$sector_val == 0),]
sectordata <- sectordata[, -3]
sectordata <- sectordata[!(sectordata$sector == "Blanks"),]
sectordata <- sectordata[!is.na(sectordata$sector),]

#creating primary sector column based on category list exraction in master frame
master_frame$category_list <- str_to_lower(master_frame$category_list, locale = "en")
sectordata$category_list <- str_to_lower(sectordata$category_list, locale = "en")
sec <- separate(master_frame, category_list, into=c("primary_sector"), sep = "\\|", extra="drop")
master_frame <- cbind(master_frame, primary_sector = sec$primary_sector)
sectorframe<-master_frame

#creating final merged frame from compressed mapping and master frame
final<-merge(x =sectorframe, y = sectordata,by.x="primary_sector",by.y="category_list")
colnames(final)[18] <- "main_sector"
final_master_frame <- final

final_master_frame$main_sector <- as.factor(str_to_title(final_master_frame$main_sector))
summary(final_master_frame$main_sector)

#vector initialization for country, FT and range
country1 <- c("USA")
country2 <- c("GBR")
country3 <- c("IND")
FT <- c("venture")
min_range <- c(5000000)
max_range <- c(15000000)
final_master_frame$country_code <- str_to_upper(final_master_frame$country_code, locale = "en")

#create D1,D2,D3 frame by filtering on country code and FT
D1 <- filter(final_master_frame, country_code == country1, funding_round_type == FT, raised_amount_usd >= 5000000, raised_amount_usd <=15000000)
D2 <- filter(final_master_frame, country_code == country2, funding_round_type == FT, raised_amount_usd >= 5000000, raised_amount_usd <=15000000)
D3 <- filter(final_master_frame, country_code == country3, funding_round_type == FT, raised_amount_usd >= 5000000, raised_amount_usd <=15000000)

#grouping final master frame on main sector
group_sector<- group_by(final_master_frame,main_sector)

#funcion to get D1,D2,D3 frame by merged avg_raised_amt_usd, avg_no_of_investments column
#and excluding data out of min and max investment range.
group_main_sector <- function(x,xx_grp){
     country_main_sector <- summarise(xx_grp, sum(raised_amount_usd,na.rm = T), n())
     colnames(country_main_sector) <- c("main_sector","avg_raised_amt_usd","avg_no_of_investments")
     x<-merge(x,country_main_sector,by = "main_sector")
     #x <- subset(x, avg_raised_amt_usd > min_range & avg_raised_amt_usd < max_range)
     return(x)
}

#creating final D1,D2 and D3 by calling function group_main_sector
D1<-group_main_sector(D1,group_sector)
D2<-group_main_sector(D2,group_sector)
D3<-group_main_sector(D3,group_sector)

# Table 5.1 : Sector-wise Investment Analysis
# 1. Total number of investments (count)

sum(!is.na(D1$raised_amount_usd))

sum(!is.na(D2$raised_amount_usd))

sum(!is.na(D3$raised_amount_usd))


# 2. Total amount of investment (USD)

sum(D1$raised_amount_usd,na.rm = TRUE)

sum(D2$raised_amount_usd,na.rm = TRUE)

sum(D3$raised_amount_usd,na.rm = TRUE)


# 3. Top sector (based on count of investments)	 
# 4. Second-best sector (based on count of investments)
# 5. Third-best sector (based on count of investments)


top_sector<-function(x) return (group_by(x,main_sector) %>%
                                  summarise(n=n())%>%
                                  arrange(desc(n)))
D1_top3<-top_sector(D1)[1:3,]
D2_top3<-top_sector(D2)[1:3,]
D3_top3<-top_sector(D3)[1:3,]

#For the top sector count-wise (point 3), which company received the highest investment?
Highest_investment_company <- function(df1,top3,n=1){
  df1[which(df1$main_sector==top3[["main_sector"]][n]),]%>%       
    group_by(company_permalink,name)%>%
    summarise_at(.vars="raised_amount_usd",
                 .fun=sum,na.rm = TRUE)%>%
    arrange(desc(raised_amount_usd))%>%
    return()
}

Highest_investment_company(D1,D1_top3,1)[1,]
Highest_investment_company(D2,D2_top3,1)[1,]
Highest_investment_company(D3,D3_top3,1)[1,]

Highest_investment_company(D1,D1_top3,2)[1,]
Highest_investment_company(D2,D2_top3,2)[1,]
Highest_investment_company(D3,D3_top3,2)[1,]
# company_permalink         name  raised_amount_usd
# <chr>                     <chr>             <dbl>

#writing data frame files to disk
master_frame2 <-rbind(D1,D2)
master_frame2<-rbind(master_frame2,D3)
write.csv(master_frame2,"final.csv",row.names = FALSE)
write.csv(final_master_frame,"final_master.csv",row.names = FALSE)


