

library(bigrquery)
library(dplyr)
library(ggplot2)
library(reshape2)


nd_sql <- "SELECT complaint_type, year(created_date), DAYOFWEEK(created_date), COUNT(complaint_type) AS total_count FROM [bigquery-public-data.new_york.311_service_requests] GROUP BY 1,2,3 ORDER BY 2 DESC"

s1 <- query_exec(nd_sql, project = PROJECT_ID)

tbl_df(s1)

names(s1)

#Top Complaint per day of the week by year

s2 <- s1 %>%
        mutate(cym = paste(complaint_type, f0_, f1_, sep="-")) %>%
        arrange(f0_, f1_, desc(total_count)) %>%
        group_by(f0_, f1_) %>%
        top_n(1, total_count)

print(tbl_df(s2), n=56)


#Total count of complaints by day of the week and year

s3 <- s1 %>%
        select(-complaint_type) %>%
        group_by(f0_, f1_) %>%
        summarise_each(funs(sum)) %>%
        mutate(year = factor(f0_)) %>%
        mutate(day = factor(f1_))

tbl_df(s3)

nyd <- ggplot(s3, aes(day, total_count)) +   
  geom_bar(aes(fill = year), position = "dodge", stat="identity")

nyd

dev.copy(png,'D:/sfnyc/nyd.png')
dev.off()


s4 <- s3 %>%
        group_by(day) %>%
        summarise(total = sum(total_count))

tbl_df(s4)

########SF############

sf_sql <- "SELECT complaint_type, year(created_date), DAYOFWEEK(created_date), COUNT(complaint_type) AS total_count FROM [bigquery-public-data.san_francisco.311_service_requests] GROUP BY 1,2,3 ORDER BY 2 DESC"

sf1 <- query_exec(sf_sql, project = PROJECT_ID)

#Top Complaint per day of the week by year

sf2 <- sf1 %>%
  mutate(cym = paste(complaint_type, f0_, f1_, sep="-")) %>%
  arrange(f0_, f1_, desc(total_count)) %>%
  group_by(f0_, f1_) %>%
  top_n(1, total_count)

tbl_df(sf2)

#Total count of complaints by day of the week and year

sf3 <- sf1 %>%
        select(-complaint_type) %>%
        group_by(f0_, f1_) %>%
        summarise_each(funs(sum)) %>%
        mutate(year = factor(f0_)) %>%
        mutate(day = factor(f1_))

tbl_df(sf3)

sfd <- ggplot(sf3, aes(day, total_count)) +   
  geom_bar(aes(fill = year), position = "dodge", stat="identity")

sfd

dev.copy(png,'D:/sfnyc/sfd.png')
dev.off()


sf4 <- sf3 %>%
  group_by(day) %>%
  arrange(day) %>%
  summarise(total = sum(total_count))
  

tbl_df(sf4)



#### Combine ###########

sn <- s3 %>%
        rename(NYC=total_count) %>%
        inner_join(sf3, by=c("year", "day")) 
sn

names(sn)[8] <- "SF"

sn1 <- sn[c(5,3,8)]

### Total for each day
tbl_df(sn1)

sn2 <- sn1 %>%
          group_by(day) %>%
          summarise_each(funs(sum))

tbl_df(sn2  )

sn3 <- melt(sn2, id=c("day"))

snp <- ggplot(sn3, aes(day, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

snp

dev.copy(png,'D:/sfnyc/snp.png')
dev.off()


          


