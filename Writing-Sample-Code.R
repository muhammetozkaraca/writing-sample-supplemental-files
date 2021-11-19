setwd("~/Desktop/supplemental-files/datasets")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(viridis)
library(fuzzyjoin)


edtg_data <- readxl::read_excel("EDTG_Data.xls") 

religious_terrorism <- subset(edtg_data, rel == 1)                    # Subsetting the data




religious_terrorism_sep <- separate(religious_terrorism,
                                          col = base,
                                          into = c("base1","base2","base3","base4",
                                                   "base5","base6","base7","base8",
                                                   "base9","base10"),
                                          sep = ",")

# all(is.na(religious_terrorism_seperated$`base-10`))

###################
### Group Names ### 
###################

groups_names  <- distinct(religious_terrorism_sep, gname, .keep_all = FALSE) # 203 groups exist in

######################
### Base Countries ### 
######################

countries <- religious_terrorism_sep %>% 
  pivot_longer(
    cols = starts_with("base"), 
    names_to = "origins", 
    values_to = "countries",
    values_drop_na = TRUE) %>%
  dplyr::select("countries") 


base_countries <- distinct(countries, countries, .keep_all = FALSE)


######################
### EDTG WORLD MAP ### 
######################

countries <- countries %>% 
  group_by(countries) %>%
  dplyr::summarise(freq = n())

countries <- as.data.frame(countries)
countries$countries[7] <- "Democratic Republic of the Congo"
countries$countries[53] <- "USA"
countries$countries[57] <- "Palestine"

world <- map_data("world")
countries_geocodes <- map_data("world", region = countries$countries)


names(countries)[1] <- "region"
geocoded_countries <- left_join(countries_geocodes, countries, by = "region") 


ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill="#69b3a2", alpha = 0.7)+
  geom_polygon(data = geocoded_countries, aes(x = long, y = lat, group = group, fill = freq))+
  scale_fill_viridis(name = "Numbers") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_void()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = c(0.95,0.57), plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(color = NA),
    legend.title = element_text(family="Arial Narrow",color="gray35"),
    legend.text = element_text(family="Arial Narrow",color="gray35"),
    legend.key = element_rect()) +
  ggtitle ("Number of Religious Terror Groups in Each Country") 

##############################
### EDTG - GTD COMPARISON ####
##############################
gtd_data <- readxl::read_excel('GTD(2019).xlsx')


group_names_gtd <- distinct(gtd_data, gname, .keep_all = FALSE)
group_names_gtd <- as.data.frame(group_names_gtd)  


group_names_edtg <- distinct(religious_terrorism_sep, gname, .keep_all = FALSE) # EDTG Rel. Terror Groups Names                                             
group_names_edtg <- as.data.frame(group_names_edtg)              # convert the names into dataframe 

index1 <- which(gtd_data$gname %in% group_names_edtg$gname)  # Subset GTD data based on edtg's groups

gtd_rel_ter_events <- gtd_data[index1,]                      # Finding the specific events

gtd_rel_ter_group_names <- unique(gtd_rel_ter_events$gname)  # List the names of RT groups in GTD
gtd_rel_ter_group_names <- as.data.frame(gtd_rel_ter_group_names)    # Make it dataframe
names(gtd_rel_ter_group_names) [1] <- "gname"

diff_before <- setdiff(group_names_edtg,gtd_rel_ter_group_names)

# Based on the EDTG Data where 203 groups indicated, the GTD has only 153 different coded data. 
# To figure out whether the differences are due to the punctuation problems or something related,
# I want to find out how similar the groups names in the GTD and in the EDTG datasets are.

final_result <- stringdist_join(group_names_gtd, group_names_edtg, by = "gname",
                                mode = "left", ignore_case = FALSE,
                                method = "jw",
                                max_dist = 99, distance_col = "dist") %>% 
  group_by(gname.x) %>%
  slice_min(order_by = dist, n = 1)

final_result$dist <- 1 - final_result$dist
most_similar <- final_result %>% 
  filter(dist > 0.7) %>% 
  filter (dist < 1)

# After checking the similarity statistics between the two columns one by one,
# I the fixed the groups names in the original EDTG data in line with the punctuation problems 
# I found out in between the GTD and the EDTG. 
# Now, I reload the tidied data into R environment.

# write_xlsx(most_similar, 'most_similar.xlsx')

tidied_edtg_base <- readxl::read_excel('EDTG-tidied.xlsx')
tidied_edtg_names <- tidied_edtg_base$gname
tidied_edtg_names <- as.data.frame(tidied_edtg_names)

index2 <- which(gtd_data$gname %in% tidied_edtg_base$gname)     # Subset GTD data based on edtg's groups
rt_events_gtd2 <- gtd_data[index2,]                             # Finding the specific events

# write_xlsx(rt_events_gtd2, 'religious_terror_attacks_dataset.xlsx')


rt_groups_names_gdt2 <-unique(rt_events_gtd2$gname)             # List the names of RT groups
rt_groups_names_gdt2 <- as.data.frame(rt_groups_names_gdt2) 

#write_xlsx(rt_events_gtd_names_dt2, 'edteg-gtd.xlsx')
names(tidied_edtg_names) [1] <- "gname"
names(rt_groups_names_gdt2) [1] <- "gname"

diff_after <- setdiff(tidied_edtg_names$gname,rt_groups_names_gdt2$gname)
diff_names_df_after <- as.data.frame(diff_after)


# after cross-checking, as can be seen, the difference decreased to 16 group from 50.

################################
### Rel Terror Attacks Table ###
################################

rt_events_gtd2_table <- rt_events_gtd2 %>%
  as.data.frame() %>%
  dplyr::select(gname,nkill) %>%
  group_by(gname) %>%
  dplyr::summarise(number_of_attacks = n(),
                   number_of_murdered = sum(nkill, na.rm=TRUE)) %>%
  dplyr::select (gname,number_of_attacks,number_of_murdered) %>%
  rename ("Names" = "gname", "Number of Attacks" = "number_of_attacks", 
          "Number of Killed People" = "number_of_murdered")

# write_xlsx(rt_events_gtd2_summarised, "terrorgroupsnames.xlsx")

#############################################
###  Religious Terror Attacks - World Map ###
#############################################

rt_events_gtd2$period[rt_events_gtd2$iyear > 1969  & rt_events_gtd2$iyear < 1980] = "1970-1979"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1979  & rt_events_gtd2$iyear < 1990] = "1980-1989"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1989  & rt_events_gtd2$iyear < 2000] = "1990-1999"
rt_events_gtd2$period[rt_events_gtd2$iyear > 1999  & rt_events_gtd2$iyear < 2010] = "2000-2009"
rt_events_gtd2$period[rt_events_gtd2$iyear > 2009  & rt_events_gtd2$iyear < 2020] = "2010-2019"


# Build the map

mybreaks <- c(25, 50, 100, 200, 400, 600, 1200)
summary(rt_events_gtd2$nkill)
hist(rt_events_gtd2$nkill)


rt_events_gtd2 %>%
  arrange(nkill) %>% 
  mutate(eventid=factor(eventid, unique(eventid))) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="#69b3a2", alpha = 0.7) +
  geom_point(data = rt_events_gtd2, aes(x=longitude, y=latitude, size=nkill, color=nkill), 
             shape = 20, stroke = FALSE) + 
  scale_size_continuous(range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="viridis", breaks=mybreaks, name = "Number of Killings") +
  theme_void() +
  guides(colour = guide_legend()) +
  ggtitle("Religious Terror Attacks Worldwide (1970-2019)") +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    text = element_text(color = "#22211d"),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(hjust=0.5, vjust = 0.75, 
    )) +
  facet_wrap(~`period`)



# ggsave("facetted.png", width = 14, height = 6, dpi = "screen")


##############################################
### REGRESSİON VAR. DATA ANALYSIS ############
##############################################

# military
data_military <- readxl::read_excel("military-index.xls") %>%
  dplyr::select("Country Name", "1990","1991","1992","1993","1994","1995","1996","1997",
                "1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"Country Name", names_to = "years", values_to = "military-spending")


# gdp
gdp_per_capita <- readxl::read_excel("gdp-per-capita-current-us.xls") %>%
  select("Country Name", "1990","1991","1992","1993","1994","1995","1996","1997",
         "1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
         "2011","2012","2013","2014") %>%
  pivot_longer(!"Country Name", names_to = "years", values_to = "gdp_per_capita")

merged <- cbind(data_military,
                gdp_per_capita$gdp_per_capita)

names(merged)[1] <- "country"
names(merged)[2] <- "year"
names(merged)[3] <- "military_spending"
names(merged)[4] <- "gdp_per_capita"

merged$year <- as.numeric(merged$year)

# polity
untidy_polity <- readxl::read_excel ("polity.xls") %>% 
  filter(year>1989) %>%
  filter(year<2015) %>%
  dplyr::select(country, year, polity) 

untidy_polity[untidy_polity == -66] <- NA
untidy_polity[untidy_polity == -77] <- 0

which(untidy_polity$polity==-88) 

# write_xlsx(untidy_polity, "untidy_polity.xlsx")

tidy_polity <- readxl::read_excel ("untidy_polity.xlsx")
which(tidy_polity$polity==-88) 


# In the codebook, it is stated that "-66" values are missing due to cases of foreign intervention
# Therefore, in this analysis, I treated them as "NA" values.

# Second, in the codebook, it is also stated that, "-77" values can be treated as 0 due to cases of "interregnum".

# Third, "-88" values are treated as cases of transition. Therefore, the change between two years
# is advised to be prorated. I will do so by handcoding in excel sheet. Please look at  "Polity5 Project: Dataset Users’ 
# Manual v2018" for detailed descriptions for such cases.







# state-fragility
state_fragility <- readxl::read_excel("state-fragility.xls") %>%
  filter(year<2015) %>%
  dplyr::select(country, year, sfi)

head(state_fragility)

# RAS DATA

ras3_state <- readxl::read_excel("Religion-and-State.XLSX")     

ras3_state$MX01X1990 # Restrictions on public observance of rel. services,
                     # festivals and/or holidays, including the Sabbath.

ras3_state$NX01X1990 # Restrictions on religious political parties

ras3_state$SBX1990   # Official Support

# secular constitutions
ras3_data_secular <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "SAX1990", "1991" = "SAX1991", "1992" = "SAX1992", "1993" = "SAX1993",
                "1994" = "SAX1994", "1995" = "SAX1995", "1996" = "SAX1996", "1997" = "SAX1997", 
                "1998" = "SAX1998", "1999" = "SAX1999", "2000" = "SAX2000", "2001" = "SAX2001",
                "2002" = "SAX2002", "2003" = "SAX2003", "2004" = "SAX2004", "2005" = "SAX2005",
                "2006" = "SAX2006", "2007" = "SAX2007", "2008" = "SAX2008", "2009" = "SAX2009",
                "2010" = "SAX2010", "2011" = "SAX2011", "2012" = "SAX2012", "2013" = "SAX2013",
                "2014" = "SAX2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "secular_constitution") 

ras3_data_secular$year <- as.numeric(ras3_data_secular$year)

# official support of state

ras3_data_state_support <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "SBX1990", "1991" = "SBX1991", "1992" = "SBX1992", "1993" = "SBX1993",
                "1994" = "SBX1994", "1995" = "SBX1995", "1996" = "SBX1996", "1997" = "SBX1997", 
                "1998" = "SBX1998", "1999" = "SBX1999", "2000" = "SBX2000", "2001" = "SBX2001",
                "2002" = "SBX2002", "2003" = "SBX2003", "2004" = "SBX2004", "2005" = "SBX2005",
                "2006" = "SBX2006", "2007" = "SBX2007", "2008" = "SBX2008", "2009" = "SBX2009",
                "2010" = "SBX2010", "2011" = "SBX2011", "2012" = "SBX2012", "2013" = "SBX2013",
                "2014" = "SBX2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "state_support") 

ras3_data_state_support$year <- as.numeric(ras3_data_state_support$year)


# restrictions on religious political parties

ras3_data_religious_parties <- ras3_state %>%
  dplyr::rename("country" = "COUNTRY",
                "1990" = "NX01X1990", "1991" = "NX01X1991", "1992" = "NX01X1992", 
                "1993" = "NX01X1993", "1994" = "NX01X1994", "1995" = "NX01X1995", 
                "1996" = "NX01X1996", "1997" = "NX01X1997", "1998" = "NX01X1998", 
                "1999" = "NX01X1999", "2000" = "NX01X2000", "2001" = "NX01X2001",
                "2002" = "NX01X2002", "2003" = "NX01X2003", "2004" = "NX01X2004", 
                "2005" = "NX01X2005", "2006" = "NX01X2006", "2007" = "NX01X2007", 
                "2008" = "NX01X2008", "2009" = "NX01X2009", "2010" = "NX01X2010", 
                "2011" = "NX01X2011", "2012" = "NX01X2012", "2013" = "NX01X2013",
                "2014" = "NX01X2014") %>%
  dplyr::select("country", "1990","1991","1992","1993","1994","1995","1996","1997","1998",
                "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                "2011","2012","2013","2014") %>%
  pivot_longer(!"country", names_to = "year", values_to = "restriction_on_rp") 

ras3_data_religious_parties$year <- as.numeric(ras3_data_religious_parties$year)

ras_data_final <- cbind(ras3_data_secular,
                        ras3_data_state_support$state_support,
                        ras3_data_religious_parties$restriction_on_rp)

names(ras_data_final)[4] <- "state_support"
names(ras_data_final)[5] <- "restriction_on_rp"


str(ras_data_final)
head(ras_data_final)



# subset GTD Data
subsetted_gtd <- rt_events_gtd2 %>%
  dplyr::select(country_txt,iyear, nkill, eventid) %>%
  filter(iyear>1989) %>%
  filter(iyear<2015) %>%
  group_by(country_txt, iyear) %>% 
  dplyr::summarize(number_of_attacks = n(), total_killed = sum(nkill)) %>%
  rename("year" = "iyear", "country" = "country_txt") %>%
  as.data.frame()



a <- distinct(polity, country, .keep_all = FALSE) # As RAS Data has limited geographical 
polity_deneme <- readxl::read_excel("polity_deneme.xls") 
a2 <- distinct(polity_deneme, country, .keep_all = FALSE) # As RAS Data has limited geographical 


# Merging different variables into single dataset


untidied_final <- ras_data_final %>% 
  full_join(merged, by=c("country","year")) %>%
  full_join(state_fragility, by=c("country","year")) %>%
  full_join(tidy_polity, by=c("country","year")) %>%
  full_join(subsetted_gtd, by=c("country","year"))

head(untidied_final)
summary(untidied_final)

head(ras_data_final)
distinct(ras_data_final, country, .keep_all = FALSE) # As RAS Data has limited geographical 
                                                     # scope compared to other cases, RAS does not
                                                     # consider certain countries which has low population
                                                     # (for the exact threshold,
                                                     # please see the dataset's guidelines)
                                                     # Therefore, I will keep only countries coded
                                                     # in the RAS 3.
                                                
# write_xlsx(untidied_final, "untidy_final.xlsx")










