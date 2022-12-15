
## NOW GATHER/PROCESS SHERIFF RURAL/UNINCORPORATED AREAS
# gather the figures for the unincorporated/rural areas covered by sheriffs departments
sheriffs_crime_murder <- cal_crime %>% select(year,county,ncic_code,homicide_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,homicide_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,for_rape_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,agg_assault_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,robbery_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,burglary_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,l_ttotal_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,vehicle_theft_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")

# gather the figures for the unincorporated/rural areas covered by sheriffs departments
sheriffs_clearance_murder <- cal_crime %>% select(year,county,ncic_code,homicide_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,homicide_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,for_rape_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,agg_assault_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,robbery_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,burglary_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,l_ttotal_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,vehicle_theft_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")



## Clearance Rates By Agency
# Filter for crime incident counts in jurisdictions in our five counties 
cal_clearance_murder <- cal_crime %>% select(year,county,ncic_code,homicide_clr_sum) %>% spread(year,homicide_clr_sum) %>% select(1,2,28:39)
cal_clearance_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_clr_sum) %>% spread(year,for_rape_clr_sum) %>% select(1,2,28:39)
cal_clearance_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_clr_sum) %>% spread(year,agg_assault_clr_sum) %>% select(1,2,28:39)
cal_clearance_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_clr_sum) %>% spread(year,robbery_clr_sum) %>% select(1,2,28:39)
cal_clearance_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_clr_sum) %>% spread(year,burglary_clr_sum) %>% select(1,2,28:39)
cal_clearance_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_clr_sum) %>% spread(year,l_ttotal_clr_sum) %>% select(1,2,28:39)
cal_clearance_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_clr_sum) %>% spread(year,vehicle_theft_clr_sum) %>% select(1,2,28:39)

# make quick tables that we can use for a quick simple map to be improved later
murders_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_murder,by=c("place"="ncic_code")) %>% left_join(murders_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Murder")
sexassaults_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_rape,by=c("place"="ncic_code")) %>% left_join(sexassaults_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Sexual Assault")
assaults_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_assault,by=c("place"="ncic_code")) %>% left_join(assaults_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Aggravated Assault")
robberies_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_robbery,by=c("place"="ncic_code")) %>% left_join(robberies_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Robbery")
burglaries_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_burglary,by=c("place"="ncic_code")) %>% left_join(burglaries_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Burglary")
thefts_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_theft,by=c("place"="ncic_code")) %>% left_join(thefts_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Theft")
autothefts_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_autotheft,by=c("place"="ncic_code")) %>% left_join(autothefts_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Vehicle Theft")

colnames(murders_clearance_places) <- sub(".x", "clr", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub(".x", "clr", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub(".x", "clr", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub(".x", "clr", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub(".x", "clr", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub(".x", "clr", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub(".x", "clr", colnames(autothefts_clearance_places))
colnames(murders_clearance_places) <- sub(".y", "total", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub(".y", "total", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub(".y", "total", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub(".y", "total", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub(".y", "total", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub(".y", "total", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub(".y", "total", colnames(autothefts_clearance_places))
colnames(murders_clearance_places) <- sub("countotal", "county", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub("countotal", "county", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub("countotal", "county", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub("countotal", "county", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub("countotal", "county", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub("countotal", "county", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub("countotal", "county", colnames(autothefts_clearance_places))

clearance_places <- rbind(murders_clearance_places,sexassaults_clearance_places,assaults_clearance_places,robberies_clearance_places,burglaries_clearance_places,thefts_clearance_places,autothefts_clearance_places)

clearance_places$`2010clr_pct` <- round(clearance_places$`2010clr`/clearance_places$`2010total`,3)*100
clearance_places$`2011clr_pct` <- round(clearance_places$`2011clr`/clearance_places$`2011total`,3)*100
clearance_places$`2012clr_pct` <- round(clearance_places$`2012clr`/clearance_places$`2012total`,3)*100
clearance_places$`2013clr_pct` <- round(clearance_places$`2013clr`/clearance_places$`2013total`,3)*100
clearance_places$`2014clr_pct` <- round(clearance_places$`2014clr`/clearance_places$`2014total`,3)*100
clearance_places$`2015clr_pct` <- round(clearance_places$`2015clr`/clearance_places$`2015total`,3)*100
clearance_places$`2016clr_pct` <- round(clearance_places$`2016clr`/clearance_places$`2016total`,3)*100
clearance_places$`2017clr_pct` <- round(clearance_places$`2017clr`/clearance_places$`2017total`,3)*100
clearance_places$`2018clr_pct` <- round(clearance_places$`2018clr`/clearance_places$`2018total`,3)*100
clearance_places$`2019clr_pct` <- round(clearance_places$`2019clr`/clearance_places$`2019total`,3)*100
clearance_places$`2020clr_pct` <- round(clearance_places$`2020clr`/clearance_places$`2020total`,3)*100
clearance_places$`2021clr_pct` <- round(clearance_places$`2021clr`/clearance_places$`2021total`,3)*100

# for map/table making purposes, changing Inf and NaN in calc fields to NA
clearance_places <- clearance_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
clearance_places <- clearance_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

write_csv(clearance_places,"data/output/clearances_byagency_20102021.csv")
clearance_places %>% filter(county=="Los Angeles County") %>% write_csv(clearance_places,"data/output/clearances_losangeles.csv")
