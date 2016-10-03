EE.ST <- EE %>% 
  filter(Bundesland=="ST" & Zeitpunkt.der.Inbetriebnahme > "2000-01-01") %>% 
  mutate(time = as.Date(as.yearmon(Zeitpunkt.der.Inbetriebnahme))) %>%
  group_by(PLZ, time, Energie) %>%
  summarise(Leistung.kW = sum(Leistung.kW)) %>% group_by(PLZ, Energie) %>%
  mutate(total = cumsum(Leistung.kW)) %>% ungroup()

ggplot(EE.ST, aes(x=time, y=total, group=Energie)) + 
  geom_area(aes(fill=Energie)) +
  facet_wrap(~PLZ, ncol=4)

# convert the PLZ column into NUTS3 codes
# if more than one NUTS3 code is plausible for a postal code area (e.g. 064XX) sample at random
EE.ST$NUTS <- sapply(EE.ST$PLZ, function(x){
  tryCatch(sample(PLZ_NUTS$NUTS_3[PLZ_NUTS$CODE %in% x], size=1),
           error=function(e){NA})
})

# convert the PLZ column into NUTS3 codes
# if more than one NUTS3 code is plausible for a postal code area (e.g. 064XX) sample at random
EE$NUTS <- sapply(EE$PLZ, function(x){
  tryCatch(sample(PLZ_NUTS$NUTS_3[PLZ_NUTS$CODE %in% x], size=1),
           error=function(e){NA})
})

tmp <- EE.ST %>% group_by(Energie, NUTS) %>% summarise(total = sum(total))
ST.pl <- merge(DE.pl3 %>% filter(grepl("(DEE)", x = DE.pl3$id)), tmp, by.x="id", by.y="NUTS")
# plot Saxony-Anhalt
ggplot(ST.pl) +
  geom_polygon(data=DE.pl1 %>% filter(id == "DEE"), aes(x=long, y=lat, group=group), fill=NA, color=alpha("grey10", 1), size=0.5) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=total), color=alpha("grey60", 0.6), size=0.5) +
  facet_wrap(~Energie, ncol=3)+
  scale_fill_gradient(low="white", high="steelblue") +
  coord_map()
