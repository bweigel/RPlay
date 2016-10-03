library(dplyr) 
library(tidyr)
library(ggplot2)
library(stringr)

read.NET <- function(df, enc="latin1"){
  require(stringr)
  require(dplyr)
  bl.dic <- c(BW = "Baden-Würtemberg", BY = "Bayern", HE = "Hessen", NI = "Niedersachsen",
              NW = "Nordrhein-Westfalen", RP = "Rheinland-Pfalz", SL = "Saarland", BB = "Brandenburg",
              BE = "Berlin", HH = "Hamburg", MV = "Mecklenburg- Vorpommern", SH = "Schleswig-Holstein",
              SN = "Sachsen", ST = "Sachsen-Anhalt", TH = "Thüringen", HB = "Bremen")
  
  EE <- read.table(df, sep=";", stringsAsFactors = F, header=T, fileEncoding = enc) %>%
    mutate(Installierte.Leistung..kW. = Installierte.Leistung..kW. %>% str_replace(",", ".") %>% as.numeric())
  names(EE)[c(5:7,11)] <- c("Bundesland", "PLZ", "Leistung.kW", "Energie")
  
  EE$Bundesland <- toupper(EE$Bundesland)

  # fill in Bundesland in rows where Bundesland is missing
  EE$Bundesland[which(EE$Bundesland == " ")] <- NA
  EE$Bundesland[which(EE$Bundesland == "")] <- NA
  BL_PLZ <- EE[!is.na(EE$Bundesland),] %>% select(Bundesland, PLZ) %>% distinct()
  EE[is.na(EE$Bundesland),]$Bundesland <- BL_PLZ$Bundesland[match(EE[is.na(EE$Bundesland),]$PLZ, BL_PLZ$PLZ)]
  
  out <- EE %>% 
    select(Bundesland, PLZ, Leistung.kW, Energie, Zeitpunkt.der.Inbetriebnahme) %>%
    group_by(Bundesland, PLZ, Energie, Zeitpunkt.der.Inbetriebnahme) %>% summarise_each(funs(sum))
  
  return(out)
}

EE.l <- list()
EE.l <- append(EE.l, list(read.NET("~/Data/EE-Anlagen/50Hertz_Anlagenstammdaten_2014.csv")))
EE.l <- append(EE.l, list(read.NET("~/Data/EE-Anlagen/Amprion_Anlagenstammdaten_2014.csv")))
EE.l <- append(EE.l, list(read.NET("~/Data/EE-Anlagen/TenneT_Anlagenstammdaten_2014.csv")))
EE.l <- append(EE.l, list(read.NET("~/Data/EE-Anlagen/TransnetBW_Anlagenstammdaten_2014.csv")))


EE <- do.call(rbind, EE.l)
EE <- EE %>% ungroup() %>% 
  mutate(Energie = str_replace(Energie, "(Kl[[:cntrl:]]+rgas)", "Klärgas"),
         Zeitpunkt.der.Inbetriebnahme = as.Date(Zeitpunkt.der.Inbetriebnahme, "%d.%m.%Y"))

EE$Energie[which(EE$Energie == "")] <- "NA"
EE$Energie[which(EE$Energie == "NV")] <- "NA"

EE <- EE  %>% 
  group_by(Bundesland, PLZ, Energie, Zeitpunkt.der.Inbetriebnahme) %>%
  summarise(Leistung.kW = sum(Leistung.kW))

EE.wide <- spread(EE, Energie, Leistung.kW, fill = 0)
EE.wide$Windenergie <- EE.wide$Windenergie + EE.wide$`Windenergie an Land`
EE.wide$EEGas <- EE.wide$Deponiegas + EE.wide$Grubengas + EE.wide$Klärgas
EE.wide <- EE.wide[,-c(13,5,7,8)]

EE <- gather(EE.wide, Energie, Leistung.kW, -Bundesland, -PLZ, -Zeitpunkt.der.Inbetriebnahme)

save(EE, file="EE_Data.RDa")