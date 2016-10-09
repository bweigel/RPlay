annuitaetenTitlgungsplan <- function(dahrlehenssumme=100000, zins=0.05, tilgung=0.02){
  annuitaet <- dahrlehenssumme*(zins+tilgung)
  
  f <- function(){
    if(dahrlehenssumme <= annuitaet) {
      
      tmp <- data.frame(restschuld=dahrlehenssumme, 
                        zins=dahrlehenssumme*zins, 
                        tilgung=dahrlehenssumme-(dahrlehenssumme*zins), 
                        annuitaet=dahrlehenssumme)
      df_zins <<- rbind(df_zins, tmp)
      return(0)
    } else {
      zinsanteil <- dahrlehenssumme*zins
      tilgung <- annuitaet-zinsanteil
       
      tmp <- data.frame(restschuld=dahrlehenssumme, 
                        zins=zinsanteil, 
                        tilgung=tilgung, 
                        annuitaet=zinsanteil+tilgung)
      
      if(exists("df_zins", envir = parent.frame())) df_zins <<- rbind(df_zins, tmp)
      else {
        assign("df_zins", tmp, envir = parent.frame())
      }
      dahrlehenssumme <<- dahrlehenssumme - tilgung 
    }
    f()
  }
  f()
  return(df_zins)
}

annuitaetenTitlgungsplan(dahrlehenssumme = 500000, zins = 0.02, 0.02)
annuitaetenTitlgungsplan(dahrlehenssumme = 500000, zins = 0.05, 0.02)

sum(annuitaetenTitlgungsplan(dahrlehenssumme = 500000, zins = 0.01, 0.02)["annuitaet"])
sum(annuitaetenTitlgungsplan(dahrlehenssumme = 500000, zins = 0.02, 0.02)["annuitaet"])

########################################

tilgungsdarlehenTitlgungsplan <- function(dahrlehenssumme=100000, zins=0.05, tilgung=5000){
  rate <- dahrlehenssumme*zins+tilgung
  
  f <- function(){
    if(dahrlehenssumme <= tilgung) {
      
      tmp <- data.frame(restschuld=dahrlehenssumme, 
                        zins=dahrlehenssumme*zins, 
                        tilgung=tilgung, 
                        rate=dahrlehenssumme)
      df_zins <<- rbind(df_zins, tmp)
      return(0)
    } else {
      zinsanteil <- dahrlehenssumme*zins
      rate <<- tilgung+zinsanteil
      
      tmp <- data.frame(restschuld=dahrlehenssumme, 
                        zins=zinsanteil, 
                        tilgung=tilgung, 
                        rate=rate)
      
      if(exists("df_zins", envir = parent.frame())) df_zins <<- rbind(df_zins, tmp)
      else {
        assign("df_zins", tmp, envir = parent.frame())
      }
      dahrlehenssumme <<- dahrlehenssumme - tilgung 
    }
    f()
  }
  f()
  return(df_zins)
}

tilgungsdarlehenTitlgungsplan()["rate"] %>% cumsum
annuitaetenTitlgungsplan()["annuitaet"] %>% cumsum

x <- 1:nrow(annuitaetenTitlgungsplan()["annuitaet"] %>% cumsum)
y <- annuitaetenTitlgungsplan()["annuitaet"] %>% cumsum %>% unlist
plot(x,y,ty="l")

x <- 1:nrow(tilgungsdarlehenTitlgungsplan()["rate"] %>% cumsum)
y <-tilgungsdarlehenTitlgungsplan()["rate"] %>% cumsum %>% unlist
lines(x,y, col="red")
