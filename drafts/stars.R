stars <- readr::read_csv("data/hygdata_v3.csv")

library(ggplot2)
library(astrolibR)

# for a latitude of -35deg, e.g Adelaide, we can see +/- 45deg,
# i.e. -80 to +10

mystars <- stars[-1,] %>% ## exclude sol
  filter(mag < 8) #%>% # visible limit is about 6.5
  # filter(dec > -80 & dec < 10) # southern sky horizon based on -35deg
  
# adelaide <- list(lat = -34.9285, lon = 138.6007)

# staraltaz <- eq2hor(mystars$ra, mystars$dec, lat = adelaide$lat, lon = adelaide$lon, 
#        jd = juldate(as.integer(strsplit(as.character(Sys.Date()), "-")[[1]])))
# s <- bind_cols(staraltaz)

## location of moon for 30 days either side of today
moon <- as.data.frame(moonpos(juldate(as.integer(strsplit(as.character(Sys.Date()), "-")[[1]])) + -30:30))
moon$ra <- moon$ra / 60 # degrees to hours

p <- ggplot(mystars, aes(ra, dec)) +
  geom_point(aes(alpha = mag, cex = mag)) +
  geom_point(data = moon, aes(ra, dec), cex = 8, pch = 20, alpha = 0.7, color = "yellow") +
  scale_alpha_continuous(range = c(0.001, 1), trans = "reverse") +
  scale_size_continuous(range = c(0.001, 0.5), trans = "reverse") + 
  geom_hline(yintercept = c(-80, 10), color = "red", linetype = "dashed") + 
  geom_hline(yintercept = -35, color = "red", linetype = "dashed", alpha = 0.5) 
  
p

p + coord_polar(theta = "x")
