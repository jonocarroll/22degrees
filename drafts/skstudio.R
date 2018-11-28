## based on 
## https://www.skstudio.london/constellation-map-code
## for now just spitballing what to do with this

# Load packages
# -- -- -- -- -- -- 
library(ggplot2)
library(dplyr)

# Data includes

# -- -- -- -- -- -- 
# "stars" == the HYG star database archive that lists all recorded stars with coordinates and characteristics
# This data can be found on https://github.com/astronexus/HYG-Database or above

# -- -- -- -- -- -- 
# "constellations" == a dataset of segments for constellations (the Western skyculture is used below) 
# This data can be found on https://github.com/Stellarium/stellarium/tree/master/skycultures 
# (throughout a manipulated .csv of this data is used and can be found above)

# -- -- -- -- -- -- 
download.file("https://github.com/astronexus/HYG-Database/raw/master/hygdata_v3.csv", destfile = "data/hygdata_v3.csv")
stars <- read.csv('data/hygdata_v3.csv', header = TRUE, stringsAsFactors = FALSE)
# "stars.min"
stars <- stars[,c('hip', 'proper', 'ra', 'dec', 'dist', 'x', 'y', 'z', 'lum', 'mag')]
# Northern Hemisphere/Southern Hemisphere (below will focus on the Northern Hemisphere)
stars.n <- stars[which(stars$dec >= -20 & stars$mag <= 5.5 & is.na(stars$hip) == 0),]
stars.s <- stars[which(stars$dec < 20 & stars$mag <= 5.5 & is.na(stars$hip) == 0),]

constellations <- read.csv('data/ModernConstellations.csv', header = TRUE, stringsAsFactors = FALSE)

# -- -- -- -- -- -- 
# Here, I manipulate the original format of "constellations" that lists a sequence of Hipparcos star IDs 
# (Hipparcos star IDs are the primary key to the "stars" data above)
# (the original format is explained in the Stellarium user guide, chapter 9.7)
# I use a for-loop to visualise and later check the segments are correct and then apply this to all constellations of interest
# (this loop puts segments start (x1,y1) and end (x2,y2) in a way we can use later)

c <- constellations[which(constellations$Short == constellations$Short[1]),]
c <- data.frame(t(c[,-c(1,2)]))
c <- data.frame(hip = c[which(is.na(c) == FALSE),])
for(i in seq(1, (dim(c)[1]-1), by = 1)){
  c$P1[i] <- stars[which(stars$hip == c$hip[i]), 'hip']
  c$x1[i] <- stars[which(stars$hip == c$hip[i]), 'ra']
  c$y1[i] <- stars[which(stars$hip == c$hip[i]), 'dec']
  c$P2[i] <- stars[which(stars$hip == c$hip[i+1]), 'hip']
  c$x2[i] <- stars[which(stars$hip == c$hip[i+1]), 'ra']
  c$y2[i] <- stars[which(stars$hip == c$hip[i+1]), 'dec']
}
# The loop above creates rows for every pair along the sequence
# P1&P2, P2&P3, P3&P4,...
# We only want
# P1&P2, P3&P4, P4&P5,...
c <- c[seq(1, (dim(c)[1]-1), by = 2),]
c$Short <- constellations$Short[1]

# -- -- -- -- -- -- 
# Create a function mimicing above for all constellations of interest
sticks <- function(short){
  c1 <- constellations[which(constellations$Short == short),]
  c1 <- data.frame(t(c1[,-c(1,2)]))
  c1 <- data.frame(hip = c1[which(is.na(c1) == FALSE),])
  for(i in seq(1, (dim(c1)[1]-1), by = 1)){
    c1$P1[i] <- stars[which(stars$hip == c1$hip[i]), 'hip']
    c1$x1[i] <- stars[which(stars$hip == c1$hip[i]), 'ra']
    c1$y1[i] <- stars[which(stars$hip == c1$hip[i]), 'dec']
    c1$P2[i] <- stars[which(stars$hip == c1$hip[i+1]), 'hip']
    c1$x2[i] <- stars[which(stars$hip == c1$hip[i+1]), 'ra']
    c1$y2[i] <- stars[which(stars$hip == c1$hip[i+1]), 'dec']
  }
  c1 <- c1[seq(1, (dim(c1)[1]-1), by = 2),]
  c1$Short <- short
  return(c1)
}

# -- -- -- -- -- -- 
# Bind all constellations "sticks" to single data.frame
for(i in 2:dim(constellations)[1]){
  c <- rbind(c, sticks(constellations$Short[i]))
}

# -- -- -- -- -- -- 
# Filter for Northern Hemisphere constellations only
# Pegasus & Pisces are manipulated below since these constellations "cross right-ascension = 0" 
# rather than "going the long way round" assumed in plotting
# (since this was only two segments I manually added connecting lines from both directions 
# meeting at x = 0 rather than explore a more sophisticated method)

c.n <- c[which(c$Short %in% c('And', 'Ari', 'Cas', 'Ori', 'Per', 
                              #'Psc', issue with lines crossing x = 0, manual psc binded
                              'Tau', 'Tri', 'Aur',
                              'Cam', 'Cnc', 'CMi', 'Gem', 'Leo', 'LMi', 'Lyn', 'Mon', 'UMa',
                              'Boo', 'CVn', 'Com', 'Dra', 'Her', 'Ser', 'UMi', 'Aql',
                              'Cep', 'Cyg', 'Del', 'Equ', 'Lac', 'Lyr', 
                              #'Peg', issue with lines crossing x = 0, manual peg binded
                              'Sge', 'Vul')),]
c.s <- c[which(!c$Short %in% c('And', 'Ari', 'Cas', 'Ori', 'Per', 
                              #'Psc', issue with lines crossing x = 0, manual psc binded
                              'Tau', 'Tri', 'Aur',
                              'Cam', 'Cnc', 'CMi', 'Gem', 'Leo', 'LMi', 'Lyn', 'Mon', 'UMa',
                              'Boo', 'CVn', 'Com', 'Dra', 'Her', 'Ser', 'UMi', 'Aql',
                              'Cep', 'Cyg', 'Del', 'Equ', 'Lac', 'Lyr', 
                              #'Peg', issue with lines crossing x = 0, manual peg binded
                              'Sge', 'Vul')),]

# Pegasus: Join with new lines across x = 0 (one to (x1,y1)->(23.999, y2) & (0,y2)->(x2,y2))
peg.t <- c[which(c$Short == 'Peg'),]
peg.manual.lines <- data.frame('hip' = c(NA, NA, NA, NA),
                               'P1' = c(NA, NA, NA, NA),
                               'x1' = c(23.079348, 0, 23.062901, 0), 
                               'y1' = c(15.205264, 15.183596, 28.082789,29.090432), 
                               'P2' = c(NA, NA, NA, NA),
                               'x2' = c(23.9999, 0.220598, 23.9999, 0.139791),
                               'y2' = c(15.183596, 15.183596, 29.090432, 29.090432),
                               'Short' = c('Peg', 'Peg', 'Peg', 'Peg'))

peg.t <- rbind(peg.t, peg.manual.lines)
peg.t <- peg.t[-c(1,12),]

c.n <- rbind(c.n, peg.t)

# Pisces: Join with new lines across x = 0 (one to (x1,y1)->(23.999, y2) & (0,y2)->(x2,y2))
psc.t <- c[which(c$Short == 'Psc'),]
psc.manual.lines <- data.frame('hip' = c(NA, NA),
                               'P1' = c(NA, NA),
                               'x1' = c(23.988525, 0), 
                               'y1' = c(6.863321, 8.190271), 
                               'P2' = c(NA, NA),
                               'x2' = c(23.9999, 0.343295),
                               'y2' = c(8.190271, 8.190271),
                               'Short' = c('Psc', 'Psc'))

psc.t <- rbind(psc.t, psc.manual.lines)
psc.t <- psc.t[-c(13),]

c.n <- rbind(c.n, psc.t)

# -- -- -- -- -- -- 

# Read in the long names (also found in the skycultures folders on Stellarium's github)
long.names <- read.csv('data/cNames.csv', header = TRUE)
c.n <- merge(c.n, long.names, by = 'Short')
# Store labels and calculate suitable coordinates to put them
labels <- data.frame(c.n %>% group_by(Long) %>% summarise(l.x = mean(x1), l.y = mean(-y1)))
# Manual labels for Pegasus & Pisces are added within ggplot
labels <- labels[-which(labels$Long %in% c('Pegasus', 'Pisces')),]

# -- -- -- -- -- -- 
# Plotting

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) FALSE)
}

bg <- 'grey5'
txt <- 'grey60'

# Northern Hemisphere 1296x1296
ggplot() + 
  geom_point(data = stars.n, aes(x = ra, y = -dec,
                                 alpha = -1*mag, size = -1*mag), col = '#FFFFFF', pch = 18) +
  geom_segment(data = c.n,
               aes(x = x1, y = -y1, xend = x2, yend = -y2), col = '#e5c100', alpha = .7) +
  geom_text(data = labels, aes(x = l.x, y = l.y, label = Long), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 6) +
  # Manual labels for Pegasus & Pisces
  geom_text(aes(x = 23.988525, y = -6.863321, label = 'Pisces'), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 5) +
  geom_text(aes(x = 23.079348, y = -15.205264, label = 'Pegasus'), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 5) +
  scale_x_continuous(breaks = c(0:24), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, -90, by = -10), expand = c(0,0)) +
  scale_size_continuous(range = c(.1, 5)) +
  scale_alpha_continuous(range = c(0.05, .9)) +
  theme(panel.grid.major = element_line(colour = 'grey40'),
        panel.grid.minor = element_line(colour = 'grey40'),
        plot.background = element_rect(fill = bg, colour = bg),
        panel.background = element_rect(fill = bg, colour = bg),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_radar(start = pi)


# With Seasonal legend
# https://in-the-sky.org/data/ra_season.php

txt <- 'grey70'
#library(extrafont)
#font_import()

ggplot() + 
  geom_point(data = stars.n, aes(x = ra, y = -dec, alpha = -1*mag, size = -1*mag, colour = ra), pch = 18) +
  geom_segment(data = c.n,
               aes(x = x1, y = -y1, xend = x2, yend = -y2), col = '#FFFFFF', alpha = .6) +
  geom_text(data = labels, aes(x = l.x, y = l.y, label = Long), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 6, family = 'Andale Mono') +
  # Manual labels for Pegasus & Pisces
  geom_text(aes(x = 23.988525, y = -6.863321, label = 'Pisces'), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 5, family = 'Andale Mono') +
  geom_text(aes(x = 23.079348, y = -15.205264, label = 'Pegasus', family = 'Andale Mono'), 
            col = txt, alpha = .8, size = 3.5, nudge_y = 5) +
  scale_x_continuous(breaks = c(0:24), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, -90, by = -10), expand = c(0,0)) +
  scale_size_continuous(range = c(1, 8)) +
  scale_alpha_continuous(range = c(0.05, .9)) +
  scale_colour_gradientn(colours = c('#D89800',
                                     '#7FDFF7',
                                     '#4AF26F',
                                     '#FFFF00',
                                     '#D89800'),
                         breaks = c(6, 12, 18),
                         labels = c('Winter', 'Spring', 'Summer')) +
  theme(panel.grid.major = element_line(colour = 'grey40'),
        panel.grid.minor = element_line(colour = 'grey40'),
        plot.background = element_rect(fill = bg, colour = bg),
        panel.background = element_rect(fill = bg, colour = bg),
        legend.position = 'top',
        legend.text = element_text(colour = 'grey90', size = 14, family = 'Andale Mono'),
        legend.title = element_text(colour = 'grey90', size = 16, family = 'Andale Mono'),
        legend.background = element_rect(fill = bg),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  coord_radar(start = pi) +
  guides(colour = guide_colorbar(title ='Most Visible in', 
                                 title.position = 'top', title.hjust = 0.5,
                                 barwidth = 18, barheight = 1),
         alpha = FALSE, size = FALSE) 

## southern
## has some 0-crossing constellations which need repairing
ggplot() + 
  geom_segment(data = c.s,
               aes(x = x1, y = -y1, xend = x2, yend = -y2), col = '#e5c100', alpha = .7) +
  coord_polar(start = pi, clip = "on")
