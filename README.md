# 22degrees
Attempts to investigate moon halo star counts vs rain events in R

# Motivation

Based entirely on [this tweet](https://twitter.com/carroll_jono/status/1067623022008315904):

"In Kamilaroi traditions (Australia), the no. of stars inside a Moon halo tells you how many days til rain comes. In Tanana traditions (Alaska), the no. of stars in halo is number of days before the weather will change. #IndigenousScience"

Can we produce a data visualisation which attempts to address this? Maybe some code which can count the stars within [22degrees of the moon](https://en.wikipedia.org/wiki/22%C2%B0_halo) at various times of the year and compare it to historical rainfall data?

I'm gathering interested people to work through this together as a collaborative project.

# Resources

  - [bomrang](https://github.com/ropensci/bomrang) for historical Australian weather (rain) data
  - Locations of stars, e.g. https://github.com/astronexus/HYG-Database
  - Converstions from RA/DEC to ALT/AZ, e.g. https://cran.r-project.org/package=astrolibR ?
  - Locations of the moon at various times, e.g. https://cran.r-project.org/package=astrolibR `moonpos`?
  - A way to plot stars, e.g. https://twitter.com/5amVintage/status/1052683184847147009
  - A way to count stars within a boundary (R)
  - A way to visualize each of these things over time (gganimate)
