MRMCapstone
===========

The goal of MRMCapstone is to create visualizations of earthquake data, in fulfillment of the Mastering Software Development in R Capstone project.

Example
-------

It's basic use is as follows:

``` r
cleanData <- read_tsv("signif.txt") %>% eq_clean_data()

cleanData %>%
  ggplot(aes(x = DATE, size=EQ_PRIMARY, fill = DEATHS, label = LOCATION_NAME)) +
  geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) +
  geom_timeline_label(nmax = 10, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) +
  guides(size = guide_legend(title = "Richter scale value"), fill = guide_colourbar(title = "# Deaths")) +
  theme_quakes()
```
