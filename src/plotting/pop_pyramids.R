# population pyramids

# script by Eva Lieungh
# started 2023-01-26

# data from INCLINE OSF
# Veronica alpina
# Sibbaldia procumbens



# basic template from here: https://www.geeksforgeeks.org/how-to-create-a-population-pyramid-in-r/
ggplot( df, aes(x = age, y = population)) +   geom_bar(stat = “identity”) + coord_flip()