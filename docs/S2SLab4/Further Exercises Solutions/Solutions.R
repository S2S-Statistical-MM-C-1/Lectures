library(PASWR2)
library(ggplot2)

#Vitoria apartments

data("VIT2005")

#Show the number of apartments in each level of "out". Which method is the best for displaying this information?

VIT2005$out <- factor(x = VIT2005$out, levels = c("E25", "E50", "E75", "E100"),
                      labels = c("25%", "50%", "75%", "100%"))

expo_tab <- table(VIT2005$out)

pie(x = expo_tab,
    col = c("slategray1", "slategray2", "slategray3", "slategray4"),
    main = "Exposure of Apartments in Vitoria")

barplot(height = expo_tab,
        col = c("slategray1", "slategray2", "slategray3", "slategray4"),
        main = "Exposure of Apartments in Vitoria",
        xlab = "Exposure to the Elements",
        ylab = "Frequency")

#new categorical variable
VIT2005$size <- cut(x = VIT2005$area,
                   breaks = c(50, 100, 150, 200),
                   labels = c("Small", "Medium", "Large"))

size_prop_tab <- prop.table(table(VIT2005$size, VIT2005$out), margin = 1)

barplot(height = t(size_prop_tab),
        beside = TRUE,
        legend.text = TRUE,
        col = c("slategray1", "slategray2", "slategray3", "slategray4"),
        main = "Exposure Levels in Small/Medium/Large Apartments",
        xlab = "Size",
        ylab = "Proportion")

#quantitative variable is totalprice - what is the shape of the distribution? Is this normal?

hist(x = VIT2005$totalprice,
     breaks = 10,
     freq = FALSE,
     col = "azure3",
     main = "Price of Apartments in Vitoria",
     xlab = "Price (€)")

price_dens <- density(VIT2005$totalprice)

lines(price_dens, lwd = 2, col = "darkslategray")

qqnorm(y = VIT2005$totalprice)
qqline(y = VIT2005$totalprice)

#compare ditributions of price between small/medium/large apartments
price_small <- subset(x = VIT2005,
                      subset = (size == "Small"),
                      select = totalprice)

price_medium <- subset(x = VIT2005,
                      subset = (size == "Medium"),
                      select = totalprice)

price_large <- subset(x = VIT2005,
                      subset = (size == "Large"),
                      select = totalprice)

par(mfrow = c(1, 3))

hist(x = price_small$totalprice,
     breaks = seq(from = 150000, to = 600000, by = 20000),
     col = "lightcyan2",
     ylim = c(0, 35),
     main = "Price of Small Apartments",
     xlab = "Price (€)")

hist(x = price_medium$totalprice,
     breaks = seq(from = 150000, to = 600000, by = 20000),
     col = "lightcyan3",
     ylim = c(0, 35),
     main = "Price of Medium Apartments",
     xlab = "Price (€)")

hist(x = price_large$totalprice,
     breaks = seq(from = 150000, to = 600000, by = 20000),
     col = "lightcyan4",
     ylim = c(0, 35),     
     main = "Price of Large Apartments",
     xlab = "Price (€)")

par(mfrow = c(1, 1))




