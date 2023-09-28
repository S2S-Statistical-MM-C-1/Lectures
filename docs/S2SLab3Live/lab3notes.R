Reading in data
  .txt files
  .csv file

Working with data frames - change order? Filtering/subsetting dataframes first then summarising information then adding variables/merging
  changing type of variables
  removing NAs
  creating new variables
  sorting data frames
  merging
  logical operators on data frames (?)
  tables
  summary functions (e.g. tapply() and aggregate())
  
Functions
  probability
  flow control - look at Craigs lab to introduce for loops
  creating functions
  
Exercises
  1. Read in data and manipulate the data frame
  2. Create own function and apply it to something
  
  
  
  
  
  
  
weight <- round(c(rnorm(2, 72.7, 14), rnorm(1, 86.7, 14) , rnorm(3, 72.7, 14), 
                   rnorm(2, 86.7, 14), rnorm(5, 72.7, 14)), 2)
weight <- as.data.frame(weight)

height <- round(c(rnorm(2, 1.64, 0.12), rnorm(1, 1.78, 0.12) , rnorm(3, 1.64, 0.12), 
                   rnorm(2, 1.78, 0.12), rnorm(5, 1.64, 0.12)), 2)
height <- as.data.frame(height)

handw <- cbind(weight, height)

write.csv(handw, file="measurements.csv", row.names=FALSE)  

subject <- data.frame(ldl = 148, hdl = 78, trig = 120, age = 41, gender = "male", 
                      smoke = "current", weight = 84.05, height = 1.79)  

#make treatment with factor treatment/placebo for some existing patients from chol and some new patients
#add patient IDs
treatment <- data.frame(patient_id = c("P103", "P472", "P901", "P063", "P234", "P843", "P104", "P332", "P951",
                               "P215", "P613", "P461", "P086", "P117"),
                        treatment = c("Treatment", "Placebo", "Placebo", "Treatment", "Treatment",
                                      "Treatment", "Treatment", "Placebo", "Treatment", "Placebo",
                                      "Placebo", "Treatment", "Treatment", "Placebo"))

ids <- sample(c("P103", "P725", "P901", "P063", "P753", "P843", "P004", "P332", "P951",
         "P215", "P613", "P461", "P912", "P117"), replace=FALSE, size=14)

chol$id <- ids[1:13]
chol <- chol[, c(7, 1:6)]

write.table(chol, file ="chol.txt", row.names=FALSE)

write.csv(treatment, file = "treatment.csv", row.names=FALSE)  


str(cars)


x <- seq(-3.5, 3.5, length=100)
y <- dnorm(x)
normal <- data.frame(x=x, y=y)

library(scales)

pnorm_plot <-ggplot(normal, aes(x=x, y=y, ymin=0, ymax=y, xmax=1.8))+
  geom_line()+
  geom_area(aes(x=stage(x, after_stat=oob_censor(x, c(-3.5, 1.1))),
                alpha=0.7), show.legend=FALSE, fill="cadetblue")+
  annotate("segment", x=1.1, xend=1.1, y=-0.01, yend=dnorm(1.1), col="cadetblue4", alpha=0.96)+
  annotate("text", label="pnorm(q)", x=-0.14, y=0.14, col="darkslategrey",
           family="mono")+
  annotate("text", label="q", x=1.1, y=-0.036, col="darkslategrey", family="mono")+
  coord_cartesian(clip='off', ylim=c(0, 0.41), xlim=c(-3.6, 3.6))+
  scale_y_continuous(expand=expansion())+
  theme_classic()+
  theme(axis.title=element_blank(),
        plot.background=element_blank(),
        panel.grid.major=element_line(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(1,1,1.6,1), "lines"))

qnorm_plot <- ggplot(normal, aes(x=x, y=y, ymin=0, ymax=y))+
  geom_line()+
  geom_area(aes(x=stage(x, after_stat=oob_censor(x, c(-3.5, -0.8))),
                alpha=0.7), fill="seagreen", show.legend=FALSE)+
  annotate("segment", x=-0.8, xend=-0.8, y=-0.03, yend=dnorm(-0.8), col="seagreen", alpha=0.96)+
  annotate("text", label="p", x=-1.3, y=0.08, col="darkslategrey",
           family="mono")+
  annotate("text", label="qnorm(p)", x=-0.8, y=-0.07, col="darkslategrey", 
           family="mono")+
  coord_cartesian(clip='off', ylim=c(0, 0.41), xlim=c(-3.6, 3.6))+
  scale_y_continuous(expand=expansion())+
  theme_classic()+
  theme(axis.title=element_blank(),
        plot.background=element_blank(),
        panel.grid.major=element_line(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(1,1,1.6,1), "lines"))

dnorm_plot <- ggplot(normal, aes(x=x, y=y, ymin=0, ymax=y))+
  geom_line()+
  annotate("segment", x=1, xend=1, y=-0.01, yend=dnorm(1), linetype=2, col="darkslategrey")+
  annotate("pointrange", xmin=-3.9, x=1, xmax=1, y=dnorm(1), linetype=2, size=0.2,
           col="darkslategrey")+
  annotate("text", label="x", x=1, y=-0.036, col="darkslategrey", family="mono")+
  annotate("text", label="dnorm(x)", x=-2.7, y=0.27, family="mono")+
  coord_cartesian(ylim=c(0, 0.41), xlim=c(-3.6, 3.6), clip='off')+
  scale_y_continuous(expand=expansion())+
  theme_classic()+
  theme(axis.title=element_blank(),
        plot.background=element_blank(),
        panel.grid.major=element_line(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(1,1,1.6,1), "lines"))

library(gridExtra)
library(svglite)
svglite("Images/normal_plots.svg", width=10, height=2)
grid.arrange(pnorm_plot, qnorm_plot, dnorm_plot, ncol=3)
dev.off()

#prob function table
dist <- c("Normal", "Binomial", "Exponential", "Geometric", "Hypergeometric", "Negative Binomial",
          "Poisson", "Student's t", "Uniform", "Chi-square")
func_names <- c("`norm`", "`binom`", "`exp`", "`geom`", "`hyper`", "`nbinom`", "`pois`", "`t`", 
                "`unif`", "`chisq`")
args <- c("`mean = 0`, `sd = 0`", "`size =`, `prob =`", "`rate =`", "`prob =`", "`m =`, `n =`, `k =`",
          "`size =`, `prob =`, `mu =`", "`lambda =`", "`df =`", "`min = 0`, `max = 1`",
          "`df =`")
dist_table <- data.frame(Distribution = dist, `R Name` = func_names, Arguments = args)




