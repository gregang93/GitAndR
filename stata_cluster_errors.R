require(plm)
require(lmtest)  # for waldtest()
# get data and load as pdata.frame
url <- "http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt"
p.df <- read.table(url)
names(p.df) <- c("firmid", "year", "x", "y")
p.df <- pdata.frame(p.df, index = c("firmid", "year"))
head(p.df)


# fit same model with plm (needed for clustering)
pm1 <- plm(y ~ x, data = p.df, model = "pooling")

G <- length(unique(p.df$year))   #Number of clusters
N <- length(p.df$year)     # Number of observations
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
time_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "time", adjust = T)
coeftest(pm1, vcov = time_c_vcov)





setwd("C:\\Users\\Greg\\Desktop\\FYP_extra\\ST4242 Analysis of Longitudinal Data\\Data")
library(foreign)
tlc <- read.dta("tlc.dta")
tlclong <- reshape(tlc, idvar="id",
                   varying=c("y0","y1","y4","y6"),
                   v.names="y", timevar="time",time=1:4, direction="long")
tlclong$Treat = as.numeric(tlclong$trt == "Succimer")

p.df <- pdata.frame(tlclong, index = c("id", "time"))



# fit same model with plm (needed for clustering)
pm1 <- plm(y ~ Treat, data = p.df, model = "pooling")

G <- length(unique(p.df$id))   #Number of clusters
N <- length(p.df$id)     # Number of observations
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
time_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = time_c_vcov)
