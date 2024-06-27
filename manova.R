# Loading libraries
library(MASS)
library(MVN)
library(mvtnorm)
library(onewaytests)
library(heplots)
library(ggplot2)
library(gridExtra)
library(ggpointdensity)
library(viridis)

# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical = TRUE,
                 stringsAsFactors = TRUE)


# Subset of data where each industry sector appears > 50 times
industries <- names(table(data$industry_sector))
industry_table <- table(data$industry_sector)
industry_data <- data[data$industry_sector %in% 
                        industries[industry_table > 50],]
industry_data$industry_sector <- factor(industry_data$industry_sector)

### MANOVA Assumptions ###

# 1. Observations are randomly and independently sampled
# 2. Variables follow a multivariate normal distribution
# 3. Covariance matrices are equal for each group


### Normality Tests ###

## Univariate Normality

# log_revenues
shapiro.test(data$log_revenues)

# log_revenues for geo_area groups
onewaytests::nor.test(log_revenues ~ geo_area, 
                      data=data, 
                      plot=NULL)

# log_revenues for industry_sector groups
onewaytests::nor.test(log_revenues ~ industry_sector, 
                      data=industry_data, 
                      plot=NULL)


# log_profits
shapiro.test(data$log_profits)

# log_profits for geo_area groups
onewaytests::nor.test(log_profits ~ geo_area, 
                      data=data, 
                      plot=NULL)

# log_profits for industry_sector groups
onewaytests::nor.test(log_profits ~ industry_sector, 
                      data=industry_data, 
                      plot=NULL)


## Multivariate Normality

# Mardia's test
mvn(data[, c("log_revenues", "log_profits")], 
    mvnTest="mardia")

# Henze-Zirkler's test 
mvn(data[, c("log_revenues", "log_profits")], 
    mvnTest="hz")

# Royston's test 
mvn(data[, c("log_revenues", "log_profits")], 
    mvnTest="royston")

# Doornik-Hansen's test 
mvn(data[, c("log_revenues", "log_profits")], 
    mvnTest="dh")

# Henze-Zirkler's test for each geo_area group
tapply(X=data[, c("log_revenues", "log_profits")], 
       INDEX=data$geo_area, 
       FUN=mvn)

# Henze-Zirkler's test for each industry_sector group
tapply(X=industry_data[, c("log_revenues", "log_profits")], 
       INDEX=industry_data$industry_sector, 
       FUN=mvn)



### Homogeneity of Covariances ###

#Box's M-test for geo_area
heplots::boxM(data[, c("log_revenues", "log_profits")], 
              data$geo_area)


#Box's M-test for industry_sector
heplots::boxM(industry_data[, c("log_revenues", "log_profits")], 
              industry_data$industry_sector)



### One-way ANOVA ###

# geo_area
summary(aov(log_revenues ~ geo_area, data=data))
summary(aov(log_profits ~ geo_area, data=data))

# industry_sector
summary(aov(log_revenues ~ industry_sector, data=data))
summary(aov(log_profits ~ industry_sector, data=data))


### MANOVA ###

## MANOVA for geo_area 
m1 <- manova(cbind(log_revenues, log_profits) ~ geo_area, data=data)
summary(m1, intercept = TRUE)
summary.aov(m1)


## MANOVA for industry_sector
m2 <- manova(cbind(log_revenues, log_profits) ~ industry_sector, data=data)
summary(m2, intercept = TRUE)
summary.aov(m2)



### Visualisations ###

## Boxplots

# Boxplots log_revenues by geo_area
ggplot(data, aes(x=geo_area, y=log_revenues)) +
  geom_boxplot(fill="#21918c", alpha=0.7) +
  theme_minimal(base_size=13) + 
  labs(x = "",
       y = "log(Revenues)",
       title = "Revenues by Geographic Area") +
  theme(plot.title = element_text(hjust = 0.5))


# Boxplots log_profits by geo_area
ggplot(data, aes(x=geo_area, y=log_profits))+
  geom_boxplot(fill="#3b528b", alpha=0.7) +
  theme_minimal(base_size=13) + 
  labs(x = "",
       y = "log(Profits)",
       title = "Profits by Geographic Area") +
  theme(plot.title = element_text(hjust = 0.5))



# Boxplots log_revenues by industry_sector
ggplot(industry_data, aes(x=industry_sector, y=log_revenues)) +
  geom_boxplot(fill="#21918c", alpha=0.7) +
  theme_minimal(base_size=13) + 
  labs(x = "",
       y = "log(Revenues)",
       title = "Revenues by Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))


# Boxplots log_profits by industry_sector
ggplot(industry_data, aes(x=industry_sector, y=log_profits))+
  geom_boxplot(fill="#3b528b", alpha=0.7) +
  theme_minimal(base_size=13) +
  labs(x = "",
       y = "log(Profits)",
       title = "Profits by Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))



## Histograms & QQ-Plots

# Histograms by geo_area
hist_plot <- 
  ggplot(data, 
         mapping=aes(x=log_revenues)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#3b528b", 
                 color = "#1b2640", 
                 alpha = 0.7, 
                 bins = 20) +
  stat_function(lwd=1,
                color="#440154",
                fun=dnorm, 
                args=list(mean=mean(data$log_revenues), 
                          sd=sd(data$log_revenues))) +
  xlim(c(2, 11)) +
  facet_grid(~data$geo_area, scales="free") +
  theme_minimal(base_size=13) + 
  labs(x = "log(Revenues)",
       y = "Density",
       title = "Histograms & QQ-Plots for log(Revenues)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 


# QQ-Plots by geo_area
qq_plot <-  
  ggplot(data=data, 
         mapping=aes(sample=log_revenues)) +
  stat_qq(alpha=0.4) +
  stat_qq_line() +
  ylim(c(2, 11.5)) +
  facet_grid(~data$geo_area, scales="free") +
  theme_minimal(base_size=13) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background=element_blank(),
        strip.text.x=element_blank()) 

# Grid plot
grid.arrange(hist_plot, qq_plot, nrow=2)



#### Multivariate Normal Plots

## 2D-KDE
ggplot(data, aes(x=log_revenues, y=log_profits)) +
  geom_density_2d_filled() +
  xlim(c(3.5,10.2)) +
  ylim(c(1.8, 9.2)) +
  theme_classic(base_size=13) +
  labs(x = "log(Revenues)",
       y = "log(Profits)",
       title = "Distribution log(Revenues) & log(Profits)") +
  theme(legend.position="none",
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5))


## 2D-KDE with bivariate normal contour lines

# Mean vector
mu <- c(mean(data$log_revenues), mean(data$log_profits))

# Covariance Matrix
sigma <- cov(cbind(data$log_revenues, data$log_profits))

# Contour line data
data_grid <- expand.grid(s1=seq(1, 11, length.out=200), 
                         s2=seq(1, 11, length.out=200))
q_sample <- cbind(data_grid, 
                  prob = mvtnorm::dmvnorm(data_grid, mean=mu, sigma=sigma))

# 2D-KDE and contour lines
ggplot(data, aes(x=log_revenues, y=log_profits)) +
  geom_density_2d_filled() +
  geom_contour(data=q_sample, 
               aes(x=s1, y=s2, z=prob),
               color="white",
               alpha=0.7,
               lwd=0.5) +
  xlim(c(3.5,10.2)) +
  ylim(c(1.8, 9.2)) +
  theme_classic(base_size=13) +
  labs(x = "log(Revenues)",
       y = "log(Profits)",
       title = "Distribution log(Revenues) & log(Profits)") +
  theme(legend.position="none",
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5))



## Point density plot with real data
scatter_real <- 
  ggplot(data, aes(x=log_revenues, y=log_profits)) +
  geom_pointdensity(size=3,
                    alpha=0.9) +
  scale_color_viridis() +
  xlim(c(3.5,10.2)) +
  ylim(c(1.8, 9.2)) +
  theme_minimal(base_size=13) +
  labs(x = "log(Revenues)",
       y = "log(Profits)",
       title = "log(Revenues) vs. log(Profits)") +
  theme(legend.position="none",,
        plot.title = element_text(hjust = 0.5))



## Point density plot with random data

# Random draws from a bivariate normal distribution
bvn_data <- mvrnorm(n=1500,
                    mu=mu,
                    Sigma=sigma)

scatter_bvn <- ggplot(mapping=aes(x=bvn_data[,1], y=bvn_data[,2])) +
  geom_pointdensity(size=3,
                    alpha=0.9) +
  scale_color_viridis() +
  xlim(c(3.5,10.2)) +
  ylim(c(1.8, 9.2)) +
  theme_minimal(base_size=13) +
  labs(x = "log(Revenues)",
       y = "log(Profits)",
       title = "Random Draws form a Bivariate Normal Distribution") +
  theme(legend.position="none",,
        plot.title = element_text(hjust = 0.5))

# Grid plot
grid.arrange(scatter_real, scatter_bvn, nrow=1)

