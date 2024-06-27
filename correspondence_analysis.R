# Loading libraries
library(ca)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(dplyr)
library(fpc)

# Setting random seed
set.seed(42)

# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical = TRUE,
                 stringsAsFactors = TRUE)


# Changing quantitative variables to factors
data$score1 <- as.factor(data$score1)
data$default <- as.factor(data$default)


## Frequency Tables & Chi-square test of independence 

# Frequency table for geo_area and industry_sector
geo_industry <- table(data$geo_area, data$industry_sector)
chisq.test(geo_industry)

# Frequency table for geo_area and juridical_form
geo_juridical <- table(data$geo_area, data$juridical_form)
chisq.test(geo_juridical)

# Frequency table for juridical_form and industry_sector
juridical_industry <- table(data$juridical_form, data$industry_sector)
chisq.test(juridical_industry)

# Frequency table for geo_area and score1
geo_score1 <- table(data$geo_area, data$score1)
chisq.test(geo_industry)

# Frequency table for industry_sector and score1
industry_score1 <- table(data$industry_sector, data$score1)
chisq.test(geo_industry)

# Frequency table for juridical_form and score1
juridical_score1 <- table(data$juridical_form, data$score1)
chisq.test(geo_industry)



### Correspondence Analysis

## geo_industry
ca_geo_industry <- CA(geo_industry, 
                      graph = FALSE)

# Variance explained 
var_exp <- ca_geo_industry$eig[,2]

ggplot(data.frame(var_exp)) + 
  geom_bar(mapping = aes(x = 1:length(var_exp),
                         y = var_exp),
           fill = "#21918c",
           stat = "identity") +
  geom_line(mapping = aes(x = 1:length(var_exp), 
                          y = cumsum(var_exp)),
            stat = "identity") +
  geom_point(mapping = aes(x = 1:length(var_exp), 
                           y = cumsum(var_exp)),
             stat = "identity") + 
  scale_x_continuous(n.breaks=4) +
  ylim(0,101) +
  theme_minimal() +
  labs(x = "Principal Components",
       y = "Variance Explained (%)",
       title = "PCA Variance Explained") +
  theme(plot.title = element_text(hjust = 0.5)) 


# Bi-plot without energy
fviz_ca_biplot(ca_geo_industry, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.2, 0.6) + 
  xlim(-0.6, 1.2)

# Bi-plot with energy
fviz_ca_biplot(ca_geo_industry, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))



## geo_juridical
ca_geo_juridical <- CA(geo_juridical, 
                       graph = FALSE)

# Bi-plot 
fviz_ca_biplot(ca_geo_juridical, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))


## juridical_industry
ca_juridical_industry <- CA(juridical_industry, 
                       graph = FALSE)

# Bi-plot 
fviz_ca_biplot(ca_juridical_industry, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))


## geo_score1
ca_geo_score1 <- CA(geo_score1, 
                            graph = FALSE)

# Bi-plot 
fviz_ca_biplot(ca_geo_score1, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))


## industry_score1
ca_industry_score1 <- CA(industry_score1, 
                    graph = FALSE)

# Bi-plot 
fviz_ca_biplot(ca_industry_score1, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Industry Sector vs External Score") +
  theme(plot.title = element_text(hjust = 0.5))


## juridical_score1
ca_juridical_score1 <- CA(juridical_score1, 
                         graph = FALSE)

# Bi-plot 
fviz_ca_biplot(ca_juridical_score1, 
               repel = TRUE,
               pointsize = 3,
               labelsize = 4,
               col.col = "#21918c",
               col.row = "#440154",
               shape.row = 15, 
               shape.col = 19) +
  theme_minimal() +
  labs(title = "Correspondence Analysis - Geographic Area vs Industry Sector") +
  theme(plot.title = element_text(hjust = 0.5))



## Multiple Correspondence Analysis

# Selecting the variables for the MCA
mca_data <- data[, c("geo_area", "industry_sector", "default")]

# MCA
mca_res <- MCA(mca_data,
               graph = FALSE)

# Bi-plot
plot_data <- mca_res$var$coord[,1:2]
plot_data <- as.data.frame(plot_data)

plot_data$category <- c(rep("geo_area", 5), 
                        rep("industry_sector", 20), 
                        rep("default", 2))

plot_data <- plot_data %>% 
  dplyr::rename("dim1" = "Dim 1",
               "dim2" = "Dim 2")


ggplot(plot_data, aes(x=dim1, 
                      y=dim2, 
                      col=category, 
                      shape=category)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(size=3,
             show.legend = FALSE) + 
  geom_text_repel(label=rownames(plot_data),
                  box.padding = 0.3) +
  theme_minimal() +
  scale_shape_manual(values=c(17, 15, 19)) + 
  scale_color_manual(values=c("#dbca27","#21918c", "#440154")) +
  labs(title = "MCA - Geographic Area vs Industry Sector vs Default",
       x = "Dim 1 (5.00%)",
       y = "Dim 2 (4.77%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")





## MCA with province

# Selecting the variables for the MCA
mca2_data <- data[, c("province", "industry_sector", "default", "score1")]

# MCA
mca2_res <- MCA(mca2_data,
               graph = FALSE)

# Bi-plot
plot2_data <- mca2_res$var$coord[,1:2]
plot2_data <- as.data.frame(plot2_data)

plot2_data$category <- c(rep("province", 106), 
                        rep("industry_sector", 20), 
                        rep("default", 2,),
                        rep("score1", 9))

plot2_data <- plot2_data %>% 
  dplyr::rename("dim1" = "Dim 1",
                "dim2" = "Dim 2")


ggplot(plot2_data, aes(x=dim1, 
                      y=dim2, 
                      col=category, 
                      shape=category)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(size=3,
             show.legend = FALSE) + 
  geom_text_repel(label=rownames(plot2_data),
                  box.padding = 0.3) +
  theme_minimal() +
  scale_shape_manual(values=c(17, 15, 19, 18)) + 
  scale_color_manual(values=c("#dbca27","#21918c", "#440154", "#31688e")) +
  labs(title = "MCA - Province, Industry Sector, Default & Score",
       x = "Dim 1 (1.28%)",
       y = "Dim 2 (1.20%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") 

# Zoomed in version
ggplot(plot2_data, aes(x=dim1, 
                       y=dim2, 
                       col=category, 
                       shape=category)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(size = 3,
             show.legend = FALSE,
             alpha=0.9) +
  theme_minimal() +
  scale_shape_manual(values=c(17, 15, 19, 18)) + 
  scale_color_manual(values=c("#dbca27","#21918c", "#440154", "#31688e")) +
  labs(title = "MCA - Province, Industry Sector, Default & Score",
       x = "Dim 1 (1.28%)",
       y = "Dim 2 (1.20%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  xlim(-1.75, 1.6) +
  ylim(-2, 1.5)


## DBSCAN Clustering

dbscan_res <- dbscan(plot2_data[, c("dim1", "dim2")],
                     eps = 0.2,
                     MinPts = 3)

plot2_data$cluster <- as.factor(dbscan_res$cluster)

cluster_data <- plot2_data[plot2_data$cluster != 0,]

# Scatterplot
ggplot(NULL, aes(x=dim1, 
                       y=dim2, 
                       col=cluster)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(data=cluster_data,
             size=3.5,
             show.legend = FALSE,
             alpha=0.9) + 
  scale_color_manual(values=c("#dbca27","#21918c", "#440154", "#414487", "#7ad151", "#22a884", "black")) +
  geom_point(data = plot2_data[plot2_data$cluster == 0,],
             alpha = 0.7) +
  theme_minimal() +
  labs(title = "DBSCAN Clusters",
       x = "Dim 1 (1.28%)",
       y = "Dim 2 (1.20%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  xlim(-1.75, 1.6) +
  ylim(-2, 1.5)
