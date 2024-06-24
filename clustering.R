# Loading libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(factoextra)
library(scales)


# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical=TRUE,
                 stringsAsFactors=TRUE)


# Setting random seed
set.seed(42)


### Principal Component Analysis ###
data_num <- data[, c("score1", "score2", "log_age", "log_revenues", 
                     "log_profits", "sqrt_gmr", "last_statement_age", 
                     "core_income_ratio", "cash_asset_ratio", 
                     "consolidated_liabilities_ratio", "month","month_day")]


# Singular value decomposition
svd <- prcomp(data_num, 
              center = TRUE, 
              scale = TRUE)

# SVD Summary
summary(svd)


## Variance Analysis

# Variance of the principal components
pc_var <- svd$sdev ** 2

# Percentage of variance explained by the principal components
var_exp <- pc_var / sum(pc_var) * 100

# PC Scree Plot 
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
  scale_x_continuous(n.breaks=10) +
  ylim(0,101) +
  theme_minimal() +
  labs(x = "Principal Components",
       y = "Variance Explained (%)",
       title = "PCA Variance Explained") +
  theme(plot.title = element_text(hjust = 0.5)) 


# Contribution of each variable
fviz_pca_var(svd,
             legend.title = "Contribution (%)",
             col.var = "contrib",
             gradient.cols = viridis_pal()(30),
             repel = TRUE) + 
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "Correlation Circle") +
  theme(plot.title = element_text(hjust = 0.5))


## Save the first 2 PC's for further analysis
data$pc1 <- svd$x[,"PC1"]
data$pc2 <- svd$x[,"PC2"]


# Elbow plot for k-means
fviz_nbclust(data[,c("pc1", "pc2")], kmeans, 
             method = "wss",
             linecolor = "#21918c") + 
  labs(x = "Number of Clusters",
       y = "Total Within Sum of Squares",
       title = "Optimal Number of Clusters for K-Means") +
  geom_vline(xintercept = 3, linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# k-means algorithm
km <- kmeans(x = data[,c("pc1", "pc2")], 
             centers = 3, 
             nstart = 1000)

# Adding cluster variable to the data set
data$cluster <- as.factor(km$cluster)
data$cluster
# Relevelling to plot the histograms in the desired order
levels(data$cluster) <- c("C2", "C3", "C1")
data$cluster <- relevel(data$cluster, ref = 3)


# Scatter-plot of the clusters
ggplot(data = data, 
       mapping = aes(x = pc1,
                     y = pc2,
                     color = cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "3-Means-Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4)))


# Histogram for the different clusters
ggplot(data = data,
       mapping = aes(x = log_revenues, 
                     fill = cluster,
                     colour = cluster)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 position="identity",
                 bins = 20,
                 alpha = 0.5) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               alpha = 0,
               linewidth = 0.8) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  theme_minimal() +
  labs(x = "Number of rented Bikes",
       y = "Density",
       title = "Histogram and KDE for each Cluster") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank())


# Parallel Coordinates
ggparcoord(data = data,
           columns = c(6, 10, 11, 12, 13, 14, 21),
           alphaLines = 0.1,
           groupColumn = "cluster") + 
  theme_minimal() +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "",
       y = "",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank()) +
  ylim(-2, 5)

colnames(data)

library(fpc)

Dbscan_cl <- dbscan(data[,c("pc1", "pc2")], eps = 0.5, MinPts = 5)
Dbscan_cl$cluster
table(Dbscan_cl$cluster) 


plot(Dbscan_cl, as.matrix(data[,c("pc1", "pc2")]), main = "DBScan")
pairs(data[,c("pc1", "pc2")], col = Dbscan_cl$cluster + 1L)
fviz_cluster(Dbscan_cl, data = as.matrix(data[,c("pc1", "pc2")]), geom = "point")
