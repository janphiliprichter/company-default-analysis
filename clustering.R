# Loading libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(ggpubr)
library(factoextra)
library(scales)
library(cluster)
library(fpc)


# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical = TRUE,
                 stringsAsFactors = TRUE)


# Setting random seed
set.seed(42)


# Numerical data for dimensionality reduction
data_num <- data[, c("log_age", "log_revenues", 
                     "log_profits", "sqrt_gmr", "last_statement_age", 
                     "core_income_ratio", "cash_asset_ratio", 
                     "consolidated_liabilities_ratio", "month", "month_day")]



#### Principal Component Analysis ####


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
           fill = "#440154",
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



### DBSCAN Clustering ###

dbscan_cl <- dbscan(data[,c("pc1", "pc2")], eps = 0.335, MinPts = 5)
table(dbscan_cl$cluster) 

# Adding cluster variable to the data set
data$dbscan_cluster <- as.factor(dbscan_cl$cluster)


# Relevelling to plot the histograms in the desired order
levels(data$dbscan_cluster) <- c( "No cluster", "C1", "C2", "C3")


# Scatterplot of the clusters
ggplot(data = data, 
       mapping = aes(x = pc1,
                     y = pc2,
                     color = dbscan_cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("black", "#21918c", "#d1af06", "#440154")) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "DBSCAN-Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4)))



### k-means & k-medoids clustering ###

# Elbow plot
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
k_means <- kmeans(x = data[,c("pc1", "pc2")], 
                  centers = 3, 
                  nstart = 1000)

# Adding cluster variable to the data set
data$kmean_cluster <- as.factor(k_means$cluster)

# Relevelling to plot the histograms in the desired order
levels(data$kmean_cluster) <- c("C2", "C3", "C1")
data$kmean_cluster <- relevel(data$kmean_cluster, ref = 3)


# k-medoids algorithm
k_medoids <- pam(x = data[,c("pc1", "pc2")], 
                 metric = "euclidean",
                 k = 3)

# Adding cluster variable to the data set
data$kmedoids_cluster <- as.factor(k_medoids$cluster)


# Relevelling to plot the histograms in the desired order
levels(data$kmedoids_cluster) <- c("C3", "C2", "C1")
data$kmedoids_cluster <- relevel(data$kmedoids_cluster, ref = 3)


# Scatterplot of the clusters
plot1 <- ggplot(data = data, 
       mapping = aes(x = pc1,
                     y = pc2,
                     color = kmean_cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#440154", "#21918c", "#d1af06")) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "3-Means-Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4)))


plot2 <- ggplot(data = data, 
       mapping = aes(x = pc1,
                     y = pc2,
                     color = kmedoids_cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#d1af06", "#440154", "#21918c")) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "3-Medoids-Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

grid.arrange(plot1, plot2, ncol=2)

d1af06
# Histogram for the different clusters
ggplot(data = data,
       mapping = aes(x = log_revenues, 
                     fill = kmean_cluster,
                     colour = kmean_cluster)) +
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
  labs(x = "log(Revenues)",
       y = "Density",
       title = "Histogram and KDE for each Cluster") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank())


# Parallel Coordinates
ggparcoord(data = data,
           columns = c(6, 10, 11, 12, 13, 14, 21),
           alphaLines = 0.1,
           groupColumn = "kmean_cluster") + 
  theme_minimal() +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "",
       y = "",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank()) +
  ylim(-2, 5)



#### Multidimensional Scaling ####

dism = dist(scale(data_num), 
            method = "manhattan", 
            diag = TRUE, 
            upper = TRUE)

mds <- cmdscale(dism, 
                k = 2, 
                eig = TRUE)

data$mds_d1 <- mds$points[, 1]
data$mds_d2 <- mds$points[, 2]


# k-means algorithm
k_means_mds <- kmeans(x = data[,c("mds_d1", "mds_d2")], 
                  centers = 3, 
                  nstart = 1000)

# Adding cluster variable to the data set
data$kmean_mds_cluster <- as.factor(k_means_mds$cluster)

# Relevelling to plot the histograms in the desired order
levels(data$kmean_mds_cluster) <- c("C2", "C3", "C1")
data$kmean_mds_cluster <- relevel(data$kmean_mds_cluster, ref = 3)



ggplot(data = data, 
       mapping = aes(x = mds_d1,
                     y = mds_d2,
                     color = kmean_mds_cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#440154", "#21918c", "#d1af06")) +
  theme_minimal() +
  labs(x = "Dim 1",
       y = "Dim 2",
       title = "3-Means-Clustering") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4)))




# Histogram for the different clusters
ggplot(data = data,
       mapping = aes(x = log_revenues, 
                     fill = kmean_mds_cluster,
                     colour = kmean_mds_cluster)) +
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
  labs(x = "log(Revenues)",
       y = "Density",
       title = "Histogram and KDE for each Cluster") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank())


# Parallel Coordinates
ggparcoord(data = data,
           columns = c(6, 10, 11, 12, 13, 14, 21),
           alphaLines = 0.1,
           groupColumn = "kmean_mds_cluster") + 
  theme_minimal() +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "",
       y = "",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank()) +
  ylim(-2, 5)


