# Loading libraries
library(dplyr)
library(dendextend) 

# Reading the data set
data <- read.csv("preprocessed_data.csv",
                 tryLogical = TRUE,
                 stringsAsFactors = TRUE)


# Data for hierarchical clustering
hc_data <- data %>% 
  select(province, age, revenues, profits, 
         gross_margin_ratio, core_income_ratio, cash_asset_ratio, 
         consolidated_liabilities_ratio) %>% 
  group_by(province) %>% 
  filter(n() >= 10) %>% 
  summarise(across(everything(), median)) %>%
  as.data.frame()

row.names(hc_data) <- hc_data$province
hc_data <- hc_data[,-1]


# Defining function for mahalanobis distance matrix
mah <- function(x, cx = NULL) {
  if(is.null(cx)) cx <- cov(x)
  out <- lapply(1:nrow(x), function(i) {
    mahalanobis(x = x, 
                center = do.call("c", x[i, ]),
                cov = cx)
  })
  return(as.dist(do.call("rbind", out)))
}


# Mahalanobis distrance matrix
mahalanobis_distance <- mah(hc_data)
 


# Hierarchical clustering with Ward's method
hc1 <- hclust(mahalanobis_distance,
             method = "ward.D")
dend1 <- as.dendrogram(hc1)


# Dendrogram plot
par(mar=c(3,1,1,1))
dend1 %>%
  set("labels_cex", 1.1) %>%
  set("branches_lwd", 1.75) %>%
  set("labels_col", value = c("#7ad151","#22a884", "#414487", "#440154"), k=4) %>%
  set("branches_k_color", value = c("#7ad151","#22a884", "#414487", "#440154"), k = 4) %>%
  plot(axes = FALSE, horiz = TRUE, main = "Hierarchical Clustering - Ward's Method")




# Hierarchical clustering with complete linkage
hc2 <- hclust(mahalanobis_distance,
              method = "complete")

dend2 <- as.dendrogram(hc2)



# List of dendrogram plots
dl <- dendlist(
  dend1 %>%   
    set("labels_cex", 1.1) %>%
    set("branches_lwd", 1.5) %>%
    set("branches_lty", 1) %>%
    set("labels_col", value = c("#7ad151","#22a884", "#414487", "#440154"), k=4) %>%
    set("branches_k_color", value = c("#7ad151","#22a884", "#414487", "#440154"), k = 4),
  dend2 %>% 
    set("labels_cex", 1.1) %>%
    set("branches_lwd", 1.5) %>%
    set("branches_lty", 1) %>%
    set("labels_col", value = c("#7ad151","#22a884", "#414487", "#440154"), k=4) %>%
    set("branches_k_color", value = c("#7ad151","#22a884", "#414487", "#440154"), k = 4)
)

# Tanglegram plot
tanglegram(dl,
           common_subtrees_color_lines = FALSE, 
           highlight_branches_lwd=FALSE, 
           margin_inner=2,
           lwd=1.5, 
           axes = FALSE,
           main_left = "Ward's Method",
           main_right = "Complete Linkage")
