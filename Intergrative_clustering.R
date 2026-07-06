contribution_threshold <- 0.7
pca_num_threshold <- 15
var_threshold <- 0.2
p_threshold <- 0.1
pca_num <- 10
top_num <- 10

setwd("/Users/wangzeyuan/Library/Mobile Documents/com~apple~CloudDocs/alff亚型/experiment")

load('alff_result_list.rda')

mdd_n_alff <- alff_result_list$mdd_n_alff
mdd_y_alff <- alff_result_list$mdd_y_alff
merged_n <- alff_result_list$merged_n
merged_y <- alff_result_list$merged_y
kmeans_result_n <- alff_result_list$kmeans_result_n
kmeans_result_y <- alff_result_list$kmeans_result_y

merged_n$f1 <- merged_n$HAMD1+merged_n$HAMD2+merged_n$HAMD3
merged_n$f2 <- merged_n$HAMD4+merged_n$HAMD5+merged_n$HAMD6+merged_n$HAMD9+merged_n$HAMD10+merged_n$HAMD11
merged_n$f3 <- merged_n$HAMD12+merged_n$HAMD13+merged_n$HAMD14
merged_n$f4 <- merged_n$HAMD7+merged_n$HAMD8
merged_n$f5 <- merged_n$HAMD15+merged_n$HAMD16+merged_n$HAMD17
merged_n <- merged_n[,c(1:26,44:48)]

merged_n_ori <- merged_n

for (i in 1:ncol(merged_n)) {
  merged_n[,i] <- merged_n[,i]%>%scale()
}

merged_y$f1 <- merged_y$HAMD1+merged_y$HAMD2+merged_y$HAMD3
merged_y$f2 <- merged_y$HAMD4+merged_y$HAMD5+merged_y$HAMD6+merged_y$HAMD9+merged_y$HAMD10+merged_y$HAMD11
merged_y$f3 <- merged_y$HAMD12+merged_y$HAMD13+merged_y$HAMD14
merged_y$f4 <- merged_y$HAMD7+merged_y$HAMD8
merged_y$f5 <- merged_y$HAMD15+merged_y$HAMD16+merged_y$HAMD17
merged_y <- merged_y[,c(1:23,41:45)]

merged_y_ori <- merged_y

for (i in 1:ncol(merged_y)) {
  merged_y[,i] <- merged_y[,i]%>%scale()
}

res<-NbClust(merged_n, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10,
             method = "kmeans", index = "silhouette")
cat(res$Best.nc[1])

kmeans_result_n <- kmeans(merged_n, centers = 9)

# 将轮廓系数结果提取为数据框
silhouette_data <- data.frame(
  Clusters = 2:10,
  Silhouette = res$All.index
)

y_val <- silhouette_data$Silhouette[silhouette_data$Clusters == 9]

sub_a <- ggplot(silhouette_data, aes(x = Clusters, y = Silhouette)) +
  geom_line(color = "#F4A464FF", size = 1) +
  geom_point(color = "#912534FF", size = 3) +
  annotate("segment", x = 9, xend = 9, y = 0.02, yend = y_val,
           linetype = "dashed", color = '#BF2729FF', size = 0.8) +
  theme_classic2() +
  labs(
    title = 'K-means on MDD Cohort I',
    x = "Number of clusters",
    y = "Avg (SI)"
  ) +scale_x_continuous(breaks = 1:10)+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# kmeans_result_n <- res$Best.partition%>%as.data.frame()

res<-NbClust(merged_y, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10,
             method = "kmeans", index = "silhouette")
cat(res$Best.nc[1])

kmeans_result_y <- kmeans(merged_y, centers = 9)

# 将轮廓系数结果提取为数据框
silhouette_data <- data.frame(
  Clusters = 2:10,
  Silhouette = res$All.index
)

y_val <- silhouette_data$Silhouette[silhouette_data$Clusters == 9]

sub_b <- ggplot(silhouette_data, aes(x = Clusters, y = Silhouette)) +
  geom_line(color = "#F4A464FF", size = 1) +
  geom_point(color = "#912534FF", size = 3) +
  annotate("segment", x = 9, xend = 9, y = 0.02, yend = y_val,
           linetype = "dashed", color = '#BF2729FF', size = 0.8) +
  labs(
    title = 'K-means on MDD Cohort II',
    x = "Number of clusters",
    y = "Avg (SI)"
  ) +scale_x_continuous(breaks = 1:10)+
  theme_classic2()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot_grid(sub_a,sub_b, ncol = 2)
ggsave(str_c('kmeans_si.pdf'),width = 9, height = 4)

setwd("/Users/wangzeyuan/Library/Mobile Documents/com~apple~CloudDocs/alff亚型/experiment")

load('alff_result_list.rda')

mdd_n_alff <- alff_result_list$mdd_n_alff
mdd_y_alff <- alff_result_list$mdd_y_alff
merged_n <- alff_result_list$merged_n
merged_y <- alff_result_list$merged_y

merged_n$f1 <- merged_n$HAMD1+merged_n$HAMD2+merged_n$HAMD3
merged_n$f2 <- merged_n$HAMD4+merged_n$HAMD5+merged_n$HAMD6+merged_n$HAMD9+merged_n$HAMD10+merged_n$HAMD11
merged_n$f3 <- merged_n$HAMD12+merged_n$HAMD13+merged_n$HAMD14
merged_n$f4 <- merged_n$HAMD7+merged_n$HAMD8
merged_n$f5 <- merged_n$HAMD15+merged_n$HAMD16+merged_n$HAMD17

merged_n <- merged_n[,c(1:26,44:48)]

merged_n_ori <- merged_n

for (i in 1:ncol(merged_n)) {
  merged_n[,i] <- merged_n[,i]%>%scale()
}

merged_y$f1 <- merged_y$HAMD1+merged_y$HAMD2+merged_y$HAMD3
merged_y$f2 <- merged_y$HAMD4+merged_y$HAMD5+merged_y$HAMD6+merged_y$HAMD9+merged_y$HAMD10+merged_y$HAMD11
merged_y$f3 <- merged_y$HAMD12+merged_y$HAMD13+merged_y$HAMD14
merged_y$f4 <- merged_y$HAMD7+merged_y$HAMD8
merged_y$f5 <- merged_y$HAMD15+merged_y$HAMD16+merged_y$HAMD17

merged_y <- merged_y[,c(1:23,41:45)]

merged_y_ori <- merged_y

for (i in 1:ncol(merged_y)) {
  merged_y[,i] <- merged_y[,i]%>%scale()
}

res<-NbClust(merged_n, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10,
             method = "kmeans", index = "silhouette")
cat(res$Best.nc[1])

kmeans_result_n <- res$Best.partition%>%as.data.frame()

res<-NbClust(merged_y, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10,
             method = "kmeans", index = "silhouette")
cat(res$Best.nc[1])

kmeans_result_y <- res$Best.partition%>%as.data.frame()

kmeans_result_n <- kmeans(merged_n, centers = 9)

pca_result <- prcomp(merged_n)

# 提取 PCA 转换后的数据
pca_data <- as.data.frame(pca_result$x[, 1:2])  # 提取前两个主成分
pca_data$cluster <- as.factor(kmeans_result_n$cluster)  # 添加聚类结果

# 提取聚类中心在 PCA 空间中的坐标
centers_pca <- as.data.frame(predict(pca_result, kmeans_result_n$centers))
centers_pca$cluster <- as.factor(1:nrow(centers_pca))  # 添加聚类中心的标签

# 绘制 PCA 可视化的散点图和聚类中心


kmeans_result_y <- kmeans(merged_y, centers = 9)

pca_result <- prcomp(merged_y)

# 提取 PCA 转换后的数据
pca_data <- as.data.frame(pca_result$x[, 1:2])  # 提取前两个主成分
pca_data$cluster <- as.factor(kmeans_result_y$cluster)  # 添加聚类结果

# 提取聚类中心在 PCA 空间中的坐标
centers_pca <- as.data.frame(predict(pca_result, kmeans_result_y$centers))
centers_pca$cluster <- as.factor(1:nrow(centers_pca))  # 添加聚类中心的标签

# 绘制 PCA 可视化的散点图和聚类中心


var_values <- apply(mdd_n_alff, 2, var)
threshold <- quantile(var_values, var_threshold)  # 5% 分位数
mdd_n_alff_filtered <- mdd_n_alff[, var_values > threshold]

var_values <- apply(mdd_y_alff, 2, var)
threshold <- quantile(var_values, var_threshold)  # 5% 分位数
mdd_y_alff_filtered <- mdd_y_alff[, var_values > threshold]

# 主成分分析
pca_result <- prcomp(mdd_n_alff_filtered, center = TRUE)

# 查看主成分贡献率
loadings <- pca_result$rotation  # 提取特征权重（变量在主成分上的贡献）
top_features <- apply(abs(loadings), 2, function(x) names(sort(x, decreasing = TRUE)[1:pca_num]))  # 每个主成分选 5 个贡献最大的特征

top_features_n <- top_features[,1:top_num]


# 主成分分析
pca_result <- prcomp(mdd_y_alff_filtered, center = TRUE)

# 查看主成分贡献率
loadings <- pca_result$rotation  # 提取特征权重（变量在主成分上的贡献）
top_features <- apply(abs(loadings), 2, function(x) names(sort(x, decreasing = TRUE)[1:pca_num]))  # 每个主成分选 5 个贡献最大的特征

top_features_y <- top_features[,1:top_num]

top_features <- top_features_n%>%intersect(top_features_y)

merged_n1 <- mdd_n_alff[,colnames(mdd_n_alff)%in%top_features]%>%cbind(merged_n[,27:31])%>%cbind(kmeans_result_n$cluster)
merged_y1 <- mdd_y_alff[,colnames(mdd_y_alff)%in%top_features]%>%cbind(merged_y[,24:28])%>%cbind(kmeans_result_y$cluster)
colnames(merged_n1)[33] <- 'tag'
colnames(merged_y1)[33] <- 'tag'

merged_n1$tag1 <- merged_n1$tag
merged_n1$tag[merged_n1$tag1==1] <- 6
merged_n1$tag[merged_n1$tag1==2] <- 1
merged_n1$tag[merged_n1$tag1==3] <- 2
merged_n1$tag[merged_n1$tag1==4] <- 7
merged_n1$tag[merged_n1$tag1==5] <- 3
merged_n1$tag[merged_n1$tag1==6] <- 4
merged_n1$tag[merged_n1$tag1==7] <- 8
merged_n1$tag[merged_n1$tag1==8] <- 9
merged_n1$tag[merged_n1$tag1==9] <- 5

merged_y1$tag1 <- merged_y1$tag
merged_y1$tag[merged_y1$tag1==1] <- 5
merged_y1$tag[merged_y1$tag1==2] <- 1
merged_y1$tag[merged_y1$tag1==3] <- 4
merged_y1$tag[merged_y1$tag1==4] <- 2
merged_y1$tag[merged_y1$tag1==5] <- 6
merged_y1$tag[merged_y1$tag1==6] <- 7
merged_y1$tag[merged_y1$tag1==7] <- 8
merged_y1$tag[merged_y1$tag1==8] <- 9
merged_y1$tag[merged_y1$tag1==9] <- 3

merged_n1 <- merged_n1[,-ncol(merged_n1)]
merged_y1 <- merged_y1[,-ncol(merged_y1)]

for (i in 1:32) {
  merged_n1[,i] <- merged_n1[,i]%>%scale()
  merged_y1[,i] <- merged_y1[,i]%>%scale()
}

cor_matrix <- matrix(0,9,9)

for (i in 1:9) {
  for (j in 1:9){
    temp_n <- merged_n1[merged_n1$tag==i,]
    temp_y <- merged_y1[merged_y1$tag==j,]
    
    temp <- 0
    for (k in 1:32) {
      temp <- temp+(mean(temp_n[,k])-mean(temp_y[,k]))^2
    }
    cor_matrix[i,j] <- temp/32
  }
}

save(cor_matrix,file = 'cor_matrix.rda')

cor_matrix1 <- matrix(0,9,9)
cor_matrix1[1,1] <- 8
cor_matrix1[2,2] <- 8
cor_matrix1[3,3] <- 8
cor_matrix1[4,4] <- 8
cor_matrix1[5,5] <- 8
cor_matrix1[6,1] <- 3
cor_matrix1[7,1] <- 3
cor_matrix1[8,5] <- 3
cor_matrix1[9,1] <- 3
cor_matrix1[1,6] <- 4
cor_matrix1[1,7] <- 4
cor_matrix1[7,8] <- 4
cor_matrix1[8,9] <- 4

col_fun = circlize::colorRamp2(c(5,1), c('#BF2729FF','#FFEC9DFF'))

ht_opt(legend_border = "black")

cor_matrix2 <- 1/cor_matrix
rownames(cor_matrix2) <- str_c('Cluster ',1:9)
colnames(cor_matrix2) <- str_c('Cluster ',1:9)


pdf('Similarity.pdf',width = 6,height = 5)

Heatmap(cor_matrix2,cluster_rows = F,cluster_columns = F,col = col_fun,show_column_names = T,
        show_row_names = T,column_names_rot = 45,column_names_centered = T, row_names_side = "left",row_title = "MDD Cohort I Clusters",column_title = "MDD Cohort II Clusters",column_title_side = 'bottom',
        row_title_gp = gpar(fontsize = 16),column_title_gp = gpar(fontsize = 16),heatmap_legend_param = list(
          title = "Similarity",
          title_gp = gpar(fontsize = 14, fontface = "bold"),  # 图例标题
          labels_gp = gpar(fontsize = 12)                     # 图例标签
        ),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.rect(x = x, y = y, width = width, height = height, 
                    gp = gpar(col = 'black', fill = NA))
          if (cor_matrix1[i, j]==8) {
            grid.points(x, y, pch = 8, size = unit(15, "pt"),
                        gp = gpar(col = "black"))
          }
          if (cor_matrix1[i, j]==3) {
            grid.points(x, y, pch = 3, size = unit(15, "pt"),
                        gp = gpar(col = "black"))
          }
          if (cor_matrix1[i, j]==4) {
            grid.points(x, y, pch = 4, size = unit(15, "pt"),
                        gp = gpar(col = "black"))
          }
        })
dev.off()

# for (i in 1:9) {
#   cat(i,':',which(cor_matrix[i,]==min(cor_matrix[i,])),'\n')
# }
# 
# for (i in 1:9) {
#   cat(i,':',which(cor_matrix[,i]==min(cor_matrix[,i])),'\n')
# }

kmeans_result_n <- kmeans(merged_n, centers = 9)

pca_result <- prcomp(merged_n)

# 提取 PCA 转换后的数据
pca_data <- as.data.frame(pca_result$x[, 1:2])  # 提取前两个主成分
pca_data$cluster <- merged_n1$tag  # 添加聚类结果
pca_data$cluster[pca_data$cluster>5] <- 'Rest'
pca_data$cluster <- factor(pca_data$cluster,levels = c('1','2','3','4','5','Rest'))
# 提取聚类中心在 PCA 空间中的坐标
# centers_pca <- as.data.frame(predict(pca_result, kmeans_result_n$centers))
# centers_pca$cluster <- 1:nrow(centers_pca)  # 添加聚类中心的标签
# centers_pca$tag1 <- centers_pca$cluster
# centers_pca$cluster[centers_pca$tag1==1] <- 6
# centers_pca$cluster[centers_pca$tag1==2] <- 1
# centers_pca$cluster[centers_pca$tag1==3] <- 2
# centers_pca$cluster[centers_pca$tag1==4] <- 7
# centers_pca$cluster[centers_pca$tag1==5] <- 3
# centers_pca$cluster[centers_pca$tag1==6] <- 4
# centers_pca$cluster[centers_pca$tag1==7] <- 8
# centers_pca$cluster[centers_pca$tag1==8] <- 9
# centers_pca$cluster[centers_pca$tag1==9] <- 5
# centers_pca <- centers_pca[centers_pca$cluster<6,]
# centers_pca$cluster <- factor(centers_pca$cluster,levels = c('1','2','3','4','5'))

# 绘制 PCA 可视化的散点图和聚类中心
sub_a <- ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = cluster), size = 3) +  # 数据点
  labs(
    title = "MDD Cohort I Clusters",
    x = "PC 1",
    y = "PC 2",
    color = "Cluster"
  ) + theme_classic()+scale_color_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF','#F0F0F0'))+
  theme(plot.title = element_text(hjust = 0.5))

kmeans_result_y <- kmeans(merged_y, centers = 9)

pca_result <- prcomp(merged_y)

pca_data <- as.data.frame(pca_result$x[, 1:2])  # 提取前两个主成分
pca_data$cluster <- merged_y1$tag  # 添加聚类结果
pca_data$cluster[pca_data$cluster>5] <- 'Rest'
pca_data$cluster <- factor(pca_data$cluster,levels = c('1','2','3','4','5','Rest'))
# 提取聚类中心在 PCA 空间中的坐标
# centers_pca <- as.data.frame(predict(pca_result, kmeans_result_y$centers))
# centers_pca$tag <- 1:nrow(centers_pca)  # 添加聚类中心的标签
# centers_pca$tag1 <- centers_pca$tag
# centers_pca$tag[centers_pca$tag1==1] <- 5
# centers_pca$tag[centers_pca$tag1==2] <- 1
# centers_pca$tag[centers_pca$tag1==3] <- 4
# centers_pca$tag[centers_pca$tag1==4] <- 2
# centers_pca$tag[centers_pca$tag1==5] <- 6
# centers_pca$tag[centers_pca$tag1==6] <- 7
# centers_pca$tag[centers_pca$tag1==7] <- 8
# centers_pca$tag[centers_pca$tag1==8] <- 9
# centers_pca$tag[centers_pca$tag1==9] <- 3
# centers_pca <- centers_pca[centers_pca$tag<6,]
# centers_pca$tag <- factor(centers_pca$tag,levels = c('1','2','3','4','5'))

# 绘制 PCA 可视化的散点图和聚类中心
sub_b <- ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = cluster), size = 3) +  # 数据点
  # geom_point(data = centers_pca, aes(x = PC1, y = PC2, color = tag), 
  #            size = 5, shape = 23, stroke = 2) +
  labs(
    title = "MDD Cohort II Clusters",
    x = "PC 1",
    y = "PC 2",
    color = "Cluster"
  ) + theme_classic()+scale_color_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF','#F0F0F0'))+
  theme(plot.title = element_text(hjust = 0.5))

setwd("/Users/wangzeyuan/Library/Mobile Documents/com~apple~CloudDocs/alff亚型/experiment")
plot_grid(sub_a,sub_b, ncol = 2)
ggsave(str_c('kmeans_scatter.pdf'),width = 9, height = 4)

tag_n <- merged_n1$tag  # 添加聚类结果
tag_n[tag_n>5] <- 'Rest'
tag_n_table <- tag_n%>%table()%>%as.data.frame()

tag_y <- merged_y1$tag  # 添加聚类结果
tag_y[tag_y>5] <- 'Rest'
tag_y_table <- tag_y%>%table()%>%as.data.frame()

tag_n_table$percent <- round(tag_n_table$Freq / sum(tag_n_table$Freq) * 100, 1)
tag_y_table$percent <- round(tag_y_table$Freq / sum(tag_y_table$Freq) * 100, 1)

tag_n_table$. <- str_c('Cluster ',tag_n_table$.[1:5])%>%append('Rest')
tag_y_table$. <- str_c('Cluster ',tag_y_table$.[1:5])%>%append('Rest')

pdf('percentage.pdf',width = 6,height = 4)
ha = rowAnnotation(
  percent = anno_text(
    paste0(round(tag_n_table$Freq / sum(tag_n_table$Freq) * 100, 1), "%"),
    just = "center",location = 0.5
  )
)+
  rowAnnotation('MDD Cohort I' = anno_barplot(tag_n_table$Freq, gp = gpar(fill = c('#F4A464FF')), 
                                            axis_param = list(direction = "reverse"), 
                                            bar_width = 1, width = unit(4, "cm")))+
  rowAnnotation(foo = anno_text(tag_n_table$., location = 0.5, just = "center",
                                gp = gpar(col = "black", border = "black"),
                                width = max_text_width(tag_n_table$.)*1.2))+
  rowAnnotation('MDD Cohort II' = anno_barplot(tag_y_table$Freq, gp = gpar(fill = c('#BF2729FF')), 
                                         bar_width = 1, width = unit(4, "cm")))+
  rowAnnotation(
    percent = anno_text(
      paste0(round(tag_y_table$Freq / sum(tag_y_table$Freq) * 100, 1), "%"),
      just = "center",location = 0.5
    )
  )
draw(ha)
dev.off()
# 2:2, 3:4, 5:9, 6:3, 9:1

merged_n1 <- mdd_n_alff[,colnames(mdd_n_alff)%in%top_features]%>%cbind(merged_n_ori[,27:31])%>%cbind(kmeans_result_n$cluster)
merged_y1 <- mdd_y_alff[,colnames(mdd_y_alff)%in%top_features]%>%cbind(merged_y_ori[,24:28])%>%cbind(kmeans_result_y$cluster)
colnames(merged_n1)[33] <- 'tag'
colnames(merged_y1)[33] <- 'tag'

# 2,2
temp_n <- merged_n1[merged_n1$tag==2,]
temp_y <- merged_y1[merged_y1$tag==2,]

temp_n$tag <- 1
temp_y$tag <- 1

# 3,4
temp_n1 <- merged_n1[merged_n1$tag==3,]
temp_y1 <- merged_y1[merged_y1$tag==4,]

temp_n1$tag <- 2
temp_y1$tag <- 2

temp_n <- temp_n%>%rbind(temp_n1)
temp_y <- temp_y%>%rbind(temp_y1)

# 5,9
temp_n1 <- merged_n1[merged_n1$tag==5,]
temp_y1 <- merged_y1[merged_y1$tag==9,]

temp_n1$tag <- 3
temp_y1$tag <- 3

temp_n <- temp_n%>%rbind(temp_n1)
temp_y <- temp_y%>%rbind(temp_y1)

# 6,3
temp_n1 <- merged_n1[merged_n1$tag==6,]
temp_y1 <- merged_y1[merged_y1$tag==3,]

temp_n1$tag <- 4
temp_y1$tag <- 4

temp_n <- temp_n%>%rbind(temp_n1)
temp_y <- temp_y%>%rbind(temp_y1)

# 9,1
temp_n1 <- merged_n1[merged_n1$tag==9,]
temp_y1 <- merged_y1[merged_y1$tag==1,]

temp_n1$tag <- 5
temp_y1$tag <- 5

temp_n <- temp_n%>%rbind(temp_n1)
temp_y <- temp_y%>%rbind(temp_y1)

tag_matched_n <- temp_n
tag_matched_y <- temp_y

tag_matched_n$total <- tag_matched_n$f1+tag_matched_n$f2+tag_matched_n$f3+tag_matched_n$f4+tag_matched_n$f5
tag_matched_y$total <- tag_matched_y$f1+tag_matched_y$f2+tag_matched_y$f3+tag_matched_y$f4+tag_matched_y$f5

if(!dir.exists('Factor')){
  dir.create('Factor')
}
setwd('Factor')

# f1

sub_a <- ggboxplot(tag_matched_n, 
         x = "tag", y = "f1",
         fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "3"),c("2", "4"),c("2", "5"),c("3", "4"),c("3", "5")),
                                               method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                  x = "tag", y = "f1",
                  fill = "tag") + stat_compare_means(comparisons = list(c("1", "3"),c("1", "4"),c("2", "3"),c("3", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Factor 1', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('F1.pdf'),width = 4.5, height = 3.6)

# f2

sub_a <- ggboxplot(tag_matched_n, 
                   x = "tag", y = "f2",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "3"),c("1", "4"),c("1", "5"),c("2", "3"),c("2", "4"),c("2", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                   x = "tag", y = "f2",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "4"),c("1", "5"),c("2", "4"),c("2", "5"),c("3", "4"),c("3", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Factor 2', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('F2.pdf'),width = 4.5, height = 3.6)


# f3

sub_a <- ggboxplot(tag_matched_n, 
                   x = "tag", y = "f3",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "3"),c("1", "4"),c("2", "4"),c("3", "4"),c("4", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                   x = "tag", y = "f3",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "3"),c("1", "4"),c("2", "3"),c("3", "4"),c("3", "5"),c("4", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Factor 3', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('F3.pdf'),width = 4.5, height = 3.6)

# f4

sub_a <- ggboxplot(tag_matched_n, 
                   x = "tag", y = "f4",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "4"),c("3", "4"),c("4", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                   x = "tag", y = "f4",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "3"),c("1", "5"),c("2", "4"),c("3", "4"),c("4", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Factor 4', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('F4.pdf'),width = 4.5, height = 3.6)

# f5

sub_a <- ggboxplot(tag_matched_n, 
                   x = "tag", y = "f5",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "5"),c("2", "3"),c("2", "4"),c("2", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                   x = "tag", y = "f5",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "3"),c("2", "4"),c("2", "5"),c("3", "4")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Factor 5', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('F5.pdf'),width = 4.5, height = 3.6)

# total

sub_a <- ggboxplot(tag_matched_n, 
                   x = "tag", y = "total",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "4"),c("1", "5"),c("2", "4"),c("2", "5"),c("3", "4"),c("3", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Score')

sub_b <- ggboxplot(tag_matched_y, 
                   x = "tag", y = "total",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "3"),c("2", "3"),c("2", "4"),c("3", "4"),c("3", "5")),
                                                      method = "wilcox",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Score')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('HAMD Total', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('total.pdf'),width = 4.5, height = 3.6)


# alff

setwd("/Users/wangzeyuan/Library/Mobile Documents/com~apple~CloudDocs/Features")

load('fMRI_roi.rda')

fMRI_roi_list <- fMRI_roi$short%>%unique()

mdd_n_alff_sub <- mdd_n_alff[rownames(mdd_n_alff)%in%rownames(tag_matched_n),]
mdd_y_alff_sub <- mdd_y_alff[rownames(mdd_y_alff)%in%rownames(tag_matched_y),]

setwd("/Users/wangzeyuan/Library/Mobile Documents/com~apple~CloudDocs/alff亚型/experiment")

if(!dir.exists('Alff')){
  dir.create('Alff')
}
setwd('Alff')

network_result <- matrix(0,12,2)%>%as.data.frame()
colnames(network_result) <- c('network','match')
network_result$network <- fMRI_roi_list

network_cor <- matrix(0,12,5)%>%as.data.frame()
colnames(network_cor) <- c('network','cor','p_cor','rho','p_rho')
network_cor$network <- fMRI_roi_list

for (i in 1:12) {
  
  mdd_n_alff_sub1 <- mdd_n_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]
  mdd_y_alff_sub1 <- mdd_y_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]
  
  temp_n <- rowMeans(mdd_n_alff_sub1)%>%as.data.frame()
  temp_n$tag <- tag_matched_n$tag[match(rownames(temp_n),rownames(tag_matched_n))]
  
  temp_y <- rowMeans(mdd_y_alff_sub1)%>%as.data.frame()
  temp_y$tag <- tag_matched_y$tag[match(rownames(temp_y),rownames(tag_matched_y))]
  
  result_n <- compare_means(.~tag,temp_n,'t.test')
  result_n <- result_n[result_n$p.signif!='ns',]
  
  result_y <- compare_means(.~tag,temp_y,'t.test')
  result_y <- result_y[result_y$p.signif!='ns',]
  
  if(nrow(result_n)>0&nrow(result_y)>0){
    temp1 <- c(result_n$group1%>%str_c('_',result_n$group2),result_n$group2%>%str_c('_',result_n$group1))
    temp2 <- result_y$group1%>%str_c('_',result_y$group2)
    network_result[i,2] <- length(temp1%>%intersect(temp2))
  }
  
  temp_n_mean <- temp_n %>%
    group_by(tag) %>%
    summarise(mean_value = mean(., na.rm = TRUE))
  
  temp_y_mean <- temp_y %>%
    group_by(tag) %>%
    summarise(mean_value = mean(., na.rm = TRUE))
  
  # 假设有两列变量 x 和 y
  temp <- cor.test(temp_n_mean$mean_value,temp_y_mean$mean_value, method = "pearson")
  network_cor$cor[i] <- temp$estimate
  network_cor$p_cor[i] <- temp$p.value
  
  temp <- cor.test(temp_n_mean$mean_value,temp_y_mean$mean_value, method = "spearman")
  network_cor$rho[i] <- temp$estimate
  network_cor$p_rho[i] <- temp$p.value
}

network_result <- network_result%>%cbind(network_cor[,c(2:5)])
network_result %>%
  ggplot(aes(y = match, x = fct_reorder(network, match, .desc = TRUE))) +
  geom_bar(aes(fill=fct_reorder(network, match, .desc = TRUE)),stat = "identity", position = "identity") +  # 使用 position = "identity"
  geom_text(aes(label = round(match,2)), vjust = -0.5, size = 4)+
  theme_classic2() +
  theme(
    plot.title = element_text(size = 15,hjust = 0.5),
    axis.text.x = element_text(size = 12,angle = 45, hjust = 1),
    legend.position = 'none'
  ) +
  scale_fill_manual(values=rev(c('#1E8E99FF','#51C3CCFF','#99F9FFFF','#B2FCFFFF','#CCFEFFFF','#E5FFFFFF','#FFE5CCFF','#FFCA99FF','#FFAD65FF','#FF8E32FF','#CC5800FF','#993F00FF')))+
  ylim(0,5.2)+
  # scale_y_continuous(expand = c(0, 0)) +  # 去除 y 轴底部间隙
  labs(
    title = 'Significance Cross-cohort Consistency',
    y = 'Number',
    x = NULL
  )

ggsave('network_consistency.pdf',width = 4.8, height = 4.1)

network_result$p_cor_sig <- ''
network_result$p_cor_sig[7] <- '***'
network_result$p_cor_sig[4] <- '*'

network_result %>%
  ggplot(aes(y = cor, x = fct_reorder(network, cor, .desc = TRUE))) +
  geom_bar(aes(fill=fct_reorder(network, cor, .desc = TRUE)),stat = "identity", position = "identity") +  # 使用 position = "identity"
  geom_text(aes(label = round(cor,2)), vjust = -0.5, size = 4)+
  geom_text(aes(label = p_cor_sig), vjust = 1.5, size = 4, color = 'white')+
  theme_classic2() +
  theme(
    plot.title = element_text(size = 15,hjust = 0.5),
    axis.text.x = element_text(size = 12,angle = 45, hjust = 1),
    legend.position = 'none'
  ) +
  scale_fill_manual(values=rev(c('#1E8E99FF','#51C3CCFF','#99F9FFFF','#B2FCFFFF','#CCFEFFFF','#E5FFFFFF','#FFE5CCFF','#FFCA99FF','#FFAD65FF','#FF8E32FF','#CC5800FF','#993F00FF')))+
  ylim(-0.2,1)+
  # scale_y_continuous(expand = c(0, 0)) +  # 去除 y 轴底部间隙
  labs(
    title = 'ALFF Cross-Cohort Consistency',
    y = 'Pearson.Cor',
    x = NULL
  )

ggsave('network_cor.pdf',width = 5, height = 4.1)


# DMN
i=4
  
mdd_n_alff_sub1 <- mdd_n_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]
mdd_y_alff_sub1 <- mdd_y_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]

temp_n <- rowMeans(mdd_n_alff_sub1)%>%as.data.frame()
temp_n$tag <- tag_matched_n$tag[match(rownames(temp_n),rownames(tag_matched_n))]

temp_y <- rowMeans(mdd_y_alff_sub1)%>%as.data.frame()
temp_y$tag <- tag_matched_y$tag[match(rownames(temp_y),rownames(tag_matched_y))]

sub_a <- ggboxplot(temp_n, 
                   x = "tag", y = ".",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "4"),c("1", "5"),c("2", "3"),c("2", "4"),c("3", "5")),
                                                      method = "t.test",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Mean ALFF')

sub_b <- ggboxplot(temp_y, 
                   x = "tag", y = ".",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "4"),c("1", "5"),c("2", "3"),c("3", "4"),c("3", "5")),
                                                      method = "t.test",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Mean ALFF')
  
combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('DMN Mean ALFF', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(str_c('DMN.pdf'),width = 4.5, height = 3.6)


# FPTC
i=7

mdd_n_alff_sub1 <- mdd_n_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]
mdd_y_alff_sub1 <- mdd_y_alff_sub[,fMRI_roi$short==fMRI_roi_list[i]]

temp_n <- rowMeans(mdd_n_alff_sub1)%>%as.data.frame()
temp_n$tag <- tag_matched_n$tag[match(rownames(temp_n),rownames(tag_matched_n))]

temp_y <- rowMeans(mdd_y_alff_sub1)%>%as.data.frame()
temp_y$tag <- tag_matched_y$tag[match(rownames(temp_y),rownames(tag_matched_y))]

sub_a <- ggboxplot(temp_n, 
                   x = "tag", y = ".",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "4"),c("1", "5"),c("2", "3")),
                                                      method = "t.test",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort I',
       x = NULL,
       y = 'Mean ALFF')

sub_b <- ggboxplot(temp_y, 
                   x = "tag", y = ".",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "4"),c("1", "5"),c("2", "3")),
                                                      method = "t.test",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    legend.position = 'none'
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Mean ALFF')

combined_plots <- plot_grid(sub_a,sub_b,ncol = 2)
title <- ggdraw() +
  draw_label('FPTC Mean ALFF', size = 15, hjust = 0.5)
plot_grid(title, combined_plots, ncol = 1, rel_heights = c(0.1, 1))

ggsave('FPTC.pdf',width = 4.5, height = 3.6)

ggboxplot(temp_y, 
                   x = "tag", y = ".",
                   fill = "tag") + stat_compare_means(comparisons = list(c("1", "2"),c("1", "4"),c("1", "5"),c("2", "3")),
                                                      method = "t.test",label = "p.signif")+
  theme_classic2() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4)+
  theme(
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank()
  ) + scale_fill_manual(values=c('#00496FFF','#0F85A0FF','#EDD746FF','#ED8B00FF','#DD4124FF'))+
  labs(title = 'MDD Cohort II',
       x = NULL,
       y = 'Mean ALFF',
       fill = "MDD Group")

ggsave('MDD_group.pdf',width = 9, height = 3.6)

tag_matched <- list(tag_matched_n=tag_matched_n,tag_matched_y=tag_matched_y)
save(tag_matched,file = 'tag_matched.rda')







