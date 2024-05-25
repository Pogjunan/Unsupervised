library(MASS)
library(cluster)
library(factoextra)
library(daisy)
library(factoextra)
library(NbClust)
library(mclust)
library(caret)
library(fastDummies)
install.packages("ggpubr")

obe <- read.csv("Obesity.csv",header=TRUE)

dummy <- dummyVars(" ~ .", data = obe)

# predict 함수를 사용하여 원핫인코딩 적용
obe_encoded <- predict(dummy, newdata = obe)

# 데이터프레임으로 변환
obe_encoded <- as.data.frame(obe_encoded)

# 결과 출력
obe_encoded <- obe_encoded[,1:31]


write.csv(obe_encoded, file = "obe_encoded.csv", row.names = FALSE)

# 결과 출력
print("CSV 파일로 저장되었습니다: obe_encoded.csv")


kmeans_result <- kmeans(obe_encoded, centers = 5)

# 군집 결과를 원본 데이터에 추가
obe$cluster <- kmeans_result$cluster

# 군집 결과 확인
head(obe)

ggplot(obe, aes(x = Age, y = Weight, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Result",
       x = "Age",
       y = "Weight",
       color = "Cluster") +
  theme_minimal()

km.res <- kmeans(obe_encoded_4, 2, nstart = 25)
fviz_cluster(km.res, data = obe_encoded_4)


obe_encoded_scaled <- scale(obe_encoded)

wss <- (nrow(obe_encoded_scaled) - 1) * sum(apply(obe_encoded_scaled, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(obe_encoded_scaled, centers = i, nstart = 25)$tot.withinss)
}

# 결과 시각화
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

obe_encoded_scaled <- scale(obe_encoded)

sil_width <- numeric(15)
for (i in 2:15) {
  km <- kmeans(obe_encoded_scaled, centers = i, nstart = 25)
  sil_width[i] <- mean(silhouette(km$cluster, dist(obe_encoded_scaled))[, 3])
}

# 결과 시각화
plot(1:15, sil_width, type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width")


install.packages("Rtsne")
library(Rtsne)

tsne_results <- Rtsne(obe_encoded_scaled, dims = 2, perplexity = 30)
plot(tsne_results$Y, col = km.res$cluster, pch = 19, main = "t-SNE Plot")

fviz_cluster(km.res, data = obe_encoded)

sil <- silhouette(km.res$cluster, dist(obe_encoded))
plot(sil, col = 1:2, border = NA)

# 3차원 시도


# 필요한 패키지 설치 및 로드
install.packages("plotly")
library(plotly)
library(dplyr)
library(cluster)
library(factoextra)

# 원핫인코딩된 데이터셋과 클러스터링 수행
# 이 예제에서는 obe_encoded 데이터셋을 가정합니다.
# obe_encoded <- <원핫인코딩된 데이터셋>

# 거리 행렬 계산 및 계층적 클러스터링 수행
d <- dist(obe_encoded, method = "euclidean")
hc <- hclust(d, method='ward.D2')  # 'ward.D2' 방법을 사용합니다
sub_grp <- cutree(hc, k = 4)
table(sub_grp)

# 클러스터 정보를 추가하여 데이터프레임 생성
rs <- obe_encoded %>%
  mutate(cluster = sub_grp)

# 데이터셋을 3차원으로 변환 (PCA 사용)
pca_result <- prcomp(obe_encoded, scale. = TRUE)
pca_data <- data.frame(pca_result$x[, 1:3])  # 첫 3개의 주성분 사용
pca_data$cluster <- as.factor(sub_grp)  # 클러스터 정보 추가

# 3차원 시각화
fig <- plot_ly(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, colors = "Set1", type = "scatter3d", mode = "markers",
               marker = list(size = 1)) %>%  # 점의 크기 줄이기
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = '3D PCA Cluster Plot')

# 시각화 출력
fig


## Age: 나이
## Gender: 성별
## Height: 키(미터)
## Weight: 체중(kg)
obe_encoded_4 <- obe_encoded[,1:5]
km.res <- kmeans(obe_encoded_4, 2, nstart = 25)
fviz_cluster(km.res, data = obe_encoded_4)


