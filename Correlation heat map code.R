library(corrplot)
df <- read.csv("D:/math project/All citys data/Visakhapatnam.csv")

corr_mat <- cor(df[,c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene")])

# Print the correlation matrix
print(corr_mat)

corrplot(corr_mat, method = "color", type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.offset = 0.6, diag = FALSE)