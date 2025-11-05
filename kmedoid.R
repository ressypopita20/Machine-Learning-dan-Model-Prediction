library(readr) 
library(skimr)
Customer_Data1 <- read_csv("C:/Users/LAPTOP HP/OneDrive/Dokumen/Semester 6/Data Mining/Customer_Data.csv")
skim(Customer_Data1)

#Preprocessing
colSums(is.na(Customer_Data1)) #Mengecek missing values

Customer_Data0 <- na.omit(Customer_Data1) # Menghapus semua baris yang mengandung NA
str(Customer_Data0)
colSums(is.na(Customer_Data0)) #Mengecek kembali missing values

Customer_Data <- scale(Customer_Data0[-1])  #Standarisasi data
head(Customer_Data)


#Variabel yang digunakan
variabel_cluster <- Customer_Data[, c(1,3,4,5,6,7,13)]
Data_Kel2 <- cbind(cust_id = Customer_Data0$cust_id, as.data.frame(variabel_cluster))
head(Data_Kel2)
skim(variabel_cluster)


#Banyak Cluster
library(factoextra)
fviz_nbclust(variabel_cluster,kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "red") +  # Garis bantu di k = 4
  ggtitle("Elbow Method dengan Garis Bantu")

#Cluster K medoid
library(cluster)
kmed2 <- pam(variabel_cluster, k = 4)
kmed2
data_klasterisasi <- cbind(as.data.frame(variabel_cluster),cluster = as.factor(kmed2$clustering)) #menambahkan nilai klaster ke datafix
data_klasterisasi
str(data_klasterisasi)

fviz_cluster(kmed2, data = data_klasterisasi,
             main = 'Plot Klasterisasi berdasarkan Klaster',
             xlab = 'X',
             ylab = 'Y',
             geom = "point", #menunjukkan titik-titik data dalam ruang berdasarkan klaster
             ggtheme = theme_minimal()) #membuat plot klaster

#cluster 1
Data_Cluster1 <- subset(data_klasterisasi, cluster == 1)
head(Data_Cluster1)  

#cluster 2
Data_Cluster2 <- subset(data_klasterisasi, cluster == 2)
head(Data_Cluster2)


#cluster 3
Data_Cluster3 <- subset(data_klasterisasi, cluster == 3)
head(Data_Cluster3)


#cluster 4
Data_Cluster4 <- subset(data_klasterisasi, cluster == 4)
head(Data_Cluster4)

#Banyaknya anggota setiap cluster
table(data_klasterisasi$cluster)

#karakteristik cluster
aggregate(. ~ cluster, data = data_klasterisasi[,-1], FUN = mean, na.rm = TRUE)
