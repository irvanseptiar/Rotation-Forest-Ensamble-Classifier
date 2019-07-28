library(readr)
library(SDMTools)
library(rotationForest)
library(readxl)
library(caret)


#READING DATASET HASIL GLOBAL ENCODING
a = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=2\\DATAREADYFORRTFL=2.xlsx")
b = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=3\\DATAREADYFORRTFL=3.xlsx")
c = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=4\\DATAREADYFORRTFL=4.xlsx")
d = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=5\\DATAREADYFORRTFL=5.xlsx")
e = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=6\\DATAREADYFORRTFL=6.xlsx")
f = read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=7\\DATAREADYFORRTFL=7.xlsx")


data = list(a)
save_indek = c("b")
memory.size(8065)


set.seed(123)
#PROGRAM UTAMA
for (i in 1:length(data)){
  start.time <- Sys.time()
  data_seleksi = data[[i]]
  sample <- sample.int(n = nrow(data_seleksi), size = floor(.7*nrow(data_seleksi)), replace = F)
  
  #SPLIT DATA MENJADI TRAINING DAN TESTING
  training_data <- data_seleksi[sample,]
  test_data <- data_seleksi[-sample,]
  
  x_train <- training_data[,!colnames(training_data) %in% "TARGET"]
  y_train <- as.factor(training_data$TARGET)
  
  x_test <- test_data[,!colnames(test_data) %in% "TARGET"]
  y_test <- as.factor(test_data$TARGET)
  
  #MENENTUKAN PARAMETER TERBAIK K DAN L ROTATION FOREST
  K = c(1)#1,5,10,15,20,(round(ncol(x_train)/3)))
  L = c(100)#10,20,30,40,50,60,70,80,90,100)
  hasil_PCA  <- list()
  hasil_IPCA <- list()
  
  
  for (j in 1: length(K))
  {
    hasil_PCA[[j]]   <- matrix(data = NA , nrow = length(L) , ncol = 9)
    colnames(hasil_PCA[[j]] ) <- c("Akurasi","Sensitifitas","Presisi","MCC","Spesifitas","PPV","NPV","F1","K/L")
    hasil_IPCA [[j]] <- matrix(data = NA , nrow = length(L) , ncol = 9)
    colnames(hasil_IPCA [[j]]) <- c("Akurasi","Sensitifitas","Presisi","MCC","Spesifitas","PPV","NPV","F1","K/L")
    
    for(k in 1:length(L))
      {
      print(paste("Sedang Proses K ke-", j, "dan L ke -", k))
      # ROTATION FOREST MENGGUNAKAN IPCA
      rotForIPCA <- rtf(x_train, y_train, K[j], L[k]) 
      predikIPCA <- prediksi(rotForIPCA,x_test)
      
      #CONFUSION MATRIX IPCA
      cmIPCA = confusion.matrix(y_test,predikIPCA)
      TP = cmIPCA[2,2]
      FP = cmIPCA[1,2]
      FN = cmIPCA[2,1]
      TN = cmIPCA[1,1]
      
      #EVALUASI MODEL ROTATION FOREST MENGGUNAKAN IPCA
      acc_IPCA = (TP+TN)/(TP+FP+TN+FN)
      sen_IPCA = TP/(TP+FN)
      PE_IPCA = TP/(TP+FP)
      MCC_IPCA = ((TP*TN)-(FP*FN))/((sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TP+FP)*sqrt(TN+FN)))
      SP_IPCA = TN/(TN+FP)
      PPV_IPCA = TP/ (TP+FP)
      NPV_IPCA = TN/(TN+FN)
      F1S_IPCA = 2*((sen_IPCA*PPV_IPCA)/(sen_IPCA+PPV_IPCA))
      
      hasil_IPCA [[j]][k,1] = acc_IPCA
      hasil_IPCA [[j]][k,2] = sen_IPCA
      hasil_IPCA [[j]][k,3] = PE_IPCA
      hasil_IPCA [[j]][k,4] = MCC_IPCA
      hasil_IPCA [[j]][k,5] = SP_IPCA
      hasil_IPCA [[j]][k,6] = PPV_IPCA
      hasil_IPCA [[j]][k,7] = NPV_IPCA
      hasil_IPCA [[j]][k,8] = F1S_IPCA
      
      hasil_IPCA [[j]][k,9] <- paste0("K:",K[j], " | L:" ,L[k])
      
      
      #ROTATION FOREST MENGGUNAKAN PCA
      rotForPCA <- rotFor(x_train, y_train, K[j], L[k])
      predikPCA <- prediksi(rotForPCA,x_test)
      
      #CONFUSION MATRIX PCA
      cmPCA = confusion.matrix(y_test,predikPCA)
      TP = cmPCA[2,2] 
      FP = cmPCA[1,2] 
      FN = cmPCA[2,1] 
      TN = cmPCA[1,1] 
      
      
      #EVALUASI MODEL ROTATION FOREST MENGGUNAKAN PCA
      acc_PCA = (TP+TN)/(TP+FP+TN+FN)
      sen_PCA = TP/(TP+FN)
      PE_PCA = TP/(TP+FP)
      MCC_PCA = ((TP*TN)-(FP*FN))/(sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TP+FP)*sqrt(TN+FN))
      SP_PCA = TN/(TN+FP)
      PPV_PCA = TP/ (TP+FP)
      NPV_PCA = TN/(TN+FN)
      F1S_PCA = 2*((sen_PCA*PPV_PCA)/(sen_PCA+PPV_PCA))
      
      hasil_PCA[[j]][k,1] = acc_PCA
      hasil_PCA[[j]][k,2] = sen_PCA
      hasil_PCA[[j]][k,3] = PE_PCA
      hasil_PCA[[j]][k,4] = MCC_PCA
      hasil_PCA[[j]][k,5] = SP_PCA
      hasil_PCA[[j]][k,6] = PPV_PCA
      hasil_PCA[[j]][k,7] = NPV_PCA
      hasil_PCA[[j]][k,8] = F1S_PCA
      
      hasil_PCA [[j]][k,9] <- paste0("K:",K[j], " | L:" ,L[k])
      gc()
    }
    end.time <- Sys.time()
    lamaproses <- end.time - start.time
    show(lamaproses)
  }
  hasil_PCA   <- do.call(rbind, hasil_PCA)
  hasil_IPCA  <- do.call(rbind, hasil_IPCA)
  
  #MENYIMPAN HASIL EVALUASI MODEL
  write.csv(format(hasil_PCA, scientific=FALSE), 
            file = paste0("D:\\THESIS\\PROGRAM\\HASILRUNNING/","HASIL PCA(GE=3 L=100)",as.character(save_indek[i]),".csv")) 
  write.csv(format(hasil_IPCA, scientific=FALSE), 
            file = paste0("D:\\THESIS\\PROGRAM\\HASILRUNNING/","HASIL IPCA(GE=3 L=100)",as.character(save_indek[i]),".csv"))
  
}
