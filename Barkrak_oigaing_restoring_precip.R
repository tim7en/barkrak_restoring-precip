#Barkrak_oigaing restoring precip
barkrak <- read.csv ('barkrak_meteo.csv')
oigaing <- read.csv ('Oigaing_2016_2019.csv')
oigaing$X[is.na(oigaing$X)] <- 0
oigaing$Date <- paste0(oigaing$Yil, '-', oigaing$Oy, '-', oigaing$Kun)
oigaing$Date <- as.Date (oigaing$Date)
oigaing$X <- as.numeric(oigaing$X)
barkrak$P_sum <- as.numeric(barkrak$P_sum)
barkrak$Date <- as.Date(barkrak$Date)
daily_corv1 <- merge (oigaing, barkrak, by = 'Date')

plot (oigaing$X, ylab = "Precip, mm", xlab = "Observations")
lines (oigaing$X, col = "blue")
lines (barkrak$P_sum, col = "red")
legend (c("Oigaing", "Barkrak"), lty = 1, bty = "n", col = c("blue", "red"), x= 800, y = 60)

plot ( daily_corv1$X,daily_corv1$P_sum, xlab = "Oigaing, P (mm)", ylab ="Barkrak, P (mm)")

P_barkrak <- ecdf (barkrak$P_sum)
plot (P_barkrak, main = "?????????????? ?????????????????????????? ?????????????????? ?????????????? ?????? ???? ??????????????", xlab = "P, mm")


P_oigaing <- ecdf (oigaing$X)
plot (P_oigaing, main = "Функция распределения, Ойгаинг", xlab = "P, mm")

oigaing$percentile_oigaing <-  (P_oigaing(oigaing$X))
barkrak$percentile_barkrak <- (P_barkrak(barkrak$P_sum))



#quantile (P_oigaing, daily_corv1$percentile_barkrak)

barkrak$barkrak_restored <- 0
for (i in seq (1, nrow (barkrak))){
  #print (paste(i, quantile(P_oigaing, barkrak$percentile_barkrak[i]), sep = "-" ))
  barkrak$barkrak_restored[i] <- quantile (P_oigaing, barkrak$percentile_barkrak[i])
}


barkrak$barkrak_restored <- round (barkrak$barkrak_restored, 1)
barkrak$barkrak_restored[barkrak$barkrak_restored == 	0.5] <-0

plot (oigaing$percentile_oigaing, oigaing$X)

plot (1- barkrak$percentile_barkrak, barkrak$P_sum, main = "Кривая обеспеченности для МС на л. Баркрак Средний", ylab = "P, mm", xlab = "Обеспеченность в долях единиц", pch = 24, col = "red")
points (1- barkrak$percentile_barkrak, barkrak$barkrak_restored, pch = 21, col = "blue")

legend (0.1, 40, pch = c(23,21), c("Перед", "После"), col = c("red", "blue"), bty = "n")

plot (barkrak$percentile_barkrak, barkrak$P_sum)
points (barkrak$percentile_barkrak, barkrak$barkrak_restored)

plot (barkrak$P_sum , barkrak$barkrak_restored, xlab = "Измерянные значения (P, mm)", ylab = "Восстановленные значения (P, mm)")
lines (barkrak$P_sum, barkrak$P_sum, col = "red")
legend (25, 20, "Y = -0.4920 + 0.4332*X, R^2 = 0.73", bty = "n")

summary (lm (barkrak$P_sum ~barkrak$barkrak_restored))

#Y = -0.4920 + 0.4332 * X
#R^2 = 0.7315

plot (barkrak$percentile_barkrak, barkrak$barkrak_restored)
plot (oigaing$percentile_oigaing, oigaing$X)


data_check <- data.frame ("barkrak_restored" = barkrak$barkrak_restored,
                          #"oigaing_p" = daily_corv1$X,
                          "barkrak_original" = barkrak$P_sum,
                          "Date" = barkrak$Date)


#add elevation factor to a sum of precipitation
data_check$barkrak_restored <- data_check$barkrak_restored+ data_check$barkrak_restored*0.8
data_check$barkrak_restored <- as.numeric (data_check$barkrak_restored)

data_check <- data_check[order(data_check$Date),]

plot (data_check$Date, data_check$barkrak_restored)
lines (data_check$Date, data_check$barkrak_restored)
lines (data_check$Date, data_check$barkrak_original, col = "red")
#lines (data_check$Date, data_check$oigaing_p, col = "green")

sum(data_check$barkrak_restored)

plot (data_check$barkrak_original, data_check$barkrak_restored)
summary (lm (data_check$barkrak_restored~ data_check$barkrak_original))

test1 <- merge (data_check, oigaing, by = "Date")
plot (test1$barkrak_restored ~ test1$X)

write.csv (data_check, "data_check.csv", row.names = F)
