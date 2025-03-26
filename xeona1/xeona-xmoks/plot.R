
rm(list = ls(all = TRUE))                 # remove all variables
temps <- c(9, 9, 10, 11, 12, 14, 16, 17, 19, 21, 22, 22, 24, 25, 24, 23, 20, 18, 15, 14, 12, 11, 10, 9)
len <- length(temps)
message("count ", len)
plot(temps, ylim = c(0,30), type = "o", ann = FALSE)
title(main = "temperatures", xlab = "hour", ylab = "air temperature [C]")

