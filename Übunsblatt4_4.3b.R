library(jpeg)

url <- "https://upload.wikimedia.org/wikipedia/commons/6/6e/Mona_Lisa_bw_square.jpeg"
file <- "Mona_Lisa_bw_square.jpeg"
download.file(url, destfile = file, mode = "wb")

pic <- readJPEG(file) 

n_pic <- nrow(pic)
svd_pic <- svd(pic)

plot(1:n_pic,log(svd_pic$d))

plot(1:200, log(svd_pic$d[1:200]))

for(k in c(5, 10, 20, 50, 100, 200)){ 
  approx_k <- svd_pic$u[, 1:k] %*% diag(svd_pic$d[1:k]) %*% t(svd_pic$v[, 1:k]) 
  image(t(approx_k), col = grey(seq(0, 1, length = 256))) 
  title(main = paste0("k = ", k, ", gespeicherte Zahlen: ", 2*n_pic*k + k,
                      ", ca ", round((2*n_pic*k + k)/n_pic^2*100), "% vom Original"),
                      cex.main = 0.5) }
