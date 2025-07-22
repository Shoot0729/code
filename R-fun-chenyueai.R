a <- data.frame (a1 = c(2,4,6), 
                 a2 = c(3,5,8),
                 a3 = c(4,1,9))

pmin_apply <- function(...){
        input_matrix <- (...)
        args <- list(...)
        input_matrix <- as.matrix(args[[1]])
        result <- apply(input_matrix,1,min) 
        return(result)
}

pmin_for <- function(...) {
        input_matrix <- do.call(cbind, list(...))
        n <- nrow(input_matrix)
        result <- numeric(n)
        for (i in 1:n) {
                result[i] <- min(input_matrix[i, ])
        }
        return(result)
}

b1 <- data.frame(name_b1 =c("文" , "颜" ,"唐", "黄"),score =c(95,96,97,98))


b2 <- data.frame(name_b2 =c("文", "颜"),github =c("uuu1016","yanyutong111"))

`%merge%` <- function(x, y) {
        merged <- merge(x, y, 
                        by.x = "name_b1", 
                        by.y = "name_b2", 
                        all = FALSE)
        rownames(merged) <- merged$name_b1
        merged$name_b1 <- NULL
        merged
}
