
#' Quantile normalize
#'
#' Performs quantile normalization in the input data.
#'
#' @param m A matrix or dataframe with the data to be normalized
#' @param axis If normalization will be performed column-wise (default) or
#' row-wise.
#' @param train_means A vector of rank means.
#'
#' @export
quant_norm <- function(m, axis = 2, train_means = NULL) {
        m_class <- class(m)[1]
        if ((!m_class %in% c("data.frame", "matrix")) | any(dim(m) == 0)){
                stop("Invalid input object.", call. = F)
        }
        if (!axis %in% c(1, 2)){
                stop("Invalid axis.", call. = F)
        }
        if (axis == 1) {
                m <- t(m)
        }
        # Get ranks of the matrix
        m_rank <- apply(m, 2, rank, ties.method = "average")

        # Sort the matrix
        m_sort <- apply(m, 2, sort)

        # Calculate row means of the sorted matrix
        if(!is.null(train_means)){
                means <- train_means
        }else{
                means <- rowMeans(m_sort)
        }

        # Create a normalized matrix with the same dimensions
        m_norm <- matrix(0, nrow = nrow(m), ncol = ncol(m))

        for (i in 1:ncol(m)) {
                m_norm[, i] <- means[rank(m[, i], ties.method = "average")]
        }

        if (axis == 1) {
                m_norm <- t(m_norm)
        }
        dimnames(m_norm) <- dimnames(m)
        if (m_class == "data.frame"){
                m <- as.data.frame(m)
        }
        return(m_norm)
}

#' Standarize data
#'
#' Performs z-score standarization.
#'
#' @param m A matrix or dataframe with the data to be normalized
#' @param axis Numeric. `1` or `2`. If normalization will be performed
#' column-wise (`2`, default) or row-wise (`1`).
#' @param scale Logical. If data should be scaled (divided by sd).
#' @param center Logical. If data should be centered (substract the mean).
stand <- function(m, axis = 2, scale = T, center = T){
        m_class <- class(m)[1]
        if ((!m_class %in% c("data.frame", "matrix")) | any(dim(m) == 0)){
                stop("Invalid input object.", call. = F)
        }
        if (!axis %in% c(1, 2)){
                stop("Invalid axis.", call. = F)
        }
        if (!scale & !center){
                warning("No processing will be done", call. = F)
        }
        if (axis == 1){
                m <- t(m)
        }
        if (center){
                m <- apply(m, 2, function(x) x - mean(x))
        }
        if (scale){
                keep <- apply(m, 2, sd) > 0
                if (any(!keep)){
                        warning(sprintf("There are %s with SD = 0. They will be removed.",
                                        c("rows", "columns")[axis]))
                }
                m <- m[, keep]
                m <- apply(m, 2, function(x) x/sd(x))
        }
        if (axis == 1){
                m <- t(m)
        }
        if (m_class == "data.frame"){
                m <- as.data.frame(m)
        }
        return (m)
}
