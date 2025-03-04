
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
#' Performs z-score standarization. Supports standarization when the input
#' matrix cntains `NA`s, computing the standardized matrix by excluding them.
#'
#' @param m A matrix or dataframe with the data to be normalized
#' @param axis Numeric. `1` or `2`. If normalization will be performed
#' column-wise (`2`, default) or row-wise (`1`).
#' @param scale Logical. If data should be scaled (divided by sd).
#' @param center Logical. If data should be centered (substract the mean).
#'
#' @export
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
        na_mask <- is.na(m)
        if (axis == 1){
                m <- t(m)
                na_mask <- t(na_mask)
        }
        if (any(na_mask)){
                warning("The matrix contains NAs. They will be excluded during standardization.", call. = F)
                not_valid_entries <- colSums(!na_mask) <= 1
                if (any(not_valid_entries)){
                        warning(sprintf("%s %s with one or less not NAs observations. Dropped from matrix.",
                                        sum(not_valid_entries),
                                        c("rows", "columns")[axis]))
                }
                m <- m[, !not_valid_entries]
                na_mask <- na_mask[, !not_valid_entries]
        }
        format_m_mask <- function(m, na_mask, fun){
                m_list <- m_lst <- apply(m, 2, fun)
                m <- matrix(NA,
                            nrow = nrow(na_mask),
                            ncol = ncol(na_mask),
                            dimnames = dimnames(na_mask))
                m[!na_mask] <- unlist(m_list)
                return(m)
        }
        if (center){
                m <- format_m_mask(m,
                                   na_mask,
                                   function(x) x[!is.na(x)] - mean(x[!is.na(x)]))
        }
        if (scale){
                keep <- apply(m, 2, function(x) sd(x[!is.na(x)])) > 0
                if (any(!keep)){
                        warning(sprintf("%s %s with SD = 0. Dropped from matrix.",
                                        sum(keep),
                                        c("rows", "columns")[axis]))
                }
                m <- m[, keep]
                na_mask <- na_mask[, keep]
                m <- format_m_mask(m,
                                   na_mask,
                                   function(x) x[!is.na(x)]/sd(x[!is.na(x)]))
        }
        if (axis == 1){
                m <- t(m)
        }
        if (m_class == "data.frame"){
                m <- as.data.frame(m)
        }
        return (m)
}
