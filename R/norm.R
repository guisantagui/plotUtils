
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
        return(m_norm)
}
