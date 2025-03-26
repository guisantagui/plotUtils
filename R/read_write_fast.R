#' Read table faster
#'
#' Wrapper function to data.table::fread, to work as read.table, but faster
#' @param f A path to the file to be loaded
#' @param row.names Integer. The column number where row names are.
#' @param sep Separator between columns. Defaults to `"auto"` (automatic).
#' @param sep2 Separator within columns. If each cell is a vector of values a
#' list column will be returned. Defaults to `"auto"` (automatic).
#'
#' @return A `data.frame` with the contents of the file `f`.
#'
#' @export
read_table_fast <- function(f, row.names = NULL, sep = "auto", sep2 = "auto"){
        df <- data.frame(fread(f, sep = sep, sep2 = sep2))
        if (!is.null(row.names)){
                rownames(df) <- df[, row.names]
                df <- df[, -row.names]
        }
        return(df)
}

#' Write table faster
#'
#' Wrapper function to data.table::fwrite, to work as write.csv, but faster
#' @param df A `data.frame`.
#' @param f A path to the file to be written.
#' @param row.names Logical. If row names should be written in the file.
#' @param col.names Logical. If column names should be written in the file.
#' @param sep Separator between columns. Defaults to `","`.
#' @param sep2 Separator within columns. Defaults to `c("","|","")`. sep2[1] is
#' written at the start of the output field, sep2[2] is placed between each item
#' and sep2[3] is written at the end. sep2[1] and sep2[3] may be any length
#' strings including empty "" (default). sep2[2] must be a single character and
#' (when list columns are present and therefore sep2 is used) different from
#' both sep and dec.
#' @param quote When "auto", character fields, factor fields and column names
#' will only be surrounded by double quotes when they need to be; i.e., when the
#' field contains the separator sep, a line ending \n, the double quote itself
#' or (when list columns are present) sep2[2] (see sep2 below). If FALSE the
#' fields are not wrapped with quotes even if this would break the CSV due to
#' the contents of the field. If TRUE double quotes are always included other
#' than around numeric fields, as write.csv.
#'
#' @return A `data.frame` with the contents of the file `f`.
#'
#' @export
write_table_fast <- function(df,
                             f,
                             row.names = T,
                             col.names = T,
                             sep = ",",
                             sep2 = c("","|",""),
                             quote = "auto"){
        if(row.names){
                rn <- rownames(df)
                df <- data.table(df)
                df[, V1 := rn]
                setcolorder(df, c("V1", setdiff(names(df), "V1")))
        }else{
                df <- data.table(df)
        }
        fwrite(df,
               f,
               col.names = col.names,
               sep = sep,
               sep2 = sep2,
               quote = quote)
}
