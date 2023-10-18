
#' Title
#'
#' @param x 一个字符串
#' @param split 根据特定字符对字符串进行分割
#'
#' @return 一个字符串向量
#' @export
#'
#' @examples
#' x <- c("alfa,bravo,charlie,delta")
#' BR_strsplit(x,split = ",")

BR_strsplit <- function(x, split) {
  strsplit(x, split = split)[[1]]
}


