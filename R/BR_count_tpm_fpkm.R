


#' Title 这是一个可以把counts值转换成TPM值或者FPKM值的函数
#' @description 没啥好描述的
#' @param exp_data 需要输入一个counts矩阵，行名是基因名，
#' 列名是样本名，不知道啥样的可以看看示例数据head(tcga_TestData)
#' @param method 目前就支持两种，将counts转成TPM或者FPKM，二选一
#' @param genome 可以使用hg19基因组或者hg38基因组，
#' 取决于你上游处理用的啥，其实差别不是太大
#'
#' @return 返回的是一个TPM或者FPKM矩阵，下面得例子就不举了，可以用示例数据试试
#' @export
#'
#' @examples
#' x <- c("alfa,bravo,charlie,delta")
#'
#' @importFrom dplyr %>% filter select
#'

BR_count_tpm_fpkm <- function(exp_data,method,genome = "hg19"){


  if (genome == "hg19") {
    gene_length <- hg19
  }

  if (genome == "hg38") {
    gene_length <- hg38
  }

  if (!all(genome %in% c("hg19", "hg38"))) {
    stop("大哥/大姐别乱填，输入正确的参考基因组，hg19或hg38，默认是hg19")
  }


  colnames(gene_length) <- c("gene_name","Length")
  exp_data$gene_name <- rownames(exp_data)
  use_data <- dplyr::inner_join(gene_length,exp_data)

  if (method == "FPKM") {

    result_value <- use_data
    for (i in 3:ncol(use_data)) {
      result <- round((use_data[,i]*1000*1000000)/(use_data[,2]*as.numeric(sum(use_data[,i]))),3)

      result_value[,i] <- result
    }

  }

  if (method == "TPM") {

    result_value <- use_data
    for (i in 3:ncol(use_data)) {
      result <- round((use_data[,i]*1000*1000000)/(use_data[,2]*sum((use_data[,i]*1000/use_data[,2]))),3)

      result_value[,i] <- result
    }

  }


  if (!all(method %in% c("TPM", "FPKM"))) {
    stop("就俩功能，counts转换成TPM或FPKM，二选一，需要别的以后空了再加吧")
  }

  result_value <- result_value %>% dplyr::select(-Length)

  return(result_value)

}






