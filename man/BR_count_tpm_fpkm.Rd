% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BR_count_tpm_fpkm.R
\name{BR_count_tpm_fpkm}
\alias{BR_count_tpm_fpkm}
\title{Title 这是一个可以把counts值转换成TPM值或者FPKM值的函数}
\usage{
BR_count_tpm_fpkm(exp_data, method, genome = "hg19")
}
\arguments{
\item{exp_data}{需要输入一个counts矩阵，行名是基因名，
列名是样本名，不知道啥样的可以看看示例数据head(tcga_TestData)}

\item{method}{目前就支持两种，将counts转成TPM或者FPKM，二选一}

\item{genome}{可以使用hg19基因组或者hg38基因组，
取决于你上游处理用的啥，其实差别不是太大}
}
\value{
返回的是一个TPM或者FPKM矩阵，下面得例子就不举了，可以用示例数据试试
}
\description{
没啥好描述的
}
\examples{
x <- c("alfa,bravo,charlie,delta")

}
