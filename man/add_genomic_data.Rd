% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_genomic_data.R
\name{add_genomic_data}
\alias{add_genomic_data}
\title{Merge the metadata and genomic_data by the col_merge.}
\usage{
add_genomic_data(metadata, genomic_data, col_merge, count, time, mutation = T)
}
\arguments{
\item{metadata}{the dataset metadata containing the column col_merge}

\item{genomic_data}{The dataset containing the substitutions, deletions and missing data. The form of this dataset is based on the output of nextclade and differents mutation.}

\item{col_merge}{The name of the column that will be used to merge the data}

\item{count}{the name of the column used to desaggregate the metadata}

\item{time}{the name of the column where the dates are found format = "\%Y-\%m-\%d"}

\item{mutation}{}
}
\value{
The function adds according to col_merge the columns of mutations coming from genomic_data in metadata
}
\description{
Merge the metadata and genomic_data by the col_merge.
Metadata and genomic_data must have the same name of the column that will be used for merge.
}
