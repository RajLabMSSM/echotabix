% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_query.R
\name{construct_query}
\alias{construct_query}
\title{Construct query}
\usage{
construct_query(
  query_chrom = NULL,
  query_start_pos = NULL,
  query_end_pos = query_start_pos,
  query_dat = NULL,
  query_chrom_col = "CHR",
  query_start_col = "POS",
  query_end_col = query_start_col,
  query_snp_col = "SNP",
  standardise_colnames = FALSE,
  as_blocks = TRUE,
  style = c("NCBI", "UCSC"),
  samples = character(),
  as_string = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{query_chrom}{Which chromosome to query (e.g. 1 or "chr1").}

\item{query_start_pos}{The first position to query.}

\item{query_end_pos}{The last position to query.}

\item{query_dat}{Variant-level summary statistics.}

\item{query_chrom_col}{Name of the chromosome column in 
\code{query_dat} (e.g. "CHR").}

\item{query_start_col}{Name of the starting genomic position
column in \code{query_dat} (e.g. "POS","start").}

\item{query_end_col}{Name of the ending genomic position
column in \code{query_dat} (e.g. "POS","end"). 
Can be the same as \code{query_start_col} when \code{query_dat}
 only contains SNPs that span 1 base pair (bp) each.}

\item{query_snp_col}{Name of the RSID or variant ID column (e.g. "SNP").}

\item{standardise_colnames}{Automatically rename all columns to a standard 
nomenclature using \link[MungeSumstats]{standardise_header}.}

\item{as_blocks}{If \code{TRUE} (default), will query a single range 
per chromosome that covers all ranges requested plus anything in between.}

\item{style}{Style to return \link[GenomicRanges]{GRanges} in:
\itemize{
\item{"NCBI": }{Chromosome format: 1 (default)}
\item{"UCSC": }{Chromosome format: "chr1"}
}
Uses \link[GenomeInfoDb]{seqlevelsStyle}.}

\item{samples}{[Optional] Sample names to subset the VCF by. 
If this option is used, the \link[GenomicRanges]{GRanges} object will be
converted to a \link[VariantAnnotation]{ScanVcfParam} for usage by 
\link[VariantAnnotation]{readVcf}.}

\item{as_string}{Convert a \link[GenomicRanges]{GRanges} object 
to a concatenated string of coordinates 
(e.g. "chr4:70000-90000,chr10:200-150001"). 
This can be used for specifying
which regions you want to query (e.g. when using \code{tabix}).
Uses \link[echotabix]{granges_to_string}.}

\item{verbose}{Print messages.}
}
\value{
\link[GenomicRanges]{GRanges} or
\link[VariantAnnotation]{ScanVcfParam} object.
}
\description{
Convert a \link[data.table]{data.table} containing variant-level 
summary statistics into a \link[GenomicRanges]{GRanges} object to be used 
for querying tabix-indexed files. Depending on the parameters supplied, 
the resulting \link[GenomicRanges]{GRanges} object can contain one or more
ranges, across one or more chromosomes.
}
\section{Parameters sets}{

If you supply the \emph{Set 1} of parameters, you do not need to supply
the \emph{Set 2} parameters (and vice versa). 
\emph{Extra Parameters} apply to both \emph{Set 1} and \emph{Set 2}.
\itemize{
\item{Set 1: }{
\code{query_chrom},  \code{query_start_pos},  \code{query_end_pos}
}
\item{Set 2: }{
 \code{query_dat},  \code{query_chrom_col}, 
  \code{query_start_col},  \code{query_end_col},  \code{as_blocks}
}
\item{Extra Parameters: }{
  \code{style},  \code{samples}, \code{as_string},  \code{verbose}
}
}
}

\examples{
gr <- echotabix::construct_query(query_dat=echodata::BST1)
}
