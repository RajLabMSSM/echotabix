#' VCF to \link[data.table]{data.table}
#' 
#' Converts a \pkg{VariantAnnotation} \link[VariantAnnotation]{CollapsedVCF}
#' or \link[VariantAnnotation]{ExpandedVCF} object 
#' to a \link[data.table]{data.table}. 
#'  
#' Adapted from \code{MungeSumstats:::vcf2df}, which was adapted from
#' a Gist by 
#' \href{https://gist.github.com/zhujack/849b75f5a8305edaeca1001dfb9c3fe9}{
#' zhujack}.
#' 
#' @param vcf Variant Call Format (VCF) file imported into R 
#' as a \pkg{VariantAnnotation} 
#' \link[VariantAnnotation]{CollapsedVCF} or 
#' \link[VariantAnnotation]{ExpandedVCF} object. 
#' @param expand Expand data into multiple columns using
#'  \link[VariantAnnotation]{expand}. 
#' @param verbose Print messages. 
#'  
#' @return A \link[data.table]{data.table}.
#' 
#' @export
#' @importFrom VariantAnnotation info header geno expand
#' @importFrom Biostrings strsplit
#' @importFrom utils type.convert 
#' @importFrom data.table data.table
#' @examples
#' vcf_file <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'                         package = "echotabix") 
#' vcf <- VariantAnnotation::readVcf(file = vcf_file)
#' vcf_dt <- echotabix::vcf_to_dt(vcf = vcf)
vcf_to_dt <- function(vcf,
                      expand = TRUE,
                      verbose = TRUE) { 
    
    messager("Converting",
             if(expand) "expanded" else "collapsed",
             "VCF to data.table",v=verbose)
    #### Check there's actually data to parse
    if(nrow(vcf)==0){
        messager("Cannot convert to data.table if nrow(vcf)==0.",
                 "Returning empty VCF instead.",v=verbose)
        return(vcf)
    }
    #### Parser ####
    v2df <- function(x, 
                     ...) {
        ## Function to parse ANN column in to a dataframe
        .anncols = function(anncol,
                            headerstring) {
            anncols = Biostrings::strsplit(
                sub("Functional annotations: '",'',
                    headerstring),' \\| ')[[1]]
            dfannempty = data.frame(matrix(vector(), 0, length(anncols),
                                           dimnames=list(c(), anncols)),
                                    stringsAsFactors=FALSE)
            dd <- lapply(lapply(anncol,`[`,1),
                         function(x){Biostrings::strsplit(x,'\\|')[[1]]})
            ncls <- max(unlist(lapply(dd, length)))
            
            yy = data.frame(suppressWarnings(
                do.call(rbind,
                        c(dfannempty[seq(1,ncls)], dd))),
                            stringsAsFactors=FALSE)
            yy = data.frame(lapply(yy,utils::type.convert))
            colnames(yy) = paste("ANN",anncols[seq(1,ncls)],sep="..")
            return(yy)
        }
        
        
        df = as.data.frame(x@rowRanges) 
        df = cbind(df, 
                   as.data.frame(VariantAnnotation::info(x))
                   ) 
        if ( any(c('ANN', 'EFF') %in% names(VariantAnnotation::info(x))) ) {
            ann = c('ANN', 'EFF')[ c('ANN', 'EFF') %in% names(
                VariantAnnotation::info(x)) ][1]
            dfann = .anncols(
                df$ANN, 
                VariantAnnotation::info(
                    VariantAnnotation::header(x)
                )[ann, ]$Description)
            df = df[, colnames(df) != ann]
            df = cbind(df, dfann)
        }
        geno_data <- VariantAnnotation::geno(x)
        SNPs <- rownames(geno_data[[1]])
        n  = names(geno_data)
        tmp = lapply(n, function(col) {
            return(as.data.frame(geno_data[[col]]))
        })
        ncols = unlist(lapply(tmp, FUN = ncol))
        tmp = do.call(cbind, tmp)
        rownames(tmp) <- NULL 
        colnames(tmp) = paste(rep(n, times = ncols), colnames(tmp),
                              sep = "_")
        df = cbind(df, tmp)
        df[vapply(df, is.list, FUN.VALUE = logical(1))] <- 
            apply(df[vapply(df, is.list, FUN.VALUE = logical(1))], 2, 
                  function(x) { 
                      unlist(lapply(x, paste, sep=",", collapse=";"))  } ) 
        #### Add SNPs back in ####
        df <- cbind(SNP=SNPs, df)
        #### Remove duplicate rows ####
        df <- unique(df)
        return(df)
    }
    
    if (expand) { 
        dat <- v2df(VariantAnnotation::expand(x = vcf)) 
    } else { 
        dat <- v2df(x = vcf) 
    }
    #### Convert to data.table ####
    dat <- data.table::data.table(dat)
    return(dat)
}
