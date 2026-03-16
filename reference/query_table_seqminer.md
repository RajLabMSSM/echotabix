# Query tabular: seqminer

**Limitations of seqminer**:

- Doesn't work with remote files.

- Assumes header column always starts with "#" (which is often
  incorrect) without the option for the user to specify otherwise. The
  only way that summary statistics can be used with seqminer is if they
  adhere to this convention, or were indexed using seqminer's
  [tabix.createIndex](https://rdrr.io/pkg/seqminer/man/tabix.createIndex.html)
  function.

- Maintainers are unresponsive to requests for bug fixes.

**Advantages of seqminer**:

- [tabix.read.table](https://rdrr.io/pkg/seqminer/man/tabix.read.table.html)
  automatically converts query results to data.frame format (though this
  can now also be done by
  [scanTabix_to_dt](https://rajlabmssm.github.io/echotabix/reference/scanTabix_to_dt.md)).

seqminer appears to be maintained to some degree (based on the latest
commits), but the maintainers have been unresponsive to bug reports for
years. This limits the consistent usability of seqminer.

## Usage

``` r
query_table_seqminer(target_path, target_index, query_granges, verbose = TRUE)
```

## Source

[GitHub Issues: coordinate order
error](https://github.com/zhanxw/seqminer/issues/25)

[GitHub Issues: remote file
error](https://github.com/zhanxw/seqminer/issues/20)

[Lab contact details for `seqminer`
maintainer](https://www.utsouthwestern.edu/labs/zhan/contact/)

[`seqminer` publication](https://pubmed.ncbi.nlm.nih.gov/26394715/)

[`seqminer2` publication (same package and GitHub repo, just
updated)](https://pubmed.ncbi.nlm.nih.gov/32756942/)

## Arguments

- target_path:

  Path to tabix file.

- target_index:

  Tabix index file for `target_path`.

- query_granges:

  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object to be used for querying the `target_path` file. Alternatively,
  can be variant-level summary statistics to be converted into a
  [GRanges](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object by
  [construct_query](https://rajlabmssm.github.io/echotabix/reference/construct_query.md).

- verbose:

  Print messages.
