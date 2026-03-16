# Package index

## Convert

Convert files to tabix-indexed format (sort, compress, index).

- [`convert()`](https://rajlabmssm.github.io/echotabix/reference/convert.md)
  : Convert to tabix

- [`convert_and_query()`](https://rajlabmssm.github.io/echotabix/reference/convert_and_query.md)
  : Convert and query

- [`run_bgzip()`](https://rajlabmssm.github.io/echotabix/reference/run_bgzip.md)
  : Run bgzip

- [`run_gunzip()`](https://rajlabmssm.github.io/echotabix/reference/run_gunzip.md)
  : Run gunzip

- [`sort_coordinates()`](https://rajlabmssm.github.io/echotabix/reference/sort_coordinates.md)
  : Sort coordinates

- [`index()`](https://rajlabmssm.github.io/echotabix/reference/index-topic.md)
  : Tabix-index a file

- [`dt_to_vcf()`](https://rajlabmssm.github.io/echotabix/reference/dt_to_vcf.md)
  :

  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) to
  VCF

## Query

Query tabix-indexed local or remote files.

- [`query()`](https://rajlabmssm.github.io/echotabix/reference/query.md)
  : Query tabix
- [`query_table()`](https://rajlabmssm.github.io/echotabix/reference/query_table.md)
  : Query a tabix file
- [`query_vcf()`](https://rajlabmssm.github.io/echotabix/reference/query_vcf.md)
  : Query VCF file
- [`construct_query()`](https://rajlabmssm.github.io/echotabix/reference/construct_query.md)
  : Construct query
- [`scanTabix_to_dt()`](https://rajlabmssm.github.io/echotabix/reference/scanTabix_to_dt.md)
  : scanTabix to data.table

## Read and Parse

Read and parse compressed or VCF files.

- [`read_bgz()`](https://rajlabmssm.github.io/echotabix/reference/read_bgz.md)
  : Read bgz
- [`vcf_to_dt()`](https://rajlabmssm.github.io/echotabix/reference/vcf_to_dt.md)
  : Variant Call Format (VCF) –\> data.table

## Path Construction

Construct and validate file paths for tabix and VCF files.

- [`construct_tabix_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_tabix_path.md)
  : Construct tabix path
- [`construct_vcf_path()`](https://rajlabmssm.github.io/echotabix/reference/construct_vcf_path.md)
  : VCF path

## Genomic Utilities

Genome build conversion, coordinate helpers, and column inference.

- [`liftover()`](https://rajlabmssm.github.io/echotabix/reference/liftover.md)
  : Genome build liftover
- [`granges_to_string()`](https://rajlabmssm.github.io/echotabix/reference/granges_to_string.md)
  : GRanges to string
- [`infer_chrom_type()`](https://rajlabmssm.github.io/echotabix/reference/infer_chrom_type.md)
  : Infer chromosome type
- [`check_delimiter()`](https://rajlabmssm.github.io/echotabix/reference/check_delimiter.md)
  : Check delimiter

## Other

- [`is_vcf()`](https://rajlabmssm.github.io/echotabix/reference/is_vcf.md)
  : Is VCF
