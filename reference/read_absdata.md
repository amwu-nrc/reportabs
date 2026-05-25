# Read ABS time series data.

Read ABS time series data from the
[amwudata](https://github.com/amwu-nrc/amwudata) package.

## Usage

``` r
read_absdata(name = NULL, refresh = FALSE, export_dir = tempdir())
```

## Arguments

- name:

  The name, as a string, of the dataset to download.A full list of
  available data is available at https://github.com/amwu-nrc/amwudata

- refresh:

  TRUE to download even if the file already exists

- export_dir:

  The directory in which to save downloaded data. Defaults to a
  temporary directory.

## Value

data
