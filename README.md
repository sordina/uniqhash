# Uniqhash

Intercept a stream of filenames and output only the ones that have changed.

## Graph

<!--
digraph {
  filename -> name_and_digest;
  filename -> read_if_exists -> digest -> name_and_digest;
  name_and_digest -> cache_delay_one -> if_name_lookup_not_digest;
  name_and_digest -> if_name_lookup_not_digest;
  if_name_lookup_not_digest -> emit_filename;
}
-->

![](images/dot_15703.png)

## Usage

    > filename_lister | uniqhash

## Binaries

* [uniqhash-1.1.0.0-MacOSX-10.7.5-11G63b.zip](http://sordina.binaries.s3.amazonaws.com/uniqhash-1.1.0.0-MacOSX-10.7.5-11G63b.zip)
