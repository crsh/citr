context("Search Bib(La)TeX file")

test_that("BibTeX and BibLaTeX", {
  expected_list <- c(
    "Baumer, Cetinkaya-Rundel, Bray, Loi, & Horton (2014). R Markdown: Integrating a reproducible analysis tool into introductory statistics. arXiv preprint arXiv:1402.1894."
    , "Savage & Vickers (2009). Empirical study of data sharing by authors publishing in PLoS journals. PloS one."
    , "Stodden (2009). The legal framework for reproducible scientific research: Licensing and copyright. Computing in Science & Engineering."
  )

  bibtex_bib <- query_bib("2014", "bib_files/zotero_better_bibtex.bib", use_betterbiblatex = FALSE)
  reference_list <- paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[1])

  bibtex_bib <- query_bib("savage", "bib_files/zotero_better_bibtex.bib", use_betterbiblatex = FALSE)
  reference_list <- paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[2])

  bibtex_bib <- query_bib("plos", "bib_files/zotero_better_bibtex.bib", use_betterbiblatex = FALSE)
  reference_list <- paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[2])

  expect_null(query_bib("foo bar", "bib_files/zotero_better_bibtex.bib", use_betterbiblatex = FALSE))
})

test_that("Caching", {
  options(citr.bibliography_cache = NULL)

  bibtex_bib <- query_bib("", bib_file = "bib_files/zotero_better_bibtex.bib", use_betterbiblatex = FALSE)
  expect_is(getOption("citr.bibliography_cache"), "bibentry")

  bibtex_cached <- query_bib("", bib_file = "no_valid_path", use_betterbiblatex = FALSE)
  expect_identical(bibtex_bib, bibtex_cached)

  bibtex_bib <- query_bib("", bib_file = "bib_files/problematic_entries.bib", cache = FALSE, use_betterbiblatex = FALSE)
  expect_false(identical(bibtex_bib, bibtex_cached))
})
