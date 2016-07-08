context("Search Bib(La)TeX file")

test_that("BibTeX and BibLaTeX", {
  expected_list <- c(
    "Baumer, Cetinkaya-Rundel, Bray, Loi, & Horton (2014). R Markdown: Integrating a reproducible analysis tool into introductory statistics. arXiv preprint arXiv:1402.1894."
    , "Savage & Vickers (2009). Empirical study of data sharing by authors publishing in PLoS journals. PloS one."
    , "Stodden (2009). The legal framework for reproducible scientific research: Licensing and copyright. Computing in Science & Engineering."
  )

  bibtex_bib <- citr:::query_bib("2014", "bib_files/zotero_better_bibtex.bib")
  reference_list <- citr:::paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[1])

  bibtex_bib <- citr:::query_bib("savage", "bib_files/zotero_better_bibtex.bib")
  reference_list <- citr:::paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[2])

  bibtex_bib <- citr:::query_bib("plos", "bib_files/zotero_better_bibtex.bib")
  reference_list <- citr:::paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list[2])

  expect_null(citr:::query_bib("foo bar", "bib_files/zotero_better_bibtex.bib"))
})