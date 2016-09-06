context("Reference list")

test_that("BibTeX and BibLaTeX", {
  expected_list <- c(
    "Baumer, Cetinkaya-Rundel, Bray, Loi, & Horton (2014). R Markdown: Integrating a reproducible analysis tool into introductory statistics. arXiv preprint arXiv:1402.1894."
    , "Savage & Vickers (2009). Empirical study of data sharing by authors publishing in PLoS journals. PloS one."
    , "Stodden (2009). The legal framework for reproducible scientific research: Licensing and copyright. Computing in Science & Engineering."
  )

  bibtex_bib <- query_bib("", "bib_files/zotero_better_bibtex.bib", cache = FALSE, use_betterbiblatex = FALSE)
  reference_list <- paste_references(bibtex_bib)
  expect_equal(reference_list, expected_list)

  biblatex_bib <- query_bib("", "bib_files/zotero_better_biblatex.bib", cache = FALSE, use_betterbiblatex = FALSE)
  reference_list <- paste_references(biblatex_bib)
  expect_equal(reference_list, expected_list)
})

test_that("Possibly problematic entries", {
  expected_list <- c(
    "Wilson et al. (2014). Best Practices for Scientific Computing. PLoS Biology."
    , "Baumer, Cetinkaya-Rundel, Bray, Loi, & Horton (n.d.). R Markdown: Integrating a reproducible analysis tool into introductory statistics. arXiv preprint arXiv:1402.1894."
    , "Baumer, Cetinkaya-Rundel, Bray, Loi, & Horton (in press). R Markdown: Integrating a reproducible analysis tool into introductory statistics. arXiv preprint arXiv:1402.1894."
    , "Terry M. Therneau & Patricia M. Grambsch (2000). Modeling Survival Data: Extending the Cox Model."
    , "Abstracts from the 2014 Annual Scientific Meeting of the American Psychosomatic Society (2014). Psychosomatic Medicine."
    , "Gast & De Houwer (2013). The influence of extinction and counterconditioning instructions on evaluative conditioning effects. Learning and Motivation."
    , "De Houwer (2011). Evaluative conditioning: A review of procedure knowledge and mental process theories."
  )

  problematic_bib <- query_bib("", "bib_files/problematic_entries.bib", cache = FALSE, use_betterbiblatex = FALSE)
  reference_list <- paste_references(problematic_bib)
  expect_equal(reference_list, expected_list)
})
