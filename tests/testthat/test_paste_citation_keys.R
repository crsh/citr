context("Citation keys")

test_that("", {
  expected_list <- "@baumer_markdown_2014, @savage_empirical_2009, and @stodden_legal_2009"
  bibtex_bib <- query_bib("", "bib_files/zotero_better_bibtex.bib", cache = FALSE)
  reference_list <- paste_citation_keys(names(bibtex_bib))
  expect_equal(reference_list, expected_list)

  expected_list <- "@baumer_markdown_2014 and @savage_empirical_2009"
  reference_list <- paste_citation_keys(names(bibtex_bib)[1:2])
  expect_equal(reference_list, expected_list)

  expected_list <- "[@baumer_markdown_2014; @savage_empirical_2009; @stodden_legal_2009]"
  reference_list <- paste_citation_keys(names(bibtex_bib), in_paren = TRUE)
  expect_equal(reference_list, expected_list)
})