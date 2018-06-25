# bib <- RefManageR::ReadBib("tests/testthat/bib_files/zotero_better_bibtex.bib")
#
# person_list <- lapply(bib, function(x) x$author)
# person_list <- do.call(c, person_list)
#
# author_display_names <- lapply(
#   person_list
#   , function(x) paste(
#     paste(x[["given"]], collapse = " ")
#     , x["family"]
#   )
# )
#
# author_display_names <- unlist(author_names)
#
#
#
# new_person_list <- person_list
# new_person_list[8] <- person_list[1]
#
# # for(i in length(person_list)) {
#   references_to_update <- which(sapply(bib, function(x) identical(x, bib[author = author_display_names[i]])))
#   if(length(references_to_update) > 0) {
#     author_to_replace <- which(identical(bib[references_to_update]$author, person_list[i]))
#     bib[author = author_display_names[i]]$author[author_to_replace] <- new_person_list[i]
#   }
# # }
#
#
