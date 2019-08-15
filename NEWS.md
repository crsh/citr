# citr 0.3.2

- Fixed bug related to use of absolute file paths that caused errors finding the bibliography files (reported by @bblodfon and @neurotroph, #52)

# citr 0.3.1

- Improves `tidy_bib_file()`-search for reference handles (reported by @Robinlovelace, #48, and @heinonmatti, #49)
- Addin now correctly uses absolute rather than relative paths to update reference files

# citr 0.3.0

- New function `tidy_bib_file()` removes duplicate and unneeded entries from a Bib(La)Tex-file.
- Added `encoding` option to functions that read Bib(La)TeX-files and a corresponding global option. (reported by @sammo3182 #11)
- Error message in `insert_citation()` are printed to the dialog instead of the console (reported by @sammo3182 #11)
- RStudio addin
    - Access to Zotero group libraries (#9)
    - New settings tab

# citr 0.2.0

- Zotero/Juris-M library can now be accessed directly via the Better BibTeX extension (https://github.com/retorquere/zotero-better-bibtex). The document's bibliography file is updated accordingly. See `?insert_citation`.
- bibliography entries are no longer checked for missing fields (suggested by @awmercer, #8)
- If the current document is a child (i.e., included in a larger R Markdown document) the parent file is searched for bibliography paths (suggested and contributed to by @yihui, #7, #10)
- References in addin selection and inserted markdown citations are ordered alphabetically

# citr 0.1.0

Initial release.
