# citr 0.2.0

- Zotero/Juris-M library can now be accessed directly via the Better Bib(La)TeX extension (https://github.com/retorquere/zotero-better-bibtex). The document's bibliography file is updated accordingly. See `?insert_citation`.
- bibliography entries are no longer checked for missing fields (suggested by @awmercer, #8)
- If the current document is a child (i.e., included in a larger R Markdown document) the parent file is searched for bibliography paths (suggested and contributed to by @yihui, #7, #10)
- References in addin selection and inserted markdown citations are ordered alphabetically

# citr 0.1.0

Initial release.