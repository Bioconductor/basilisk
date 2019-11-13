# Generating the core lists

The list of core packages in `core_list` is based on the package lists at https://docs.anaconda.com/anaconda/packages/pkg-docs/.
Unfortunately, they don't seem to provide an easy way to actually get the list in this table, so I've just scraped it off the HTML.

```r
dir.create("core_lists", showWarnings=FALSE)
base.url <- "https://docs.anaconda.com/anaconda/packages"

all.os <- c(
    linux ="py3.7_linux-64", 
    macosx="py3.7_osx-64",
    win32 ="py3.7_win-32", 
    win64 ="py3.7_win-64"
)
all.xpath <- c(
    linux ="packages-for-64-bit-linux-with-python-3-7",
    macosx="packages-for-macos-with-python-3-7",
    win32 ="packages-for-32-bit-windows-with-python-3-7",
    win64 ="packages-for-64-bit-windows-with-python-3-7"
)

library(rvest)
for (os in names(all.os)) {
    HTML <- read_html(file.path(base.url, all.os[os]))
    curxpath <- sprintf('//*[@id="%s"]/table', all.xpath[os])
    curnode <- html_nodes(HTML, xpath=curxpath)
    tab <- html_table(curnode)[[1]]

    tab <- tab[!grepl("^_", tab[,1]),]
    write(paste0(tab[,1], "==", tab[,2]), file=file.path("core_lists", os))
}
```
