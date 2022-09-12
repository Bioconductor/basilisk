# Managing Python environments within Bioconductor

|Environment|Status|
|---|---|
|BioC-release|[![Release OK](https://bioconductor.org/shields/build/release/bioc/basilisk.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/basilisk/)|
|BioC-devel|[![Devel OK](https://bioconductor.org/shields/build/devel/bioc/basilisk.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/basilisk/)|

**basilisk** provides a standardized mechanism for handling Python dependencies within Bioconductor packages.
It does so by automatically provisioning and managing one or more Conda environments per BioC package,
ensuring that the end-user is not burdened with the responsibility of meeting any Python-based `SystemRequirements`.
We integrate with **reticulate** to allow intuitive calling of Python code within R,
with additional protection to ensure that multiple Python environments can be called within the same R session.

Most "users" of this package are expected to be Bioconductor package developers;
end users should not need to interact with the **basilisk** machinery, all going well.
Users can follow the typical installation process for Bioconductor packages:

```r
install.packages("BiocManager") # if not already installed
BiocManager::install("basilisk")

# Bioconductor package developers may prefer to use the devel version:
BiocManager::install("basilisk", version="devel") 
```

The [vignette](https://bioconductor.org/packages/release/bioc/vignettes/basilisk/inst/doc/motivation.html) provides instructions on how to adapt a client package to use **basilisk**.
A minimal example is provided in the [`inst/example`](inst/example/) directory and contains code like:

```r
# Provision an environment.
my_env <- BasiliskEnvironment(envname="my_env_name",
    pkgname="name.of.package",
    packages=c("pandas==0.25.1")
)

# Run reticulate code using that environment.
res <- basiliskRun(env=my_env, fun=function(args) {
    out <- reticulate::import("pandas")
    # Do something with pandas
    return(some_r_object)
})
```

Detailed documentation for each function is available through the usual methods, i.e., `?basiliskRun`.
See the [Bioconductor landing page](https://bioconductor.org/packages/basilisk) for more links;
some examples of **basilisk** client packages include [**crisprScore**](https://bioconductor.org/packages/crisprScore) and [**velociraptor**](https://bioconductor.org/packages/velociraptor).

Bugs can be posted to the [Issues](https://github.com/LTLA/basilisk/issues) of this repository.
Pull requests are welcome.
