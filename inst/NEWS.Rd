\name{basiliskNews}
\title{basilisk News}
\encoding{UTF-8}

\section{Version 1.18.0}{\itemize{
\item Switch to the latest Miniforge installer (24.3.0-0) by default.
This is preconfigured to use the conda-forge channel and avoids issues with the non-FOSS licensing of the Anaconda repositories.
Users can switch back to the old Miniconda installer by setting the \code{BASILISK_USE_MINIFORGE=0} environment variable,
but this will likely be deprecated in the next release.

\item Channel specifications in the user's \code{.condarc} are now ignored.
Only the channels by the developer in \code{setupBasiliskEnv()} (e.g., via \code{BasiliskEnvironment}) will be respected,
to improve the consistency of the constructed environments across devices.
}}

\section{Version 1.4.0}{\itemize{
\item Support installation from Python package directories on the file system.

\item Clean Conda package directories during a system installation to reduce disk usage.
}}

\section{Version 1.2.0}{\itemize{
\item Added support for different Conda channels in the \code{BasiliskEnvironment()} constructor.

\item Added locking to \code{setupBasiliskEnv()} for safe parallel construction of environments. 

\item Ensure that environments are always activated before use in \code{useBasiliskEnv()}.
}}

\section{Version 1.0.0}{\itemize{
\item
New package \pkg{basilisk}, for integration of Python in Bioconductor packages.
}}

