\name{basiliskNews}
\title{basilisk News}
\encoding{UTF-8}

\section{Version 1.2.0}{\itemize{
\item Added support for different Conda channels in the \code{BasiliskEnvironment()} constructor.

\item Added locking to \code{setupBasiliskEnv()} for safe parallel construction of environments. 

\item Ensure that environments are always activated before use in \code{useBasiliskEnv()}.

\item Support installation from Python package directories on the file system.
}}

\section{Version 1.0.0}{\itemize{
\item
New package \pkg{basilisk}, for integration of Python in Bioconductor packages.
}}

