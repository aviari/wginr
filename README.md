# WGInR (Asdog/Aspup)

WGInR ('Whole Genomes In R') are C and R tools 
from the Lyon Cancer Bioinformatics Facility
to manipulate Whole Genomes in R.

(this includes 'asdog' and 'aspup' scripts
 for Copy Number Analysis)

## Distribution

- Csrc    : C sources (for samtools, bam2baf, ...)
- Rsrc    : R sources (lx, xlx, asdog, ...)

- ports   : precompiled binaries for various ports
- scripts : shell scripts
- docs    : some (rudimentary) documentation

## Prerequisites

this distribution contains sources as well as precompiled binaries and R libraries
for Linux (64 bits) and MacOSX (≥ 10.12). So you may not need to recompile
on these ports.

### required software for compiling

 - gcc compiler     version >= 4.6          ```gcc --version```
 - gnu make         version >= 3.81         ```make --version```
 
### required software for running
 
 - R                version >= 3.0.1        ```R --version```
 - latex            version >= 2010         ```pdflatex --version```

note: texlive (including pdflatex) is usually installed with Rstudio
so you don't need to worry about the last requirement if Rstudio in installed.

you can check your config by running :

```
scripts/checkconfig
```

## Compilation & Installation

On linux or MacosX you may not need to recompile sources
and jump directly to [Running](#Running).

note: you do **not** need to be root to compile and install,
since the installation will be done in user space only.

At the root of distribution type:

```
make
```

if everything runs fine then type:

```
make install
```

R will try to install packages from CRAN or the local directory
Rsrc/packages/source.

In case of trouble, see 'Optional Setup' below

Finally some (rudimentary) tests can be run by:

```
make test
```

In addition :
```
make clean     cleanup temporary installation files
make distclean cleanup and restore initial distrib (dev only)
```

(note for devs: please do a 'make distclean' before commiting)

### Compilation troubles

#### R packages directory

If 'make install' complains when installing R packages, this
is probably because you don't have write access to
the system-wide package directory. To install R packages at your
home directory:

(csh)
```
mkdir $home/R
setenv R_LIBS $home/R
```

(bash)
```
mkdir $HOME/R
export R_LIBS=$HOME/R
```

note: you may change the name of the repository to whatever
you prefer (e.g. $home/MyRlibs) as long as you declare it in R_LIBS.

#### R Cran repository

'make install' will possibly download some additional R packages
from CRAN.

The default CRAN repository is : http://cran.univ-lyon1.fr
to change it:

- edit ```Rsrc/Makefile```
- and change ```CRAN_REPOS = http://cran.univ-lyon1.fr ``` to whatever you prefer.

If the CRAN repository cannot be used for any reason
(no internet connection, package has been removed), then 
the installation will proceed from local copies of the packages
located in Rsrc/packages/source. However, it sounds always better
to use the CRAN fresher versions.

In case of trouble with R package installation,
please read: docs/Readme_RPackages.txt

#### curses
curses is needed by some (viewing) utilities in samtools.
if curses is not installed then Makefile in samtools should detect
and skip it (I hope so actually... didn't try yet).

## Running


-----------------
-6- Path setup
-----------------

Add the following directories to your path:

<WGInRRoot>/scripts

<WGInRRoot>/ports/<portname>/bin

with <portname>: 
    x386-linux  :   Linux 64 bits
    x386-darwin :   MacOSx 64 bits 

to know your <portname>, type:
    <WGInRRoot>/scripts/guessport
    
-----------------
-7- Usage
-----------------
tdb... see docs/Readme_Anthony.txt

## Authors

* **Alain Viari** (alain.viari[at]inria.fr)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments


