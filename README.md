# WGInR (Asdog/Aspup)

WGInR ('Whole Genomes In R') are C and R tools 
from the Lyon Cancer Bioinformatics Facility
to manipulate Whole Genomes in R.

(this includes 'asdog' and 'aspup' scripts
 for Copy Number Analysis)

clone as

```sh
  git clone https://github.com/aviari/wginr.git
```

## Distribution

- Csrc    : C sources (bam2baf, fasta2basta, ...)
- Rsrc    : R sources (lx, xlx, asdog, ...)

- ports   : will hold binaries for your port
- scripts : shell scripts
- docs    : some additional documentation
- docker  : docker recipe (see below)

## Prerequisites

### Required software for compiling  and running

 - gcc compiler     version >= 4.6          ```gcc --version```
 - gnu make         version >= 3.81         ```make --version```
 - R                version >= 3.0.1        ```R --version```
 - pdflatex         version >= 2010         ```pdflatex --version```

note: pdflatex is needed for producing pdf reports. if not installed the
scripts should (I actually hope so...) detect and skip pdf reporting
(other output are still produced).

note: texlive (including pdflatex) is usually installed with Rstudio
so you don't need to worry about the last requirement if Rstudio is installed.

you can check your config by running :

```sh
scripts/checkconfig
```

### Alternative Docker recipe

Alternatively, a docker recipe is also available as [docker/Dockerfile](docker/Dockerfile)
see [here](docs/Docker.md) for more information.

### Required authorizations

You do **not** need to be root to compile and install,
since the installation will be done in user space. 
However on some system R will try to install the packages
in a system-wide location (for access by all users).
You should check that you have a write-access to these
locations and, if not, instruct R to install in a local directory.

To check this :

```sh
scripts/checkrlibs
```

if the scripts output looks like this 

> current R library path: 'PATH' write ok

then everything is ok. else follow the scripts instructions,
eg:

> current R library path: 'PATH' no write autorization
>
> you should set the R_LIBS environment variable :
>
>(csh)
```sh
  mkdir $home/R
  setenv R_LIBS $home/R
```
>(bash)
```sh
  mkdir $HOME/R
  export export R_LIBS=$HOME/R
```
note: you may change the name of the R_LIBS repository to whatever you prefer
       (e.g. $home/MyRLibs) as long as you declare it in R_LIBS

## Compilation & Installation

At the root of distribution type:

```sh
make
```

if everything runs fine then type:

```sh
make install
```

R will try to install packages from CRAN or the local directory
```Rsrc/packages/source```

In case of trouble, see [Compilation/Installation troubles](#Compilation-Installation-troubles) below

All other (C) binaries will be installed in: ```ports/\<portname\>/bin```

where \<portname\> is your current port:

- x386-linux  :   Linux 64 bits
- x386-darwin :   MacOSx 64 bits 

to know your current \<portname\>, type:

```sh
scripts/guessport
```

Finally some (rudimentary) tests can be run by:

```sh
make test
```

In addition :

```sh
make clean     cleanup temporary installation files
make distclean cleanup and restore initial distrib (dev only)
```

note for devs: please do a ```make distclean``` before commiting :smirk:

### Compilation Installation troubles

#### R Cran repository

'make install' will possibly download some additional R packages
from CRAN.

The default CRAN repository is : http://cran.irsn.fr
to change it, set the environment variable CRAN_REPOS
to whatever you prefer.

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

### Path setup

Add the following directories to your path:

- ```<wginr_root>/scripts```
- ```<wginr_root>/ports/<portname>/bin```

### R_LIBS setup

finally if you had to set the ```R_LIBS``` environment variable
during installation, you will also need to set it before running
scripts.

Everything could be added in your ```.cshrc/.bashrc``` file.
The following script may help you to setup this

```
scripts/showrc
```

## Usage & Documentation

### Asdog
Asdog (CNV analysis) usage is described in [Asdog_Scripts](docs/Asdog_Scripts.md) 

A sample session, including some explanations about scripts and files, is
available in [samples](samples/README.md).

Aspup is a variant of Asdog designed for then Agilent Oneseq backbone.
It is not documented yet (sorry).

### R libraries (for developpers only)

Documentation for R libraries can be found in their respective directory:

- [lx](Rsrc/lx/inst/doc/lx.pdf)
- [xlx](Rsrc/xlx/inst/doc/xlx.pdf)
- [rbgzf](Rsrc/rbgzf/inst/doc/rbgzf.pdf)
- [thmm](Rsrc/thmm/inst/doc/thmm.pdf)


## Authors

* Alain Viari (alain.viari[at]inria.fr)
* Anthony Ferrari (anthony.ferrari[at]lyon.unicancer.fr)

## License

This project is licensed under the GPL License - see the [LICENSE](LICENSE)
file for details



