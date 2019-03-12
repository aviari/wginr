# Asdog sample session

This is a Asdog sample session
using purely artificial data.
Results are therefore biologically irrelevant
but it will give you a quick introduction
to the scripts calling sequence.

## Files

- **ref.fst** : reference genome in fasta format
- **ref.mappable.bed** : genome mappable regions (0-based bed format)

Also not strictly necessary it is a good practice to provide
a mappable bed file. It  indicates which regions of the chromosomes
should be considered for the analysis. You may at least provide
regions excluding telomeres and centromeres but it is also a
good idea to remove regions that are duplicated along the
standard genome.
The bed file we currently use for hg19 is provided
here (hg38 will come soon).

You indicates this file with options or simply by naming it
\<reference_name\>**.mappable.bed** where \<reference_name\>
is the name (without extension) of the genome reference file.

- **tumor.bam** : tumor Bam file
- **normal.bam** : paired normal Bam file

These are simply the tumor and paired normal Bam files
produced by your favorite aligner (of course on the same
genome reference as above).

## Reference genome conversion

The reference genome (fasta) should, once for all, be converted
into the **basta** format.
a **basta** file is merely the same as the fasta file plus a header allowing
indexed access. It can be (bgzf) compressed. It has about the same size 
as the fasta file (uncompressed) and about 700 Mb in compressed form
for the human genome.

```sh
fasta2basta -z ref.fst ref.bst
```

## Bam to Baf conversion

Each bam file should be converted to **baf** format.
A **baf** file contains the counts for each allele at each position.
It can be (bgzf) compressed.
Its size depends upon the cover but is usually around 3Gb when compressed,
for a typical human genome.

```sh
bam2baf -z normal.bam normal.baf
bam2baf -z tumor.bam tumor.baf
```

> important: the bam file should have been sorted
> for the conversion to work

We are now ready to run the asdog scripts

## Building the GC model

The first step is to build the GC correction model for tumor
(usually we do not need to build the model for the paired normal).
This is done by the ```asdog.gcmodel.r``` script.

> In the following command line, only the first line is mandatory.
> The other lines will just change the default parameters due to the artificial
> nature of this demo sample (very small size).
> Don't use these modified default values in your own scripts 
> (unless you know what you are doing)

```sh
asdog.gcmodel.r --base=tumor --ref=ref \
   --chrs=1 --gcmodel.large.winsize=0  \
   --gcmodel.sample.gcbins=0 \
   --gcmodel.regions.minreg=1000 \
   --gcmodel.regions.binsize=1000 \
   --gcmodel.sample.size=1000
```

The ```--base``` option specifies the tumor baf file
and the ```--ref```indicates the reference genome used.

The output is a \<base\>**.gcmodel.rds** rds file readable
in R (with ```readRDS```) and therefore by other scripts.
The script also produces a \<base\>**.gcmodel.pdf** file
giving report on the GC model.

> important note
> the ```--chrs``` option indicates the indexes (in \<ref\>.fst)
> of the chromosomes to study. The default is **1:22**. This means:
> 1) the first 22 chromosomes in your reference file should be the standard 22 autosomes
> 2) the X/Y chromosomes are, by default, excluded.
> the sex chromosomes are not (yet) handled as special cases in asdog (we will
> add this in next version), because they may be either absent (in females) or
> haploid (in males). So if you add them, Asdog should report an
> homozygous deletion of Y in females and an haploid X and Y in males.
>
> However some caveats may appear :
> 1) the heterozygous positions in X and Y in normal cannot be determined in males
> (and some scripts may crash).
> 2) due to similar regions between X and Y, some computations may not be correct
> (e.g. not perfect Y deletion in females).
> The same problem holds for the mapper either.

## Correcting read counts

Now we will use this GC model to compute corrected read counts
(on tumor).

```sh
asdog.gccorrect.r --gcmodel=tumor.gcmodel
```

Note that we just have to specify the GC model rds file,
not the baf nor reference files. This is because the location of
these files have been kept with the model. If, for any reason,
you moved these files around in between the calls, then you should
specify them again.

The output is a \<base\>**.gccorrect.rds** rds file.
The script also produces a \<base\>**.gccorrect.pdf** file
giving report on the GC correction.

## Building the Ploidy model

We now can build the Ploidy model to estimate the tumour
contamination (cellularity) and ploidy.

Here we have to specify the (previously computed) 
GC corrected tumour rds file as
well as the baf file for the paired normal. This is because
the script will use heterozygous positions on normal to compute
allelic frequencies in tumour. From the values of read counts and 
allelic frequencies in tumour it will built the tumour ploidy model.

```sh
asdog.plmodel.r --normal=normal --gccorrect=tumor.gccorrect
```

The output is a \<base\>**.plmodel.rds** rds file.
The script also produces a \<base\>**.plmodel.pdf** file
giving report on the Ploidy model.

Note: it is also possible to run the ```asdog.plmodel.r```script
with a previously computed Ploidy model (with ```--plmodel```option)
in order the recompute a new model based on a previous one
by changing some of the parameters.

## Segmenting chromosomes

Finally we can segment the chromosomes using the corrected read counts
and the ploidy model.

```sh
asdog.segment.r --gccorrect=tumor.gccorrect --plmodel=tumor.plmodel
```

The output is a \<base\>**.segment.rds** rds file (readable in R).
The script also produces:
- a \<base\>**.segment.rcaf.txt.gz** file containg segments based on 
copy numbers **and** allelic frequencies. this is useful for detecting
LOH regions without copy number change.
- a \<base\>**.segment.rrc.txt.gz** file containg segments based on
copy numbers only. this segmentation has a finer grain as the previous
one (that was based on heterozygous sites only).
- a \<base\>**.plmodel.pdf** file giving report on the segmentation.

# Run as test

You may run this sample session by :

```sh
run_test
```

and cleanup everything by :

```sh
run_cleanup
```
