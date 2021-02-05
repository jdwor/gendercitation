
The `gendercitation` repository is a collection of R files for carrying
out analyses of gender balance in reference lists. The files carry out
the procedure described in [“The extent and drivers of gender imbalance
in neuroscience reference lists”](https://www.nature.com/articles/s41593-020-0658-y), by
Dworkin, Linn, Teich, Zurn, Shinohara, and Bassett.

The files are designed to analyze data drawn from the Web of Science
database. The procedure for downloading WOS data can be found in the
file “Step0\_PullingWOSdata.pdf”. Data drawn from other sources will
need to be standardized to the WOS format in order to use the provided R
code.

After downloading data, the R files can be applied beginning with
“Step1\_CleanWOSFiles.R”. This code is not fully automated, so files
should be run interactively, and code should be edited when necessary (I
have attempted to point out likely instances within the text).

The code is currently in progress, and will be updated as new steps
become available. Feel free to reach out to
jordan.dworkin\[at\]nyspi\[dot\]columbia\[dot\]edu or
jordandworkin\[at\]gmail\[dot\]com if you have any questions about the
code.
