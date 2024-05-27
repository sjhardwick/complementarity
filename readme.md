# complementarity

This project computes Drysdale's trade complementarity, bias and intensity indexes using trade data from the CEPII BACI database.

## Usage

To compute the indexes, you first need to download the BACI dataset of your choice from the CEPII website: <http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37>

Save the dataset in a subfolder /data/.

The main code file is complementarity.R. You will need to check the file names in lines 9 and 27 to make sure they reflect the BACI version you have downloaded.

## References

Drysdale, P. D. (1967). *Japanese–Australian Trade: An approach to the study of bilateral trade flows*. PhD dissertation. The Australian National University, Canberra. <https://dx.doi.org/10.25911/5d7a271538333>

Drysdale, P. D. and Garnaut, R. (1982). 'Trade Intensities and the Analysis of Bilateral Trade Flows in a Many-Country World: A survey.' *Hitotsubashi Journal of Eonomics 22*(2), pp. 62-84. <https://dx.doi.org/10.15057/7939>

Gaulier, G. and Zignago, S. (2010). BACI: International trade database at the product-level (the 1994-2007 version). CEPII Working Paper No. 2010--23. <http://dx.doi.org/10.2139/ssrn.1994500>
