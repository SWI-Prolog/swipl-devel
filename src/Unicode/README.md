The files here generate ../pl-umap.c. Before running one must obtain the
following Unicode data files from the Unicode distribution:

	* UnicodeData.txt
	* DerivedCoreProperties.txt
	* BidiMirroring.txt
	* EastAsianWidth.txt
	* PropList.txt

Then run

	% swipl prolog_syntax_map.pl

We used the Unicode 17.0.0 files
