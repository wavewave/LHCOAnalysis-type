#!/bin/bash 

cabal install transformers
cabal install hscolour
# libghc-hscolour-dev libghc-hstringtemplate-dev gtk2hs-buildtools libghc-gtk-dev libghc-gtk-doc 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install --force-reinstalls ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

# for dep installation
$HOME/.cabal/bin/build bootstrap --config=build.conf

# for documentation of dep packages
$HOME/.cabal/bin/build haddockboot --config=build.conf 

# for documentation of this package
cabal install  --enable-documentation
cabal haddock --hyperlink-source
cabal copy 

# upload documentation
tar cvzf LHCOAnalysis-type.tar.gz $HOME/.cabal/share/doc/LHCOAnalysis* 
echo $CR | curl --digest -T LHCOAnalysis-type.tar.gz -K - $SRVRURL 

# this is needed for checking
cabal install --enable-tests


