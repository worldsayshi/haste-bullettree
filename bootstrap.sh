#!/usr/bin/env bash

set -e

apt-get update
cd /home/vagrant/
wget https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
cd /
sudo tar xvf /home/vagrant/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
sudo /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs

apt-get install -y libgmp-dev libz-dev libbz2-dev git inotify-tools nodejs npm

ln -s /usr/bin/nodejs /usr/bin/node


cabal update
cabal install cabal cabal-install

cd /home/vagrant
echo 'export PATH=/home/vagrant/.cabal/bin:$PATH' > .bash_profile

git clone https://github.com/valderman/haste-compiler.git
git clone https://github.com/worldsayshi/haste-perch.git

cabal install ./haste-compiler ./haste-perch
cd haste-compiler
haste-boot --force --local

cd ../haste-perch
haste-cabal install
haste-cabal install lens-family
