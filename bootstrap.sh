#!/usr/bin/env bash

set -e

apt-get update
cd /home/vagrant/
wget https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
cd /
sudo tar xvf /home/vagrant/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz
sudo /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs

apt-get install -y libgmp-dev libz-dev libbz2-dev git inotify-tools nodejs npm

ln -fs /usr/bin/nodejs /usr/bin/node


su - vagrant -c 'cabal update'
su - vagrant -c 'cabal install cabal cabal-install'

cd /home/vagrant
echo 'export PATH=/home/vagrant/.cabal/bin:$PATH' > .bash_profile

rm -rf haste-compiler
rm -rf haste-perch
su - vagrant -c 'git clone https://github.com/valderman/haste-compiler.git'
su - vagrant -c 'git clone https://github.com/worldsayshi/haste-perch.git'

su - vagrant -c 'cabal install ./haste-compiler ./haste-perch'
cd haste-compiler
su - vagrant -c 'haste-boot --force --local'

cd ../haste-perch
su - vagrant -c 'haste-cabal install'
su - vagrant -c 'haste-cabal install lens-family'
