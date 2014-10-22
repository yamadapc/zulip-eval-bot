# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "phusion/ubuntu-14.04-amd64"

  cmd = "apt-get update && " \
        "apt-get install -q -y haskell-platform nodejs && " \
        "cabal update && " \
        "cabal install cabal cabal-install && " \
        "cabal install mueval && " \
        "echo 'export PATH=$HOME/.cabal/bin:$PATH' >> $HOME/.bashrc && " \
        "mkdir $HOME/go && echo 'export GOPATH=$HOME/go >> $HOME/.bashrc && " \
        "echo 'alias node=\"nodejs\"' >> $HOME/.bashrc"

  config.vm.provision :shell, :inline => cmd
end
