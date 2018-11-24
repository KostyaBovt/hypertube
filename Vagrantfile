# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://vagrantcloud.com/search.
  config.vm.box = "ubuntu/trusty64"


  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # NOTE: This will enable public access to the opened port
  # config.vm.network "forwarded_port", guest: 80, host: 8080
  config.vm.network "forwarded_port", guest: 8080, host: 8080
  config.vm.network "forwarded_port", guest: 5432, host: 5433

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine and only allow access
  # via 127.0.0.1 to disable public access
  # config.vm.network "forwarded_port", guest: 80, host: 8080, host_ip: "127.0.0.1"

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  #  config.vm.provider "virtualbox" do |vb|
      # Display the VirtualBox GUI when booting the machine
  #    vb.gui = true

      # Customize the amount of memory on the VM:
      # vb.memory = "1024"
  #  end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.
  # config.vm.provision "shell", inline: <<-SHELL
  #   apt-get update
  #   apt-get install -y apache2
  # SHELL

    config.vm.provision "shell", inline: <<-SHELL

        wget http://ftp.gnu.org/gnu/make/make-4.1.tar.gz
        tar xvf make-4.1.tar.gz
        cd make-4.1/
        ./configure
        make
        sudo make install
        cd ..
        rm -rf make-4.1.tar.gz make-4.1
        echo "export PATH=/usr/local/bin:$PATH" >> ~/.bashrc
        source ~/.bashrc

        wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
        sudo dpkg -i erlang-solutions_1.0_all.deb
        rm erlang-solutions_1.0_all.deb

        sudo add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -sc)-pgdg main"
        wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -

        sudo apt-get update

        sudo apt-get --yes --force-yes install esl-erlang
        sudo apt-get --yes --force-yes install git
        sudo apt-get --yes --force-yes install postgresql-10
        sudo apt-get --yes --force-yes install libgd-dev
        sudo apt-get --yes --force-yes install libwebp-dev

        sudo locale-gen en_US en_US.UTF-8 hu_HU hu_HU.UTF-8
        sudo dpkg-reconfigure locales

        sudo sed -i "s/localhost/*/g" /etc/postgresql/10/main/postgresql.conf
        sudo sed -i "s/#listen/listen/g" /etc/postgresql/10/main/postgresql.conf
        echo "host Hypertube Hypertube 10.0.2.2/32 md5" | sudo tee --append /etc/postgresql/10/main/pg_hba.conf
        sudo /etc/init.d/postgresql restart

        sudo su - postgres
        sudo su postgres -c "createuser vagrant -s"
        sudo su vagrant -c  "createdb"
        sudo su vagrant -c  "psql < /vagrant/Hypertube.sql"

    SHELL

end
