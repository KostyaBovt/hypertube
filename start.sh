vagrant up
vagrant ssh -- -t 'cd /vagrant/hyper; ./start.sh'
cd streaming
npm install
npm start &
