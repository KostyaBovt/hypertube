vagrant up
vagrant ssh -- -t 'cd /vagrant/hyper; make; ./start.sh'
cd streaming
npm install
npm start &
