const axios   = require('axios');


axios({method: 'get', url: 'http://localhost:8080/api/auth/udata', headers: {'Cookie': "x-auth-token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOjEsImxvYyI6bnVsbCwic2NwIjoidXNlciJ9.0erryWwM7OyLwccK_TxGes2x_yciOdTM3hWPjhLrfZ0"}, withCredentials: true})
  .then(function (response) {
    // console.log(response);
    console.log('granred');
  })
  .catch(function (error) {
    // console.log(error);
    console.log('forbiden');
  });
// eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOjEsImxvYyI6bnVsbCwic2NwIjoidXNlciJ9.0erryWwM7OyLwccK_TxGes2x_yciOdTM3hWPjhLrfZ0
// eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOjEsImxvYyI6bnVsbCwic2NwIjoidXNlciJ9.0erryWwM7OyLwccK_TxGes2x_yciOdTM3hWPjhLrfZ0
// eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOjEsImxvYyI6bnVsbCwic2NwIjoidXNlciJ9.0erryWwM7OyLwccK_TxGes2x_yciOdTM3hWPjhLrfZ0