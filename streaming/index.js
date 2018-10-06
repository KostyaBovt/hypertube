const express 	= require('express');
const fs 		= require('fs');
const needle 	= require('needle');
const axios 	= require('axios');
const rax 		= require('retry-axios');
var path 		= require('path');


var cors = require('cors')
var torrentStream = require('torrent-stream');

var API_KEY = 'ec8920bdb431590b48d7b0205e7d6a49';  // API key for themoviedb.org

const port = 3200;

const app = express();
app.use(cors());

app.use(express.static(path.join(__dirname, 'public')));
app.use(express.static('public'));
app.use('/public', express.static(path.join(__dirname, 'public')));

app.use('/video', express.static('/tmp/test_video'));

// ============================== some hardcode values

var locale = 'ru';

// ==============================



// ============================== get films list

app.get('/films', async (request, response) => {
	console.log(request.query);

	limit = request.query.limit;
	page = request.query.page;
	minimum_rating = request.query.minimum_rating;
	query_term = request.query.query_term;
	genre = request.query.genre;
	sort_by = request.query.sort_by;
	order_by = request.query.order_by;

	filters = '';
	if (limit) {
		sign = filters ? "&" : "?";
		filters += sign + 'limit=' + limit;
	}
	if (page) {
		sign = filters ? "&" : "?";
		filters += sign + 'page=' + page;
	}
	if (minimum_rating) {
		sign = filters ? "&" : "?";
		filters += sign + 'minimum_rating=' + minimum_rating;
	}
	if (query_term) {
		sign = filters ? "&" : "?";
		filters += sign + 'query_term=' + query_term;
	}
	if (genre) {
		sign = filters ? "&" : "?";
		filters += sign + 'genre=' + genre;
	}
	if (sort_by) {
		sign = filters ? "&" : "?";
		filters += sign + 'sort_by=' + sort_by;
	}
	if (order_by) {
		sign = filters ? "&" : "?";
		filters += sign + 'order_by=' + order_by;
	}

	// console.log('API request: ' + 'https://yts.ag/api/v2/list_movies.json' + filters + '\n');

	const interceptorId = rax.attach();
	try {
		var films_res = await axios({
		  url: 'https://yts.ag/api/v2/list_movies.json' + filters,
		  raxConfig: {
		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
		    retry: 50,

		    // Retry twice on errors that don't return a response (ENOTFOUND, ETIMEDOUT, etc).
		    noResponseRetries: 2,
		 
		    // Milliseconds to delay at first.  Defaults to 100.
		    retryDelay: 100,
		 
		    // HTTP methods to automatically retry.  Defaults to:
		    // ['GET', 'HEAD', 'OPTIONS', 'DELETE', 'PUT']
		    httpMethodsToRetry: ['GET', 'HEAD', 'OPTIONS', 'DELETE', 'PUT'],
		 
		    // The response status codes to retry.  Supports a double
		    // array with a list of ranges.  Defaults to:
		    // [[100, 199], [429, 429], [500, 599]]
		    httpStatusCodesToRetry: [[100, 199], [429, 429], [500, 599]],
		 
		    // If you are using a non static instance of Axios you need
		    // to pass that instance here (const ax = axios.create())
		    // instance: ax,
		 
		    // You can detect when a retry is happening, and figure out how many
		    // retry attempts have been made
		    onRetryAttempt: (err) => {
		      const cfg = rax.getConfig(err);
		      console.log(`Retry attempt (movie list) #${cfg.currentRetryAttempt}`);
		    }
		  }
		});
	} catch(err) {
		response.send({'movies': [], 'error': 'api failed'});
		return;
	}

	var films = films_res.data.data['movies'];
	// console.log(films);

	try {
		const promisesArray = films.map(element => {
			let url = 'https://api.themoviedb.org/3/movie/' + element.imdb_code + '?api_key=' + API_KEY + ( locale == 'ru' ? '&language=ru' : '') + '&append_to_response=credits&language=ru';
			console.log('url: ' + url);
			return axios(url);
		});

		const detailed_result_const = await axios.all(promisesArray.map(p => p.catch(() => 'NOT_RESOLVED_MOVIE_INFO')));
		var detailed_result = detailed_result_const;
		console.log(detailed_result);

	} catch(err) {
	    console.error('Error:', err);
		// response.send({'movies': [], 'error': 'api failed'});
		// return;
	}


	var answer_movies = [];
	var counter = 0;
	films.forEach(function(element) {
		var movie = {};
		movie['name'] = element.title;
		movie['year'] = element.year;
		movie['cover_image_url'] = element.large_cover_image;
		movie['rating'] = element.rating;
		movie['imdb_code'] = element.imdb_code;

		if (detailed_result[counter]['data']) {
			movie['full_info_detailed'] = detailed_result[counter]['data'];
		} else {
			movie['full_info_detailed'] = null;
		}
		movie['full_info_general'] = element;
		answer_movies.push(movie);
		counter++;
	})


	response.send({'movies': answer_movies});

	// ======== OLD VAR ===========

	// answer_movies = [];
	// for (const element of films) {
	// 	var movie = {};
	// 	movie['name'] = element.title;
	// 	movie['year'] = element.year;
	// 	movie['cover_image_url'] = element.large_cover_image;
	// 	movie['rating'] = element.rating;
	// 	movie['imdb_code'] = element.imdb_code;
	// 	movie['full_info_general'] = element;


	// 	try {
	// 		var full_info_response = await axios({
	// 		  url: 'https://api.themoviedb.org/3/movie/' + element.imdb_code + '?api_key=' + API_KEY + ( locale == 'ru' ? '&language=ru' : '') + '&append_to_response=credits',
	// 		  raxConfig: {
	// 		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
	// 		    retry: 3,
			 
	// 		    // Retry twice on errors that don't return a response (ENOTFOUND, ETIMEDOUT, etc).
	// 		    noResponseRetries: 2,
			 
	// 		    // Milliseconds to delay at first.  Defaults to 100.
	// 		    retryDelay: 3000,
			 
	// 		    // HTTP methods to automatically retry.  Defaults to:
	// 		    // ['GET', 'HEAD', 'OPTIONS', 'DELETE', 'PUT']
	// 		    httpMethodsToRetry: ['GET', 'HEAD', 'OPTIONS', 'DELETE', 'PUT'],
			 
	// 		    // The response status codes to retry.  Supports a double
	// 		    // array with a list of ranges.  Defaults to:
	// 		    // [[100, 199], [429, 429], [500, 599]]
	// 		    httpStatusCodesToRetry: [[100, 199], [429, 429], [500, 599]],
			 
	// 		    // If you are using a non static instance of Axios you need
	// 		    // to pass that instance here (const ax = axios.create())
	// 		    // instance: ax,
			 
	// 		    // You can detect when a retry is happening, and figure out how many
	// 		    // retry attempts have been made
	// 		    onRetryAttempt: (err) => {
	// 		      const cfg = rax.getConfig(err);
	// 		      console.log(`Retry attempt (movie details) #${cfg.currentRetryAttempt}`);
	// 		    }
	// 		  }
	// 		});
	// 		movie['name'] = full_info_response.data.title;
	// 		movie['cover_image_url'] = 'http://image.tmdb.org/t/p/original//' + full_info_response.data.poster_path;
	// 	} catch(err) {
	// 		// response.send({'movies': [], 'error': 'api failed'});
	// 		movie['full_info_detailed'] = null;
	// 		// return;
	// 	}

	// 	movie['full_info_detailed'] = full_info_response.data;
	// 	movie['full_info_general'] = element;


	// 	answer_movies.push(movie);
	// }

	// response.send({'movies': answer_movies});

	// ======== OLD VAR ===========

})


// ==============================


// ============================== get films list

app.get('/film', async (request, response) => {
	console.log(request.query);

	hash = request.query.hash;

	if (fs.existsSync("/tmp/videos/" + hash)) {
		fs.readdir(directoryPath, function (err, files) {
		    //handling error
		    if (err) {
		        console.log('Unable to scan directory: ' + err);
		    } 
		    //listing all files using forEach
		    files.forEach(function (file) {
		        // Do whatever you want to do with the file
		        console.log(file); 
		    });
		});
	}

	// response.send('Hello from Express!');
	// return;

 	sources = ["udp://open.demonii.com:1337/announce", "udp://tracker.openbittorrent.com:80", "udp://tracker.coppersurfer.tk:6969", "udp://glotorrents.pw:6969/announce", "udp://tracker.opentrackr.org:1337/announce", "udp://torrent.gresille.org:80/announce", "udp://p4p.arenabg.com:1337", "udp://tracker.leechers-paradise.org:6969"];

	magnet = "magnet:?xt=urn:btih:" + hash + "&tr=" + encodeURI(sources[0]) + "&tr=" + encodeURI(sources[1]);

	var engine = torrentStream(magnet, {path: '/tmp/videos/' + hash});
	engine.on('ready', function() {
		engine.files.forEach(function(file) {
			console.log('filename:', file.name);
			var stream = file.createReadStream();
			stream.pipe(fs.createWriteStream('/tmp/videos/' + hash + "/" + file.name));
		});
	});
    response.send('Hello from Express!');

})

// ==============================



// ============================== some test api

app.get('/test_download', (request, response) => {

	test_video = {
		hash: "3F32D7B8981EDC1CA01B95CF16C3B52DA33A3111",
        title: "The Naked and the Dead",
        title_english: "The Naked and the Dead",
        title_long: "The Naked and the Dead (1958)",
	}
 	test_sources = ["udp://open.demonii.com:1337/announce", "udp://tracker.openbittorrent.com:80", "udp://tracker.coppersurfer.tk:6969", "udp://glotorrents.pw:6969/announce", "udp://tracker.opentrackr.org:1337/announce", "udp://torrent.gresille.org:80/announce", "udp://p4p.arenabg.com:1337", "udp://tracker.leechers-paradise.org:6969"];

 	// test_magnet = "magnet:?xt=urn:btih:" + test_video['hash'] + "&dn=" + encodeURI(test_video['title']) + "&tr=" + encodeURI(test_sources[0]) + "&tr=" + encodeURI(test_sources[1]);
 	test_magnet = "magnet:?xt=urn:btih:" + test_video['hash'] + "&tr=" + encodeURI(test_sources[0]) + "&tr=" + encodeURI(test_sources[1]);

	// var test_link = 'magnet:?xt=urn:btih:1E1730F3CB736BBCBD965B471A7C98D0F9A4F965&dn=Nancy+%282018%29+%5BWEBRip%5D+%5B720p%5D+%5BYTS%5D+%5BYIFY%5D&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2F9.rarbg.com%3A2710%2Fannounce&tr=udp%3A%2F%2Fp4p.arenabg.com%3A1337&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969&tr=udp%3A%2F%2Ftracker.internetwarriors.net%3A1337&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.zer0day.to%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969%2Fannounce&tr=udp%3A%2F%2Fcoppersurfer.tk%3A6969%2Fannounce';

	var engine = torrentStream(test_magnet, {path: '/tmp/test_video'});
	engine.on('ready', function() {
		engine.files.forEach(function(file) {
			console.log('filename:', file.name);
			var stream = file.createReadStream();
			stream.pipe(fs.createWriteStream('/tmp/test_video/' + file.name));
		});
	});
    response.send('Hello from Express!');
})

app.get('/test_get_list', (request, response) => {

	needle.get('https://api.themoviedb.org/3/movie/343611?api_key=' + API_KEY, function(error, n_response) {
	  if (!error && n_response.statusCode == 200) {
	    console.log(n_response.body);
		response.send('needle responsed with ok');
	  }
	});
})


app.get('/get_movie', (request, response) => {

	var path = "/tmp/test_video/The.Naked.And.The.Dead.1958.720p.BluRay.x264-[YTS.AM].mp4";

    const head = {
      // 'Content-Length': fileSize,
      'Content-Type': 'video/mp4',
    }
    response.writeHead(200, head)
    fs.createReadStream(path).pipe(response)
})

// ======================================

app.listen(port, (err) => {
    if (err) {
        return console.log('something bad happened', err);
    }
    console.log(`server is listening on ${port}`);
})