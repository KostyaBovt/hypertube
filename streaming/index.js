const express 	= require('express');
const fs 		= require('fs');
const needle 	= require('needle');
const axios 	= require('axios');
const rax 		= require('retry-axios');
var Client 		= require('node-torrent');
var Transmission = require('transmission');
var pather 		= require('path');
const srt2vtt = require('srt-to-vtt');
var cookieParser = require('cookie-parser')

const OpenSubtitles = require('opensubtitles-api');
const OS = new OpenSubtitles({
	useragent: "Hypertube v1",
	username: "hypertube_optimus",
	password: "33cats44dogs",
	ssl: true
});

// the most trashful code you have ever seen in your life...
// dont try it at home
// to test: tt0078788


var cors = require('cors')
var torrentStream = require('torrent-stream');

var API_KEY = 'ec8920bdb431590b48d7b0205e7d6a49';  // API key for themoviedb.org

const port = 3200;

const app = express();
app.use(cors());
app.use(cookieParser());

app.use(express.static(pather.join(__dirname, 'public')));
app.use(express.static('public'));
app.use('/public', express.static(pather.join(__dirname, 'public')));

app.use('/videos', express.static('/tmp/videos'));

// ============================== some hardcode values

var locale = 'ru';

// ============================== some usefull functions

app.use((req, res, next) => {
	res.set({
		'Access-Control-Allow-Origin':			'http://localhost:3000',
		'Access-Control-Allow-Credentials':		'true',
	});
	next();
});

app.get('/test', async (request, response) => {
	console.log(request.cookies);
	response.send('ok');
});


async function validateUser(cookies) {

	return new Promise(async (resolve, reject) => {
		if (!cookies['x-auth-token']) {
			reject();
		} else {
			try {
				var auth_info = await axios({
					method: 'get',
					url: 'http://localhost:8080/api/auth/udata',
					headers: {'Cookie': "x-auth-token=" + cookies['x-auth-token']},
					withCredentials: true
				});
				console.log(auth_info.data);
			} catch (error) {
				reject();
			}
		}	
		resolve(auth_info);
	});

}

app.get('/subtitles', async (request, response) => {

	async function downloadSubtitles(url, name) {

	  const path_srt = '/tmp/subs/' + name + '.srt';
	  const path_vtt = '/tmp/subs/' + name + '.vtt';

	  // axios image download with response type "stream"
	  const response = await axios({
	    method: 'GET',
	    url: url,
	    responseType: 'stream'
	  })

	  // pipe the result stream into a file on disc
	  // response.data.pipe(fs.createWriteStream(path_srt));
	  response.data.pipe(srt2vtt()).pipe(fs.createWriteStream(path_vtt));

	  // return a promise and resolve when download finishes
	  return new Promise((resolve, reject) => {
	    response.data.on('end', () => {
	      resolve()
	    })

	    response.data.on('error', () => {
	      reject()
	    })
	  })

	}

	var result = await OS.search({
		imdbid: 'tt1675434'
	})

	var locales = ['ru', 'en'];

	var arrayLength = locales.length;
	for (var i = 0; i < arrayLength; i++) {
		if (result[locales[i]]) {
			var url  = result[locales[i]]['url'];
			var name = 'tt1675434' + '_' + locales[i];
			await downloadSubtitles(url, name);
		}
	}

	response.send({subtitles: result});

})

// ============================== get OUR popular films

app.get('/popular_films', async (request, response) => {
	console.log(request.query);

	try {
		var validation = await validateUser(request.cookies);
		// var validation = await validateUser({'x-auth-token': "laskjdfa80ur2rh2kjh23kj4h2l3j4h2kj3h4k32j4h"});
	} catch(error) {
		response.send({'success': false, 'error': 'invalid token'});
		return;
	}

	// language: to request from api user settings
	language = validation.data['payload']['locale'] || 'en';

	// integer 1 - 1000
	page = request.query.page || 1;

	// desc or asc
	order = request.query.order || 'desc';

	const {Client}  = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()

	const res = await db.query('SELECT * from popular_films order by count ' + order + ' limit 20 offset ' + (page - 1) * 20 + ';');
	await db.end()

	var rows = res['rows'];


	try {
		const promisesArray = rows.map(element => {
			let url = 'https://api.themoviedb.org/3/movie/' + element.imdb_id + '?api_key=' + API_KEY + '&language=' + language;
			return axios(url);
		});

		const detailed_result_const = await axios.all(promisesArray.map(p => p.catch(() => 'NOT_RESOLVED_MOVIE_INFO')));
		var detailed_result = detailed_result_const;

	} catch(err) {
	    console.error('Error:', err);
	}

	final_response = detailed_result.map(element => {return element.data});

	for (var i = final_response.length - 1; i >= 0; i--) {
		final_response[i]['poster_path'] = 'http://image.tmdb.org/t/p/w342' + final_response[i]['poster_path'];
		final_response[i]['popular_films_count'] = rows[i]['count'];
	}

	response.send(final_response);

});


// ============================== get films list NEW VERSION

app.get('/films', async (request, response) => {
	console.log(request.query);

	try {
		var validation = await validateUser(request.cookies);
		// var validation = await validateUser({'x-auth-token': "laskjdfa80ur2rh2kjh23kj4h2l3j4h2kj3h4k32j4h"});
	} catch(error) {
		response.send({'success': false, 'error': 'invalid token'});
		return;
	}

	// language: to request from api user settings
	language = validation.data['payload']['locale'] || 'en';

	// integer 1 - 1000
	page = request.query.page;


	// popularity.asc, popularity.desc, release_date.asc, release_date.desc, revenue.asc, revenue.desc, primary_release_date.asc, primary_release_date.desc, original_title.asc, original_title.desc, vote_average.asc, vote_average.desc, vote_count.asc, vote_count.desc
	// default: popularity.desc
	sort_by = request.query.sort_by;

	// string yyyy-mm-dd
	release_date_gte = request.query.release_date_gte;

	// string yyyy-mm-dd
	release_date_lte = request.query.release_date_lte;

	// integer >= 0
	vote_count_gte = request.query.vote_count_gte;

	// integer >= 1
	vote_count_lte = request.query.vote_count_lte;

	// integer >= 0
	vote_average_gte = request.query.vote_average_gte;

	// integer >= 0
	vote_average_lte = request.query.vote_average_lte;

	// string: list of ganres ids
	// en
	// {"genres":[{"id":28,"name":"боевик"},{"id":12,"name":"приключения"},{"id":16,"name":"мультфильм"},{"id":35,"name":"комедия"},{"id":80,"name":"криминал"},{"id":99,"name":"документальный"},{"id":18,"name":"драма"},{"id":10751,"name":"семейный"},{"id":14,"name":"фэнтези"},{"id":36,"name":"история"},{"id":27,"name":"ужасы"},{"id":10402,"name":"музыка"},{"id":9648,"name":"детектив"},{"id":10749,"name":"мелодрама"},{"id":878,"name":"фантастика"},{"id":10770,"name":"телевизионный фильм"},{"id":53,"name":"триллер"},{"id":10752,"name":"военный"},{"id":37,"name":"вестерн"}]}
	// ru
	// {"genres":[{"id":28,"name":"Action"},{"id":12,"name":"Adventure"},{"id":16,"name":"Animation"},{"id":35,"name":"Comedy"},{"id":80,"name":"Crime"},{"id":99,"name":"Documentary"},{"id":18,"name":"Drama"},{"id":10751,"name":"Family"},{"id":14,"name":"Fantasy"},{"id":36,"name":"History"},{"id":27,"name":"Horror"},{"id":10402,"name":"Music"},{"id":9648,"name":"Mystery"},{"id":10749,"name":"Romance"},{"id":878,"name":"Science Fiction"},{"id":10770,"name":"TV Movie"},{"id":53,"name":"Thriller"},{"id":10752,"name":"War"},{"id":37,"name":"Western"}]}
	with_genres = request.query.with_genres;


	// string: matching search
	query = request.query.query;

	// common filters: language, page
	// filters only without query: sort_by, release_date_lte, release_date_gte, vote_count_gte, vote_count_lte, vote_average_gte, vote_average_lte, with_genres


	filters = '';

	if (API_KEY) {
		sign = filters ? "&" : "?";
		filters += sign + 'api_key=' + API_KEY;
	}
	if (page) {
		sign = filters ? "&" : "?";
		filters += sign + 'page=' + page;
	}

	if (language) {
		sign = filters ? "&" : "?";
		filters += sign + 'language=' + language;
	}


	if (query) {
		sign = filters ? "&" : "?";
		filters += sign + 'query=' + query;
	} else {
		
		if (sort_by) {
			sign = filters ? "&" : "?";
			filters += sign + 'sort_by=' + sort_by;
		}
		if (release_date_gte) {
			sign = filters ? "&" : "?";
			filters += sign + 'release_date.gte=' + release_date_gte;
		}
		if (release_date_lte) {
			sign = filters ? "&" : "?";
			filters += sign + 'release_date.lte=' + release_date_lte;
		}
		if (vote_count_gte) {
			sign = filters ? "&" : "?";
			filters += sign + 'vote_count.gte=' + vote_count_gte;
		}
		if (vote_count_lte) {
			sign = filters ? "&" : "?";
			filters += sign + 'vote_count.lte=' + vote_count_lte;
		}
		if (vote_average_gte) {
			sign = filters ? "&" : "?";
			filters += sign + 'vote_average.gte=' + vote_average_gte;
		}
		if (vote_average_lte) {
			sign = filters ? "&" : "?";
			filters += sign + 'vote_average.lte=' + vote_average_lte;
		}
		if (with_genres) {
			sign = filters ? "&" : "?";
			filters += sign + 'with_genres=' + with_genres;
		}
	}


	if (query) {
		var url = 'https://api.themoviedb.org/3/search/movie' + filters;
	} else {
		var url = 'https://api.themoviedb.org/3/discover/movie' + filters;
	}

	const interceptorId = rax.attach();
	try {
		console.log('MAKING API REQUEST: ' + url);
		var films_res = await axios({
		  url: url,
		  raxConfig: {
		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
		    retry: 2,

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
		// console.log(err);
		response.send({'success': false, 'error': 'api failed'});
		return;
	}

	for (var i = films_res.data['results'].length - 1; i >= 0; i--) {
		films_res.data['results'][i]['poster_path'] = 'http://image.tmdb.org/t/p/w342' + films_res.data['results'][i]['poster_path'];
	}


	response.send({'movies': films_res.data});


	// this is usefull snipet to make several async requests with promises
	// try {
	// 	const promisesArray = films.map(element => {
	// 		let url = 'https://api.themoviedb.org/3/movie/' + element.imdb_code + '?api_key=' + API_KEY + ( locale == 'ru' ? '&language=ru' : '') + '&append_to_response=credits&language=ru';
	// 		console.log('url: ' + url);
	// 		return axios(url);
	// 	});

	// 	const detailed_result_const = await axios.all(promisesArray.map(p => p.catch(() => 'NOT_RESOLVED_MOVIE_INFO')));
	// 	var detailed_result = detailed_result_const;
	// 	console.log(detailed_result);

	// } catch(err) {
	//     console.error('Error:', err);
	// }


})


// ============================== get film details by id

app.get('/film_details', async (request, response) => {
	console.log(request.query);

	try {
		var validation = await validateUser(request.cookies);
		// var validation = await validateUser({'x-auth-token': "laskjdfa80ur2rh2kjh23kj4h2l3j4h2kj3h4k32j4h"});
	} catch(error) {
		response.send({'success': false, 'error': 'invalid token'});
		return;
	}

	// language: to request from api user settings
	language = validation.data['payload']['locale'] || 'en';

	// integer: id
	id = request.query.id;

	filters = '';

	if (API_KEY) {
		sign = filters ? "&" : "?";
		filters += sign + 'api_key=' + API_KEY;
	}

	if (language) {
		sign = filters ? "&" : "?";
		filters += sign + 'language=' + language;
	}

	var url = 'https://api.themoviedb.org/3/movie/' + id + filters + '&append_to_response=credits';

	const interceptorId = rax.attach();
	try {
		console.log('MAKING API REQUEST: ' + url);
		var films_res = await axios({
		  url: url,
		  raxConfig: {
		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
		    retry: 2,

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
		// console.log(err);
		response.send({'success': false, 'error': 'api failed'});
		return;
	}

	var imdb_id = films_res.data['imdb_id'];
	var url2 = 'https://tv-v2.api-fetch.website/movie/' + imdb_id;

	try {
		console.log('MAKING API REQUEST2: ' + url2);
		var films_res2 = await axios({
		  url: url2,
		  raxConfig: {
		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
		    retry: 2,

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
		// console.log(err);
		response.send({'success': false, 'error': 'api2 failed'});
		return;
	}


	response.send({'success': true, 'movie_details_1': films_res.data, 'movie_details_2': films_res2.data});
})



// ============================== start download and return links to movie and subtitles files

var walkSync = function(dir, filelist) {
      var path = path || require('path');
      var fs = fs || require('fs'),
          files = fs.readdirSync(dir);
      filelist = filelist || [];
      files.forEach(function(file) {
          if (fs.statSync(path.join(dir, file)).isDirectory()) {
              filelist = walkSync(path.join(dir, file), filelist);
          }
          else {
              filelist.push(path.join(dir, file));
          }
      });
      return filelist;
};



app.get('/film', async (request, response) => {
	console.log(request.query);

	try {
		var validation = await validateUser(request.cookies);
		// var validation = await validateUser({'x-auth-token': "laskjdfa80ur2rh2kjh23kj4h2l3j4h2kj3h4k32j4h"});
	} catch(error) {
		response.send({'success': false, 'error': 'invalid token'});
		return;
	}

	// language: to request from api user settings
	language = validation.data['payload']['locale'] || 'en';

	async function downloadSubtitles_sub(url, name) {

	  const path_vtt = '/tmp/videos/' + imdb_id + '/subs/' + name + '.vtt';

	  // axios image download with response type "stream"
	  const response = await axios({
	    method: 'GET',
	    url: url,
	    responseType: 'stream'
	  })

	  // pipe the result stream into a file on disc
	  // response.data.pipe(fs.createWriteStream(path_srt));
	  response.data.pipe(srt2vtt()).pipe(fs.createWriteStream(path_vtt));

	  // return a promise and resolve when download finishes
	  return new Promise((resolve, reject) => {
	    response.data.on('end', () => {
	      resolve()
	    })

	    response.data.on('error', () => {
	      reject()
	    })
	  })

	}

	imdb_id = request.query.imdb_id;
	resolution = request.query.resolution;

	var return_object = {};

	if (!imdb_id || !resolution) {
	    response.send({success: false, error: "invalid query parameters"});
	    return;
	}

	const {Client}  = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()

	const res = await db.query("SELECT * from popular_films where imdb_id='" + imdb_id + "';");
	if (!res.rowCount) {
		var sql_update = "insert into popular_films values('" + imdb_id + "', 1)";
	} else {
		var sql_update = "update popular_films set count=" + (parseInt(res.rows[0]['count']) + 1) + " where imdb_id='" + imdb_id + "';";
	}

	const res2 = await db.query(sql_update);
	await db.end()

	var dir_path = "/tmp/videos/" + imdb_id + "/" + resolution;
	var dir_path_subs = "/tmp/videos/" + imdb_id + "/subs";

	if (fs.existsSync(dir_path)) {

		var files_sub = walkSync(dir_path_subs);
		var return_files_sub = {};

		for (var i = files_sub.length - 1; i >= 0; i--) {
			file_name_end = files_sub[i].substring(files_sub[i].length - 7, files_sub[i].length);
			console.log('we have file name end: ');
			console.log(file_name_end);
	        if (file_name_end == '_ru.vtt') {
	        	return_files_sub['ru'] = "http://localhost:3200" + files_sub[i].substring(4, files_sub[i].length);
	        	console.log('we have return_files_sub ru : ' + return_files_sub['ru']);
	        }
	        if (file_name_end == '_en.vtt') {
	        	return_files_sub['en'] = "http://localhost:3200" + files_sub[i].substring(4, files_sub[i].length);
	        	console.log('we have return_files_sub en : ' + return_files_sub['en']);
	        }	        
		}

		var files = walkSync(dir_path);
		var return_file = '';

		for (var i = files.length - 1; i >= 0; i--) {
			ext = pather.extname(files[i]);
	        if (ext == '.mkv' || ext == '.mp4') {
	        	return_file = files[i];
	        }
		}

	    if (return_file) {
	    	return_object['movie_link'] = "http://localhost:3200" + return_file.substring(4, return_file.length);
	    	return_object['subs'] = return_files_sub;
	        console.log("will return link: http://localhost:3200" + return_file.substring(4, return_file.length));
		    return_object['success'] = true;
		    response.send(return_object);
		    return;	        
	    } else {
		    response.send({success: false, error: "no video files aviable for this film"});
		    return;
	    }

	}

    if (!return_object['movie_link']) {

	    if (!fs.existsSync('/tmp/videos')) {
	        fs.mkdirSync('/tmp/videos');
	    }

	    if (!fs.existsSync('/tmp/videos/' + imdb_id)) {
	        fs.mkdirSync('/tmp/videos/' + imdb_id);
	    }

	    if (!fs.existsSync(dir_path)) {
	        fs.mkdirSync(dir_path);
	    }

	    if (!fs.existsSync(dir_path_subs)) {
	        fs.mkdirSync(dir_path_subs);
	        var to_down_subs = true;
	    } else {
	    	var to_down_subs = false;
	    }

	    // download subtitles first:
		if (to_down_subs) {
			// down_subs(imdb_id);
			var result = await OS.search({
				imdbid: imdb_id
			})

			console.log('we have result on OS search: ');
			// console.log(result);

			var locales = ['ru', 'en'];

			var arrayLength = locales.length;
			for (var i = 0; i < arrayLength; i++) {
				if (result[locales[i]]) {
					var url  = result[locales[i]]['url'];
					var name = imdb_id + '_' + locales[i];
					await downloadSubtitles_sub(url, name);
					console.log('downloaded subs: ' + name);
					var list_files = walkSync(dir_path_subs);
					console.log('we have such subs files:');
					console.log(list_files);
				}
			}
		}


    	var url2 = 'https://tv-v2.api-fetch.website/movie/' + imdb_id;
		try {
			console.log('MAKING API REQUEST2: ' + url2);
			var films_res2 = await axios({
			  url: url2,
			  raxConfig: {
			    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
			    retry: 2,

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
			// console.log(err);
			response.send({'success': false, 'error': 'api2 failed'});
			return;
		}

		var magnet = films_res2.data['torrents']['en'][resolution]['url'];

		var engine = torrentStream(magnet, {path: dir_path});
	    var return_file = '';



		engine.on('ready', function() {

			for (var i = engine.files.length - 1; i >= 0; i--) {
		        ext = pather.extname(engine.files[i].name);
		        if (ext == '.mkv' || ext == '.mp4') {
					console.log('Start download filename:', engine.files[i].name);
					var stream = engine.files[i].createReadStream();
					var return_file_path = engine.files[i].path;


					stream.on('readable', function() {

						var files_sub = walkSync(dir_path_subs);
						var return_files_sub = {};

						for (var i = files_sub.length - 1; i >= 0; i--) {
							file_name_end = files_sub[i].substring(files_sub[i].length - 7, files_sub[i].length);
					        if (file_name_end == '_ru.vtt') {
					        	return_files_sub['ru'] = "http://localhost:3200" + files_sub[i].substring(4, files_sub[i].length);
					        }
					        if (file_name_end == '_en.vtt') {
					        	return_files_sub['en'] = "http://localhost:3200" + files_sub[i].substring(4, files_sub[i].length);
					        }	        
						}


						return_object['movie_link'] = "http://localhost:3200/videos/" + imdb_id + '/' + resolution + "/" + encodeURI(return_file_path);
				    	return_object['success'] = true;
				    	return_object['subs'] = return_files_sub;
					    response.send(return_object);
					})
				}
			}

		});

	    engine.on('idle', () => {
	    	console.log('finished download');
	    });
    }

})

// ==============================



// ============================== get films list OLD VERSION

app.get('/films_old', async (request, response) => {
	console.log(request.query);

 // 1-50
	limit = request.query.limit;

 // integer
	page = request.query.page;

 // 1- 9
	minimum_rating = request.query.minimum_rating;

 // string: matching on: Movie Title/IMDb Code, Actor Name/IMDb Code, Director Name/IMDb Code
	query_term = request.query.query_term;

 // Action	 Adventure	 Animation	 Biography
 // Comedy	 Crime	 Documentary	 Drama
 // Family	 Fantasy	 Film-Noir	 Game-Show
 // History	 Horror	 Music	 Musical
 // Mystery	 News	 Reality-TV	 Romance
 // Sci-Fi	 Sport	 Talk-Show	 Thriller
 // War	 Western
	genre = request.query.genre;

 // title, year, rating, peers, seeds, download_count, like_count, date_added
	sort_by = request.query.sort_by;

 // desc, asc
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
		console.log('MAKING API REQUEST: https://yts.am/api/v2/list_movies.json' + filters);
		var films_res = await axios({
		  url: 'https://yts.am/api/v2/list_movies.json' + filters,
		  proxy: {host: '178.219.86.106', port: 44262},
		  raxConfig: {
		    // Retry 3 times on requests that return a response (500, etc) before giving up.  Defaults to 3.
		    retry: 2,

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
		console.log(err);
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

})



// ====================================== server start

app.listen(port, (err) => {
    if (err) {
        return console.log('something bad happened', err);
    }
    console.log(`server is listening on ${port}`);
})