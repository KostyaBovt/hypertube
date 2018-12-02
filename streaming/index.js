const express 	= require('express');
const fs 		= require('fs');
const axios 	= require('axios');
const srt2vtt = require('srt-to-vtt');
const cookieParser = require('cookie-parser')
const rimraf = require('rimraf');

const OpenSubtitles = require('opensubtitles-api');
const OS = new OpenSubtitles({
	useragent: "Hypertube v1",
	username: "hypertube_optimus",
	password: "33cats44dogs",
	ssl: true
});

const cors = require('cors')

const API_KEY = 'ec8920bdb431590b48d7b0205e7d6a49';  // API key for themoviedb.org
const OMDB_API_KEY = '651e2d43';

const port = 3200;

const app = express();
app.use(cors());
app.use(cookieParser());

app.use((req, res, next) => {
	res.set({
		'Access-Control-Allow-Origin':			'http://localhost:3000',
		'Access-Control-Allow-Credentials':		'true',
	});
	next();
});

// ============================== this is check every 10 minutes if we need to clean old(30 days unused) films

setTimeout(async function runCleaner() {
	const {Client}  = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()

	const result_to_delete = await db.query( "SELECT imdb_id, MAX(seen) FROM history GROUP BY imdb_id HAVING NOW() - MAX(seen) > INTERVAL '30 days'");

	var params = [];
	for (var i = result_to_delete.rows.length - 1; i >= 0; i--) {
		console.log('now to delete this film :');
		console.log(result_to_delete.rows[i]['imdb_id']);
		params.push(result_to_delete.rows[i]['imdb_id']);
		if (fs.existsSync('/tmp/videos/' + result_to_delete.rows[i]['imdb_id'])) {
			var temp_imdb_id = result_to_delete.rows[i]['imdb_id'];
			rimraf('/tmp/videos/' + result_to_delete.rows[i]['imdb_id'], function () { 
				console.log('=========\ndeleted folder /tmp/videos/' + temp_imdb_id + '\n=========\n');
			});
			
		}
	}
	if (result_to_delete.rows.length == 0) {
		console.log('nothing to clean');
	}
	await db.end()
	
	setTimeout(runCleaner, 1000 * 60 * 60);
}, 3000);

// ============================== function to validate user

const userAuth = async (req, res, next) => {
	const { cookies } = req;
	if (cookies['x-auth-token']) {
		try {
			const response = await axios({
				method: 'GET',
				url: 'http://localhost:8080/api/auth/udata',
				headers: { 'Cookie': "x-auth-token=" + cookies['x-auth-token'] },
				withCredentials: true
			});
			req.user = response.data.payload;
			next();
		} catch (e) {
			res.json({'success': false, 'error': 'invalid token'});
		}
	} else {
		res.json({'success': false, 'error': 'invalid token'});
	}
}

// ======================= function to get watched films by curent user

const getWatched = async (req, res, next) => {
	
	var user_id = req.user.id;

	const {Client}  = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()

	const result_watched = await db.query( "SELECT film_id, count(*) from history where user_id=$1 group by film_id", [user_id]);
	// console.log('============== watched films============');
	// console.log(result_watched.rows);

	var watched_films_rows = result_watched.rows;

	var watched_films_mapped = {};
	for (var i = watched_films_rows.length - 1; i >= 0; i--) {
		watched_films_mapped[watched_films_rows[i]['film_id']] = parseInt(watched_films_rows[i]['count']);
	}
	req.user.watched = watched_films_mapped;

	await db.end();
	next();
}

app.all('*', userAuth, getWatched);

// ======================= put watched film by curent user in DB

app.get('/watched/:movieId', async (req, res) => {
	
	const { movieId } = req.params;
	var userId = req.user.id;

	const url = `https://api.themoviedb.org/3/movie/${movieId}`;
	const params = {
		api_key: API_KEY,
		id: movieId
	};

	console.log(`Making a request (${url}) with params`, params);

	try {
		const response = await axios.get(url, { params });
		var imdb_id = response.data['imdb_id'];
		console.log(imdb_id);
	} catch (e) {
		console.error(e);
		res.json({'success': false, 'error': 'failed while updating watched statistics'});
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
	const resultWatched = await db.query( "INSERT into history values ($1, $2, $3)", [userId, movieId, imdb_id]);
	await db.end();

	res.json({'success': true, 'result': 'watched statistics was updated'});
});


// ============================== get OUR popular films

app.get('/popular_films', async (request, response) => {
	// console.log(request.user);

	// language: to request from api user settings
	var language = request.user.locale || 'en';

	// integer 1 - 1000
	var page = parseInt(request.query.page) || 1;
	var offset = (page - 1) * 20;

	// desc or asc
	var order = request.query.order == 'asc' ?  'asc' : 'desc';

	var watched = request.user.watched;

	const {Client}  = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()

	const res = await db.query("SELECT film_id, imdb_id, COUNT(seen) FROM history GROUP BY film_id, imdb_id ORDER BY COUNT(seen) " + order + " OFFSET $1", [offset]);

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
		final_response[i]['watched_films_count'] = watched[final_response[i]['id']] || 0 ;
	}

	response.send(final_response);

});


app.get('/films', (req, res, next) => {
	if (req.query.with_genres) {
		req.query.with_genres += "";
	}
	next();
}, async (req, res) => {
	const defaultFilters = {
		search: {
			"include_adult": "false"
		},
		discover: {
			"include_adult": "false",
			"include_video": "false",
			"with_release_type": "1|2|3",
			"vote_count.gte": "5"
		}
	};
	let url = "https://api.themoviedb.org/3";
	let params = {};
	const filters = req.query;

	var watched = req.user.watched;

	if (filters.query) {
		url += "/search/movie";
		params = { ...defaultFilters.search, ...filters };
	} else {
		url += "/discover/movie";
		params = { ...defaultFilters.discover, ...filters };
	}

	params.api_key = API_KEY;
	params.language = req.user.locale;
	
	console.log(`Making a request (${url}) with params`, params);

	try {
		const response = await axios.get(url, { params });
		const { data } = response;
		data.results.forEach(movie => {
			if (movie.poster_path) {
				movie.poster_path = 'http://image.tmdb.org/t/p/w342' + movie.poster_path;
				movie.watched_films_count = watched[movie.id] || 0;
			}
		});
		res.json({ 'success': true, 'movies': data });
	} catch (e) {
		console.error(e);
		res.json({ 'success': false, 'error': 'api failed' });
	}
});

app.get('/film_details/:movieId', async (req, res, next) => {
	const { movieId } = req.params;
	const url = `https://api.themoviedb.org/3/movie/${movieId}`;
	const params = {
		api_key: API_KEY,
		language: req.user.locale,
		id: movieId,
		append_to_response: "credits"
	};

	var watched = req.user.watched;

	console.log(`Making a request (${url}) with params`, params);

	try {
		const response = await axios.get(url, { params });
		const { imdb_id, poster_path } = response.data;
		response.data.poster_path = 'http://image.tmdb.org/t/p/w342' + poster_path;
		response.data.watched_films_count = watched[movieId] || 0;
		req.movie = { imdb_id, movie_details: response.data };
		next();
	} catch (e) {
		console.log('tmdb request failed...');
		console.error(e);
		res.json({'success': false, 'error': 'TMDb request failed'});
	}
}, (req, res, next) => {
	const { movie_details } = req.movie;
	const { crew, cast } = movie_details.credits;
	const transformPicUrls = (member) => {
		if (member.profile_path) {
			member.profile_path = `http://image.tmdb.org/t/p/w342/${member.profile_path}`;
		}
		return member;
	}

	console.log(crew, cast);

	const directors = crew.filter((member => member.job === "Director")).map(transformPicUrls);
	const producers = crew.filter((member => member.job === "Producer")).map(transformPicUrls);
	const main_cast = cast.splice(0, 5).map(transformPicUrls);

	movie_details.credits = { crew: { directors, producers }, main_cast };

	next();
}, async (req, res, next) => {
	const { imdb_id } = req.movie;

	if (!imdb_id) return next();

	const url = `https://tv-v2.api-fetch.website/movie/${imdb_id}`;
	console.log(`Making a request (${url})`);

	try {
		const response = await axios.get(url);
		console.log(response.data);
		const { torrents } = response.data;

		if (torrents && torrents['en']) {
			const streaming = [];
			Object.keys(torrents['en']).forEach(resolution => {
				const streamingUrl = `http://localhost:3200/film/${imdb_id}/${resolution}`;
				streaming.push(streamingUrl);
			});
			req.movie.streaming = streaming;
		}
	} catch (e) {
		console.log('popcorntime request failed...');
		console.log(e.response.status, e.response.text, e.response.data);
	} finally {
		next();
	}
}, (req, res) => {
	const { movie_details, streaming } = req.movie;
	res.json({'success': true,  movie_details, streaming });
});

const pump = require('pump');
const torrentEngineManager = require('./torrentEngineManager');

app.get('/film/:id/:resolution', (req, res, next) => { // Parsing range headers
	const { id, resolution } = req.params;
	const { range } = req.headers;

	console.log("streaming request", req.params, range);

	req._streaming = {};
	req._streaming.movie = { id, resolution };

	if (range) {
		const parts = range.replace(/bytes=/, "").split("-");
		req._streaming.range = {
			start: parseInt(parts[0], 10),
			end: parts[1] ? parseInt(parts[1], 10) : undefined
		};
	} else {
		req._streaming.range = {
			start: 0,
			end: undefined
		};
	}

	next();
}, async (req, res, next) => { // Checking if this movie can be downloaded
	const { id, resolution } = req._streaming.movie;

	const response = await axios.get(`https://tv-v2.api-fetch.website/movie/${id}`);
	console.log('response', response.data.torrents);

	const { torrents } = response.data;
	if (!torrents) { // There is no torrent links for this movie, so it can't be downloaded
		console.log("There is no torrent links for this movie, so it can't be downloaded");
		return res.sendStatus(404); // Sending response 404 and not going further
	} else {
		const { url, size } = torrents['en'][resolution];
		req._streaming.movie.magnetLink = url;
	}

	next();
}, (req, res, next) => { // Checking if directory for this movie is already created
	const { id } = req._streaming.movie;
	const movieDirPath = `/tmp/videos/${id}`;

	fs.access(movieDirPath, fs.constants.R_OK, (err) => {
		if (!err) {
			console.log(`Directory for movie ${id} already exists`);
			next();
		} else if (err && err.errno === -2) { // Dir not exists
			console.log(`Creating directory for movie ${id}...`);
			fs.mkdir(movieDirPath, (err) => {
				if (err) {
					console.error(err);
					res.sendStatus(500);
				} else {
					console.log('Directory created!');
					next();
				}
			});
		} else {
			console.error(err);
			res.sendStatus(500);
		}
	});
}, (req, res, next) => { // Checking if this resolution is already downloaded
	const { id, resolution } = req._streaming.movie;
	const movieFilePath = `/tmp/videos/${id}/${resolution}`;

	req._streaming.movie.file = {
		name: `${resolution}`,
		path: movieFilePath,
		exists: false
	}

	fs.stat(movieFilePath, (err, fileStats) => {
		if (!err) {
			req._streaming.movie.file.size = fileStats.size;
			req._streaming.movie.file.exists = true;
			console.log(`Resoulution ${resolution} already exists for movie ${id}`);
			next();
		} else if (err && err.errno === -2) {
			console.log(`Resoulution ${resolution} not exists yet for movie ${id}`);
			next();
		} else {
			console.error(e);
			return res.sendStatus(500);
		}
	});
}, (req, res, next) => {
	const { movie } = req._streaming;

	if (movie.file.exists) {
		next();
	} else {
		const engineId = `${movie.id}-${movie.resolution}`;
		console.log(`Creating new ${engineId} engine...`);
		torrentEngineManager.createNewEngine(engineId, movie.magnetLink, (engine) => {
			console.log(`${engineId} engine created!`);
			const torrentFile = torrentEngineManager.getMovieFileByEngineId(engineId);

			
			fs.readFile(`/tmp/videos/${movie.id}/${movie.resolution}.json`, 'utf8', (err, data) => {
				if (err && err.errno !== -2) {
					console.log(`Unexpected error while reading stat file for movie ${movie.id}`);
					console.error(err);
					res.sendStatus(500);
					return;
				}
				
				if (err && err.errno === -2) {
					data = {};
				} else {
					data = JSON.parse(data);
				}

				data = {
					originalName: torrentFile.name,
					extension: torrentFile.name.split('.').pop(),
					path: movie.file.path,
					fullSize: torrentFile.length
				};

				console.log('writing data', data);
				const stat = JSON.stringify(data);

				fs.writeFile(`/tmp/videos/${movie.id}/${movie.resolution}.json`, stat, (err) => {
					if (err) {
						console.log(`Unexpected error while writing stat file for movie ${movie.id}`);
						console.error(err);
						res.sendStatus(500);
						return;
					}
					
					const torrentFileReadStream = torrentFile.createReadStream();
					const localFileWriteStream = fs.createWriteStream(movie.file.path);
		
					pump(torrentFileReadStream, localFileWriteStream, (err) => {
						if (err) {
							console.log(`${engineId} download pipe closed with error: `);
							console.error(err);
						} else {
							console.log(`${engineId} download pipe closed, movie successfully downloaded!`);
						}
		
						torrentEngineManager.destroyEngine(engineId, () => {
							console.log(`Engine ${engineId} was destroyed!`);
						});
					});

					movie.file.size = 0;
		
					engine.on('idle', () => {
						console.log(`Engine ${engineId} idle ...`);
					});
		
					next();
				});
			});
		});
	}
}, (req, res, next) => {
	const { movie } = req._streaming;

	fs.readFile(`/tmp/videos/${movie.id}/${movie.resolution}.json`, 'utf8', (err, data) => {
		if (err) {
			console.log(`Error reading stat file for movie ${movie.id}`);
			console.error(err);
			res.sendStatus(500);
		} else {
			const stat = JSON.parse(data);
			console.log(`Stat file for movie ${movie.id}[${movie.resolution}]`, stat);
			req._streaming.movie.fullSize = stat.fullSize;
			req._streaming.movie.file.extension = stat.extension;
			next();
		}
	});
}, (req, res, next) => {
	const { movie } = req._streaming;
	
	if (movie.file.size !== movie.fullSize) { // File is still downloading
		console.log(`Movie full size ${movie.fullSize}, local file size ${movie.file.size} ...`);
		console.log('File is still downloading so stream source is torrent');
		
		const engineId = `${movie.id}-${movie.resolution}`;
		const torrentFile = torrentEngineManager.getMovieFileByEngineId(engineId);

		req._streaming.range.end = req._streaming.range.end || torrentFile.length - 1;
		req._streaming.source = {
			readStream: torrentFile.createReadStream(req._streaming.range),
			size: torrentFile.length
		}
	} else { // File is fully downloaded
		console.log('File is fully downloaded so stream source is local file');
		req._streaming.range.end = req._streaming.range.end || movie.file.size - 1;
		req._streaming.source = {
			readStream: fs.createReadStream(movie.file.path, req._streaming.range),
			size: movie.file.size
		}
	}

	next();
}, (req, res) => {
	const { movie, source, range } = req._streaming;

	const chunkSize = (range.end - range.start) + 1
	const head = {
		'Content-Range': `bytes ${range.start}-${range.end}/${source.size}`,
		'Accept-Ranges': 'bytes',
		'Content-Length': chunkSize,
		'Content-Type': `video/${movie.file.extension}`,
	}

	res.writeHead(206, head);
	console.log('Starting stream...');

	pump(source.readStream, res, (err) => {
		console.log(`Movie streaming pipe closed`);
		// console.error(err);
	});
});

app.get('/subtitles/:imdbid/:resolution/:language', (req, res, next) => { // Cheking if /tmp/videos/subs dir exists
	fs.access('/tmp/videos/subs', fs.constants.R_OK, (err) => {
		if (!err) {
			next();
		} else if (err && err.errno === -2) {
			fs.mkdir('/tmp/videos/subs', (err) => {
				if (!err) {
					next();
				} else {
					console.log('Unexpected error while creating /tmp/videos/subs directory')
					console.error(err);
					res.sendStatus(500);
				}
			});
		} else {
			console.log('Unexpected error while accessing /tmp/videos/subs directory')
			console.error(err);
			res.sendStatus(500);
		}
	});
}, (req, res, next) => { // Checking if subtitles is already downloaded
	const { imdbid, resolution, language } = req.params;

	const fileName = `${imdbid}[${resolution}]-${language}.vtt`;
	const fullPath = `/tmp/videos/subs/${fileName}`;

	req._subtitles = {
		file: {
			name: fileName,
			path: fullPath,
			exists: false
		}
	}

	fs.access(fullPath, fs.constants.R_OK, (err) => {
		if (!err) {
			req._subtitles.file.exists = true;
			next();
		} else if (err && err.errno === -2) {
			next();
		} else {
			console.log(`Unexpected error while accessing ${fullPath}`);
			console.error(err);
			res.sendStatus(500);
		}
	});
}, async (req, res, next) => {
	const { imdbid, resolution } = req.params;
	const { file } = req._subtitles;

	if (file.exists) {
		next();
		return;
	}

	fs.readFile(`/tmp/videos/${imdbid}/${resolution}.json`, 'utf8', (err, data) => {
		if (err) {
			console.log(`Unexpected error while reading stat file for ${imdbid}`);
			console.error(err);
			res.sendStatus(500);
			return;
		}

		const movieStat = JSON.parse(data);
		req._subtitles.searchParams = {
			filename: movieStat.originalName,
			fps: "23.976",
			imdbid
		}

		console.log(`Will search subtitles with these params: `, req._subtitles.searchParams);
		next();
	});
}, async (req, res, next) => {
	const { language } = req.params;
	const { file, searchParams } = req._subtitles;

	if (file.exists) {
		next();
		return;
	}

	try {
		const searchResults = await OS.search(searchParams);
	
		if (!searchResults[language]) {
			return res.sendStatus(404); // No subtitles with this language
		} else {
			req._subtitles.srtDownloadUrl = searchResults[language].url;
			console.log('subtitles found', searchResults[language]);
			next();
		}
	} catch (e) {
		console.log('Error while searching subtitles:', searchParams.imdbid);
		console.error(e);
		res.sendStatus(500);
	}
}, async (req, res, next) => {
	const { file, srtDownloadUrl } = req._subtitles;

	if (file.exists) {
		console.log(`Subtitles ${file.name} already downloaded`);
		next();
		return;
	}

	try {
		const response = await axios({
			method: 'GET',
			url: srtDownloadUrl,
			responseType: 'stream'
		});
		
		const localFileWriteStream = fs.createWriteStream(file.path);

		localFileWriteStream.on("open", () => {
			console.log(`Subtitles ${file.name} is not downloaded yet, starting download...`);
			pump(response.data, srt2vtt(), localFileWriteStream, (err) => {
				if (err) {
					console.log(`${file.name} pipe closed with error:`);
					console.error(err);
					res.sendStatus(500);
				} else {
					console.log(`${file.name} pipe closed with no errors, subtitles downloaded`);
					next();
				}
			});
		});
	} catch (e) {
		console.log('Error while downloading subtitles:', srtDownloadUrl);
		console.error(e);
		res.sendStatus(500);
	}
}, async (req, res) => {
	const { file } = req._subtitles;

	fs.stat(file.path, (err, stat) => {
		if (err) {
			console.log('Error occurred while getting subtitles file stats:');
			console.error(err);
			res.sendStatus(500);
		} else {
			const head = {
				'Content-Length': stat.size,
				'Content-Type': 'text/vtt',
			}
			res.writeHead(200, head);

			const subtitlesReadStream = fs.createReadStream(file.path);

			subtitlesReadStream.on("open", () => {
				pump(subtitlesReadStream, res, (err) => {
					if (err) {
						console.log('Subtitles read stream closed with erorr:');
						console.error(err);
					} else {
						console.log('Subtitles read stream closed with no erorrs.');
					}
				});
			});
		}
	});
});

app.listen(port, () => {
	console.log(`server is listening on ${port}`);

	fs.access('/tmp/videos', fs.constants.F_OK, (err) => {
		if (err) { // Directory does not exsist
			return fs.mkdir('/tmp/videos', (err) => {
				if (err) {
					console.log('error while creating /tmp/videos dir');
					return;
				};
				console.log('/tmp/videos dir created');
			});
		}
		console.log('/tmp/videos dir already exsists');
	});
})