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
	const { origin } = req.headers;
	const allowedOrigins = ['http://localhost:3000', 'http://localhost:8080'];

	if (allowedOrigins.includes(origin)) {
		res.set('Access-Control-Allow-Origin', origin);
	}

	res.set({
		'Access-Control-Allow-Credentials': 'true',
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
	const { rows } = await db.query(`
		SELECT
			imdb_id, MAX(seen)
		FROM
			history
		GROUP BY
			imdb_id
		HAVING
			NOW() - MAX(seen) > INTERVAL '30 minutes'
	`);
	await db.end()

	for (let i = 0; i != rows.length; i++) {
		const { imdb_id } = rows[i];
		fs.access(`/tmp/videos/${imdb_id}`, fs.constants.R_OK, (err) => {
			if (!err) {
				rimraf(`/tmp/videos/${imdb_id}`, (err) => {
					if (err) {
						console.log(`Error while deleting /tmp/videos/${imdb_id} dir`);
						console.error(err);
					}
				});
			}
		});
	}

	setTimeout(runCleaner, 1000 * 60 * 60);
}, 3000);

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

const getWatched = async (req, res, next) => {
	const { id: user_id } = req.user;
	const { Client } = require('pg');

	const db = new Client({
	  user: 'Hypertube',
	  host: 'localhost',
	  database: 'Hypertube',
	  password: '12345',
	  port: 5433,
	});

	await db.connect()
	const { rows } = await db.query(`
		SELECT
			film_id, count(*)
		FROM
			history
		WHERE
			user_id=$1
		GROUP BY
			film_id
		`,
		[user_id]
	);
	await db.end();

	let watched = {}

	rows.forEach(film => {
		const { film_id } = film;
		watched[film_id] = parseInt(film.count, 10);
	});
	req.user.watched = watched;

	next();
}

app.all('*', userAuth, getWatched);

const setMovieAsWatched = async (imdb_id, userId) => {
	const url = `https://api.themoviedb.org/3/movie/${imdb_id}`;
	const params = {
		api_key: API_KEY
	};

	const response = await axios.get(url, { params });
	const { id: movieId } = response.data;
	
	const { Client }  = require('pg');
	const db = new Client({
		user: 'Hypertube',
		host: 'localhost',
		database: 'Hypertube',
		password: '12345',
		port: 5433,
	});

	await db.connect()
	await db.query( "INSERT into history values ($1, $2, $3)", [userId, movieId, imdb_id]);
	await db.end();
	return true;
};


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

	const watched = req.user.watched;

	if (filters.query) {
		url += "/search/movie";
		params = { ...defaultFilters.search, ...filters };
	} else {
		url += "/discover/movie";
		params = { ...defaultFilters.discover, ...filters };
	}

	params.api_key = API_KEY;
	params.language = req.user.locale;

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

	const watched = req.user.watched;

	try {
		const response = await axios.get(url, { params });
		const { imdb_id, poster_path, backdrop_path } = response.data;
		if (response.data.poster_path) {
			response.data.poster_path = 'http://image.tmdb.org/t/p/w500' + poster_path;
		} else {
			response.data.poster_path = 'http://www.theprintworks.com/wp-content/themes/psBella/assets/img/film-poster-placeholder.png';
		}
		response.data.watched_films_count = watched[movieId] || 0;
		req.movie = { imdb_id, movie_details: response.data };
		next();
	} catch (e) {
		console.error(e);
		res.json({'success': false, 'error': 'TMDb request failed'});
	}
}, (req, res, next) => {
	const { movie_details } = req.movie;
	const { crew, cast } = movie_details.credits;
	const getNamesOnly = (member) => {
		return member.name;
	}

	const directors = crew.filter((member => member.job === "Director")).map(getNamesOnly);
	const producers = crew.filter((member => member.job === "Producer")).map(getNamesOnly);
	const main_cast = cast.splice(0, 5).map(getNamesOnly);

	movie_details.credits = { crew: { directors, producers }, main_cast };

	next();
}, async (req, res, next) => {
	const { imdb_id } = req.movie;

	if (!imdb_id) return next();

	const url = `https://tv-v2.api-fetch.website/movie/${imdb_id}`;

	try {
		const response = await axios.get(url);
		const { torrents, trailer } = response.data;

		if (torrents && torrents['en']) {
			const streaming = Object.keys(torrents['en']).map(resolution => {
				return `http://localhost:3200/film/${imdb_id}/${resolution}`;
			});
			req.movie.streaming = streaming;
		}

		if (trailer) {
			req.movie.movie_details.trailer = trailer;
		}
	} catch (e) {
		console.log('popcorntime request failed...');
		if (e.response) {
			console.log(e.response.status, e.response.text, e.response.data);
		} else {
			console.error(e);
		}
	} finally {
		next();
	}
}, async (req, res, next) => {
	const { imdb_id } = req.movie;
	const searchResults = await OS.search({
		imdbid: imdb_id,
		fps: "23.976",
	});

	const subtitles = ['en', 'ru'].reduce((result, currLang) => {
		if (searchResults[currLang]) {
			result.push({
				kind: 'subtitles',
				src: `http://localhost:3200/subtitles/${imdb_id}/${currLang}`,
				srcLang: currLang
			});
		}
		return result;
	}, []);

	req.movie.subtitles = subtitles;	
	next();
}, (req, res) => {
	const { movie_details, streaming, subtitles } = req.movie;
	res.json({'success': true,  movie_details, streaming, subtitles });
});

const pump = require('pump');
const torrentEngineManager = require('./torrentEngineManager');

app.get('/film/:id/:resolution', (req, res, next) => { // Parsing range headers
	const { id, resolution } = req.params;
	const { range } = req.headers;

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
}, async (req, res, next) => {
	const { range, movie } = req._streaming;
	if (range.start === 0) {
		try {
			setMovieAsWatched(movie.id, req.user.id);
		} catch (e) {
			console.log(`Error while setting movie ${$movie.id} as watched:`);
			console.error(e);
		}
	}

	next();
}, async (req, res, next) => { // Checking if this movie can be downloaded
	const { id, resolution } = req._streaming.movie;

	const response = await axios.get(`https://tv-v2.api-fetch.website/movie/${id}`);

	const { torrents } = response.data;
	if (!torrents) { // There is no torrent links for this movie, so it can't be downloaded
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
			next();
		} else if (err && err.errno === -2) { // Dir not exists
			fs.mkdir(movieDirPath, (err) => {
				if (err) {
					console.error(err);
					res.sendStatus(500);
				} else {
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
			next();
		} else if (err && err.errno === -2) {
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
		torrentEngineManager.createNewEngine(engineId, movie.magnetLink, (engine) => {
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
						}
		
						torrentEngineManager.destroyEngine(engineId);
					});

					movie.file.size = 0;
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
			req._streaming.movie.fullSize = stat.fullSize;
			req._streaming.movie.file.extension = stat.extension;
			next();
		}
	});
}, (req, res, next) => {
	const { movie } = req._streaming;
	
	if (movie.file.size !== movie.fullSize) { // File is still downloading
		
		const engineId = `${movie.id}-${movie.resolution}`;
		const torrentFile = torrentEngineManager.getMovieFileByEngineId(engineId);

		req._streaming.range.end = req._streaming.range.end || torrentFile.length - 1;
		req._streaming.source = {
			readStream: torrentFile.createReadStream(req._streaming.range),
			size: torrentFile.length
		}
	} else { // File is fully downloaded
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

	pump(source.readStream, res);
});

app.get('/subtitles/:imdbid/:language', (req, res, next) => { // Cheking if /tmp/videos/subs dir exists
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
	const { imdbid, language } = req.params;

	const fileName = `${imdbid}-${language}.vtt`;
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
	const { imdbid } = req.params;
	const { file } = req._subtitles;

	if (file.exists) {
		next();
		return;
	}

	fs.readdir(`/tmp/videos/${imdbid}`, (err, files) => {
		if (err) {
			console.log('Error while reading movie directory:');
			console.error(err);
			res.sendStatus(500);
			return;
		}

		const statFile = files.find(fileName => {
			const extension = fileName.split('.').pop();
			return extension === 'json';
		});

		fs.readFile(`/tmp/videos/${imdbid}/${statFile}`, 'utf8', (err, data) => {
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
	
			next();
		});
		
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
			pump(response.data, srt2vtt(), localFileWriteStream, (err) => {
				if (err) {
					console.log(`${file.name} pipe closed with error:`);
					console.error(err);
					res.sendStatus(500);
				} else {
					next();
				}
			});
		});
	} catch (e) {
		console.log('Error while downloading subtitles:', srtDownloadUrl);
		if (e.response) {
			console.log('error status', e.response.status);
			res.sendStatus(e.response.status);
		} else {
			console.error(e);
		}
	}
}, async (req, res) => {
	const { file } = req._subtitles;

	fs.stat(file.path, (err, stat) => {
		if (err) {
			console.log('Error occurred while getting subtitles file stats:');
			console.error(err);
			res.sendStatus(500);
		} else {
			res.sendFile(file.path);
		}
	});
});

app.listen(port, () => {
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
