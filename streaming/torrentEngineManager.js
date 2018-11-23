const torrentStream = require('torrent-stream');

const _engines = {};

const engineExists = (id) => {
    return !!_engines[id];
}

const createNewEngine = (id, magnetLink, callback) => {
    if (_engines[id] !== undefined) {
        const { status, engine } = _engines[id];
        if (status === 'creating') {
            engine.on('ready', () => {
                status = 'ready';
                callback(engine);
            });
        } else {
            console.log("Something strange is happening in the engineManager:");
            console.log(`Trying to create engine ${id} when it already exists with status '${status}'`);
            return null;
        }
    } else {
        _engines[id] = {
            engine: torrentStream(magnetLink),
            status: 'creating'
        };
    
        _engines[id].engine.on('ready', () => {
            _engines[id].status = 'ready';
            callback(_engines[id].engine);
        });
    }
}

const destroyEngine = (id, callback) => {
    const { engine } = _engines[id];
    if (engine !== undefined) {
        engine.remove(() => {
            engine.destroy(() => {
                _engines[id] = undefined;
                callback();
            });
        });
    }
}

const getEnginebyId = (id) => {
    return _engines[id];
}

const getMovieFileByEngineId = (id) => {
    const { files } = _engines[id].engine;

    for (let i = 0; i !== files.length; i++) {
        const extension = files[i].name.split('.').pop();
        if (extension === 'mp4' || extension === 'mkv') {
            return files[i];
        }
    }
    return null;
}

module.exports = {
    engineExists,
    createNewEngine,
    destroyEngine,
    getEnginebyId,
    getMovieFileByEngineId
}
