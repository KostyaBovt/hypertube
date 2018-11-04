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

var list = walkSync('/tmp/videos');
console.log(list);
console.log('\n\n');
console.log(list.map( element => element.substring(4, element.length)));