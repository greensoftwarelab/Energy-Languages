/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Andreas Schmelz 2016-02-14
*/

const cluster = require('cluster');
const numCPUs = require('os').cpus().length * 2;
var fs = require('fs');

const d = parseInt(process.argv[2]) || 200;

if (d % 8 != 0) {
  console.error('d must be multiple of 8');
  process.exit(-1);
}
if (d * d / numCPUs % 8 != 0) {
  console.error('cannot distribute equal across cpus');
  process.exit(-1);
}

if (cluster.isMaster) {
  var alive = numCPUs;
  var part_buffer = new Array(numCPUs);
  for (var i = 0; i < numCPUs; i++) {
    var worker = cluster.fork();
    var j = i;

    worker.on('message', function(e) {
      part_buffer[this.id - 1] = new Buffer(e.data);
      this.kill();
      alive--;
      if (alive == 0) {
        //var fd = fs.openSync('test3.pbm', 'w');
        //fs.writeSync(fd, 'P4\n'+d+' '+d+'\n');
        process.stdout.write('P4\n'+d+' '+d+'\n')
        for (var i = 0; i < numCPUs; i++) {
          process.stdout.write(part_buffer[i]);
          //fs.writeSync(fd, part_buffer[i], 0, part_buffer[i].length);
        }
      }

    });
  }
} else if (cluster.isWorker) {
  var id = cluster.worker.id;
  var start = Math.floor((id - 1) * d / numCPUs), // incl
      end = Math.floor(id * d / numCPUs);   // excl

  var byte_acc = 0,
      bit_num = 0,
      iter = 50,
      limit = 4;

  //console.log('create buffer with '+(d * d / 8 / numCPUs));
  var buff = new Buffer(d * d / 8 / numCPUs);

  (function() {
    var xd = 2 / d;
    var it = 0;
    for (var y = start; y < end; y++) {
      var yd = 2 * y / d - 1;
      for (var x = 0; x < d; x++) {

        var sum = doCalc(
          xd * x - 1.5,
          yd
        );

        byte_acc |= (sum <= limit);
        bit_num++;

        if (bit_num === 8) {
          buff[it++] = byte_acc;
          byte_acc = 0,
          bit_num = 0;
        } else {
          byte_acc <<= 1;
        }
      }
    }
  })();

  process.send(buff);
}

function doCalc(Cr, Ci) {
  var Zr = 0,
      Zi = 0,
      Tr = 0,
      Ti = 0;
  for (var i = 0; i < iter && Tr + Ti <= limit; i++ ) {
    Zi = 2 * Zr * Zi + Ci,
    Zr = Tr - Ti + Cr,
    Tr = Zr * Zr,
    Ti = Zi * Zi;
  }
  return Tr + Ti;
};