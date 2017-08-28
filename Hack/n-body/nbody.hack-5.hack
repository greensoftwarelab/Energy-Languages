<?hh

/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Stuart Carnie
   converted from C++ version by Branimir Maksimovic
*/

define('PI', 3.141592653589793);
define('SOLAR_MASS', 4.0 * PI * PI);
define('DAYS_PER_YEAR', 365.24);

class Body {
    public float $x, $y, $z, $vx, $vy, $vz, $mass;

    public static function jupiter():Body {
        static $p;
        if (!isset($p)) {
            $p = new Body;
            $p->x = 4.84143144246472090e+00;
            $p->y = -1.16032004402742839e+00;
            $p->z = -1.03622044471123109e-01;
            $p->vx = 1.66007664274403694e-03 * DAYS_PER_YEAR;
            $p->vy = 7.69901118419740425e-03 * DAYS_PER_YEAR;
            $p->vz = -6.90460016972063023e-05 * DAYS_PER_YEAR;
            $p->mass = 9.54791938424326609e-04 * SOLAR_MASS; 
        }
        return $p;
    }

    public static function saturn():Body {
        static $p;
        if (!isset($p)) {
            $p = new Body;
            $p->x = 8.34336671824457987e+00;
            $p->y = 4.12479856412430479e+00;
            $p->z = -4.03523417114321381e-01;
            $p->vx = -2.76742510726862411e-03 * DAYS_PER_YEAR;
            $p->vy = 4.99852801234917238e-03 * DAYS_PER_YEAR;
            $p->vz = 2.30417297573763929e-05 * DAYS_PER_YEAR;
            $p->mass = 2.85885980666130812e-04 * SOLAR_MASS;
        }
        return $p;
    }

    public static function uranus():Body {
        static $p;
        if (!isset($p)) {
            $p = new Body;
            $p->x = 1.28943695621391310e+01;
            $p->y = -1.51111514016986312e+01;
            $p->z = -2.23307578892655734e-01;
            $p->vx = 2.96460137564761618e-03 * DAYS_PER_YEAR;
            $p->vy = 2.37847173959480950e-03 * DAYS_PER_YEAR;
            $p->vz = -2.96589568540237556e-05 * DAYS_PER_YEAR;
            $p->mass = 4.36624404335156298e-05 * SOLAR_MASS;
        }
        return $p;
    }

    public static function neptune():Body {
        static $p;
        if (!isset($p)) {
            $p = new Body;
            $p->x = 1.53796971148509165e+01;
            $p->y = -2.59193146099879641e+01;
            $p->z = 1.79258772950371181e-01;
            $p->vx = 2.68067772490389322e-03 * DAYS_PER_YEAR;
            $p->vy = 1.62824170038242295e-03 * DAYS_PER_YEAR;
            $p->vz = -9.51592254519715870e-05 * DAYS_PER_YEAR;
            $p->mass = 5.15138902046611451e-05 * SOLAR_MASS;
        }
        return $p;
    }

    public static function sun():Body {
        static $p;
        if (!isset($p)) {
            $p = new Body;
            $p->x = 0.0;
            $p->y = 0.0;
            $p->z = 0.0;
            $p->vx = 0.0;
            $p->vy = 0.0;
            $p->vz =0.0;
            $p->mass = SOLAR_MASS;
        }
        return $p;
    }


    public function offsetMomentum(float $px, float $py, float $pz):void {
        $this->vx = -$px / SOLAR_MASS;
        $this->vy = -$py / SOLAR_MASS;
        $this->vz = -$pz / SOLAR_MASS;
    }
}

class NBodySystem {

    private Vector<Body> $bodies;

    function __construct() {
        $this->bodies = Vector<Body> {
            Body::sun(),
            Body::jupiter(),
            Body::saturn(),
            Body::uranus(),
            Body::neptune()
        };

        $px = 0.0; $py = 0.0; $pz = 0.0;
        foreach ($this->bodies as $body) {
            $px += $body->vx * $body->mass;
            $py += $body->vy * $body->mass;
            $pz += $body->vz * $body->mass;
        }
        $this->bodies[0]->offsetMomentum($px, $py, $pz);
    }

    public function advance(float $dt):void {
        $bodies = $this->bodies->count();
        
        for ($i=0; $i<$bodies; ++$i) {
            $iBody = $this->bodies[$i];
            for ($j=$i+1; $j<$bodies; ++$j) {
                $jBody = $this->bodies[$j];

                $dx = $iBody->x - $jBody->x;
                $dy = $iBody->y - $jBody->y;
                $dz = $iBody->z - $jBody->z;
                $distance = \sqrt($dx*$dx + $dy*$dy + $dz*$dz);
                $mag = $dt / ($distance * $distance * $distance);
                $iBody->vx -= $dx * $jBody->mass * $mag;
                $iBody->vy -= $dy * $jBody->mass * $mag;
                $iBody->vz -= $dz * $jBody->mass * $mag;

                $jBody->vx += $dx * $iBody->mass * $mag;
                $jBody->vy += $dy * $iBody->mass * $mag;
                $jBody->vz += $dz * $iBody->mass * $mag;
            }
        }
        for ($i=0; $i<$bodies; ++$i) {
            $body = $this->bodies[$i];
            $body->x += $dt * $body->vx;
            $body->y += $dt * $body->vy;
            $body->z += $dt * $body->vz;
        }
    }

    public function energy():float {
        $e = 0.0;

        $bodies = $this->bodies->count();
        for ($i=0; $i<$bodies; ++$i) {
            $body = $this->bodies[$i];

            $e += 0.5 * $body->mass *
                ($body->vx * $body->vx
                 + $body->vy * $body->vy
                 + $body->vz * $body->vz);

            for ($j=$i+1; $j<$bodies; ++$j) {
                $jBody = $this->bodies[$j];
                
                $dx = $body->x - $jBody->x;
                $dy = $body->y - $jBody->y;
                $dz = $body->z - $jBody->z;

                $distance = \sqrt($dx*$dx + $dy*$dy + $dz*$dz);
                $e -= ($body->mass * $jBody->mass) / $distance;
            }
        }
        return $e;
    }
}

$n = $argv[1] + 1;

$bodies = new NBodySystem;

printf("%.9f\n", $bodies->energy());

while (--$n) {
    $bodies->advance(0.01);
}

printf("%.9f\n", $bodies->energy());