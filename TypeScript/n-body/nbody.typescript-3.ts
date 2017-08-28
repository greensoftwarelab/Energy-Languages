/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy 

   TypeScript install check, transliterated from C#
*/


/// <reference path="../node_modules/@types/node/index.d.ts" />

class NBodySystem {
   private bodies: Body[];

   constructor(){
      this.bodies = [
         Body.Sun(),		
         Body.Jupiter(),
         Body.Saturn(),
         Body.Uranus(),
         Body.Neptune()
      ];

      var px = 0.0;
      var py = 0.0;	
      var pz = 0.0;			
	
      var size = this.bodies.length;
      for (var i=0; i<size; i++){	
         var b = this.bodies[i];
         var m = b.mass;
         px += b.vx * m;
         py += b.vy * m;
         pz += b.vz * m;			
      }		
      this.bodies[0].offsetMomentum(px,py,pz);
   }

   advance(dt: number): void {
      var dx, dy, dz, distance, mag: number;
      var size = this.bodies.length;	
	
      for (var i=0; i<size; i++) {
         for (var j=i+1; j < size; j++) {	
            dx = this.bodies[i].x - this.bodies[j].x;
            dy = this.bodies[i].y - this.bodies[j].y;
            dz = this.bodies[i].z - this.bodies[j].z;
				
            distance = Math.sqrt(dx*dx + dy*dy + dz*dz);				   
            mag = dt / (distance * distance * distance);
				
            this.bodies[i].vx -= dx * this.bodies[j].mass * mag;
            this.bodies[i].vy -= dy * this.bodies[j].mass * mag;
            this.bodies[i].vz -= dz * this.bodies[j].mass * mag;
                                
            this.bodies[j].vx += dx * this.bodies[i].mass * mag;
            this.bodies[j].vy += dy * this.bodies[i].mass * mag;
            this.bodies[j].vz += dz * this.bodies[i].mass * mag;
         }
      }		
		
      for (var i=0; i<size; i++) {
         var body = this.bodies[i];
         body.x += dt * body.vx;
         body.y += dt * body.vy;
         body.z += dt * body.vz;
      }		
   }

   energy(): number {		
      var dx, dy, dz, distance: number;	
      var e = 0.0;		   
      var size = this.bodies.length;
		
      for (var i=0; i < size; i++) {
         e += 0.5 * this.bodies[i].mass * 
            ( this.bodies[i].vx * this.bodies[i].vx 
            + this.bodies[i].vy * this.bodies[i].vy 
            + this.bodies[i].vz * this.bodies[i].vz );
			   
         for (var j=i+1; j < size; j++) {
            dx = this.bodies[i].x - this.bodies[j].x;
            dy = this.bodies[i].y - this.bodies[j].y;
            dz = this.bodies[i].z - this.bodies[j].z;
                                
            distance = Math.sqrt(dx*dx + dy*dy + dz*dz);
            e -= (this.bodies[i].mass * this.bodies[j].mass) / distance;
         }
      }
      return e;
   }
}


class Body {
   private static PI = 3.141592653589793;
   private static SOLAR_MASS = 4 * Body.PI * Body.PI;
   private static DAYS_PER_YEAR = 365.24;

   constructor(
      public x: number, 
      public y: number, 
      public z: number, 
      public vx: number,
      public vy: number, 
      public vz: number,
      public mass: number
   ) { }

   static Jupiter(){
      return new Body(
         4.84143144246472090e+00,
         -1.16032004402742839e+00,
         -1.03622044471123109e-01,
         1.66007664274403694e-03 * Body.DAYS_PER_YEAR,
         7.69901118419740425e-03 * Body.DAYS_PER_YEAR,
         -6.90460016972063023e-05 * Body.DAYS_PER_YEAR,
         9.54791938424326609e-04 * Body.SOLAR_MASS
      );
   }

   static Saturn(){
      return new Body(
         8.34336671824457987e+00,
         4.12479856412430479e+00,
         -4.03523417114321381e-01,
         -2.76742510726862411e-03 * Body.DAYS_PER_YEAR,
         4.99852801234917238e-03 * Body.DAYS_PER_YEAR,
         2.30417297573763929e-05 * Body.DAYS_PER_YEAR,
         2.85885980666130812e-04 * Body.SOLAR_MASS
      );
   }

   static Uranus(){
      return new Body(
         1.28943695621391310e+01,
         -1.51111514016986312e+01,
         -2.23307578892655734e-01,
         2.96460137564761618e-03 * Body.DAYS_PER_YEAR,
         2.37847173959480950e-03 * Body.DAYS_PER_YEAR,
         -2.96589568540237556e-05 * Body.DAYS_PER_YEAR,
         4.36624404335156298e-05 * Body.SOLAR_MASS
      );
   }

   static Neptune(){
      return new Body(
         1.53796971148509165e+01,
         -2.59193146099879641e+01,
         1.79258772950371181e-01,
         2.68067772490389322e-03 * Body.DAYS_PER_YEAR,
         1.62824170038242295e-03 * Body.DAYS_PER_YEAR,
         -9.51592254519715870e-05 * Body.DAYS_PER_YEAR,
         5.15138902046611451e-05 * Body.SOLAR_MASS
      );
   }

   static Sun(){
      return new Body(
         0,
         0,
         0,
         0,
         0,
         0,
         Body.SOLAR_MASS
      );
   }

   offsetMomentum(px, py, pz : number): Body {
      this.vx = -px / Body.SOLAR_MASS;
      this.vy = -py / Body.SOLAR_MASS;
      this.vz = -pz / Body.SOLAR_MASS;	   
      return this;
   }
}


var n = +process.argv[2];
var nbodies = new NBodySystem();

console.log(nbodies.energy().toFixed(9));
for (var i=0; i<n; i++){ nbodies.advance(0.01); }
console.log(nbodies.energy().toFixed(9));
