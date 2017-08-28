"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Isaac Gouy *"!

Object subclass: #Body
   instanceVariableNames: 'x y z vx vy vz mass'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

Object subclass: #NBodySystem
   instanceVariableNames: 'bodies'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Shootout'!

!Body methodsFor: 'accessing'!
mass
   ^mass! !

!Body methodsFor: 'accessing'!
x
   ^x! !

!Body methodsFor: 'accessing'!
x: d1 y: d2 z: d3 vx: d4 vy: d5 vz: d6 mass: d7
   x := d1.
   y := d2. 
   z := d3. 
   vx := d4.
   vy := d5.
   vz := d6.
   mass := d7! !

!Body methodsFor: 'accessing'!
y
   ^y! !

!Body methodsFor: 'accessing'!
z
   ^z! !

!Body methodsFor: 'nbody'!
addMomentumTo: anArray
   anArray at: 1 put: (anArray at: 1) + (vx * mass).
   anArray at: 2 put: (anArray at: 2) + (vy * mass).
   anArray at: 3 put: (anArray at: 3) + (vz * mass).
   ^anArray! !

!Body methodsFor: 'nbody'!
and: aBody velocityAfter: dt        
   | dx dy dz distance mag |
   dx := x - aBody x.
   dy := y - aBody y.
   dz := z - aBody z.
   
   distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.
   mag := dt / (distance * distance * distance).

   self decreaseVelocity: dx y: dy z: dz m: aBody mass * mag.   
   aBody increaseVelocity: dx y: dy z: dz m: mass * mag! !

!Body methodsFor: 'nbody'!
decreaseVelocity: dx y: dy z: dz m: m
   vx := vx - (dx * m).
   vy := vy - (dy * m).
   vz := vz - (dz * m)! !

!Body methodsFor: 'nbody'!
increaseVelocity: dx y: dy z: dz m: m
   vx := vx + (dx * m).
   vy := vy + (dy * m).
   vz := vz + (dz * m)! !

!Body methodsFor: 'nbody'!
kineticEnergy
   ^0.5d0 * mass * ((vx * vx) + (vy * vy) + (vz * vz))! !

!Body methodsFor: 'nbody'!
offsetMomentum: anArray 
   | m |
   m := self class solarMass.
   vx := (anArray at: 1) negated / m.
   vy := (anArray at: 2) negated / m.
   vz := (anArray at: 3) negated / m! !

!Body methodsFor: 'nbody'!
positionAfter: dt
   x := x + (dt * vx).
   y := y + (dt * vy).
   z := z + (dt * vz)! !

!Body methodsFor: 'nbody'!
potentialEnergy: aBody
   | dx dy dz distance |
   dx := x - aBody x.
   dy := y - aBody y.
   dz := z - aBody z.

   distance := ((dx*dx) + (dy*dy) + (dz*dz)) sqrt.
   ^mass * aBody mass / distance! !


!Body class methodsFor: 'constants'!
daysPerYear
   ^365.24d0! !

!Body class methodsFor: 'constants'!
jupiter
   ^self new
      x: 4.84143144246472090d0
      y: -1.16032004402742839d0
      z: -1.03622044471123109d-1
      vx: 1.66007664274403694d-3 * self daysPerYear
      vy: 7.69901118419740425d-3 * self daysPerYear
      vz: -6.90460016972063023d-5 * self daysPerYear
      mass: 9.54791938424326609d-4 * self solarMass! !

!Body class methodsFor: 'constants'!
neptune
   ^self new
      x: 1.53796971148509165d1
      y: -2.59193146099879641d1
      z: 1.79258772950371181d-1
      vx: 2.68067772490389322d-3 * self daysPerYear
      vy: 1.62824170038242295d-3 * self daysPerYear
      vz: -9.51592254519715870d-5 * self daysPerYear
      mass: 5.15138902046611451d-5 * self solarMass! !

!Body class methodsFor: 'constants'!
pi
   ^3.141592653589793d0! !

!Body class methodsFor: 'constants'!
saturn
   ^self new
      x: 8.34336671824457987d0
      y: 4.12479856412430479d0
      z: -4.03523417114321381d-1
      vx: -2.76742510726862411d-3 * self daysPerYear
      vy: 4.99852801234917238d-3 * self daysPerYear
      vz: 2.30417297573763929d-5 * self daysPerYear
      mass: 2.85885980666130812d-4 * self solarMass! !

!Body class methodsFor: 'constants'!
solarMass
   ^4.0d0 * self pi * self pi! !

!Body class methodsFor: 'constants'!
sun
   ^self new
      x: 0.0d0
      y: 0.0d0
      z: 0.0d0
      vx: 0.0d0
      vy: 0.0d0
      vz: 0.0d0
      mass: self solarMass! !

!Body class methodsFor: 'constants'!
uranus
   ^self new
      x: 1.28943695621391310d1
      y: -1.51111514016986312d1
      z: -2.23307578892655734d-1
      vx: 2.96460137564761618d-3 * self daysPerYear
      vy: 2.37847173959480950d-3 * self daysPerYear
      vz: -2.96589568540237556d-5 * self daysPerYear
      mass: 4.36624404335156298d-5 * self solarMass! !


!NBodySystem methodsFor: 'nbody'!
after: dt
   1 to: bodies size do: [:i|
      i+1 to: bodies size do: [:j|                            
         (bodies at: i) and: (bodies at: j) velocityAfter: dt].
   ].   
   bodies do: [:each| each positionAfter: dt]! !

!NBodySystem methodsFor: 'nbody'!
energy
   | e |
   e := 0.0d0.
   1 to: bodies size do: [:i|       
      e := e + (bodies at: i) kineticEnergy.

      i+1 to: bodies size do: [:j| 
         e := e - ((bodies at: i) potentialEnergy: (bodies at: j))].
   ].
   ^e! !

!NBodySystem methodsFor: 'initialize-release'!
initialize
   bodies := OrderedCollection new
      add: Body sun; add: Body jupiter; add: Body saturn;
      add: Body uranus; add: Body neptune; yourself.

   bodies first offsetMomentum:
      (bodies inject: (Array with: 0.0d0 with: 0.0d0 with: 0.0d0)
         into: [:m :each | each addMomentumTo: m])! !


!Tests class methodsFor: 'benchmark scripts'!
nbody
   | bodies |
   bodies := NBodySystem new initialize.
   self stdout print: bodies energy digits: 9; nl.
   self arg timesRepeat: [bodies after: 0.01d0].
   self stdout print: bodies energy digits: 9; nl.
   ^''! !