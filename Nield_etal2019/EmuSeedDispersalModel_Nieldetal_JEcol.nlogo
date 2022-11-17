;; NETLOGO MODEL FOR EXAMINING SEED DISPERSAL BY THE EMU AND NULL MODELS OF MOVEMENT

;;Model Scaling
;; World is a 5.05 x 5.05 km landscape.
;; World consists of 101 x 101 pixels ~ each pixel represents c. 50 x 50 m cell i.e 5.05 km x 5.05km simulated world.
;; Simulation runs for 1000 hours (ticks), an assumed available fruit period.
;; 1 tick = 1 hr
;; 24 ticks = 24 hr (1 day)

;;TOGGLE TO DISABLE R EXTENSION IF HOSTING SIMULATIONS WITHIN R VIA RNETLOGO

;;extensions
;;[
  ;;r
;;:]


__includes ["distributions.nls"]

;;Define global variables
globals
[
  world-size
  distance-list
  n-seeds-list
  n-events
  heading-list
  step-length-list
  sorted-list

  rep-list
  xcor-list
  ycor-list
  emu-xcor-list
  emu-ycor-list
  scat-xcor-list
  scat-ycor-list
  step-list

  run-id

  t-trunc
]

;;Define agent 'breeds'
breed [emus emu]
breed [scats scat]

;;Initialise frugivore characteristics
emus-own

[
  seeds-consumed
  time-since-last-fed
  time-since-excrete
  inplant?
  seed-origin
  seeds-in-event
  seed-drop-time
  seed-travel-distance
  seed-dx
  seed-dy
  searching-time
  feed-count-limit
  start-patch
  origin-dist
  t_FLightLength
  real_x
  real_y

  last-walk-trunc?
 ]

;;Initialise patch characteristics
patches-own
[
  impassable?
  d-impassable
  num-seeds
  plant?
  n-barrier-nhbs
  n-visits nhbs
  moved-through?
  g
]

;;Initialise scat characteristics
scats-own
[
  n-seeds
  real_x
  real_y
]

;; Set RESET button function
to reset
  ca
  resize-world 0 (dimension - 1) 0 (dimension - 1)
  set-patch-size 2.2 * (200 / dimension)
  ask patches [
    set pcolor white
    set moved-through? false
    set impassable? false
  ]
end

to test-mv
  create-emus 1
  [
    setxy 48.818 163.256
    set heading 292.0965
  ]
end

to setup

  ca

  if not specify-nl-seed? [ set nl-seed new-seed ]
  random-seed nl-seed

  reset

  ifelse lsp-model = "percolation"
  [build-lsp-percolate] [build-lsp-diffusion]

  sprout-plants
  create-dispersers
end

;; Set OVERLAY HABITAT button function - modified to use the Eden code from the book for speed
to build-lsp-percolate
  let filled 0
  set world-size count patches

  ask patches [set impassable? false]
  let impassable-set nobody

 ;; seeds the grid with one patch as suitable
 ;; this sequentially fills the grid by selecting patches and making them suitable

  if prop-impassable > 0
  [
   ask one-of patches [
    set impassable? true
    set filled 1
    set impassable-set neighbors4
  ]

 while [filled <= (prop-impassable * world-size)]
 [
   ;; if rnd test is less than attract-suitable then an *unsuitable* patch next to a suitable patch is made suitable

    ifelse random-float 1 <= attract-impassable
    [
        ask one-of impassable-set [
    ;; if m = 0 then this is the classical Eden process as per Eden 1961, otherwise it's the noise-reduced form
          set impassable? true
          let new-edge-cells neighbors4 with [impassable? = false]

      ;; rebuilds the perimeter-list with the new candidates added and the newly colonised patch removed
          set impassable-set (patch-set (other impassable-set) new-edge-cells)
          set filled filled + 1
        ]
      ]
;    ;; otherwise pick a patch at random (this could be neighbouring a suitable patch)
;    ;; if you want to make sure it does not you'd need to use
;    ;; ask one-of patches with [(not impassable?) and (count neighbors with [impassable?] = 0)]
    [
      ask one-of patches with [not impassable?]
      [
        set impassable? true

        let new-edge-cells neighbors4 with [impassable? = false]
        set impassable-set (patch-set impassable-set new-edge-cells)

        set filled filled + 1
      ]
    ]
   ]

 ;; if single patch areas not allowed get rid of them

 if singles? = false
 [
   let drop 0
      ask patches with [impassable?]
   [
     if count neighbors with [impassable?] = 0
     [ set impassable? false
       set drop drop + 1 ]
   ]




  ask n-of drop impassable-set [set impassable? false]
 ]

    ]
    tidy-landscape
end

to tidy-landscape

 ;; make them brown
 ask patches with [impassable?] [
    set pcolor brown
    set d-impassable 0
  ]

  ifelse prop-impassable > 0
 [
  ;; This is quite slow?
 ask patches with [pcolor != brown]
 [
   ; set n-barrier-nhbs count neighbors with [pcolor = brown]
   let local-impass patches in-radius 5 with [ impassable? ]

   ifelse any? local-impass
   [
     set d-impassable distance (min-one-of local-impass [ distance myself ])
   ]
   [
     set d-impassable sqrt 2 * world-width
   ]
  ]

    ]
[

  if prop-impassable = 0 [ ask patches [ set d-impassable sqrt 2 * world-width ] ]
  ]
end


to build-lsp-diffusion
    if prop-impassable > 0
  [
    ask patches
  [
    set g random-float 1
  ]

  repeat smooth-impassable
  [
    diffuse g 0.1
  ]

  let g-list sort [g] of patches
  let thresh item (0.9 * count patches) g-list

  ask patches with [g > thresh] [set impassable? true]

  ;; if single patch areas not allowed get rid of them
  let drop 0
  if singles? = false
  [
    ask patches with [impassable?]
    [
      if count neighbors with [impassable?] = 0
      [ set impassable? false
        set drop drop + 1 ]
    ]
    let candidates patch-set ([neighbors4] of patches with [not impassable?])
    ask n-of drop candidates [ set impassable? true]
  ]




  tidy-landscape
  ]
end


to build-radii
  ask patches
  [
    set nhbs other patches in-radius 2 with [pcolor != brown]
  ]
end

;;Create dispersal agents and associated lists via CREATE DISPERSERS button
to create-dispersers

   set distance-list []
   set n-seeds-list []
   set heading-list []
   set step-length-list []
   set sorted-list []
   set xcor-list []
   set ycor-list []
   set emu-xcor-list []
   set emu-ycor-list []
   set scat-ycor-list []
   set scat-xcor-list []


  let sprout-patch max-one-of patches [d-impassable]
  ; ask n-of num-dispersers patches with [pcolor != brown and d-impassable > detect-distance]
  ask sprout-patch
  [
      sprout-emus 1
      [
        set seed-origin []
        set seeds-in-event []
        set seed-drop-time []
        set seed-travel-distance []
        set seed-dx []
        set seed-dy []

        set real_x xcor
        set real_y ycor

        set start-patch patch-here

        set last-walk-trunc? false

        set feed-count-limit 0
        set breed emus ;;
        set size 3
        set shape "bird"
        ;; setxy random-xcor random-ycor
        set color black
        set time-since-last-fed 999
        set time-since-excrete 0

        set heading 360

        if pen-down? [pd]
      ]

  ]
    build-radii
  reset-ticks
end

;;  Create plant patches within the landscape via SPROUT PLANTS button
to sprout-plants
  let n-plant-patches n-plants * count patches

  ifelse matern-cluster?
  [
       ask n-of kappa patches
       [
         ask n-of mu patches in-radius r
         [
           set pcolor green
         ]
       ]
       ask patches
       [
    if pcolor = green
    [
      set num-seeds random-poisson pois-seeds-per-plant-patch
      set plant? true
    ]
  ]
  ]
   [
    ask n-of n-plant-patches patches [
      set pcolor green
      set plant? true
      set num-seeds random-poisson pois-seeds-per-plant-patch
    ]
  ]

end

;; RUN SIMULATION
to run-simulation

  ask emus [move-emus]
  ask emus [consume-seed]
  ask patches [check-plants-have-seeds]
  ask emus [emu-defecate]
  tick
  ; if ticks = 1000 [stop] ;; stop simulation when 100 hrs have been reached
end

;; Move emu (agent) according to selected movement model on the home screen.
to move-emus

  let target no-patches
  let start-move patch-here

  ask patch-here [set moved-through? true]
  set time-since-last-fed time-since-last-fed + 1

  if random-float 1 >= p-lazy ;; 80% chance the emu will actually move in a tick
  [
    let range-1 1 ;; This is the range that movement is restricted to when an edge is approached. i.e. the agent can only move one-step

    ifelse approx-1d?
    [
      set target patch-at-heading-and-distance 0 1    ; shouldn't this be dependent on the walk?
    ]

    [
      set target get-walk-target
    ]

  face target
  let start-heading heading  ;; need this to compute dx and dy as if turtle not on centre and then faces patch heading shifts
;  if [impassable?] of patch-ahead 1 = false
;  [
;      fd 1
;  ]

  ; move towards the target step-by-step until it is reached or a barrier detected
  while [detect-barrier = false and patch-here != target]
  [
      face target   ;; keep re-orienting so as not to miss it (grid geometry)
      fd 1
  ]

  ifelse patch-here != target
    [set last-walk-trunc? true]
    [set last-walk-trunc? false]

  ;; distance to origin of movement
  let dij distance start-move
  set heading start-heading

  set seed-travel-distance map [ ?1 -> ?1 + dij ] seed-travel-distance

  set seed-dx map [ ?1 -> ?1 + (dx * dij) ] seed-dx
  set seed-dy map [ ?1 -> ?1 + (dy * dij) ] seed-dy

  ;; set displ-x displ-x + (dx * dij)
  ;; set displ-y displ-y + (dy * dij)

  set origin-dist distance start-patch

  set real_x real_x + (dx * dij)
  set real_y real_y + (dy * dij)


  set heading-list lput (heading) heading-list
  set step-length-list lput (dij) step-length-list
  set emu-xcor-list lput (real_x) emu-xcor-list
  set emu-ycor-list lput (real_y) emu-ycor-list


  ;move-to target
  ]
end

to-report check-target [t-p]

    let check false
    if t-p != nobody
    [
      if [impassable?] of t-p = false [
         set check true
     ]
    ]
    report check

end


to remove-seed

     set num-seeds num-seeds / 2 ;; as the turtles move across the landscape, they consume half the number of seeds on the patch each time the agent moves over it

end

to check-plants-have-seeds
 if plant? = true
  [ if num-seeds <= 80
    [
    set pcolor orange ;; if when an agent moves over the patch and the majority of seeds have been consumed, the patch turns brown
  ]
  ]
end

to consume-seed
 ; set seeds-consumed seeds-consumed + (num-seeds / 2) ;; turtle is ingesting/removing half the number of seeds of the plant
  if pcolor = green
  [
    set seed-origin lput patch-here seed-origin
    ;set seeds-in-event lput floor (num-seeds / 2) seeds-in-event

    let seeds-taken random-poisson (num-seeds / 2)
    if seeds-taken > num-seeds [set seeds-taken num-seeds]


    set seeds-in-event lput seeds-taken seeds-in-event

    if (distribution-GRT = "exponential")
    [
      set seed-drop-time lput ceiling (ticks + abs random-exponential mean-GRT) seed-drop-time
    ]

    if (distribution-GRT = "gamma")
    [
      let alpha  mean-GRT * mean-GRT / var-GRT
      let lambda  1 / (var-GRT / mean-GRT)

      set seed-drop-time lput ceiling (ticks + abs random-gamma alpha lambda ) seed-drop-time
    ]


    set seed-travel-distance lput 0 seed-travel-distance
    set seed-dx lput 0 seed-dx
    set seed-dy lput 0 seed-dy

    set feed-count-limit feed-count-limit + 1
    set time-since-last-fed 0
    set n-visits n-visits + 1

    set num-seeds num-seeds - seeds-taken ;; as the turtles move across the landscape, they consume half the number of seeds on the patch each time the agent moves over it
  ]
end

to emu-defecate

 let idx 0
 ; let focal-patch patch-here

 if length seed-drop-time > 0
[

 foreach seed-drop-time
  [ ?1 ->

    ifelse ?1 - ticks <= 0
    [
      ;;let dij 0
      ;;set dij distance (item idx seed-origin)
      ;; set dij eucl-distance-on-torus (item idx seed-origin) pxcor pycor

      set n-seeds-list lput (item idx seeds-in-event) n-seeds-list
      set distance-list lput (sqrt ((item idx seed-dx) ^ 2 + (item idx seed-dy) ^ 2)) distance-list
      ;;put (item idx seed-travel-distance) distance-list;;
      hatch-scats 1 [
        set shape "dot" set color pink
        set n-seeds n-seeds + last n-seeds-list
        set xcor-list lput xcor xcor-list
        set ycor-list lput ycor ycor-list
        set scat-xcor-list lput (real_x) scat-xcor-list
        set scat-ycor-list lput (real_y) scat-ycor-list
      ]

      ;; Now remove scat from relevant lists
      set seed-origin remove-item idx seed-origin
      set seeds-in-event remove-item idx seeds-in-event
      set seed-drop-time remove-item idx seed-drop-time
      set seed-travel-distance remove-item idx seed-travel-distance
      set seed-dx remove-item idx seed-dx
      set seed-dy remove-item idx seed-dy




      set n-events n-events + 1
    ]
    [
      set idx idx + 1
    ]
  ]
]
end

to write-distance
  let file-name (word dump-path "distance_" behaviorspace-run-number ".txt")
  file-open file-name
  (foreach distance-list n-seeds-list
    [ [?1 ?2] ->
      file-type behaviorspace-run-number file-type " " file-type precision ?1 3 file-type " " file-print ?2
  ])
  file-close-all
end

to write-step-length
  let file-name (word dump-path "steplength_" behaviorspace-run-number ".txt")
  file-open file-name
  (foreach step-length-list step-length-list
    [ [?1 ?2] ->
      file-type precision ?1 3 file-type " " file-print ?2
  ])
  file-close-all
end

to write-visited-locations

  let file-name (word dump-path "visited_" behaviorspace-run-number ".txt")
  file-open file-name
  (foreach emu-xcor-list emu-ycor-list
    [ [?1 ?2] ->
      file-type behaviorspace-run-number file-type " " file-type precision ?1 3 file-type " " file-print ?2
  ])
  file-close-all
end


to write-deposition
  let file-name (word dump-path "deposited_" behaviorspace-run-number ".txt")
  file-open file-name
  (foreach scat-xcor-list scat-ycor-list
    [ [?1 ?2] ->
      file-type behaviorspace-run-number file-type " " file-type precision ?1 3 file-type " " file-print ?2
  ])
  file-close-all
end


to-report get-percentiles [data q]
  set sorted-list sort (data)

  ; set i (length (sorted-list) * 95) / 100 + 0.5
  let u round (length data) * q
  let l round (length data) * (1 - q)
  let m 0.5
  ; set fraction i - round(i)
  ; set round-i round(i)
  ; set kplus1 round-i + 1

  report (list (item l sorted-list ) (item m sorted-list ) (item u sorted-list ))
end


to-report get-walk-target

  let target nobody
  let ok-target? false
  let tries 0
  let range-1 1

  if (type-of-walk = "simple random walk" )
  [
    let step-length 1.0
    set target patch-at-heading-and-distance (random-float 360) step-length
  ]

  if (type-of-walk = "correlated random walk")
  [
    let step-length 1.0
    set target patch-at-heading-and-distance (random-normal 0 stdev-angle) step-length
  ]

  if (type-of-walk = "levy walk")
  [
    let step-length r-cauchy 0 1 ;; the step-length selection produces a walk with a heavy-tailed distribution with a power-law exponent ~2.
    set target patch-at-heading-and-distance (random-float 360) step-length
  ]

  if (type-of-walk = "emu model")
  [
    ;; Move emus according to uniform turning angle distribution and step lengths taken from heavy-tailed (log-normal) distribution
    let sd 319.70
    let ave 262.21

    let step-length (random-lognormal ave sd) / 50
    set target patch-at-heading-and-distance (random-float 360) step-length ;; current uniform distribution of turning angles. Effectively a levy walk at this stage.
  ]

  report target
end

to set-dump-local
  set dump-path "/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/"
end


to-report detect-barrier
  let detect? false

  if d-impassable < detect-distance
  [
    let p-avoid exp(- beta0 * (d-impassable * 50))
    if ((random-float 1 <= p-avoid) or ([impassable?] of patch-ahead 1 = true))  [set detect? true]
  ]
  report detect?
end
@#$#@#$#@
GRAPHICS-WINDOW
2
10
419
428
-1
-1
2.2
1
10
1
1
1
0
1
1
1
0
199
0
199
1
1
1
ticks
30.0

SLIDER
79
505
256
538
prop-impassable
prop-impassable
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
78
540
258
573
attract-impassable
attract-impassable
0
1
0.0
0.01
1
NIL
HORIZONTAL

SWITCH
263
535
368
568
singles?
singles?
1
1
-1000

MONITOR
1088
57
1237
102
proportion impassable habitat
count patches with [impassable? = true] / world-size
2
1
11

SLIDER
465
217
637
250
num-dispersers
num-dispersers
0
10
1.0
1
1
NIL
HORIZONTAL

BUTTON
836
45
948
78
Run Simulation
run-simulation
T
1
T
OBSERVER
NIL
R
NIL
NIL
1

SLIDER
836
10
1009
43
simulation-length
simulation-length
0
1000
5.0
1
1
days
HORIZONTAL

BUTTON
476
10
602
43
Reset Landscape
reset
NIL
1
T
OBSERVER
NIL
L
NIL
NIL
1

MONITOR
1139
10
1237
55
n plant patches
count patches with [pcolor = green]
0
1
11

SLIDER
603
46
695
79
n-plants
n-plants
0
1.0
0.25
.01
1
NIL
HORIZONTAL

MONITOR
1111
103
1238
148
proportion forest habitat
count patches with [pcolor = black] / world-size + count patches with [pcolor = green] / world-size
2
1
11

BUTTON
836
80
899
113
Step
run-simulation
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

PLOT
1043
340
1243
490
Scat kernel
Distance (x 50 m)
Frequency
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if ticks mod 20 = 0 [histogram distance-list]"

SWITCH
650
214
751
247
pen-down?
pen-down?
0
1
-1000

MONITOR
1123
195
1180
240
seeds
sum [seeds-in-event] of emu 0
0
1
11

MONITOR
1073
243
1170
288
Mean dispersal distance (m)
mean distance-list * 50
2
1
11

MONITOR
1181
195
1238
240
Events
n-events
0
1
11

MONITOR
1172
243
1238
288
Prop search
mean [searching-time] of emus / ticks
3
1
11

MONITOR
1124
290
1239
335
Maximum dispersal distance (m)
max distance-list * 50
2
1
11

MONITOR
1016
290
1122
335
Median dispersal distance (m)
median (distance-list) * 50
2
1
11

SWITCH
477
46
601
79
matern-cluster?
matern-cluster?
1
1
-1000

SLIDER
476
81
568
114
kappa
kappa
0
50
0.0
1
1
NIL
HORIZONTAL

SLIDER
476
116
568
149
r
r
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
476
151
568
184
mu
mu
0
200
0.0
1
1
NIL
HORIZONTAL

SLIDER
604
10
795
43
pois-seeds-per-plant-patch
pois-seeds-per-plant-patch
0
2000
1000.0
50
1
λ
HORIZONTAL

SWITCH
902
80
1021
113
approx-1d?
approx-1d?
1
1
-1000

MONITOR
1080
150
1237
195
Simulation duration (days)
ticks / 24
0
1
11

SLIDER
470
301
642
334
p-lazy
p-lazy
0
1
0.0
0.1
1
NIL
HORIZONTAL

CHOOSER
466
254
644
299
type-of-walk
type-of-walk
"simple random walk" "correlated random walk" "levy walk" "emu model"
2

SLIDER
468
337
640
370
stdev-angle
stdev-angle
0
360
180.0
1
1
NIL
HORIZONTAL

PLOT
855
339
1041
490
Step Length Distribution
Step Length (Patches x 50 m)
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if ticks mod 20 = 0 [ histogram step-length-list ]"

MONITOR
984
183
1069
228
95 Percentile
(item 2 get-percentiles distance-list 0.95) * 50
3
1
11

BUTTON
634
94
697
127
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
634
128
698
161
dump-data
write-deposition\nwrite-distance\nwrite-visited-locations\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
82
470
254
503
dimension
dimension
200
400
200.0
50
1
NIL
HORIZONTAL

SLIDER
466
394
638
427
detect-distance
detect-distance
1
20
10.0
1
1
cells
HORIZONTAL

SLIDER
466
429
638
462
beta0
beta0
0
.05
0.01
.0005
1
NIL
HORIZONTAL

SLIDER
77
576
255
609
smooth-impassable
smooth-impassable
1
100
30.0
1
1
NIL
HORIZONTAL

CHOOSER
261
471
399
516
lsp-model
lsp-model
"percolation" "diffusion"
0

TEXTBOX
467
463
708
519
as beta0 incr probability of emu detecting\na barrier at greater distances increases
11
0.0
1

SLIDER
649
337
821
370
var-GRT
var-GRT
0
2.0
0.4
.01
1
NIL
HORIZONTAL

SLIDER
648
301
820
334
mean-GRT
mean-GRT
0
48
7.0
1
1
hrs
HORIZONTAL

CHOOSER
650
252
789
297
distribution-GRT
distribution-GRT
"exponential" "gamma"
1

SWITCH
513
507
659
540
specify-nl-seed?
specify-nl-seed?
1
1
-1000

SLIDER
498
542
670
575
nl-seed
nl-seed
-2147483648
2147483648
9856432.0
1
1
NIL
HORIZONTAL

INPUTBOX
688
517
981
577
dump-path
/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/analysis/grt/
1
0
String

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bird
false
0
Polygon -7500403 true true 135 165 90 270 120 300 180 300 210 270 165 165
Rectangle -7500403 true true 120 105 180 237
Polygon -7500403 true true 135 105 120 75 105 45 121 6 167 8 207 25 257 46 180 75 165 105
Circle -16777216 true false 128 21 42
Polygon -7500403 true true 163 116 194 92 212 86 230 86 250 90 265 98 279 111 290 126 296 143 298 158 298 166 296 183 286 204 272 219 259 227 235 240 241 223 250 207 251 192 245 180 232 168 216 162 200 162 186 166 175 173 171 180
Polygon -7500403 true true 137 116 106 92 88 86 70 86 50 90 35 98 21 111 10 126 4 143 2 158 2 166 4 183 14 204 28 219 41 227 65 240 59 223 50 207 49 192 55 180 68 168 84 162 100 162 114 166 125 173 129 180

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -7500403 true true 135 285 195 285 270 90 30 90 105 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Circle -1 true false 183 138 24
Circle -1 true false 93 138 24

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Simple Random Walk" repetitions="1" runMetricsEveryStep="true">
    <setup>reset
sprout-plants
create-dispersers</setup>
    <go>run-simulation</go>
    <final>write-file</final>
    <metric>distance-list</metric>
    <enumeratedValueSet variable="pen-down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pois-seeds-per-plant-patch">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-plants">
      <value value="2550"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dispersers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matern-cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-1d?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-lazy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-walk">
      <value value="&quot;simple random walk&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-GRT-exp">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="worldsize-effect" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run-simulation</go>
    <final>write-deposition
write-visited-locations
write-distance</final>
    <timeLimit steps="1000"/>
    <metric>median distance-list</metric>
    <metric>max distance-list</metric>
    <enumeratedValueSet variable="pen-down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pois-seeds-per-plant-patch">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-plants">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dispersers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matern-cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-1d?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-lazy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-walk">
      <value value="&quot;emu model&quot;"/>
      <value value="&quot;simple random walk&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-GRT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-GRT">
      <value value="&quot;gamma&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dimension">
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specify-nl-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detect-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta0">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dump-path">
      <value value="&quot;/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/analysis/worldSize/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="frag-effect" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run-simulation</go>
    <final>write-deposition
write-visited-locations
write-distance</final>
    <timeLimit steps="1000"/>
    <metric>median distance-list</metric>
    <metric>max distance-list</metric>
    <enumeratedValueSet variable="pen-down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pois-seeds-per-plant-patch">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-plants">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dispersers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matern-cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-1d?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-lazy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-walk">
      <value value="&quot;emu model&quot;"/>
      <value value="&quot;correlated random walk&quot;"/>
      <value value="&quot;levy walk&quot;"/>
      <value value="&quot;simple random walk&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-GRT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-GRT">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dimension">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-impassable">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attract-impassable">
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specify-nl-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detect-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta0">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dump-path">
      <value value="&quot;/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/analysis/habitatLoss/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="grt-sensitivity" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run-simulation</go>
    <final>write-deposition
write-visited-locations
write-distance</final>
    <timeLimit steps="1000"/>
    <metric>median distance-list</metric>
    <metric>max distance-list</metric>
    <enumeratedValueSet variable="pen-down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pois-seeds-per-plant-patch">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-plants">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dispersers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matern-cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-1d?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-lazy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-walk">
      <value value="&quot;emu model&quot;"/>
      <value value="&quot;correlated random walk&quot;"/>
      <value value="&quot;levy walk&quot;"/>
      <value value="&quot;simple random walk&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-GRT" first="2.5" step="0.5" last="10"/>
    <enumeratedValueSet variable="distribution-GRT">
      <value value="&quot;gamma&quot;"/>
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dimension">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-impassable">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attract-impassable">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specify-nl-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detect-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta0">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dump-path">
      <value value="&quot;/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/analysis/grt/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="detect-sensitivity" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run-simulation</go>
    <final>write-deposition
write-visited-locations
write-distance</final>
    <timeLimit steps="1000"/>
    <metric>median distance-list</metric>
    <metric>max distance-list</metric>
    <enumeratedValueSet variable="pen-down?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pois-seeds-per-plant-patch">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-plants">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dispersers">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matern-cluster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-1d?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-lazy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-walk">
      <value value="&quot;emu model&quot;"/>
      <value value="&quot;simple random walk&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-GRT">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-GRT">
      <value value="&quot;gamma&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dimension">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-impassable">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attract-impassable">
      <value value="0.5"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="specify-nl-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="detect-distance" first="5" step="1" last="15"/>
    <steppedValueSet variable="beta0" first="0.005" step="0.001" last="0.015"/>
    <enumeratedValueSet variable="dump-path">
      <value value="&quot;/home/gper020/gper020l02/private/Research/Papers/Current/Other/nieldSpatial/ABM/model/analysis/detect/&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
