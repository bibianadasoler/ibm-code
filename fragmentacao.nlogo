extensions [table
  gis
  palette
  rnd
]


breed [anteaters anteater]


globals[
  npatches-visited ; acho que isso nao foi usado
  seedlist
  ;i
  ; prob
  ; Ps
  ; var1 ;; checking for ptype barrier
  ; Neighbors-green
  ; Neighbors-yellow
  ; var5 ;;records last green patch a lynx was on
  ; dispersal-max
  ;
  ; ;; iterations
  ; ;;counter adds one each time a run of the model happens.  When it reaches the number in the iterations button it halts.
  ; ;;See the experiment procedure
  ; counter
  ; ;; pvisits is for plotting the number of habitat patches visited each iteration
  ; pvisits
  ; ;;avgpvisits is sum of pvisits / iterations
  ; avg-pvisits
  ; raster1
  ; raster_roads
  ; raster-out
  ; Pc
  ; current-patch
  ; n-barr
  ; sum-green
  ; sum-yellow
  ; pattern2
  ; x-visit-all
  ; y-visit-all
  ; go-multi-expt
  ; count-rows
  ; countvar
  ;
  ; surviving_lynx
  ; ;;for sens test
  ; xvar
  ; matvar
  ; var_i
  ; pvis250718
]

patches-own [
  habitat ; se é habitat ou matrix
  permeability ; valor da permeabilidade de cada patch
  road
  visits
  cluster ; não entendi

  ;  ptype
  ;  hasroad
  ;  runs-visited
  ;  number-of-visits
  ;
  ;  ;; summary variables collected when best release is run
  ;  pvis ;; patch visits resulting from a release at this patch
  ;  dispmean ;; mean dispersal distance from this patch
  ;  dispmax  ;;max dispersal distance from this patch
  ;  p2 ;; ratio of yellow to green
  ;  patch-pvisits ;;this should capture the number of times patches visited when a release expt is made from that patch from the global pvisits
]


anteaters-own [
  mycandidate ; nao entendi pq tem essa properties... a ideia é salvar depois?
  ;last-green-x
  ;   last-green-y
  ;   number-mat
  ;   leave-var
  ;   pleave
  ;   travel
  ;   distance_today
  ;   lynxonroad ;; used in road mortality
]


;;------------------------------ setting up -----------------------------------------------

to setup
  ;TESTE GIT
  ca
  Generating-a-landscape

  ask patches with [cluster = 1] [set habitat 1 set permeability 1 ; nao entendi de onde vem o 1 do cluster
    set pcolor green]
  ask patches with [cluster = 2] [set habitat 2 set permeability matrix-permeability
    set pcolor white]

;  ask patches[
;    set visits 0
;    set road false
;  ]
;
;  ask patches with [pycor = round (dim / 2)] ; faz a rodovia ficar no centro
;  [set road true
;    set pcolor black
;    set permeability (1 - road-avoidance) * permeability ; vamos manter isso?
;  ]

  setup-turtles

  reset-ticks
end


;; Generating a landscape
to Generating-a-landscape ; nao entendi a partir da seedlist

  let ext dim
  set-patch-size 3.5 * (100 / dim)
  resize-world 0 (dim - 1) 0 (dim - 1)
  ask patches [
    set cluster nobody
  ]

  set seedlist list Proportion-of-habitat Proportion-of-matrix

  ;; Criteria and scaling on very high seed counts
  if sum seedlist != ext [
    let c ext / sum seedlist
    print c
    set seedlist map floor (map [ ?1 -> c * ?1 ] seedlist)
    ; ?1 is a variable that is used to refer to the first input argument of a procedure
    ; map [ ?1 -> c * ?1 ] seedlist: This applies the function [ ?1 -> c * ?1 ] to each
    ; item in the seedlist variable. This function multiplies each item in the seedlist by a
    ; constant c, where c is some value that has been defined elsewhere in the code. The result
    ; is a new list of numbers.
    ; ou seja, multiplica cada valor do seedlist por c para obter a proporcao de habitat/matrix
    print seedlist
    ; aqui o valor 1 de seed list ficou menor que o mostrado na caixinha da interface
    let change ((item 0 seedlist) + ext - sum seedlist)
    print change
    set seedlist replace-item 0 seedlist change
    ; aqui acho que é pra fechar a soma da dim
  ]

;  ;; Scatter seed
;  let i 0
;  foreach seedlist [ ?1 -> ;
;    set i i + 1
;    repeat ?1 [
;      ask one-of (patches with [cluster = nobody])[
;        ; primeiro pega patches aleatorios e coloca cluster 1 para os X patches de habitat
;        ; (primeiro da seedlist) e depois coloca 2 para os X patches da matrix
;        set cluster i
;      ]
;    ]
;  ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDICAR PATCHES DE HABITAT
;let num-patches 8
;let patch-distance (world-width / num-patches)
;  print patch-distance
;let start-xcor (- world-width / 2 + patch-distance / 2)
;  print start-xcor
;
;repeat num-patches [
;      ask patches with [pxcor > start-xcor - patch-distance / 2 and pxcor < start-xcor + patch-distance / 2] [
;      set cluster 1
;  ]
;  set start-xcor start-xcor + patch-distance
;      ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  ;; Fill the voids by propagation
;  ;; Credit: Uri Wilensky, Patch Cluster Example
;  while [any? patches with [cluster = nobody]] [
;    ask patches with [cluster = nobody][
;      let c [cluster] of one-of neighbors4
;      if c != nobody [
;        set cluster c
;      ]
;    ]
;  ]
;let num-patches 2
;ask n-of num-patches patches [
;    set cluster 1
;  ]
;  ask patch  0  0 [ set pcolor white ]

;let num-clusters 2
;let patches-list patches
;let seedlist-item-0 item 0 seedlist
;let patches-per-cluster (floor seedlist-item-0 / num-clusters) * dim
;print patches-per-cluster
;ask n-of patches-per-cluster patches-list [
;    set cluster 1
;  ]
;  ;set patches-list patches with [cluster = nobody]

if num-patches = 2 [
ask patches with [pxcor mod num-patches = 0 and pycor mod num-patches = 0] [set cluster 1]
ask patches [
  if cluster = 1 [set pcolor green - 2]
            ]
  ]
if num-patches = 4 [
let mid-x max-pxcor / num-patches
let mid-y max-pycor / num-patches
print mid-x
print mid-y
  ask patch mid-x (- mid-y) [ set pcolor green ]
  ask patch mid-x mid-y [ set pcolor green ]
  ask patch (- mid-x) mid-y [ set pcolor green ]
  ask patch (- mid-x) (- mid-y) [ set pcolor green ]
  ]


if num-patches = 8 [
let mid-x max-pxcor / num-patches
let mid-y max-pycor / num-patches
print mid-x
print mid-y
  ask patch mid-x (- mid-y) [ set pcolor green ]
    ask patch (mid-x * 2) (- mid-y) [ set pcolor green ]
  ask patch (mid-x / 2) (- mid-y) [ set pcolor green ]
  ask patch mid-x mid-y [ set pcolor green ]
  ask patch (- mid-x) mid-y [ set pcolor green ]
  ask patch (- mid-x) (- mid-y) [ set pcolor green ]
  ]

if num-patches = 16 [
    resize-world 0 world-width * 2 0 world-height * 2
let set-patch-set patches with [pxcor >= 0 and pxcor < world-width and pycor >= 0 and pycor < world-height]
ask patches with [pxcor < 0] [
  set quarter 1
  set pcolor red
]
ask patches with [pycor > world-height] [
  set quarter 2
  set pcolor blue
]
ask patches with [pxcor > world-width] [
  set quarter 3
  set pcolor green
]
ask patches with [pycor < 0] [
  set quarter 4
  set pcolor yellow
]
  ]
end

to setup-turtles
  create-anteaters number-of-anteaters [set color black
    set size 1]
  ask anteaters [move-to rnd:weighted-one-of patches [permeability]
    if pd? [pd]] ; nao entendi o que é isso
end

to new-run

  ask turtles [die]
  ask patches with [cluster = 1] [set habitat 1 set permeability 1
    set pcolor green]
  ask patches with [cluster = 2] [set habitat 2 set permeability matrix-permeability
    set pcolor white]

  ask patches[
    set visits 0
    set road false
  ]

  ask patches with [pycor = round (dim / 2)]
  [set road true
    set pcolor black
    set permeability (1 - road-avoidance) * permeability
  ]

  setup-turtles

  reset-ticks
end

to move
  repeat steps [move1]
end

to move1
  ask anteaters [check-move
    face mycandidate
    fd 1 ; aqui ele anda um pixel por vez então?
    ask patch-here [set visits visits + 1]
  ]
end

to check-move
  let candidate-cells1 patches in-cone perceptual-range 180
  let chosen-patch rnd:weighted-one-of candidate-cells1 [permeability]
  set mycandidate chosen-patch ; mycandidate é igual ao chosen-patch? preciso de 2 objetos?

end


;; paintting ;; post simulations
;;
to paint-patch-use
  let max-use max [visits] of patches
  ask patches [set pcolor scale-color red visits max-use  1]
end

to paint-road-crossings
  paint-habitats ; isso precisa estar aqui?
  let max-use max [visits] of patches with [road = true]
  ask patches with [road = true] [set pcolor scale-color red visits max-use  1]
end


to paint-habitats
  ask patches with [cluster = 1] [set pcolor green]
  ask patches with [cluster = 2] [set pcolor white]
  ask patches with [road = true][ set pcolor black]
end

to paint-permeability
  ask patches [set pcolor scale-color blue ((permeability + 1) * 10) 21 1]
end

to update-permeability
  ask patches with [cluster = 1] [set permeability 1]
  ask patches with [cluster = 2] [set permeability matrix-permeability]

  ask patches with [road = true][ set permeability (1 - road-avoidance) * permeability ] ; isso vamos remover?
end


to-report assess-top-sections
  let sort-crossings sort-by > [visits] of patches with [road = true] ; ordena os patches por maior numero de visitas
  let total-crossings sum sort-crossings ; soma do numero total de cruzamentos na road
  let top-sections-effectiveness sum (sublist sort-crossings 0 (dim * .25)) ; soma dos cruzamentos nos patches mais usados 25% quartil
  let effectiveness top-sections-effectiveness / total-crossings
  report precision effectiveness 3
end

to save-data
  ;saves data for a given run

  ;  foreach patches-list [ ?1 ->
  ;    ifelse not any? patches with [(patch-id = ?1) and (previous-n > 0)][
  ;      set unoccupied-patch-counter (unoccupied-patch-counter + 1)
  ;      if any? patches with [(patch-id = ?1) and (current-n > 0)][
  ;        set patch-recolonization-counter (patch-recolonization-counter + 1)
  ;      ]
  ;    ][
  ;      set occupied-patch-counter (occupied-patch-counter + 1)
  ;      if not any? patches with [(patch-id = ?1) and (current-n > 0)][
  ;        set patch-extinction-counter (patch-extinction-counter + 1)
  ;      ]
  ;    ]
  ;  ]

  ;  ;saves the data to a text file
  ;  set filename (word output-file ".txt")
  ;
  ;  file-open filename
  ;  file-type run-id
  ;  file-write generations-counter
  ;  file-write change-rate
  ;  file-write count patches with [pcolor = green] / count patches
  ;  file-write H
  ;  file-write m-cost
  ;  file-write mean-disturb
  ;  file-write habitat-cost
  ;  file-write carrying-capacity
  ;  file-write lambda
  ;  file-write competition-type
  ;  file-write mutation-rate
  ;  file-write mutation-increment
  ;  file-write mean [pr-move] of turtles
  ;  file-write mean [pr-cross] of turtles
  ;  file-write mean [p-shape-m] of turtles
  ;  file-write mean [p-shape-h] of turtles
  ;  file-write pre-disperse-n
  ;  file-write count turtles
  ;  file-write total-patch-number
  ;  file-write actual-emigrations + count turtles with [origin != destination]
  ;  file-write count turtles with [origin-patch != destination-patch]
  ;  file-write (count turtles with [origin != destination] - count turtles with [origin-patch != destination-patch])
  ;  file-write current-habitat-mortality
  ;  file-write new-habitat-mortality
  ;  file-write matrix-mortality
  ;  ifelse (unoccupied-patch-counter > 0) [file-write patch-recolonization-counter / unoccupied-patch-counter][file-type " NA"]
  ;  file-type " "
  ;  ifelse (occupied-patch-counter > 0)[file-print patch-extinction-counter / occupied-patch-counter][file-print "NA"]
  ;
  ;  file-close

end
@#$#@#$#@
GRAPHICS-WINDOW
245
30
603
389
-1
-1
3.5
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
99
0
99
0
0
1
ticks
30.0

BUTTON
5
30
130
63
NIL
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

INPUTBOX
5
115
130
175
number-of-anteaters
100.0
1
0
Number

BUTTON
5
65
130
98
move
move
NIL
1
T
OBSERVER
NIL
M
NIL
NIL
1

BUTTON
245
500
360
533
NIL
paint-patch-use
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
5
175
130
235
steps
1.0
1
0
Number

SLIDER
5
400
145
433
road-avoidance
road-avoidance
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
5
310
150
343
matrix-permeability
matrix-permeability
.01
1
1.0
.01
1
NIL
HORIZONTAL

SWITCH
135
115
240
148
pd?
pd?
1
1
-1000

SLIDER
5
235
130
268
dim
dim
10
100
100.0
10
1
NIL
HORIZONTAL

BUTTON
135
30
240
63
NIL
clear-drawing
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
245
391
417
424
Proportion-of-habitat
Proportion-of-habitat
10
100
50.0
5
1
NIL
HORIZONTAL

SLIDER
245
426
417
459
proportion-of-matrix
proportion-of-matrix
5
100
100.0
5
1
NIL
HORIZONTAL

MONITOR
430
406
487
451
NIL
seedlist
17
1
11

BUTTON
125
500
240
533
NIL
paint-habitats
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
5
345
150
378
NIL
update-permeability
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
490
405
612
450
NIL
assess-top-sections
4
1
11

BUTTON
135
65
240
98
NIL
new-run
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
365
500
480
533
NIL
paint-road-crossings
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
5
435
145
468
perceptual-range
perceptual-range
1.5
10
1.5
.5
1
NIL
HORIZONTAL

CHOOSER
140
175
232
220
num-patches
num-patches
2 4 8 16
3

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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
1
@#$#@#$#@
