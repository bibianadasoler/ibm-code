extensions [
  rnd
]


globals[
  seedlist
  filename
 ]

patches-own [
  habitat
  permeability
  current_permeability
  visits
  hab_neighbors
]

turtles-own [
  mycandidate
]


;;------------------------------ setting up -----------------------------------------------

to setup
  ca
  resize-world 0 83 0 83
  set-patch-size 4

  ask patches [
    set habitat nobody
    set visits 0
  ]

  if scenario = 1 [create-scenario-1] ;"1 patch"
  if scenario = 2 [create-scenario-2-vert] ;"2 vertical patches"
  if scenario = 3 [create-scenario-2-horiz] ;"2 horizontal patches"
  if scenario = 4 [create-scenario-16-close] ;"16 pacthes close to road"
  if scenario = 5 [create-scenario-16-far] ;"16 pacthes away from road"
  if scenario = 6 [create-scenario-vert-lines] ;"vertical lines"
  if scenario = 7 [create-scenario-horiz-lines] ;"horizontal lines"
  if scenario = 8 [create-random] ;"random"

  ask patches with [habitat = 1] [set permeability 1
    set pcolor green]
  ask patches with [habitat != 1] [set habitat 2 set permeability matrix-permeability
    set pcolor white]
  ask patches with [pycor = 42] [
    set habitat 0
    set pcolor black]
  ask patches with [habitat = 0] [
    set hab_neighbors count neighbors with [habitat = 1] ]

  setup-turtles
  reset-ticks

  if save-data? [set-filename]
end

to create-scenario-1
  ask (patch 21 22) [sprout 1 ]
  ask turtles [set heading 90]
  ask turtles [ repeat 40  [fd 1 ask patch-here [sprout 1 set habitat 1]]]

  ask turtles [set heading 0]
  ask turtles [ repeat 19  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 1]
  ask turtles [ repeat 20  [fd 1 ask patch-here [set habitat 1]]]
  ct
end

to create-scenario-2-vert
  ask (patch 46 22) [sprout 1]
  ask (patch 15 22) [sprout 1]

  ask turtles [set heading 90]
  ask turtles [ repeat 20  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 0]
  ask turtles [ repeat 19  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 1]
  ask turtles [ repeat 20  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-scenario-2-horiz
  ask (patch 21 48) [sprout 1 set pcolor white]
  ask (patch 21 17) [sprout 1 set pcolor white]

  ask turtles [set heading 90]
  ask turtles [ repeat 40  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 0]
  ask turtles [ repeat 19  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-scenario-16-close
  ask (patch 5 11) [sprout 1]
  ask (patch 26 11) [sprout 1]
  ask (patch 47 11) [sprout 1]
  ask (patch 68 11) [sprout 1]

  ask turtles [set heading 90]
  ask turtles [ repeat 10  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 0]
  ask turtles [repeat 9  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 11]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 1]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 11]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-scenario-16-far
  ask (patch 5 6) [sprout 1]
  ask (patch 26 6) [sprout 1]
  ask (patch 47 6) [sprout 1]
  ask (patch 68 6) [sprout 1]

  ask turtles [set heading 90]
  ask turtles [ repeat 10  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 0]
  ask turtles [repeat 9  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 11]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 11]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 11]
  ask turtles [ repeat 10  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-scenario-vert-lines
  ask (patch 7 0) [sprout 1]
  ask (patch 28 0) [sprout 1]
  ask (patch 49 0) [sprout 1]
  ask (patch 70 0) [sprout 1]

  ask turtles [set heading 90]
  ask turtles [ repeat 5  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 0]
  ask turtles [repeat 41  [fd 1 ask patch-here [set habitat 1]]]
  ask turtles [fd 1]
  ask turtles [ repeat 41  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-scenario-horiz-lines
  ask (patch 0 10) [sprout 1]
  ask (patch 0 26) [sprout 1]
  ask (patch 0 52) [sprout 1]
  ask (patch 0 68) [sprout 1]

  ask turtles [set heading 0]
  ask turtles [ repeat 5  [fd 1 ask patch-here [sprout 1 set habitat 1]]]
  ask turtles [set heading 90 repeat 83  [fd 1 ask patch-here [set habitat 1]]]

  ct
end

to create-random
  let ext max-pycor + 1

  set seedlist list Proportion-of-habitat (100 - Proportion-of-habitat)

  let i 0
  foreach seedlist [ ?1 ->
    set i i + 1
    repeat ?1 [
      ask one-of (patches with [habitat = nobody])[
        set habitat i
      ]
    ]
  ]

  ; Credit: Uri Wilensky, Patch Cluster Example
  while [any? patches with [habitat = nobody]] [
    ask patches with [habitat = nobody][
      let c [habitat] of one-of neighbors4
      if c != nobody [
        set habitat c
      ]
    ]
  ]

end

to setup-turtles
  create-turtles number-of-individuals [set color black
    set size 1]
  ask turtles [ move-to rnd:weighted-one-of patches [permeability]
      if pd? [pd]]
end

to new-run
  ask turtles [die]
  ask patches with [habitat = 1] [set permeability 1
    set pcolor green]
  ask patches with [habitat != 1] [set habitat 2 set permeability matrix-permeability
    set pcolor white]
  ask patches[
    set visits 0]

  ask patches with [pycor = 42] [
    set habitat 0
    set pcolor black ]

  setup-turtles
  reset-ticks
end

to go
  move tick
  if stop-simulation? [if save-data? [save-data] stop]
end


to-report stop-simulation?
  report ticks = steps
end

to move
  ask turtles [check-move
    if mycandidate = nobody [rt 180]
    if mycandidate != nobody [face mycandidate fd 1]
   ask patch-here [set visits visits + 1]
  ]
end

to check-move
  let candidate-patches patches in-cone perceptual-range vision-angle
  if (count candidate-patches = 1) [
   ask turtles-on candidate-patches [face patch (max-pxcor / 2) (max-pycor / 2)]
    let temp-patches patches in-cone perceptual-range 120
    set candidate-patches temp-patches
  ]
  ask candidate-patches [let d distance myself
    set current_permeability permeability * ((1 / perceptual-range) + ((perceptual-range - d)/ (perceptual-range)))
    ;set pcolor scale-color red current_permeability 1 0
  ]
  let chosen-patch rnd:weighted-one-of candidate-patches [current_permeability]
  set mycandidate chosen-patch
end

;; paintting post simulations
to paint-patch-use
  let max-use max [visits] of patches
  ask patches [set pcolor scale-color red visits max-use  1]
end

to paint-road-crossings
  paint-habitats
  let max-use max [visits] of patches with [habitat = 0]
  ask patches with [habitat = 0] [set pcolor scale-color red visits max-use  1]
end

to paint-habitats
  ask patches with [habitat = 1] [set pcolor green]
  ask patches with [habitat = 2] [set pcolor white]
  ask patches with [habitat = 0] [set pcolor black]
end

to update-permeability
  ask patches with [habitat = 1] [set permeability 1]
  ask patches with [habitat = 2] [set permeability matrix-permeability]
  ask patches [set visits 0]
end

to-report assess-top-sections
  let sort-crossings sort-by > [visits] of patches with [habitat = 0]
  let total-crossings sum sort-crossings

  ifelse total-crossings > 0 [
  let top-sections-effectiveness sum (sublist sort-crossings 0 (max-pycor * .25)) ; QUE ESTA ACONTECENDO
  let effectiveness top-sections-effectiveness / total-crossings
    report precision effectiveness 3]
  [report 0]
end

;;; save data
to set-filename
  ifelse file-exists? (word root "/" outputfile ".csv")
   [ set filename (word root "/" outputfile ".csv") ]
   [ set filename (word root "/" outputfile ".csv")
     file-open filename
     file-print (word
       "N_individuals" ","
       "steps" ","
       "landscape_area" ","
       "habitat_area" ","
       "matrix_area" ","
       "type_scenario" ","
       "proportion_habitat_random" ","
       "perceptual-range" ","
       "matrix-permeability" ","
       "mean_hab_neighbors" ","
       "total_crossings" ","
       "assess-top-sections")
     file-close-all
   ]
end

to save-data
  file-open filename
  file-print (word number-of-individuals ","
    steps ","
    (count patches) ","
    count patches with [habitat = 1] ","
    count patches with [habitat = 2] ","
    scenario ","
    proportion-of-habitat ","
    perceptual-range ","
    matrix-permeability ","
    mean [hab_neighbors] of patches with [habitat = 0] ","
    sum [visits] of patches with [habitat = 0] ","
    assess-top-sections)
  file-close
end

to delete-file
  if file-exists? filename  [
    file-close-all
     file-delete filename ]
end
@#$#@#$#@
GRAPHICS-WINDOW
245
30
589
375
-1
-1
4.0
1
10
1
1
1
0
0
0
1
0
83
0
83
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
number-of-individuals
10.0
1
0
Number

BUTTON
5
65
130
98
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
350
435
460
468
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
100.0
1
0
Number

SLIDER
5
330
150
363
matrix-permeability
matrix-permeability
.1
1
0.2
.1
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
5
290
192
323
proportion-of-habitat
proportion-of-habitat
10
100
30.0
5
1
NIL
HORIZONTAL

BUTTON
245
435
345
468
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
365
150
398
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
380
380
510
425
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
465
435
595
468
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
405
145
438
perceptual-range
perceptual-range
0
20
10.0
5
1
NIL
HORIZONTAL

CHOOSER
5
240
217
285
scenario
scenario
1 2 3 4 5 6 7 8
7

MONITOR
655
270
712
315
matrix
count patches with [habitat = 2]
1
1
11

TEXTBOX
730
290
780
316
5456
11
104.0
1

INPUTBOX
285
480
450
540
outputfile
valendo
1
0
String

SWITCH
5
545
122
578
save-data?
save-data?
0
1
-1000

MONITOR
330
545
1130
590
NIL
filename
17
1
11

MONITOR
245
380
375
425
Crossings
sum [visits] of patches with [habitat = 0]
17
1
11

CHOOSER
5
480
275
525
root
root
"/Users/bibianaterra/Library/CloudStorage/OneDrive-Personal/Doutorado/Predicao_ferrovias/ibm-code/results" "C:\\Users\\bibia\\OneDrive\\Doutorado\\Predicao_ferrovias\\ibm-code\\results"
0

BUTTON
125
545
232
578
NIL
set-filename
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
235
545
327
578
delete file
delete-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
730
330
760
348
1600
11
92.0
1

MONITOR
655
320
712
365
habitat
count patches with [habitat = 1]
1
1
11

SLIDER
5
440
177
473
vision-angle
vision-angle
90
180
120.0
30
1
NIL
HORIZONTAL

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
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="teste-random" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="root">
      <value value="&quot;/Users/bibianaterra/Desktop/results&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-individuals">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual-range">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-permeability">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proportion-of-habitat">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-data?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-file">
      <value value="&quot;teste_random&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="teste-regular" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="root">
      <value value="&quot;/Users/bibianaterra/Desktop/results&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-individuals">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="steps">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual-range">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="1"/>
      <value value="&quot;2-v&quot;"/>
      <value value="&quot;2-h&quot;"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-permeability">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="save-data?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-file">
      <value value="&quot;teste&quot;"/>
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
1
@#$#@#$#@
