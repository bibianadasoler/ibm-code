extensions [ gis rnd  ]

globals [
  landscape ;landscape of your study area
  roads ; roads of your study area ?????
 ]

patches-own
 [
  permeability-value ; permeability value of each land cover class
 ]

to setup
  clear-all
  setup-landscape ; see the procedure below
  create-individuals ; see the procedure below
  carefully [ file-delete "coordinates.txt" ] [ ] ; check if there is a file with this name and if so, remove it
  file-open "coordinates.txt" ; create a txt file to store the coordinates

  reset-ticks

end

to setup-landscape ; settings of the landscape that will be load
  set landscape gis:load-dataset "/Users/bibianaterra/OneDrive/Doutorado/Predicao_ferrovias/ibm-code/area_pequena.asc" ; loading landscape - choose the .asc file with permeability values
  resize-world 0 gis:width-of landscape 0 gis:height-of landscape
  gis:set-world-envelope-ds gis:envelope-of landscape ;define the world size similar to the landscape imported
  gis:apply-raster landscape permeability-value ; get the values from the landscape to the variable permeability-value
  ask patches [ set permeability-value round permeability-value ] ; round the permeability value
  ask patches [ set pcolor scale-color green permeability-value 100.0 1.0 ] ; define the patch color due to its permeability value

end

to create-individuals ; to create individuals
 create-turtles num-individuals [; num choosen in interface button
    set shape "footprint other" ; define individual shape
    set color black
    move-to rnd:weighted-one-of patches [ permeability-value ] ; individuals are born in a random selection by weight
    set size 2 ; define individual size
    set pen-mode "down" ; draw a line of inidividual movement
    set pen-size 0.5
  ]
end

to go ; what individuals will do when we click in the button go
  ask turtles [ move ]; see the procedure below

  tick
  save-coord ; save coordinates in each go tick to have the trajectory of each individual
end

to move ; move based on the type-of-walk button
  if type-of-walk = "homogeneous random" [ ; individual next step will be related to the previous one
    let step-length random-gamma  mean-step sd-step
    print step-length
    let targets patches in-radius step-length ; all patches inside a circle with radius of step-length are targets
    let chosen-patch one-of targets ; choose a random patch to move
    move-to chosen-patch ; move to the chosen-patch
   ]
  if type-of-walk = "heterogeneous random" [ ; individual next step will be related to the previous one
    let step-length random-gamma  mean-step sd-step
    print step-length
    let targets patches in-radius step-length ; all patches inside a circle with radius of step-length are targets
    let chosen-patch rnd:weighted-one-of targets [ permeability-value ] ; (random selection by weight: higher values stands a greater chance of being picked) choose a random patch to move with the probability of being chosen proportional to the weight of permeability-value
    move-to chosen-patch ; move to the chosen-patch
   ]
  if type-of-walk = "correlated" [ ; individual next step will be related to the previous one
    let step-length random-gamma  mean-step sd-step
    ;set step-length 3;step-length * 3.67
    print step-length
    ;let angle rngs:rnd-vm mean-angle concentration-angle ;rngs:rnd-vm
    let angle one-of [ 1.1963653300773 5.47554984003312 2.1450604782467 5.86962408104768  6.17951818541185 5.69250917877977 1.47706168239137 1.00878899735074 4.8863545387396 5.84586414379411 0.807437885399752 6.1376292874791 0.109030315911556 3.27752748838459 2.63927350577431 0.06579293774923 ]
    print angle
    let targets patches in-cone step-length angle ; all patches inside the cone with radius of maximum-step-length and opening based on the mean-angle are targets
    ;set targets  [remove-item targets patch-here]
   ; set targets filter  targets != patch-here targets
    ask targets [ set pcolor red]
    let chosen-patch rnd:weighted-one-of targets [ permeability-value ] ; (random selection by weight: higher values stands a greater chance of being picked) choose a random patch to move with the probability of being chosen proportional to the weight of permeability-value
    face chosen-patch ; individual faces the target patch to change its direction
    move-to chosen-patch ; move to the chosen-patch
   ]
  ; other types? levy = 4.1 random walks sullivan perry 2013
   if type-of-walk = "levy" [
   ]
end


to save-coord
  ask turtles [
    let coords gis:envelope-of self
    let x first coords
    let y last coords
    file-print (word who "," x "," y "," ticks) ; save individual number, x, y and tick number
  ]
end

to-report r-cauchy [loc scl] ; 4.1
  let X (pi * (random-float 1)) ;; Netlogo tan takes degrees not radians
  report abs (loc + scl * tan(X * (180 / pi))) ;PS COLOQUEI UM ABS
end
@#$#@#$#@
GRAPHICS-WINDOW
277
10
729
577
-1
-1
6.0
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
73
0
92
1
1
1
ticks
30.0

BUTTON
8
10
74
43
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

SLIDER
51
56
190
89
num-individuals
num-individuals
1
100
17.0
1
1
NIL
HORIZONTAL

BUTTON
198
10
253
43
go
repeat 100 [go]
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
79
10
134
43
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
14
234
238
279
type-of-walk
type-of-walk
"homogeneous random" "heterogeneous random" "correlated" "levy"
2

BUTTON
139
10
194
43
go 50
repeat 50 [go]
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
71
290
181
323
save-coords
file-close
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
35
159
112
219
mean-angle
0.0
1
0
Number

INPUTBOX
30
93
113
153
mean-step
0.11
1
0
Number

INPUTBOX
116
93
205
153
sd-step
0.07
1
0
Number

INPUTBOX
117
158
215
218
concentration-angle
0.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)
There are three types of walk available in this model. 
Random - to use this one you should chose the "correlated" option and must define the mean-angle as 360, this way the individual can move to anywhere inside a circle.
Correlated - the individiual next step will be related to the previous one, so it will move to a patch inside a cone with the size defined by the mean angle

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

footprint other
true
0
Polygon -7500403 true true 75 195 90 240 135 270 165 270 195 255 225 195 225 180 195 165 177 154 167 139 150 135 132 138 124 151 105 165 76 172
Polygon -7500403 true true 250 136 225 165 210 135 210 120 227 100 241 99
Polygon -7500403 true true 75 135 90 135 105 120 105 75 90 75 60 105
Polygon -7500403 true true 120 122 155 121 161 62 148 40 136 40 118 70
Polygon -7500403 true true 176 126 200 121 206 89 198 61 186 57 166 106
Polygon -7500403 true true 93 69 103 68 102 50
Polygon -7500403 true true 146 34 136 33 137 15
Polygon -7500403 true true 198 55 188 52 189 34
Polygon -7500403 true true 238 92 228 94 229 76

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
0
@#$#@#$#@
