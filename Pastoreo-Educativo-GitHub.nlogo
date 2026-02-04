breed [vacas vaca] ;; Define una raza llamada "vacas" con individuos del tipo "vaca"

globals [
  mejor-parcela           ;; Guarda la parcela con mejores condiciones para pastoreo
  parcela-pastoreo        ;; Parcela actualmente seleccionada para pastoreo
  makgms mikgms           ;; Valores máximo y mínimo de kg MS por patch
  Area-Animal peso        ;; Área ocupada por una vaca según su peso
  has sup-pach            ;; Superficie total (ha) y superficie por patch (ha)
  DIAS-PASTOREO           ;; cálculo de los días totales de pastoreo (versión publicable)
  contador-ticks          ;; Conteo total de ticks
  ticks-totales           ;; Total de ticks a simular (según los días de pastoreo)
  ticks-por-dia           ;; Número de ticks que representan un día
  ticks-actuales-parcela  ;; Ticks transcurridos en la parcela actual
  vacas-a-seguir          ;; Lista de IDs de vacas a monitorear por tick
  promedio-kgms-inicio-dia  ;; Guarda el promedio de MS en la parcela al inicio del día
  umbral-disponibilidad     ;; Establece que una variable para los 1100 kg de disponibilidad no consumida
  consumo-total-simulacion   ;; kg MS consumidos por todas las vacas en toda la simulación
  disponibilidad-inicial-parcelas ;; Es una lista global que guarda, una sola vez, la disponibilidad media inicial

]

patches-own
[
  kgms consumo-tick  ;; consumo-tick se usa para acumular lo extraído en cada tick
  dms                ;; Digestibilidad de la materia seca (0–1)
]
turtles-own
[
  Cs-Kgms           ;; Consumo de MS en el tick actual
  consumo-diario    ;; Acumulador del consumo diario
  historial-consumo ;; Lista del consumo diario por vaca
  pV        ;; Peso vivo individual de la vaca (kg)
  CsEM      ;; Consumo diario de Energía Metabolizable (Mcal/día)
  EM-mant   ;; Requerimiento energético de mantenimiento (Mcal/día)
]


to setup ;; Procedimiento de inicialización

    ;; === 2. Limpieza e inicialización general ===
  validar-parametros
  clear-output
  ca
  reset-ticks
  set ticks-por-dia 48
  set DIAS-PASTOREO dias-permanencia * n-parcelas * vueltas ;; cálculo de los días totales de pastoreo (versión publicable)

  set ticks-totales dias-pastoreo * ticks-por-dia
  set ticks-actuales-parcela 0
  set contador-ticks 0
  set consumo-total-simulacion 0

   ;; === 3. Asignación inicial de parcelas ===
  set parcela-pastoreo n-parcelas ;; La parcela activa empieza en la última
  parcelas  ;; Asigna los plabel a los patches (ahora ya existen plabel = 1, 2, ..., n-parcelas)

  ;; === 4. Cálculo correcto de superficies ===
  ;; Partimos de que cada parcela representa 20 ha
  let patches-en-una-parcela count patches with [plabel = 1]
  set sup-pach (200 / n-parcelas) / patches-en-una-parcela ;; Cada patch representa esta cantidad de ha

  ;; Calculamos la superficie útil total basada en los patches que tienen plabel asignado
  set has sup-pach * count patches with [plabel > 0]

  ;; === 5. Variables de control de consumo ===
  set umbral-disponibilidad 1100

  ;; === 6. Datos derivados del sistema ===
  let carga (has / n-vacas)
  let Presion-pastoreo (n-vacas / ( has / n-parcelas))

  ;; === 7. Parámetros de disponibilidad ===
  set mikgms Disp-Minima
  set makgms mikgms + (TCrec * dias-permanencia * n-parcelas)

  ;; === 8. Inicialización de forraje ===
  gestionar-kgms "inicializar"

  ;; === Registro de disponibilidad inicial por parcela (kg MS / ha) ===
  set disponibilidad-inicial-parcelas []

  let p 1
  while [p <= n-parcelas] [
    set disponibilidad-inicial-parcelas
    lput ( mean [kgms] of patches with [plabel = p] / sup-pach )
    disponibilidad-inicial-parcelas
    set p p + 1
  ]

  ;; === Gráfico inicial de disponibilidad por parcela ===
  graficar-disponibilidad-parcelas

  ;; === Registro de disponibilidad inicial por parcela (kg MS / ha) ===
  set disponibilidad-inicial-parcelas []
 ;; === Registro de disponibilidad inicial por parcela (kg MS / ha) ===
set disponibilidad-inicial-parcelas []

let parcela-id 1
while [parcela-id <= n-parcelas] [
  ;; Calculamos la disponibilidad media inicial de la parcela parcela-id
  ;; y la guardamos en una lista global
  set disponibilidad-inicial-parcelas
      lput (disponibilidad-media-parcela parcela-id)
           disponibilidad-inicial-parcelas

  set parcela-id parcela-id + 1
]


  ;; === 9. Parámetros de vacas ===
  set peso 450
  let factor 200 / (400 ^ 0.75) / 10000
  set Area-Animal factor * (peso ^ 0.75)

  ;; === 10. Crear animales ===
  crear-animales

  ;; === 11. Definir vacas a seguir ===
  if count vacas >= 3 [
    set vacas-a-seguir sublist [who] of vacas 0 3
  ]
  if count vacas < 3 [
    set vacas-a-seguir [who] of vacas
  ]

  ;; === 12. Informes gráficos y de consola iniciales ===
  imprimir-disponibilidades-iniciales

  graficar-rango-disponibilidad
  set promedio-kgms-inicio-dia mean [kgms] of patches with [plabel = parcela-pastoreo] / sup-pach

end

to calcular-CsEM
  ask turtles [
    let dms-patch [dms] of patch-here
    set CsEM round ( (consumo-diario * 3.608 * dms-patch) * 10 ) / 10
  ]
end



to validar-parametros
  if dias-pastoreo <= 0 [
    user-message "Por favor, ingrese un valor positivo para los días de pastoreo."
    stop
  ]
  if dias-permanencia <= 0 [
    user-message "Por favor, ingrese un valor positivo para los días de permanencia en cada parcela."
    stop
  ]
end

to parcelas
  let intervalo (33 / (parcela-pastoreo / 2))
  let parcela 1
  repeat parcela-pastoreo / 2 [
    ask patches [
      if pycor > (world-height / 2) and pxcor >= (parcela - 1) * intervalo and pxcor < parcela * intervalo [
        set plabel parcela
      ]
      if pycor <= (world-height / 2) and pxcor >= (parcela - 1) * intervalo and pxcor < parcela * intervalo [
        set plabel parcela + (parcela-pastoreo / 2)
      ]
    ]
    set parcela parcela + 1
  ]
end

to gestionar-kgms [modo] ;; Maneja la carga de MS en los patches según el modo
  ask patches [
    ifelse (modo = "inicializar") [ ;; Si se está inicializando
      set kgms ((mikgms +
  (plabel * (makgms - mikgms) / n-parcelas) *
  ((random 200) / 200)) * sup-pach) ;; convierte de kgMS/ha a kgMS reales en el patch
  set dms 0.65
    ] [
      let CreMed-Tick (TCrec * sup-pach) / ticks-por-dia ;; convierte TCrec (kgMS/ha/día) a kgMS/patch/tick
      let crecimiento-realizado (CreMed-Tick * random-float 1) ;; agrega variabilidad entre 0–100%
      set kgms kgms + crecimiento-realizado ;; aplica crecimiento al patch
    ]
  ]
  colorear-parcelas ;; Actualiza visualización
end

to colorear-parcelas ;; Aplica color en escala al valor de kgms
  ask patches [
    set pcolor scale-color 65 (kgms / sup-pach) makgms mikgms ;; Más oscuro = más forraje
  ]
end

to crear-animales ;; Crea las vacas y las inicializa
  clear-turtles ;; Borra todas las vacas previas
  create-vacas n-vacas ;; Genera la cantidad solicitada
  ask vacas [
    move-to one-of patches with [plabel = n-parcelas] ;; Sitúa cada vaca en la parcela inicial
    set shape "cow" ;; Cambia el ícono a vaca
    set color brown ;; Color marrón
    set size 1 ;; Tamaño
    set historial-consumo [] ;; Historial vacío
    set consumo-diario 0 ;; Inicio con consumo 0
    set pV peso
  ]
end

to go
  ;; === Control general de la simulación ===
  control-pastoreo
  if parcela-pastoreo = -1 [ stop ]

  ;; === Avance temporal ===
  set contador-ticks contador-ticks + 1

  ;; === Dinámica del sistema ===
  cambiar-parcela                  ;; Ingresar en una parcela
  mover-vacas-en-parcela           ;; Selección del patch
  comer                            ;; Consumo de MS
  calcular-CsEM                    ;; Cálculo de CsEM (futuro) Balance energético(futuro) Cambio de peso
  gestionar-kgms "incrementar"     ;;Crecimiento del forraje
  graficar-disponibilidad-parcelas ;; Colorear el Patch

  ;; === Cierre del día ===
  if ticks > 0 and ticks mod ticks-por-dia = 0 [
    cerrar-dia
  ]

  ;; === Cierre de la simulación ===
  if ticks = ticks-totales [
    cerrar-simulacion
    stop
  ]

  tick
end



;; ================================================================
;; Procedimientos auxiliares para mejorar consistencia del cálculo
;; ================================================================

to-report calcular-disponibilidad-ha [nro-parcela]
  ;; Calcula la disponibilidad promedio expresada como kgMS/ha
  let area count patches with [plabel = nro-parcela] * sup-pach
  if area = 0 [ report 0 ]
  report (sum [kgms] of patches with [plabel = nro-parcela]) / area
end

to-report calcular-total-kgms-inicial [nro-parcela]
  ;; Suma el forraje remanente y lo consumido por las vacas
  report sum [kgms] of patches with [plabel = nro-parcela] + sum [consumo-diario] of vacas
end

to-report calcular-total-kgms-final [nro-parcela]
  ;; Suma el forraje que queda en la parcela al final del día
  report sum [kgms] of patches with [plabel = nro-parcela]
end




to control-pastoreo
  if ticks > ticks-totales [
    imprimir-disponibilidades-extremas
    print (word "Fin del pastoreo programado. Ticks actuales: " ticks " de " ticks-totales)
    set parcela-pastoreo -1
  ]

  if parcela-pastoreo <= 0 [
    set parcela-pastoreo n-parcelas
    print (word "Reiniciando pastoreo. Parcela actual: " parcela-pastoreo)
    set parcela-pastoreo -1
  ]


end

to cambiar-parcela ;; Gestiona el paso a una nueva parcela
  let ticks-por-permanencia dias-permanencia * ticks-por-dia ;; Cálculo por días

  if ticks-actuales-parcela < ticks-por-permanencia [
    set ticks-actuales-parcela ticks-actuales-parcela + 1 ;; Continúa en parcela
  ]

  if ticks-actuales-parcela >= ticks-por-permanencia [
    set ticks-actuales-parcela 0 ;; Reinicia contador
    ifelse parcela-pastoreo > 1 [
      set parcela-pastoreo parcela-pastoreo - 1 ;; Va a la siguiente
    ] [
      set parcela-pastoreo n-parcelas ;; Reinicia
    ]
    ask vacas [
      move-to one-of patches with [plabel = parcela-pastoreo] ;; Reubica vacas
    ]
  ]
end

to mover-vacas-en-parcela ;; Mueve vacas en su parcela
  ask vacas [
    let patches-vecinos patches in-radius 1 with [plabel = [plabel] of myself] ;; Sólo los cercanos y mismos plabel
    if any? patches-vecinos [
      let mejor-patch max-one-of patches-vecinos [kgms] ;; Elige el de más kgMS
      move-to mejor-patch
    ]
  ]
end

to comer
  ;; ============================================================
  ;; Submodelo de consumo de forraje
  ;;
  ;; El consumo se calcula por tick y está limitado por:
  ;; - un consumo diario teórico máximo (2,5 % del peso vivo),
  ;; - la disponibilidad de MS del patch por encima del umbral,
  ;; - el área animal accesible,
  ;; - y la resolución temporal (ticks por día).
  ;;
  ;; Si una vaca alcanza su consumo diario máximo,
  ;; deja de consumir forraje el resto del día.
  ;; ============================================================

  ask vacas [

    ;; ----------------------------------------------------------
    ;; 1. Consumo diario teórico máximo (kg MS / día)
    ;; ----------------------------------------------------------
    let consumo-max-diario 0.025 * peso

    ;; Si la vaca ya alcanzó su consumo máximo diario, no come
    if consumo-diario >= consumo-max-diario [
      stop
    ]

    ;; ----------------------------------------------------------
    ;; 2. Disponibilidad del patch actual (kg MS / ha)
    ;; ----------------------------------------------------------
    let patch-actual patch-here
    let disponibilidad-ha ([kgms] of patch-actual) / sup-pach

    ;; MS utilizable por encima del umbral
    let ms-utilizable-ha max (list (disponibilidad-ha - umbral-disponibilidad) 0)

    ;; ----------------------------------------------------------
    ;; 3. Consumo posible diario del patch (sin límite animal)
    ;; ----------------------------------------------------------
    let consumo-posible-diario ms-utilizable-ha * 0.5

    ;; Ajuste por área animal (kg MS / día)
    let consumo-posible-diario-animal consumo-posible-diario * Area-Animal

    ;; ----------------------------------------------------------
    ;; 4. Pasaje a consumo posible por tick
    ;; ----------------------------------------------------------
    let consumo-posible-tick consumo-posible-diario-animal / ticks-por-dia

    ;; No permitir consumo negativo
    set consumo-posible-tick max (list consumo-posible-tick 0)

    ;; ----------------------------------------------------------
    ;; 5. Consumo efectivo por tick (limitado por el máximo diario)
    ;; ----------------------------------------------------------
    let consumo-restante consumo-max-diario - consumo-diario
    let consumo-real-tick min (list consumo-posible-tick consumo-restante)

    ;; ----------------------------------------------------------
    ;; 6. Actualización de consumo y forraje
    ;; ----------------------------------------------------------
    if consumo-real-tick > 0 [

      ;; Descontar forraje del patch (kg MS totales)
      ask patch-actual [
        set kgms kgms - consumo-real-tick
      ]

      ;; Acumular consumo diario del animal
      set consumo-diario consumo-diario + consumo-real-tick
    ]
  ]
end

to calcular-EM-mantenimiento
  ask turtles [
    set EM-mant round ( ((pV * 0.02 + 2) * 1.3) * 10 ) / 10
  ]
end



to imprimir-disponibilidades-extremas ;; Reporte final
  let disponibilidades []
  let parcelasa (range 1 (n-parcelas + 1))
  foreach parcelasa [i ->
    let promedio-kgms round (mean [kgms] of patches with [plabel = i])
    set disponibilidades lput promedio-kgms disponibilidades
  ]
  print (word "Máxima disponibilidad promedio: " max disponibilidades)
  print (word "Mínima disponibilidad promedio: " min disponibilidades)
end

to imprimir-disponibilidades-iniciales ;; Imprime al comienzo
  let disponibilidades []
  let parcelasa (range 1 (n-parcelas + 1))
  foreach parcelasa [i ->
    let promedio-kgms mean [kgms] of patches with [plabel = i] / sup-pach

    set disponibilidades lput (list i promedio-kgms) disponibilidades
  ]
  ;; Buscar extremos
  let parcela-maxima [] let parcela-minima []
  let max-disponibilidad -99999 let min-disponibilidad 99999
  foreach disponibilidades [parcela ->
    let promedio last parcela
    if promedio > max-disponibilidad [ set max-disponibilidad promedio set parcela-maxima parcela ]
    if promedio < min-disponibilidad [ set min-disponibilidad promedio set parcela-minima parcela ]
  ]
  print (word "Parcela con máxima disponibilidad inicial: " first parcela-maxima " (" round max-disponibilidad ")")
  print (word "Parcela con mínima disponibilidad inicial: " first parcela-minima " (" round min-disponibilidad ")")
end


to actualizar-grafico-consumo ;; Dibuja gráfico diario
  let consumos [last historial-consumo] of vacas
  if not empty? consumos [
    let dia ticks / ticks-por-dia
    set-current-plot "Consumo Diario (KgMS)"
    set-current-plot-pen "Máximo" plotxy dia max consumos
    set-current-plot-pen "Mínimo" plotxy dia min consumos
    set-current-plot-pen "Promedio" plotxy dia mean consumos
    set-current-plot-pen "Mediana" plotxy dia median consumos
  ]
end

to imprimir-consumos-tick ;; Imprime consumos de vacas monitoreadas
  let tick-actual ticks
  let mensaje (word "Tick: " tick-actual " | ")
  let entradas []
  foreach vacas-a-seguir [ id ->
    let cs precision ([Cs-Kgms] of vaca id) 2
    set entradas lput (word "Vaca " id ": " cs " KgMS") entradas
  ]
  set mensaje word mensaje (reduce [[a b] -> (word a " | " b)] entradas)
  print mensaje

end
to graficar-rango-disponibilidad
  let disponibilidades []
  let parcelasa (range 1 (n-parcelas + 1))
  foreach parcelasa [i ->
    let promedio-kgms mean [kgms] of patches with [plabel = i] / sup-pach
    set disponibilidades lput promedio-kgms disponibilidades
  ]

  let maximo max disponibilidades
  let minimo min disponibilidades

  set-current-plot "Rango de Disponibilidad (kgMS/ha)"
  clear-plot

  ;; Separamos las columnas en el eje X
  set-current-plot-pen "Maxima-Inicio"
  plotxy 10 maximo

  set-current-plot-pen "Minima-Inicio"
  plotxy 20 minimo
end
to graficar-disponibilidad-final
  let disponibilidades []
  let parcelasa (range 1 (n-parcelas + 1))
  foreach parcelasa [i ->
    let promedio-kgms mean [kgms] of patches with [plabel = i] / sup-pach
    set disponibilidades lput promedio-kgms disponibilidades
  ]

  let maximo max disponibilidades
  let minimo min disponibilidades
  let promedio mean disponibilidades

  set-current-plot "Rango de Disponibilidad (kgMS/ha)"

  set-current-plot-pen "Maxima-Final"
  plotxy 30 maximo

  set-current-plot-pen "Minima-Final"
  plotxy 40 minimo

  ;; Opcional: Promedio general al final
  set-current-plot-pen "Promedio-Final"
  plotxy 50 promedio
end

to cerrar-dia
  ;; === Registro del consumo diario por vaca ===
  ask vacas [
    set historial-consumo lput consumo-diario historial-consumo
  ]

  ;; === Identificación del día actual ===
  let dia ticks / ticks-por-dia

  ;; === Estadísticos de consumo por vaca ===
  let consumos-dia [consumo-diario] of vacas
  let promedio-consumo mean consumos-dia
  let max-consumo max consumos-dia
  let min-consumo min consumos-dia

  ;; === Disponibilidad de forraje (kgMS/ha) ===
  let promedio-kgms-final-dia calcular-disponibilidad-ha parcela-pastoreo

  ;; === Balance de forraje (kg MS totales) ===
  let total-inicial calcular-total-kgms-inicial parcela-pastoreo
  let total-final calcular-total-kgms-final parcela-pastoreo
  let total-consumo sum consumos-dia
  let diferencia total-inicial - total-final

  ;; === Reporte diario ===
  print (word "=== DÍA " dia " FINALIZADO ===")
  print (word "Promedio kgMS/ha al inicio del día: "
              round promedio-kgms-inicio-dia)
  print (word "Promedio kgMS/ha al final del día : "
              round promedio-kgms-final-dia)

  print (word "Consumo promedio por vaca: "
              precision promedio-consumo 2 " kgMS")
  print (word "Consumo máximo: "
              precision max-consumo 2 " kgMS")
  print (word "Consumo mínimo: "
              precision min-consumo 2 " kgMS")

  print (word "📦 Forraje total al inicio (kg): "
              round total-inicial)
  print (word "🐄 Consumo total de vacas (kg): "
              round total-consumo)
  print (word "🌱 Forraje remanente en parcela (kg): "
              round total-final)
  print (word "⚖ Diferencia estimada (kg): "
              round diferencia)

  ;; === Acumulación del consumo total de la simulación ===
  set consumo-total-simulacion
      consumo-total-simulacion + total-consumo

  ;; === Actualización de gráficos ===

  actualizar-grafico-consumo
  actualizar-grafico-disponibilidad

   ;; === Reinicio de variables diarias ===
  ask vacas [
    set consumo-diario 0
  ]

end
to-report disponibilidad-media-parcela [p]
  ;; Este reporter calcula la disponibilidad promedio de forraje
  ;; para una parcela dada (identificada por p).
  ;;
  ;; La disponibilidad se expresa en kg de MS por hectárea.
  ;;
  ;; - kgms es una variable de patch (kg MS por patch)
  ;; - sup-pach es la superficie real representada por cada patch (ha)
  ;; - plabel identifica a qué parcela pertenece cada patch

  ;; Seleccionamos todos los patches que pertenecen a la parcela p
  let patches-parcela patches with [plabel = p]

  ;; Calculamos el promedio de kgMS por patch
  ;; y lo convertimos a kgMS/ha dividiendo por la superficie del patch
  report mean [kgms] of patches-parcela / sup-pach
end

to graficar-disponibilidad-parcelas
  ;; ============================================================
  ;; Gráfico: "Disponibilidad Parcelas"
  ;;
  ;; Este procedimiento dibuja un gráfico de barras comparativo
  ;; de la disponibilidad promedio de forraje por parcela.
  ;;
  ;; - Eje X: número de parcela (manejado automáticamente por NetLogo)
  ;; - Eje Y: disponibilidad promedio (kg de MS por hectárea)
  ;;
  ;; Para cada parcela se dibujan DOS barras:
  ;;   * Azul  : disponibilidad inicial (estado al inicio de la simulación)
  ;;   * Rojo  : disponibilidad final   (estado al final de la simulación)
  ;;
  ;; El gráfico permite evaluar visualmente el balance entre:
  ;; - consumo animal,
  ;; - crecimiento del forraje,
  ;; - y acumulación o déficit por parcela.
  ;;
  ;; Importante:
  ;; NetLogo no permite controlar manualmente las etiquetas del eje X,
  ;; por lo que los números de parcela que se muestran dependen
  ;; automáticamente de la escala del gráfico.
  ;; ============================================================

  ;; Seleccionamos el gráfico correspondiente
  set-current-plot "Disponibilidad Parcelas"

  ;; Limpiamos el gráfico UNA sola vez
  clear-plot

  ;; ------------------------------------------------------------
  ;; DISPONIBILIDAD INICIAL (pen azul)
  ;; ------------------------------------------------------------
  set-current-plot-pen "Inicial"
  set-plot-pen-mode 1        ;; modo barras
  set-plot-pen-interval 0.35 ;; ancho de cada barra

  let parcela-id 1
  while [parcela-id <= n-parcelas] [
    ;; Barra azul levemente desplazada a la izquierda
    plotxy (parcela-id - 0.2)
           item (parcela-id - 1) disponibilidad-inicial-parcelas
    set parcela-id parcela-id + 1
  ]

  ;; ------------------------------------------------------------
  ;; DISPONIBILIDAD FINAL (pen rojo)
  ;; ------------------------------------------------------------
  set-current-plot-pen "Final"
  set-plot-pen-mode 1
  set-plot-pen-interval 0.35

  set parcela-id 1
  while [parcela-id <= n-parcelas] [
    ;; Barra roja levemente desplazada a la derecha
    plotxy (parcela-id + 0.2)
           ( mean [kgms] of patches with [plabel = parcela-id] / sup-pach )
    set parcela-id parcela-id + 1
  ]
end
to actualizar-grafico-disponibilidad
  ;; Actualiza el gráfico de línea con la disponibilidad
  ;; media del sistema expresada en kg MS / ha

  set-current-plot "Disponibilidad"
  set-current-plot-pen "Media sistema"

  plot mean [kgms / sup-pach] of patches
end



to cerrar-simulacion
  print "=============================================="
  print "🎯 FIN DE LA SIMULACIÓN DE PASTOREO"
  print "=============================================="

  print (word "Duración de la simulación (días): "
              dias-pastoreo)

  print (word "🐄 Consumo total acumulado "
              "(kg MS totales del campo): "
              precision consumo-total-simulacion 0)

  print (word "🌱 Producción teórica total de forraje "
              "(kg MS totales del campo): "
              precision (Tcrec * dias-pastoreo * 200) 0)

  print "=============================================="
  graficar-disponibilidad-parcelas

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
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
32
0
32
0
0
1
ticks
30.0

BUTTON
20
176
109
225
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

MONITOR
396
204
490
249
KgMS/Ha Prom
mean [kgms / sup-pach] of patches
0
1
11

MONITOR
549
400
644
445
Ultima¨Parcela
mean [kgms / sup-pach] of patches with [plabel = n-parcelas]
0
1
11

MONITOR
213
13
291
58
P1 KgMS/Ha
mean [kgms / sup-pach] of patches with [ plabel = 1]
0
1
11

BUTTON
125
177
206
224
NIL
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

MONITOR
676
174
782
219
Dias de Pastoreo
ticks / 48
0
1
11

PLOT
651
16
851
166
Consumo Diario (KgMS)
Día
KgMS
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Máximo" 1.0 0 -2674135 true "" ""
"Mínimo" 1.0 0 -13345367 true "" ""
"Promedio" 1.0 0 -13840069 true "" ""
"Mediana" 1.0 0 -7500403 true "" ""

PLOT
677
437
877
587
Rango de Disponibilidad (kgMS/ha)
Parcela
Disponibilidad
0.0
1.0
0.0
100.0
true
false
"" ""
PENS
"Maxima-Inicio" 10.0 1 -5298144 true "" ""
"Minima-Inicio" 10.0 1 -15302303 true "" ""
"Maxima-Final\t" 1.0 0 -14070903 true "" ""
"Minima-Final" 1.0 0 -5516827 true "" ""
"Promedio-Final" 1.0 0 -7500403 true "" ""

PLOT
208
458
647
749
Disponibilidad Parcelas
Parcelas 
Disponibilidad (kg MS/ha) 
1.0
2.0
2000.0
2000.0
true
true
"" ""
PENS
"Inicial" 1.0 1 -14070903 true "" ""
"Final" 1.0 1 -14439633 true "" ""

PLOT
682
257
882
407
Disponibilidad
Días
Disponibilidad media (kg MS/ha) 
0.0
10.0
2000.0
10.0
true
false
"" ""
PENS
"Media sistema" 1.0 0 -15040220 true "" ""

CHOOSER
16
14
108
59
n-vacas
n-vacas
50 100 150 200 250 300 350 400 450 500 550 600
8

CHOOSER
115
13
207
58
n-parcelas
n-parcelas
4 8 12 16 20
2

CHOOSER
16
69
108
114
TCrec
TCrec
5 15 25 35 45 50 55
3

MONITOR
16
237
146
282
Días Pastoreo por Vuelta
dias-permanencia * n-parcelas
0
1
11

CHOOSER
112
70
204
115
dias-permanencia
dias-permanencia
1 2 3 5 8 12 17 23
2

CHOOSER
115
121
207
166
Vueltas
Vueltas
1 2 3 4
1

MONITOR
17
289
145
334
Días Est.Pastoreo
dias-permanencia * n-parcelas * vueltas
0
1
11

CHOOSER
17
121
109
166
Disp-Minima
Disp-Minima
1200 1300 1400 1500 1600 1800
5

@#$#@#$#@
## Título:
**Simulación de Pastoreo Rotativo con Vacas en NetLogo**

## Autores:
Roberto Rubio (Universidad Nacional del Centro – Facultad de Ciencias Veterinarias)

## Propósito del Modelo:
Este modelo simula el pastoreo rotativo de vacas sobre un conjunto de parcelas, permitiendo explorar cómo la disponibilidad de forraje, el consumo animal, la presión de pastoreo y la permanencia por parcela afectan la eficiencia del sistema.

Está diseñado como una herramienta educativa y de análisis para:
- Comprender el impacto de distintas estrategias de manejo sobre la utilización de pasturas.
- Evaluar la relación entre disponibilidad de materia seca (kg MS/ha) y consumo animal.
- Visualizar dinámicamente la evolución del recurso forrajero y su aprovechamiento por parte de los animales.
- Apoyar decisiones relacionadas con carga animal, duración del pastoreo y suplementación.

## Fenómenos que Simula:
- Crecimiento diario del forraje en función de una tasa de crecimiento definida (`TCrec`).
- Selección y consumo de pasto por vacas en función de la disponibilidad y el tamaño corporal.
- Distribución espacial del pastoreo y movimiento de animales hacia sectores de mayor disponibilidad.
- Cambio programado entre parcelas según permanencia predefinida (`dias-permanencia`).
- Acumulación y descarga de forraje en cada parche y parcela, con registros por tick y por día.
- Efectos de la presión animal sobre el consumo diario y residual.

## Entidades:
- **Turtles**: Vacas
- **Patches**: Porciones del terreno con valor de disponibilidad de MS
- **Globals**: Variables del entorno, carga animal, superficie, tiempos, etc.

## Variables Importantes:

### Globales:
- `makgms`, `mikgms`: Disponibilidad máxima y mínima de MS por patch (kgMS/ha)
- `Area-Animal`: Área ocupada por una vaca según su peso
- `series`: Índice de presión animal (área disponible / área ocupada)
- `parcela-pastoreo`: Parcela actual en pastoreo
- `ticks-por-dia`, `ticks-totales`: Tiempo simulado
- `has`, `sup-pach`: Superficie total (ha) y por patch

### Patch-own:
- `kgms`: Disponibilidad de forraje en kgMS
- `consumo-tick`: Acumulador de consumo por tick

### Turtle-own:
- `Cs-Kgms`: Consumo en el tick actual
- `consumo-diario`: Consumo acumulado del día
- `historial-consumo`: Lista con consumo diario de cada vaca

## Parámetros del Modelo:
- `Nro-Vacas`: Número total de vacas simuladas
- `dias-pastoreo`: Duración total del período de simulación (en días)
- `dias-permanencia`: Tiempo de permanencia en cada parcela (en días)
- `n-parcelas`: Número de parcelas del sistema
- `TCrec`: Tasa de crecimiento del forraje (kgMS/ha/día)
- `peso`: Peso promedio de las vacas (kg)

## Procedimientos Principales:
- `setup`: Inicializa la simulación, define parámetros y distribuye recursos.
- `go`: Ejecuta cada tick, con consumo, crecimiento, desplazamiento y registro de datos.
- `comer`: Cada vaca calcula y consume MS según disponibilidad y presión.
- `cambiar-parcela`: Avanza a la siguiente parcela cuando se cumple el tiempo de permanencia.
- `gestionar-kgms`: Controla el crecimiento diario del forraje.
- `actualizar-grafico-consumo`: Grafica el consumo diario promedio, máximo, mínimo y mediana.
- `imprimir-consumos-tick`, `imprimir-disponibilidades-iniciales/extremas`: Imprime datos para seguimiento y análisis.

## Resultados y Salidas:
- Registro de consumo por tick (archivo `.txt`)
- Registro de consumo diario por vaca (archivo `.csv`)
- Gráfico de **Disponibilidad vs Consumo diario**
- Gráfico de **Consumo Diario (KgMS)** con máximos, mínimos, promedios y medianas
- Mensajes informativos en consola y reporte de disponibilidades extremas

## Aplicaciones Potenciales:
- Diseño y evaluación de estrategias de manejo del pastoreo.
- Entrenamiento de estudiantes de veterinaria, agronomía o producción animal.
- Exploración de impactos de cambios en carga animal, tiempo de descanso y tasa de crecimiento.

## Limitaciones:
- No incluye suplementación externa ni sustitución forrajera por ahora.
- No hay representación explícita de residuos post-pastoreo ni penalización por sobrepastoreo.
- El crecimiento del forraje es aleatorio entre 0 y 100% del valor diario esperado.
- No se incluye reproducción ni dinámica de estado de salud de los animales.

## Créditos y Agradecimientos:
Modelo diseñado para propósitos educativos y de desarrollo de herramientas de simulación para sistemas pastoriles.
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
NetLogo 6.4.0
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
