;;; GALVANIZADOSCIFUNSA: requiere PiezaDatos.lsp; no necesita DN.
;;; Modelo simplificado, mm. Referencia: CIFUNSA.pdf, 150 lb, pp. 3, 6, 8, 12.
;;; DE = refuerzo de boca; DI aproximado = E1 NPT. Sin roscas.


;;; Adaptador de igual medida: envolvente aproximada, no tabla dimensional CIFUNSA.
(defun gc:datos-adaptador (medida / hembra tubo)
  (setq hembra (gc:datos "COPLE" medida) tubo (gc:datos-niple medida 100.0))
  (if (and hembra tubo)
    (list (car hembra) (cadr hembra) (+ (caddr hembra) (* 0.65 (car tubo)))
      (car tubo) (cadr tubo))))
(defun gc:adaptador (modo datos / de boca largo macho paso hombro inicio bisel)
  (setq de (/ (car datos) 2.0) boca (/ (cadr datos) 2.0) largo (caddr datos)
    macho (/ (nth 3 datos) 2.0) paso (/ (nth 4 datos) 2.0)
    hombro (* largo 0.62) inicio (* largo 0.42) bisel (* 0.08 (- macho paso)))
  (if (= modo "Estructura")
    (progn
      (gc:anillo '(0.0 0.0 0.0) '(1.0 0.0 0.0) (* de 2.0) (* boca 2.0))
      (gc:anillo (list largo 0.0 0.0) '(1.0 0.0 0.0) (* macho 2.0) (* paso 2.0))
      (gc:linea '(0.0 0.0 0.0) (list largo 0.0 0.0)) nil)
    (gc:perfil-giro
      (list (list 0.0 (- de bisel)) (list bisel de)
        (list hombro de) (list (+ hombro (* 0.08 largo)) macho)
        (list (- largo bisel) macho) (list largo (- macho bisel))
        (list largo paso) (list (+ inicio (* 0.1 largo)) paso)
        (list inicio boca) (list 0.0 boca)) nil)))
(defun gc:campos-adaptador (medida datos)
  (pd:con-descripcion
    (list (list "TIPO" "ADAPTADOR") (list "MATERIAL" "HIERRO MALEABLE")
      (list "ACABADO" "GALVANIZADO") (list "CONEXION" "MACHO-HEMBRA")
      (list "CEDULA" "") (list "CLASE" "") (list "ROSCA" "")
      (list "MEDIDA_MAYOR" (pd:numero medida)) (list "MEDIDA_MENOR" "")
      (list "UNIDAD_MEDIDA" "PULG") (list "LONGITUD_MM" (pd:numero (caddr datos)))
      (list "ORIGEN" "GCIF") (list "ESTADO_DATOS" "GEOMETRIA APROXIMADA; SIN ROSCAS; NORMA Y CLASE POR CONFIRMAR")
      (list "CATALOGO_REFERENCIA" "ENVOLVENTE APROXIMADA A PARTIR DE COPLE Y TUBO; NO DIMENSION OFICIAL"))
    (strcat "ADAPTADOR MACHO-HEMBRA GALVANIZADO " (pd:numero medida) "pulg")))

(defun gc:longitudes-niple (medida / minimo valores valor resultado)
  ;; CIFUNSA Niplesdeacero.pdf pp6-11, galvanizados; RC se trata aparte de estos largos.
  (setq minimo (cond ((<= medida 0.75) 1.5) ((<= medida 1.5) 2.0)
    ((= medida 2.0) 2.5) ((<= medida 3.0) 3.0) (T 4.0))
    valores '(1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 7.0 8.0 9.0 10.0 11.0 12.0))
  (foreach valor valores (if (>= valor minimo) (setq resultado (append resultado (list (* valor 25.4)))))) resultado)
(defun gc:datos-niple (medida longitud / fila)
  ;; Diametros de tubo cedula40, no diametros reforzados de un cople hembra.
  (setq fila (assoc medida '((0.125 10.29 6.83) (0.25 13.72 9.22) (0.375 17.15 12.52)
    (0.5 21.34 15.80) (0.75 26.67 20.93) (1.0 33.40 26.64) (1.25 42.16 35.05)
    (1.5 48.26 40.94) (2.0 60.33 52.50) (2.5 73.03 62.68) (3.0 88.90 77.93) (4.0 114.30 102.26))))
  (if (and fila longitud (> longitud 0.0)) (list (cadr fila) (caddr fila) longitud)))
(defun gc:zona-niple (datos / medida fila boca largo zona diametro)
  ;; L2 NPT en mm: https://emsalvana.com/en/npt (ASME B1.20.1).
  ;; Rebaje cilindrico visual: DI de boca existente menos 0.20 mm de holgura diametral.
  ;; No representa crestas NPT ni una profundidad de apriete certificada.
  (foreach fila '((0.125 6.70) (0.25 10.21) (0.375 10.36) (0.5 13.56)
      (0.75 13.86) (1.0 17.34) (1.25 17.95) (1.5 18.38)
      (2.0 19.21) (2.5 28.89) (3.0 30.48) (4.0 33.02))
    (if (equal (car datos) (car (gc:datos-niple (car fila) 100.0)) 0.001)
      (setq medida (car fila) zona (cadr fila))))
  (if medida
    (progn
      (setq boca (gc:datos "C90" medida) largo (caddr datos)
        diametro (- (cadr boca) 0.20))
      (if (> diametro (cadr datos))
        (list (min zona (* 0.4 largo)) diametro)))))

(defun gc:niple (modo datos / de di largo bisel perfil solido puntos zona extremo transicion)
  (setq de (car datos) di (cadr datos) largo (caddr datos))
  (setq zona (gc:zona-niple datos))
  (if (null zona) (progn (princ "\nNo se pudo calcular el rebaje del niple.") (exit)))
  (setq extremo (/ (cadr zona) 2.0) zona (car zona)
    transicion (min 1.0 (* 0.02 largo)))
  (if (= modo "Estructura")
    (progn
      (gc:anillo '(0.0 0.0 0.0) '(1.0 0.0 0.0) (* 2.0 extremo) di)
      (gc:anillo (list zona 0.0 0.0) '(1.0 0.0 0.0) de di)
      (gc:anillo (list (- largo zona) 0.0 0.0) '(1.0 0.0 0.0) de di)
      (gc:anillo (list largo 0.0 0.0) '(1.0 0.0 0.0) (* 2.0 extremo) di)
      (gc:linea '(0.0 0.0 0.0) (list largo 0.0 0.0)) nil)
    (progn
      (setq bisel (min (* 0.15 largo) (* 0.25 (/ (- de di) 2.0)))
        puntos (list (list 0.0 extremo) (list zona extremo)
          (list (+ zona transicion) (/ de 2.0))
          (list (- largo zona transicion) (/ de 2.0))
          (list (- largo zona) extremo) (list largo extremo)
          (list largo (/ di 2.0)) (list 0.0 (/ di 2.0))))
      (gc:perfil-giro puntos nil))))
(defun gc:campos-niple (medida datos)
  (pd:con-descripcion
    (list (list "TIPO" "NIPLE") (list "MATERIAL" "ACERO AL CARBON") (list "ACABADO" "GALVANIZADO")
      (list "CONEXION" "MACHO-MACHO") (list "ROSCA_REFERENCIA" "NPT") (list "ROSCA" "")
      (list "CEDULA" "40") (list "MEDIDA_MAYOR" (pd:numero medida)) (list "MEDIDA_MENOR" "")
      (list "UNIDAD_MEDIDA" "PULG") (list "LONGITUD_MM" (pd:numero (caddr datos)))
      (list "DE_REFERENCIA_MM" (pd:numero (car datos))) (list "DI_REFERENCIA_MM" (pd:numero (cadr datos)))
      (list "ORIGEN" "GCIF") (list "REVISION_GEOMETRIA" "NIPLE-REBAJE-1")
      (list "ESTADO_DATOS" "MODELO SIN ROSCAS; LONGITUD ELEGIDA")
      (list "CATALOGO_REFERENCIA" "DIAMETROS DN C40; LONGITUD ELEGIDA EN MM"))
    (strcat "NIPLE GALVANIZADO C-40 " (pd:numero medida) "pulg L=" (pd:numero (caddr datos)) "mm")))

(defun gc:datos (pieza medida / boca largo)
  (setq boca (assoc medida
    '((0.125 0.690 0.37360) (0.25 0.840 0.49163)
      (0.375 1.010 0.62701) (0.5 1.200 0.77843)
      (0.75 1.460 0.98887) (1.0 1.770 1.23863)
      (1.25 2.150 1.58338) (1.5 2.430 1.82234)
      (2.0 2.960 2.29627) (2.5 3.590 2.76216)
      (3.0 4.280 3.38850) (4.0 5.400 4.38712))))
  (setq largo (assoc medida
    (cond
     ((= pieza "COPLE")
      '((0.25 1.060) (0.375 1.160) (0.5 1.340) (0.75 1.520)
        (1.0 1.670) (1.25 1.930) (1.5 2.180) (2.0 2.530)
        (2.5 2.680) (3.0 3.180) (4.0 3.690)))
     ((= pieza "C45")
      '((0.25 0.730) (0.375 0.800) (0.5 0.880) (0.75 0.980)
        (1.0 1.120) (1.25 1.290) (1.5 1.430) (2.0 1.680)
        (2.5 1.950) (3.0 2.170) (4.0 2.610)))
     (T
      '((0.125 0.690) (0.25 0.810) (0.375 0.950) (0.5 1.120)
        (0.75 1.310) (1.0 1.500) (1.25 1.750) (1.5 1.950)
        (2.0 2.250) (2.5 2.700) (3.0 3.080) (4.0 3.790))))))
  (if (and (member pieza '("C90" "C45" "TEE" "COPLE" "CRUZ")) boca largo
           (or (/= pieza "CRUZ") (member medida '(0.5 0.75 1.0 2.0)))
           (or (= pieza "C90") (/= medida 0.125)))
    (list (* 25.4 (cadr boca)) (* 25.4 (caddr boca)) (* 25.4 (cadr largo))))
)

;;; Familias adicionales. Paginas impresas 8 a 12 del catalogo CIFUNSA.
;;; Filas de reducciones: ((NPS mayor NPS menor) A ...), dimensiones en pulgadas.
(defun gc:reduccion-fila (pieza mayor menor)
  (assoc (list mayor menor)
    (if (= pieza "BUSHING")
      '(((0.375 0.25) 0.480 0.680 0.160)
        ((0.5 0.25) 0.560 0.870 0.190) ((0.5 0.375) 0.560 0.870 0.190)
        ((0.75 0.25) 0.630 1.150 0.220) ((0.75 0.375) 0.630 1.150 0.220) ((0.75 0.5) 0.630 1.150 0.220)
        ((1.0 0.25) 0.750 1.120 0.300) ((1.0 0.375) 0.750 1.120 0.300)
        ((1.0 0.5) 0.750 1.420 0.250) ((1.0 0.75) 0.750 1.420 0.250)
        ((1.25 0.5) 0.800 1.340 0.340) ((1.25 0.75) 0.800 1.760 0.280) ((1.25 1.0) 0.800 1.760 0.280)
        ((1.5 0.5) 0.830 1.340 0.370) ((1.5 0.75) 0.830 1.630 0.370)
        ((1.5 1.0) 0.830 2.000 0.310) ((1.5 1.25) 0.830 2.000 0.310)
        ((2.0 0.5) 0.880 1.340 0.410) ((2.0 0.75) 0.880 1.630 0.410)
        ((2.0 1.0) 0.880 1.950 0.410) ((2.0 1.25) 0.880 2.480 0.340) ((2.0 1.5) 0.880 2.480 0.340)
        ((2.5 1.0) 1.070 1.950 0.440) ((2.5 1.25) 1.070 2.390 0.440)
        ((2.5 1.5) 1.070 2.680 0.440) ((2.5 2.0) 1.070 2.980 0.370)
        ((3.0 1.0) 1.130 1.950 0.480) ((3.0 1.25) 1.130 2.390 0.480)
        ((3.0 1.5) 1.130 2.680 0.480) ((3.0 2.0) 1.130 3.280 0.480) ((3.0 2.5) 1.130 3.860 0.400)
        ((4.0 1.0) 1.220 1.950 0.600) ((4.0 1.25) 1.220 2.390 0.600)
        ((4.0 1.5) 1.220 2.680 0.600) ((4.0 2.0) 1.220 3.280 0.600)
        ((4.0 2.5) 1.220 3.860 0.600) ((4.0 3.0) 1.220 4.620 0.500))
      '(((0.375 0.25) 1.130) ((0.5 0.25) 1.250) ((0.5 0.375) 1.250)
        ((0.75 0.375) 1.440) ((0.75 0.5) 1.440)
        ((1.0 0.25) 1.690) ((1.0 0.375) 1.690) ((1.0 0.5) 1.690) ((1.0 0.75) 1.690)
        ((1.25 0.5) 2.060) ((1.25 0.75) 2.060) ((1.25 1.0) 2.060)
        ((1.5 0.5) 2.310) ((1.5 0.75) 2.310) ((1.5 1.0) 2.310) ((1.5 1.25) 2.310)
        ((2.0 0.5) 2.810) ((2.0 0.75) 2.810) ((2.0 1.0) 2.810) ((2.0 1.25) 2.810) ((2.0 1.5) 2.810)
        ((2.5 1.5) 3.250) ((2.5 2.0) 3.250) ((3.0 2.0) 3.690) ((3.0 2.5) 3.690) ((4.0 3.0) 4.380))))
)

(defun gc:extra-fila (pieza medida)
  (assoc medida (cond
    ;; NPS, DE, altura, profundidad roscada, ancho banda.
    ((= pieza "CACHUCHA")
      '((0.375 1.010 0.740 0.360 0.230) (0.5 1.200 0.870 0.430 0.250)
        (0.75 1.460 0.970 0.500 0.270) (1.0 1.770 1.160 0.580 0.300)
        (1.25 2.150 1.280 0.670 0.340) (1.5 2.430 1.330 0.700 0.370)
        (2.0 2.460 1.450 0.750 0.420) (2.5 3.590 1.700 0.920 0.480)
        (3.0 4.280 1.800 0.980 0.550) (4.0 5.400 2.080 1.080 0.660)))
    ;; NPS, longitud macho, altura cabeza, ancho cuadrado.
    ((= pieza "MACHO")
      '((0.375 0.480 0.310 0.4375) (0.5 0.560 0.380 0.5625)
        (0.75 0.630 0.440 0.6250) (1.0 0.750 0.500 0.8125)
        (1.25 0.800 0.560 0.9375) (1.5 0.830 0.620 1.1250)
        (2.0 0.880 0.680 1.3125) (2.5 1.070 0.740 1.5000) (3.0 1.130 0.800 1.6875)))
    ;; NPS, longitud, ancho envolvente de tuerca, longitud de zona roscada.
    ((= pieza "UNION")
      '((0.25 1.290 1.440 0.560) (0.375 1.440 1.610 0.630)
        (0.5 1.760 1.790 0.700) (0.75 1.900 2.020 0.712)
        (1.0 2.160 2.120 0.780) (1.25 2.750 2.260 0.860)
        (1.5 2.990 2.440 0.880) (2.0 3.620 2.750 1.000)
        (2.5 4.540 3.260 1.200) (3.0 5.060 3.530 1.220) (4.0 6.080 3.950 1.430)))))
)

(defun gc:pipe-de (medida)
  (cadr (assoc medida '((0.25 13.716) (0.375 17.145) (0.5 21.336) (0.75 26.670)
    (1.0 33.401) (1.25 42.164) (1.5 48.260) (2.0 60.325)
    (2.5 73.025) (3.0 88.900) (4.0 114.300))))
)

;; Retorna DE, DI de boca, longitud total; sus detalles se consultan en las filas.
(defun gc:extra-datos (pieza mayor menor / fila boca chica de di largo)
  (setq boca (gc:datos "COPLE" mayor))
  (if boca
    (cond
      ((member pieza '("BUSHING" "CAMPANA"))
        (setq fila (gc:reduccion-fila pieza mayor menor) chica (gc:datos "COPLE" menor))
        (if (and fila chica (> mayor menor))
          (if (= pieza "BUSHING")
            (list (gc:pipe-de mayor) (cadr chica) (* 25.4 (+ (cadr fila) (nth 3 fila))))
            (list (car boca) (cadr boca) (* 25.4 (cadr fila))))))
      ((and (setq fila (gc:extra-fila pieza mayor))
            (or (/= pieza "UNION") (> (* 25.4 (nth 2 fila)) (cadr boca))))
        (cond
          ((= pieza "CACHUCHA") (list (* 25.4 (cadr fila)) (cadr boca) (* 25.4 (nth 2 fila))))
          ((= pieza "MACHO") (list (gc:pipe-de mayor) 0.0 (* 25.4 (+ (cadr fila) (nth 2 fila)))))
          ((= pieza "UNION") (list (* 25.4 (nth 2 fila)) (cadr boca) (* 25.4 (cadr fila))))))))
)

;; Todas las entidades nuevas se registran antes de asignar propiedades.
(defun gc:registrar (objeto)
  (setq gc:creadas (cons objeto gc:creadas))
  (vla-put-Layer objeto (getvar "CLAYER"))
  objeto
)
(defun gc:borrar (objeto)
  (if (and objeto (not (vlax-erased-p objeto))) (vla-Delete objeto))
)
(defun gc:disco (centro normal radio / circulo matriz regiones region)
  (setq circulo (gc:registrar (vla-AddCircle gc:espacio (vlax-3d-point centro) radio)))
  (vla-put-Normal circulo (vlax-3d-point normal))
  (vla-put-Center circulo (vlax-3d-point centro))
  (setq matriz (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element matriz 0 circulo)
  (setq regiones (vlax-variant-value (vla-AddRegion gc:espacio matriz)))
  (setq region (gc:registrar (vlax-safearray-get-element regiones 0)))
  (gc:borrar circulo)
  region
)
(defun gc:anillo (centro normal de di / exterior interior)
  (setq exterior (gc:disco centro normal (/ de 2.0))
        interior (gc:disco centro normal (/ di 2.0)))
  (vla-Boolean exterior 2 interior)
  (gc:borrar interior)
  exterior
)
(defun gc:cilindro (centro normal radio largo / perfil solido)
  (setq perfil (gc:disco centro normal radio)
        solido (gc:registrar (vla-AddExtrudedSolid gc:espacio perfil largo 0.0)))
  (gc:borrar perfil)
  solido
)
(defun gc:linea (inicio fin)
  (gc:registrar (vla-AddLine gc:espacio (vlax-3d-point inicio) (vlax-3d-point fin)))
)

;; C y D del codo Cifunsa: ancho del refuerzo y longitud de la zona de boca.
;; El diametro central se aproxima; no es una cota de fabricacion del catalogo.
(defun gc:c90-forma (medida datos / fila de di a cuello banda cuerpo radio)
  (setq fila (assoc medida
    '((0.125 0.208 0.250) (0.25 0.210 0.320) (0.375 0.230 0.360)
      (0.5 0.250 0.430) (0.75 0.270 0.500) (1.0 0.300 0.580)
      (1.25 0.340 0.670) (1.5 0.370 0.700) (2.0 0.420 0.750)
      (2.5 0.480 0.920) (3.0 0.550 0.980) (4.0 0.660 1.080))))
  (if fila
    (progn
      (setq de (car datos) di (cadr datos) a (caddr datos)
            banda (* 25.4 (cadr fila)) cuello (* 25.4 (caddr fila))
            cuerpo (+ di (* 0.65 (- de di))) radio (- a cuello))
      (list cuerpo radio cuello banda))))

(defun gc:curva90 (radio diametro / perfil solido)
  (setq perfil (gc:disco (list (- radio) 0.0 0.0) '(1.0 0.0 0.0) (/ diametro 2.0))
        solido (gc:registrar (vla-AddRevolvedSolid gc:espacio perfil
          (vlax-3d-point (list (- radio) radio 0.0))
          (vlax-3d-point '(0.0 0.0 1.0)) (/ pi 2.0))))
  (gc:borrar perfil)
  solido
)

(defun gc:c90-detallado (datos forma / de di a cuerpo radio cuello banda eps exterior interior extra)
  (setq de (car datos) di (cadr datos) a (caddr datos)
        cuerpo (car forma) radio (cadr forma) cuello (caddr forma) banda (nth 3 forma)
        eps (min 0.1 (* 0.01 (- de di)))
        exterior (gc:curva90 radio cuerpo))
  ;; Unir el cuerpo, los cuellos y los dos refuerzos antes de abrir el conducto.
  (foreach extra
    (list
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) (+ cuello eps))
      (gc:cilindro (list 0.0 (- radio eps) 0.0) '(0.0 1.0 0.0) (/ cuerpo 2.0) (+ cuello eps))
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda)
      (gc:cilindro (list 0.0 (- a banda) 0.0) '(0.0 1.0 0.0) (/ de 2.0) banda))
    (vla-Boolean exterior 0 extra) (gc:borrar extra))
  (setq interior (gc:curva90 radio di))
  (foreach extra
    (list
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) (+ cuello eps))
      (gc:cilindro (list 0.0 (- radio eps) 0.0) '(0.0 1.0 0.0) (/ di 2.0) (+ cuello eps)))
    (vla-Boolean interior 0 extra) (gc:borrar extra))
  (vla-Boolean exterior 2 interior) (gc:borrar interior)
  exterior
)

;; C45: mismos refuerzos de boca; A conserva la interseccion de tangentes.
(defun gc:curva45 (k radio diametro / perfil solido)
  (setq perfil (gc:disco (list (- k) 0.0 0.0) '(1.0 0.0 0.0) (/ diametro 2.0))
        solido (gc:registrar (vla-AddRevolvedSolid gc:espacio perfil
          (vlax-3d-point (list (- k) radio 0.0))
          (vlax-3d-point '(0.0 0.0 1.0)) (/ pi 4.0))))
  (gc:borrar perfil)
  solido
)

(defun gc:c45-detallado (datos forma / de di a cuerpo cuello banda eps k radio u normal exterior interior extra)
  (setq de (car datos) di (cadr datos) a (caddr datos)
        cuerpo (car forma) cuello (caddr forma) banda (nth 3 forma)
        eps (min 0.1 (* 0.01 (- de di))) k (- a cuello)
        radio (/ (* k (cos (/ pi 8.0))) (sin (/ pi 8.0)))
        u (cos (/ pi 4.0)) normal (list u u 0.0)
        exterior (gc:curva45 k radio cuerpo))
  (foreach extra
    (list
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) (+ cuello eps))
      (gc:cilindro (list (* (- k eps) u) (* (- k eps) u) 0.0) normal (/ cuerpo 2.0) (+ cuello eps))
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda)
      (gc:cilindro (list (* (- a banda) u) (* (- a banda) u) 0.0) normal (/ de 2.0) banda))
    (vla-Boolean exterior 0 extra) (gc:borrar extra))
  (setq interior (gc:curva45 k radio di))
  (foreach extra
    (list
      (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) (+ cuello eps))
      (gc:cilindro (list (* (- k eps) u) (* (- k eps) u) 0.0) normal (/ di 2.0) (+ cuello eps)))
    (vla-Boolean interior 0 extra) (gc:borrar extra))
  (vla-Boolean exterior 2 interior) (gc:borrar interior)
  exterior
)

;; Tee y cople: exterior reducido con bandas en las bocas; conductos lisos.
(defun gc:recto-detallado (pieza datos forma / de di a cuerpo banda exterior interior rama extra inicio longitud)
  (setq de (car datos) di (cadr datos) a (caddr datos)
        cuerpo (car forma) banda (nth 3 forma))
  (if (= pieza "COPLE")
    (progn
      (setq exterior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) a))
      (foreach extra
        (list
          (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda)
          (gc:cilindro (list (- a banda) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda))
        (vla-Boolean exterior 0 extra) (gc:borrar extra))
      (setq interior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) a)))
    (progn
      (setq exterior (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) (* 2.0 a))
            inicio (if (= pieza "CRUZ") (list 0.0 (- a) 0.0) '(0.0 0.0 0.0))
            longitud (if (= pieza "CRUZ") (* 2.0 a) a))
      (foreach extra
        (list
          (gc:cilindro inicio '(0.0 1.0 0.0) (/ cuerpo 2.0) longitud)
          (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda)
          (gc:cilindro (list (- a banda) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda)
          (gc:cilindro (list 0.0 (- a banda) 0.0) '(0.0 1.0 0.0) (/ de 2.0) banda))
        (vla-Boolean exterior 0 extra) (gc:borrar extra))
      (if (= pieza "CRUZ")
        (progn
          (setq extra (gc:cilindro (list 0.0 (- a) 0.0) '(0.0 1.0 0.0) (/ de 2.0) banda))
          (vla-Boolean exterior 0 extra) (gc:borrar extra)))
      (setq interior (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) (* 2.0 a))
            rama (gc:cilindro inicio '(0.0 1.0 0.0) (/ di 2.0) longitud))
      (vla-Boolean interior 0 rama) (gc:borrar rama)))
  (vla-Boolean exterior 2 interior) (gc:borrar interior)
  exterior
)

;; Tronco de cono axial liso. Diametros positivos, sin mallas ni roscas.
(defun gc:cono-truncado (x de1 de2 largo / perfil solido)
  (setq perfil (gc:disco (list x 0.0 0.0) '(1.0 0.0 0.0) (/ de1 2.0))
        solido (gc:registrar (vla-AddExtrudedSolid gc:espacio perfil largo
          (atan (/ (- de1 de2) (* 2.0 largo))))))
  (gc:borrar perfil) solido
)

;; Cabeza cuadrada/hexagonal simplificada. Ancho entre caras aproximado.
(defun gc:prisma (lados ancho largo x / puntos i angulo radio curva matriz regiones perfil solido)
  (setq radio (/ ancho (* 2.0 (cos (/ pi lados)))) i 0 puntos nil)
  (repeat lados
    (setq angulo (* 2.0 pi (/ (float i) lados))
          puntos (append puntos (list (* radio (cos angulo)) (* radio (sin angulo)))) i (1+ i)))
  (setq matriz (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length puntos)))))
  (vlax-safearray-fill matriz puntos)
  (setq curva (gc:registrar (vla-AddLightWeightPolyline gc:espacio matriz)))
  (vla-put-Elevation curva 0.0)
  (vla-put-Closed curva :vlax-true)
  (setq matriz (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element matriz 0 curva)
  (setq regiones (vlax-variant-value (vla-AddRegion gc:espacio matriz))
        perfil (gc:registrar (vlax-safearray-get-element regiones 0))
        solido (gc:registrar (vla-AddExtrudedSolid gc:espacio perfil largo 0.0)))
  (vla-Rotate3D solido (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(0.0 1.0 0.0)) (/ pi 2.0))
  (vla-Move solido (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point (list x 0.0 0.0)))
  (gc:borrar curva) (gc:borrar perfil) solido
)

(defun gc:perfil-giro (puntos arcos / matriz i punto curva region solido par)
  (setq matriz (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length puntos))))) i 0)
  (foreach punto puntos
    (vlax-safearray-put-element matriz i (car punto))
    (vlax-safearray-put-element matriz (1+ i) (cadr punto)) (setq i (+ i 2)))
  (setq curva (gc:registrar (vla-AddLightWeightPolyline gc:espacio matriz)))
  (vla-put-Closed curva :vlax-true)
  (foreach par arcos (vla-SetBulge curva (car par) (cadr par)))
  (setq matriz (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element matriz 0 curva)
  (setq region (gc:registrar (vlax-safearray-get-element (vlax-variant-value (vla-AddRegion gc:espacio matriz)) 0))
        solido (gc:registrar (vla-AddRevolvedSolid gc:espacio region (vlax-3d-point '(0.0 0.0 0.0))
          (vlax-3d-point '(1.0 0.0 0.0)) (* 2.0 pi))))
  (gc:borrar region) (gc:borrar curva) solido)
(defun gc:largo-boca (di / medida boca resultado)
  (foreach medida '(0.125 0.25 0.375 0.5 0.75 1.0 1.25 1.5 2.0 2.5 3.0 4.0)
    (setq boca (gc:datos "C90" medida))
    (if (equal di (cadr boca) 0.001)
      (setq resultado (car (gc:zona-niple (gc:datos-niple medida 1000.0))))))
  resultado)

(defun gc:perfil-campana (datos chica interior / largo r0 r1 b0 b1 c0 c1 medio k radio0 radio1 puntos arcos)
  (setq largo (caddr datos) b0 (* 0.20 largo) b1 (* 0.18 largo) medio (/ (+ b0 (- largo b1)) 2.0)
    r0 (/ (cadr datos) 2.0) r1 (/ (cadr chica) 2.0)
    radio0 (/ (car datos) 2.0) radio1 (/ (car chica) 2.0)
    c0 (min (* 0.22 b0) (* 0.25 (- radio0 r0))) c1 (min (* 0.22 b1) (* 0.25 (- radio1 r1))))
  ;; Reservar tramos rectos para ambos extremos del niple antes de la transicion.
  (setq b0 (min (* 0.40 largo) (gc:largo-boca (cadr datos)))
    b1 (min (* 0.40 largo) (gc:largo-boca (cadr chica)))
    medio (/ (+ b0 (- largo b1)) 2.0)
    c0 (min (* 0.22 b0) (* 0.25 (- radio0 r0)))
    c1 (min (* 0.22 b1) (* 0.25 (- radio1 r1))))
  (if interior
    (progn
      (setq k (/ (sin (/ (atan (/ (- r0 r1) (- (- largo b1) b0))) 2.0)) (cos (/ (atan (/ (- r0 r1) (- (- largo b1) b0))) 2.0)))
        puntos (list (list 0.0 r0) (list b0 r0) (list medio (/ (+ r0 r1) 2.0))
          (list (- largo b1) r1) (list largo r1) (list largo 0.0) (list 0.0 0.0))
        arcos (list (list 1 (- k)) (list 2 k))))
    (progn
      (setq r0 (+ r0 (* 0.65 (- radio0 r0))) r1 (+ r1 (* 0.65 (- radio1 r1)))
        k (/ (sin (/ (atan (/ (- r0 r1) (- (- largo b1) b0))) 2.0)) (cos (/ (atan (/ (- r0 r1) (- (- largo b1) b0))) 2.0)))
        puntos (list (list 0.0 (- radio0 c0)) (list c0 radio0) (list (- b0 c0) radio0)
          (list b0 r0) (list medio (/ (+ r0 r1) 2.0)) (list (- largo b1) r1)
          (list (+ (- largo b1) c1) radio1) (list (- largo c1) radio1) (list largo (- radio1 c1))
          (list largo 0.0) (list 0.0 0.0)) arcos (list (list 3 (- k)) (list 4 k)))))
  (list puntos arcos))
(defun gc:extra-solido (pieza datos mayor menor / de di largo fila chica cuerpo banda banda2 eps
                          exterior interior extra axial cabeza ancho pared medio macho zona)
  (setq de (car datos) di (cadr datos) largo (caddr datos) eps 0.05)
  (cond
    ((= pieza "CAMPANA")
      (setq chica (gc:datos "COPLE" menor) fila (gc:perfil-campana datos chica nil)
        exterior (gc:perfil-giro (car fila) (cadr fila)) fila (gc:perfil-campana datos chica T)
        interior (gc:perfil-giro (car fila) (cadr fila)))
      (vla-Boolean exterior 2 interior) (gc:borrar interior))
    ((= pieza "BUSHING")
      (setq fila (gc:reduccion-fila pieza mayor menor)
        zona (gc:zona-niple (gc:datos-niple mayor 1000.0)) macho (cadr zona)
        axial (* 25.4 (cadr fila)) cabeza (* 25.4 (nth 3 fila)) ancho (max (* 25.4 (nth 2 fila)) (* 1.05 de))
        pared (/ (- macho di) 2.0) banda (min (* 0.15 axial) (* 0.35 pared))
        exterior (gc:perfil-giro (list (list 0.0 0.0) (list 0.0 (- (/ macho 2.0) banda))
          (list banda (/ macho 2.0)) (list (- axial banda) (/ macho 2.0))
          (list axial (/ macho 2.0)) (list (+ axial eps) (/ macho 2.0)) (list (+ axial eps) 0.0)) nil)
        extra (gc:prisma 6 ancho (+ cabeza eps) (- axial eps))
        medio (/ ancho (* 2.0 (cos (/ pi 6.0)))) banda (min (* 0.18 cabeza) (* 0.08 ancho))
        interior (gc:perfil-giro (list (list (- axial eps) 0.0) (list (- axial eps) (- medio banda))
          (list (+ axial banda) medio) (list (- largo banda) medio)
          (list largo (- medio banda)) (list largo 0.0)) nil))
      ;; Interseccion con envolvente conica: biseles reales en las esquinas del hexagono.
      (vla-Boolean extra 1 interior) (gc:borrar interior)
      (vla-Boolean exterior 0 extra) (gc:borrar extra)
      ;; Boca hembra E1 constante: niple rebajado + 0.20 mm diametral.
      ;; Evitar el ensanche y el bisel interior que dejaban un anillo visible.
      (setq interior (gc:perfil-giro
        (list (list (- eps) 0.0) (list (- eps) (/ di 2.0))
          (list (+ largo eps) (/ di 2.0)) (list (+ largo eps) 0.0)) nil))
      (vla-Boolean exterior 2 interior) (gc:borrar interior))
    ((= pieza "MACHO")
      (setq fila (gc:extra-fila pieza mayor) axial (* 25.4 (cadr fila))
            cabeza (* 25.4 (nth 2 fila)) ancho (* 25.4 (nth 3 fila))
            exterior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) axial)
            extra (gc:prisma 4 ancho (+ cabeza eps) (- axial eps)))
      (vla-Boolean exterior 0 extra) (gc:borrar extra))
    ((= pieza "CACHUCHA")
      (setq fila (gc:extra-fila pieza mayor) banda (* 25.4 (nth 4 fila))
            cuerpo (+ di (* 0.65 (- de di))) pared (/ (- cuerpo di) 2.0)
            exterior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) (- largo pared))
            extra (gc:cono-truncado (- largo pared) cuerpo (+ di (* 0.3 (- de di))) pared))
      (vla-Boolean exterior 0 extra) (gc:borrar extra)
      (setq extra (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) banda))
      (vla-Boolean exterior 0 extra) (gc:borrar extra)
      (setq interior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) (- largo pared)))
      (vla-Boolean exterior 2 interior) (gc:borrar interior))
    ((= pieza "UNION")
      (setq cuerpo (+ di (* 0.45 (- de di))) banda (* 0.2 largo) medio (* 0.35 largo)
            exterior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ cuerpo 2.0) largo))
      (foreach extra
        (list
          (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ (+ di (* 0.65 (- de di))) 2.0) banda)
          (gc:cilindro (list (- largo banda) 0.0 0.0) '(1.0 0.0 0.0) (/ (+ di (* 0.65 (- de di))) 2.0) banda)
          (gc:prisma 6 de medio (/ (- largo medio) 2.0)))
        (vla-Boolean exterior 0 extra) (gc:borrar extra))
      (setq interior (gc:cilindro '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) largo))
      (vla-Boolean exterior 2 interior) (gc:borrar interior)))
  exterior
)

(defun gc:extra-estructura (pieza datos mayor menor / de di largo chica)
  (setq de (car datos) di (cadr datos) largo (caddr datos))
  (if (= pieza "MACHO")
    (gc:disco '(0.0 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0))
    (gc:anillo '(0.0 0.0 0.0) '(1.0 0.0 0.0) de di))
  (cond
    ((= pieza "CAMPANA")
      (setq chica (gc:datos "COPLE" menor))
      (gc:anillo (list largo 0.0 0.0) '(1.0 0.0 0.0) (car chica) (cadr chica)))
    ((member pieza '("CACHUCHA" "MACHO"))
      (gc:disco (list largo 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0)))
    (T (gc:anillo (list largo 0.0 0.0) '(1.0 0.0 0.0) de di)))
  (gc:linea '(0.0 0.0 0.0) (list largo 0.0 0.0))
  nil
)

;; Se modela en coordenadas locales; al final se aplica el UCS y la insercion.
(defun gc:bocas-hembra (pieza datos mayor menor / a u)
  ;; Punto de cara, eje hacia el interior y medida nominal de cada boca.
  (setq a (caddr datos) u 0.7071067811865476)
  (cond
    ((= pieza "C90") (list (list (list (- a) 0.0 0.0) '(1.0 0.0 0.0) mayor)
      (list (list 0.0 a 0.0) '(0.0 -1.0 0.0) mayor)))
    ((= pieza "C45") (list (list (list (- a) 0.0 0.0) '(1.0 0.0 0.0) mayor)
      (list (list (* a u) (* a u) 0.0) (list (- u) (- u) 0.0) mayor)))
    ((member pieza '("TEE" "CRUZ"))
      (append (list (list (list (- a) 0.0 0.0) '(1.0 0.0 0.0) mayor)
        (list (list a 0.0 0.0) '(-1.0 0.0 0.0) mayor)
        (list (list 0.0 a 0.0) '(0.0 -1.0 0.0) mayor))
        (if (= pieza "CRUZ") (list (list (list 0.0 (- a) 0.0) '(0.0 1.0 0.0) mayor)))))
    ((member pieza '("COPLE" "UNION" "CAMPANA"))
      (list (list '(0.0 0.0 0.0) '(1.0 0.0 0.0) mayor)
        (list (list a 0.0 0.0) '(-1.0 0.0 0.0) (if (= pieza "CAMPANA") menor mayor))))
    ((= pieza "BUSHING") (list (list (list a 0.0 0.0) '(-1.0 0.0 0.0) menor)))
    ((member pieza '("CACHUCHA" "ADAPTADOR")) (list (list '(0.0 0.0 0.0) '(1.0 0.0 0.0) mayor)))))

(defun gc:abrir-bocas (solido pieza datos mayor menor / bocas boca tubo zona profundidad corte punto eje limite)
  ;; Codos y demas piezas excluidas ya abren sus bocas con E1: macho + 0.20 mm.
  ;; No ampliar a DE del tubo ni restar cilindros contenidos en huecos existentes.
  (setq bocas (if (not (member pieza '("C90" "C45" "TEE" "CAMPANA" "BUSHING" "UNION" "CACHUCHA"))) (gc:bocas-hembra pieza datos mayor menor)))
  (foreach boca bocas
    (setq tubo (gc:datos-niple (caddr boca) 1000.0) zona (gc:zona-niple tubo)
      punto (car boca) eje (cadr boca) profundidad (car zona))
    ;; Conservar fondo ciego y transicion interior del adaptador.
    (setq limite (cond ((= pieza "CACHUCHA")
      (- (caddr datos) (* 0.325 (- (car datos) (cadr datos)))))
      ((= pieza "ADAPTADOR") (* 0.42 (caddr datos)))
      ((= pieza "BUSHING") (max 0.1 (- (caddr datos) 0.5)))
      ((member pieza '("COPLE" "UNION" "CAMPANA")) (* 0.45 (caddr datos)))
      (T (caddr datos))))
    (setq profundidad (min profundidad limite)
      punto (mapcar '- punto (mapcar '(lambda (v) (* v 0.05)) eje))
      corte (gc:cilindro punto eje (/ (+ (car tubo) 0.20) 2.0) (+ profundidad 0.05)))
    (vla-Boolean solido 2 corte) (gc:borrar corte))
  solido)

(defun gc:construir (pieza modo datos / de di a perfil exterior rama interior ramaInt solido angulo radio forma inicio longitud)
  (setq de (car datos) di (cadr datos) a (caddr datos))
  (cond
    ((= pieza "ADAPTADOR") (setq solido (gc:adaptador modo datos)))
    ((= pieza "NIPLE") (setq solido (gc:niple modo datos)))
    ((member pieza '("BUSHING" "CAMPANA" "CACHUCHA" "MACHO" "UNION"))
      (if (= modo "Estructura")
        (gc:extra-estructura pieza datos gc:medida gc:menor)
        (setq solido (gc:extra-solido pieza datos gc:medida gc:menor))))
    ((and (= pieza "C90") (/= modo "Estructura"))
      (setq forma (gc:c90-forma gc:medida datos)
            solido (gc:c90-detallado datos forma)))
    ((and (= pieza "C45") (/= modo "Estructura"))
      (setq forma (gc:c90-forma gc:medida datos)
            solido (gc:c45-detallado datos forma)))
    ((and (member pieza '("TEE" "COPLE" "CRUZ")) (/= modo "Estructura"))
      (setq forma (gc:c90-forma gc:medida datos)
            solido (gc:recto-detallado pieza datos forma)))
    ((member pieza '("C90" "C45"))
      ;; A se aproxima como distancia de la interseccion de tangentes a la boca.
      (setq angulo (if (= pieza "C45") (/ pi 4.0) (/ pi 2.0))
            radio (/ (* a (cos (/ angulo 2.0))) (sin (/ angulo 2.0))))
      (setq perfil (gc:anillo (list (- a) 0.0 0.0) '(1.0 0.0 0.0) de di))
      (if (= modo "Estructura")
        (gc:registrar (vla-AddArc gc:espacio
          (vlax-3d-point (list (- a) radio 0.0)) radio (* 1.5 pi) (+ (* 1.5 pi) angulo)))
        (progn
          (setq solido (gc:registrar (vla-AddRevolvedSolid gc:espacio perfil
            (vlax-3d-point (list (- a) radio 0.0)) (vlax-3d-point '(0.0 0.0 1.0)) angulo)))
          (gc:borrar perfil))))
    ((= pieza "COPLE")
      (setq perfil (gc:anillo '(0.0 0.0 0.0) '(1.0 0.0 0.0) de di))
      (if (= modo "Estructura")
        (gc:linea '(0.0 0.0 0.0) (list a 0.0 0.0))
        (progn
          (setq solido (gc:registrar (vla-AddExtrudedSolid gc:espacio perfil a 0.0)))
          (gc:borrar perfil))))
    ((member pieza '("TEE" "CRUZ"))
      (setq inicio (if (= pieza "CRUZ") (list 0.0 (- a) 0.0) '(0.0 0.0 0.0))
            longitud (if (= pieza "CRUZ") (* 2.0 a) a))
      (if (= modo "Estructura")
        (progn
          (gc:anillo (list (- a) 0.0 0.0) '(1.0 0.0 0.0) de di)
          (gc:anillo inicio '(0.0 1.0 0.0) de di)
          (gc:linea (list (- a) 0.0 0.0) (list a 0.0 0.0))
          (gc:linea inicio (list 0.0 a 0.0)))
        (progn
          ;; Primero unir cuerpos; despues restar ambos conductos para abrir la tee.
          (setq exterior (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ de 2.0) (* 2.0 a))
                rama (gc:cilindro inicio '(0.0 1.0 0.0) (/ de 2.0) longitud)
                interior (gc:cilindro (list (- a) 0.0 0.0) '(1.0 0.0 0.0) (/ di 2.0) (* 2.0 a))
                ramaInt (gc:cilindro inicio '(0.0 1.0 0.0) (/ di 2.0) longitud))
          (vla-Boolean exterior 0 rama)
          (vla-Boolean exterior 2 interior)
          (vla-Boolean exterior 2 ramaInt)
          (gc:borrar rama) (gc:borrar interior) (gc:borrar ramaInt)
          (setq solido exterior))))
  )
  (if (and solido (/= modo "Estructura") (boundp 'gc:medida) gc:medida
      (gc:bocas-hembra pieza datos gc:medida gc:menor))
    (gc:abrir-bocas solido pieza datos gc:medida gc:menor))
  (if (and solido (= pieza "UNION")) (vla-put-Color solido 9))
  solido
)

(defun gc:matriz-ucs (punto / x y z origen)
  (setq x (trans '(1.0 0.0 0.0) 1 0 T)
        y (trans '(0.0 1.0 0.0) 1 0 T)
        z (trans '(0.0 0.0 1.0) 1 0 T)
        origen (trans punto 1 0))
  (vlax-tmatrix
    (list (list (car x) (car y) (car z) (car origen))
          (list (cadr x) (cadr y) (cadr z) (cadr origen))
          (list (caddr x) (caddr y) (caddr z) (caddr origen))
          '(0.0 0.0 0.0 1.0)))
)

(defun gc:numero-etiqueta (valor / texto)
  (setq texto (rtos valor 2 8))
  (if (vl-string-search "." texto)
    (setq texto (vl-string-right-trim "." (vl-string-right-trim "0" texto))))
  texto
)
(defun gc:texto-etiqueta (pieza mayor menor / nombre medida)
  (setq medida (strcat (gc:numero-etiqueta mayor)
    (if menor (strcat "x" (gc:numero-etiqueta menor)) "") "pulg")
    nombre (cond
      ((= pieza "C90") "CODO 90") ((= pieza "C45") "CODO 45")
      ((= pieza "TEE") "TEE") ((= pieza "CRUZ") "CRUZ") ((= pieza "COPLE") "COPLE")
      ((= pieza "BUSHING") "REDUCCION BUSHING") ((= pieza "CAMPANA") "REDUCCION CAMPANA")
      ((= pieza "CACHUCHA") "TAPON CACHUCHA") ((= pieza "MACHO") "TAPON MACHO")
      ((= pieza "UNION") "TUERCA UNION")))
  (if nombre
    (strcat nombre (if (= pieza "UNION") " " " TUBO GALVANIZADO CEDULA 40 ") medida))
)

(defun gc:disponibles (pieza mayor / tamanos valor candidato resultado valido)
  (setq tamanos '(0.125 0.25 0.375 0.5 0.75 1.0 1.25 1.5 2.0 2.5 3.0 4.0))
  (foreach valor tamanos
    (setq valido nil)
    (if (member pieza '("BUSHING" "CAMPANA"))
      (if mayor
        (setq valido (gc:extra-datos pieza mayor valor))
        (foreach candidato tamanos
          (if (gc:extra-datos pieza valor candidato) (setq valido T))))
      (setq valido (if (= pieza "NIPLE") (gc:datos-niple valor 100.0) (if (member pieza '("C90" "C45" "TEE" "COPLE" "CRUZ"))
        (gc:datos pieza valor) (gc:datos-adicional pieza valor nil)))))
    (if valido (setq resultado (append resultado (list valor)))))
  resultado
)
(defun gc:lista-medidas (valores / texto valor)
  (setq texto "")
  (foreach valor valores
    (setq texto (strcat texto (if (= texto "") "" "/") (rtos valor 2 3))))
  texto
)

(defun c:GALVANIZADOSCIFUNSA
  (/ *error* doc undo pieza modo medida datos punto matriz objeto solido resultado bloquePieza
     seleccion gc:espacio gc:creadas gc:medida gc:menor menor opciones opcionesMenor etiqueta gc:campos longitud)
  (defun *error* (mensaje)
    (foreach objeto gc:creadas (vl-catch-all-apply 'gc:borrar (list objeto)))
    (if undo (progn (vla-EndUndoMark doc) (setq undo nil)))
    (princ "\nOperacion cancelada o fallida; se retiraron las entidades nuevas.")
    (if mensaje (princ (strcat "\nDetalle: " mensaje)))
    (princ))
  (vl-load-com)
  (if (not (and (member "PD:GUARDAR" (atoms-family 1))
                (member "PD:ETIQUETA-BREVE" (atoms-family 1))
                (member "PD:ALTURA-EXTRUSION" (atoms-family 1))
                (member "PD:DESCRIPCION" (atoms-family 1))
                (member "PD:PEDIR-MEDIDA" (atoms-family 1))
                (member "PD:ENVOLVER-BLOQUE" (atoms-family 1))
                (equal pd:version "1")))
    (princ "\nFalta cargar PiezaDatos.lsp actualizado. Use APPLOAD o CARGARRUTINAS antes de continuar.")
  (if (= (getvar "BLOCKEDITOR") 1)
    (princ "\nCierre el editor de bloques antes de crear la pieza.")
    (progn
      (if (and (boundp 'cp:pedido) cp:pedido (= (pd:valor cp:pedido "familia") "GALV"))
        (setq pieza (pd:valor cp:pedido "pieza") medida (pd:valor cp:pedido "medida")
              menor (pd:valor cp:pedido "menor") modo (pd:valor cp:pedido "modo")
              datos (if (= pieza "NIPLE") (gc:datos-niple medida (pd:valor cp:pedido "longitud"))
                      (if (member pieza '("C90" "C45" "TEE" "COPLE" "CRUZ"))
                      (gc:datos pieza medida) (gc:datos-adicional pieza medida menor))))
        (progn
      (initget 1 "C90 C45 TEE CRUZ COPLE BUSHING CAMPANA CACHUCHA MACHO UNION NIPLE ADAPTADOR")
      (setq pieza (getkword "\nPieza galvanizada [C90/C45/TEE/CRUZ/COPLE/BUSHING/CAMPANA/CACHUCHA/MACHO/UNION/NIPLE/ADAPTADOR]: "))
      (setq opciones (gc:disponibles pieza nil))
      (while (null datos)
        (setq medida (pd:pedir-medida (if (member pieza '("BUSHING" "CAMPANA"))
          "Medida MAYOR" "Medida nominal") opciones))
        (if (member medida opciones)
          (if (member pieza '("BUSHING" "CAMPANA"))
            (progn
              (setq opcionesMenor (gc:disponibles pieza medida))
              (setq menor (pd:pedir-medida "Medida MENOR" opcionesMenor))
              (setq datos (gc:datos-adicional pieza medida menor)))
            (setq datos (if (= pieza "NIPLE") (gc:datos-niple medida 100.0)
              (if (member pieza '("C90" "C45" "TEE" "COPLE" "CRUZ"))
              (gc:datos pieza medida) (gc:datos-adicional pieza medida nil))))))
        (if (null datos) (princ "\nMedida no disponible para esa pieza.")))
      (if (= pieza "NIPLE")
        (progn
          (initget "Catalogo Personalizada")
          (if (= (getkword "\nLongitud [Catalogo/Personalizada] <Catalogo>: ") "Personalizada")
            (progn (initget 7) (setq longitud (getdist "\nLongitud total del niple (mm): ")))
            (setq longitud (* 25.4 (pd:pedir-medida "Longitud comercial del niple"
              (mapcar '(lambda (mm) (/ mm 25.4)) (gc:longitudes-niple medida))))))
          (setq datos (gc:datos-niple medida longitud))))
      (initget "Solido Estructura")
      (setq modo (getkword "\nCrear [Solido/Estructura] <Solido>: "))
        ))
      (if (null datos)
        (progn (princ "\nCombinacion no disponible; no se creo geometria.") (exit)))
      (setq punto (getpoint (if (member pieza '("C90" "C45"))
        "\nInterseccion de ejes de las bocas del codo: "
        (if (member pieza '("TEE" "CRUZ")) "\nCentro de la pieza: " "\nCentro de la boca inicial (lado mayor en reducciones): "))))
      (if punto
        (progn
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
                gc:espacio (if (= (getvar "CVPORT") 1) (vla-get-PaperSpace doc) (vla-get-ModelSpace doc))
                matriz (gc:matriz-ucs punto))
          (vla-StartUndoMark doc) (setq undo T)
          (setq gc:medida medida gc:menor menor solido (gc:construir pieza modo datos) seleccion (ssadd))
          (foreach objeto gc:creadas
            (if (not (vlax-erased-p objeto))
              (progn
                (vla-TransformBy objeto matriz)
                (ssadd (vlax-vla-object->ename objeto) seleccion))))
          (if (/= modo "Estructura")
            (if (and solido (> (vla-get-Volume solido) 0.0))
              (princ "\nSolido simplificado creado.")
              (progn (*error* "No se obtuvo un solido con volumen positivo.") (exit))))
          (if (/= modo "Estructura")
            (progn
              (setq gc:campos (gc:campos-modelo pieza medida menor datos)
                    etiqueta (pd:etiqueta-breve gc:campos))
              (if (not (gc:guardar-etiqueta solido etiqueta "GCIF"))
                (progn (*error* "No se pudo guardar y verificar la etiqueta.") (exit)))))
          (if etiqueta
            (progn
              (setq bloquePieza (pd:envolver-bloque doc gc:espacio solido etiqueta pieza (trans punto 1 0)))
              (if (null bloquePieza)
                (progn (*error* "No se pudo preparar el bloque de la pieza.") (exit)))
              (setq gc:creadas (cons bloquePieza gc:creadas)
                    seleccion (ssadd (vlax-vla-object->ename bloquePieza)))))
          (vla-EndUndoMark doc) (setq undo nil)
          (sssetfirst nil seleccion)
          (princ (strcat (if (= pieza "ADAPTADOR") "\nAdaptador galvanizado: dimensiones aproximadas | " (if (= pieza "NIPLE") "\nTubo galvanizado C-40 | " "\nCIFUNSA 150 lb como referencia | ")) pieza " " (rtos medida 2 3)
            (if menor (strcat "x" (rtos menor 2 3)) "") " pulg."
            "\nDE de referencia: " (rtos (car datos) 2 2) " mm; DI aproximado de boca: " (rtos (cadr datos) 2 2) " mm."
            (if (/= modo "Estructura")
              "\nPieza con cuerpo y detalles exteriores aproximados, sin roscas."
              "\nModelo simplificado sin roscas; estructura de seccion uniforme.")
            (if etiqueta (strcat "\nEtiqueta guardada: " etiqueta) "\nEstructura sin etiqueta.")
            "\nUn DESHACER revierte la creacion y su etiqueta."))))))
  ) ; comprobacion de la biblioteca compartida

  (princ)
)
(defun c:GCIF () (c:GALVANIZADOSCIFUNSA))
(princ "\nCargado: GALVANIZADOSCIFUNSA / GCIF (solido o estructura, en mm).")
(princ)

;;; Guardado verificado en el solido nuevo; conserva las otras aplicaciones XDATA.
(defun gc:guardar-etiqueta (solido texto marcador)
  (pd:guardar solido texto marcador gc:campos)
)
(defun gc:campos-modelo-base (pieza mayor menor datos / campos)
  (setq campos (pd:campos-gcif pieza mayor menor datos))
  (if (= pieza "ADAPTADOR") (gc:campos-adaptador mayor datos)
  (if (= pieza "NIPLE") (gc:campos-niple mayor datos)
  (progn
  (if (member pieza '("BUSHING" "CAMPANA")) (setq campos (append campos '(("REVISION_GEOMETRIA" "REDUCCIONES-2")))))
  (pd:con-descripcion campos (gc:texto-etiqueta pieza mayor menor))))))
(defun gc:datos-adicional (pieza mayor menor)
  (if (= pieza "ADAPTADOR") (gc:datos-adaptador mayor) (gc:extra-datos pieza mayor menor)))
(defun gc:campos-modelo (pieza mayor menor datos / campos)
  (setq campos (gc:campos-modelo-base pieza mayor menor datos))
  (if (gc:bocas-hembra pieza datos mayor menor)
    (if (assoc "REVISION_GEOMETRIA" campos)
      (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-BOCAS-1") (assoc "REVISION_GEOMETRIA" campos) campos))
      (setq campos (append campos '(("REVISION_GEOMETRIA" "GCIF-BOCAS-1"))))))
  (if (member pieza '("C90" "C45"))
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-CODO-2") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (= pieza "BUSHING")
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-BUSHING-3") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (= pieza "TEE")
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-TEE-2") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (= pieza "CAMPANA")
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-CAMPANA-2") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (= pieza "CACHUCHA")
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-CACHUCHA-2") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (= pieza "UNION")
    (setq campos (subst '("REVISION_GEOMETRIA" "GCIF-UNION-3") (assoc "REVISION_GEOMETRIA" campos) campos)))
  (if (and (boundp 'cp:pedido) cp:pedido (= (pd:valor cp:pedido "acabado") "NEGRO") (member pieza '("C90" "C45")))
    (progn
      (setq campos (subst '("ACABADO" "NEGRO") (assoc "ACABADO" campos) campos)
        campos (subst '("REFERENCIA_COMERCIAL" "") (assoc "REFERENCIA_COMERCIAL" campos) campos))
      (pd:con-descripcion campos (strcat "CODO " (if (= pieza "C90") "90" "45") " NEGRO ROSCADO " (pd:numero mayor) "pulg")))
    campos))
(setq gc:interfaz-version "15")
(if (member "AP:AL-CARGAR" (atoms-family 1)) (ap:al-cargar))
