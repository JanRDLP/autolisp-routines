(defun c:EtiquetarPieza ( / sel ename tipoPieza tipoCodo tipoReduccion tipoTubo medida tipoBrida longTramo descXData  
                             p1 p2 etiquetaFinal)

  ;; ==============================
  ;; Función interna: verificar si objeto tiene etiqueta
  ;; ==============================
  (defun objeto-tiene-etiqueta (ename / xdata app)
    (setq xdata (assoc -3 (entget ename)))
    (if xdata
      (progn
        (setq app (assoc 1001 (cdr xdata)))
        (if app T nil)
      )
      nil
    )
  )

  ;; ==============================
  ;; Selección de objeto
  ;; ==============================
  (setq sel (entsel "\nSeleccione el objeto (3DSOLID o bloque): "))
  (if (not sel)
    (progn
      (princ "\nNo se seleccionó ningún objeto.")
      (princ)
      (exit)
    )
  )
  (setq ename (car sel))
  
  ;; === Mantener seleccionado el objeto desde el inicio ===
  (sssetfirst nil (ssadd ename))

  ;; ==============================
  ;; Verificar si el objeto ya tiene etiqueta
  ;; ==============================
  (if (objeto-tiene-etiqueta ename)
    (progn
      (alert "Este objeto ya tiene una etiqueta. No se puede etiquetar nuevamente.")
      (princ)
      (exit)
    )
  )

    ;; ==============================
  ;; 1. TIPO DE PIEZA
  ;; ==============================
  (initget "C90 C45 TRAMO RED TEE CRUZ BRIDA COPLE TAPON COCHI FY VCHECK VBOLA VGLOBO")
  (setq tipoPieza (getkword "\nElija TIPO DE PIEZA [C90/C45/TRAMO/RED/TEE/CRUZ/BRIDA/COPLE/TAPON/COCHI/FY/VCHECK/VBOLA/VGLOBO]: "))
  (setq tipoPieza
        (cond
          ((= tipoPieza "C90") "CODO90")
          ((= tipoPieza "C45") "CODO45")
          ((= tipoPieza "RED") "REDUCCION")
          (T tipoPieza)
        )
  )

  ;; ==============================
  ;; 2. TIPO DE CODO
  ;; ==============================
  (if (equal tipoPieza "CODO90")
    (progn
      (initget "LARGO CORTO")
      (setq tipoCodo (getkword "\nElija TIPO DE CODO [LARGO/CORTO]: "))
    )
  )

  ;; ==============================
  ;; 3. TIPO DE REDUCCION
  ;; ==============================
  (if (equal tipoPieza "REDUCCION")
    (progn
      (initget "CENTRICA EXCENTRICA")
      (setq tipoReduccion (getkword "\nElija TIPO DE REDUCCION [CENTRICA/EXCENTRICA]: "))
      (setq tipoReduccion
            (cond
              ((= tipoReduccion "CENTRICA") "REDUCCION CENTRICA")
              ((= tipoReduccion "EXCENTRICA") "REDUCCION EXCENTRICA")
              (T "REDUCCION")
            )
      )
    )
  )

    ;; ==============================
  ;; 4. TIPO DE TUBERIA
  ;; ==============================
  (cond
    ((member tipoPieza '("CODO90" "CODO45" "TEE" "CRUZ" "TAPON" "COCHI"))
     (initget "NEGRO GALV")
     (setq tipoTubo (getkword "\nElija TIPO DE TUBERIA [NEGRO/GALV]: ")))
    ((= tipoPieza "REDUCCION")
     (initget "NEGRO GALV")
     (setq tipoTubo (getkword "\nElija TIPO DE TUBERIA [NEGRO/GALV]: ")))
    ((= tipoPieza "COPLE")  
     (setq tipoTubo "GALV"))
    ((= tipoPieza "TRAMO")
     (initget "CON SIN GALV")
     (setq tipoTubo (getkword "\nElija TIPO DE TUBERIA [CON/SIN/GALV]: ")))
  )
  (setq tipoTubo
        (cond
          ((= tipoTubo "NEGRO") "TUBO NEGRO SOLDABLE")
          ((= tipoTubo "GALV") "TUBO GALVANIZADO")
          ((= tipoTubo "CON") "TUBO NEGRO SOLDABLE CON COSTURA")
          ((= tipoTubo "SIN") "TUBO NEGRO SOLDABLE SIN COSTURA")
          (T "")
        )
  )

  ;; ==============================
  ;; 5. CEDULA
  ;; ==============================
  (cond
    ((member tipoTubo '("TUBO NEGRO SOLDABLE" "TUBO NEGRO SOLDABLE CON COSTURA"))
     (initget "10 20 40 80")
     (setq cedula (getkword "\nElija CEDULA [10/20/40/80]: "))
    )
    ((= tipoTubo "TUBO NEGRO SOLDABLE SIN COSTURA")
     (initget "10 20 40 80 160")
     (setq cedula (getkword "\nElija CEDULA [10/20/40/80/160]: "))
    )
    ((= tipoTubo "TUBO GALVANIZADO")
     (initget "40 80")
     (setq cedula (getkword "\nElija CEDULA [40/80]: "))
    )
  )
  (setq cedula
        (cond
          ((= cedula "10") "CEDULA 10")
          ((= cedula "20") "CEDULA 20")
          ((= cedula "40") "CEDULA 40")
          ((= cedula "80") "CEDULA 80")
          ((= cedula "160") "CEDULA 160")
          (T "")
        )
  )

    ;; ==============================
  ;; 6. MEDIDA
  ;; ==============================
  (cond
    ((member tipoPieza '("CODO90" "CODO45" "TRAMO" "TEE" "CRUZ" "COPLE" "TAPON" "COCHI" "FY" "VCHECK" "VBOLA" "VGLOBO"))
     (initget 0)
     (setq medida (getstring "\nIngrese MEDIDA [0.25pulg/0.5pulg/0.75pulg/1pulg/1.5pulg/2pulg/2.5pulg/3pulg/3.5pulg/4pulg/6pulg/8pulg/10pulg] o manual: "))
     (if medida
       (progn
         (setq medida (vl-string-trim " " medida))
         (if (not (wcmatch (strcase medida) "*PULG"))
           (setq medida (strcat medida "pulg"))
         )
       )
     )
    )
    ((= tipoPieza "REDUCCION")
     (initget 0)
     (setq medida (getstring "\nIngrese MEDIDA REDUCCION: "))
    )
    ((= tipoPieza "BRIDA")
      (initget 0)
      (setq medida (getstring T "\nIngrese MEDIDA BRIDA [3pulg/6pulg/10pulg/DN15/DN25/DN40/DN50/DN65/DN80] o manual: "))
      (setq medida (vl-string-trim " " medida))
      (if (and medida (not (wcmatch (strcase medida) "*PULG*"))
                  (not (wcmatch (strcase medida) "DN*")))
        (setq medida (strcat medida "pulg"))
      )
    )
  )

  ;; ==============================
  ;; 7. TIPO DE BRIDA
  ;; ==============================
  (if (= tipoPieza "BRIDA")
    (progn
      (setq medUpper (strcase (if medida medida "")))
      (cond
        ((wcmatch medUpper "*PULG*")
         (initget 0)
         (setq tipoBrida (getstring T "\nIngrese TIPO DE BRIDA [VAPOR/CIEGA]: ")))
        ((wcmatch medUpper "DN*")
         (progn
           (setq dnVal (atoi (substr medUpper 3)))
           (cond
             ((<= dnVal 80) (setq tipoBrida "4BARRENOS"))
             ((and (> dnVal 80) (<= dnVal 200)) (setq tipoBrida "8BARRENOS"))
             ((and (> dnVal 200) (<= dnVal 350)) (setq tipoBrida "12BARRENOS"))
             ((and (> dnVal 350) (<= dnVal 450)) (setq tipoBrida "16BARRENOS"))
             ((> dnVal 450) (setq tipoBrida "20BARRENOS"))
           )
         )
        )
        (T (setq tipoBrida ""))
      )
    )
  )

  ;; ==============================
  ;; 8. LONGITUD DE TRAMO
  ;; ==============================
  (if (= tipoPieza "TRAMO")
    (progn
      (princ "\nSeleccione punto inicial del tramo: ")
      (setq p1 (getpoint))
      (princ "\nSeleccione punto final del tramo: ")
      (setq p2 (getpoint))
      (if (and p1 p2)
          (setq longTramo (distance p1 p2))
          (setq longTramo (getreal "\nIngrese LONGITUD DE TRAMO en mm: "))
      )
    )
  )

  ;; ==============================
  ;; 9. Construcción de descripción
  ;; ==============================
  (setq descXData
        (vl-remove nil
          (list
            (cond
              ((= tipoPieza "CODO90") "CODO 90")
              ((= tipoPieza "CODO45") "CODO 45")
              ((= tipoPieza "TRAMO") "TRAMO")
              ((= tipoPieza "REDUCCION") tipoReduccion) ; aquí usamos la nueva variable
              ((= tipoPieza "TEE") "TEE")
              ((= tipoPieza "CRUZ") "CRUZ")
              ((= tipoPieza "BRIDA") "BRIDA")
              ((= tipoPieza "COPLE") "COPLE")
              ((= tipoPieza "TAPON") "TAPON CACHUCHA")
              ((= tipoPieza "COCHI") "COLA DE COCHI")
              ((= tipoPieza "FY") "FILTRO Y")
              ((= tipoPieza "VCHECK") "VALVULA CHECK")
              ((= tipoPieza "VBOLA") "VALVULA DE BOLA")
              ((= tipoPieza "VGLOBO") "VALVULA DE GLOBO")
              (T "")
            )
            tipoCodo  
            tipoTubo  
            cedula
            medida  
            tipoBrida
            (if longTramo (strcat (rtos longTramo 2 2) "mm") "")
          )
        )
  )
  (setq etiquetaFinal (apply 'strcat (mapcar '(lambda (s) (strcat s " ")) descXData)))

  ;; ==============================
  ;; 10. Guardar en XDATA
  ;; ==============================
  (regapp "ETIQUETA_PIEZA")
  (entmod
    (append
      (entget ename)
      (list
        (list -3
              (list "ETIQUETA_PIEZA"
                    (cons 1000 etiquetaFinal)
              )
        )
      )
    )
  )

  (alert (strcat "Etiqueta aplicada: " etiquetaFinal))
  (princ (strcat "\nEtiqueta aplicada: " etiquetaFinal))
  (princ)
)

;; ==============================
;; Mostrar etiqueta de un objeto
;; ==============================
(defun c:MostrarEtiqueta ( / sel ename edata xdata etiquetaFinal )
  (setq sel (entsel "\nSeleccione el objeto para mostrar etiqueta: "))
  (if (not sel)
    (progn
      (princ "\nNo se seleccionó ningún objeto.")
      (exit)
    )
  )
  (setq ename (car sel))
  (setq edata (entget ename '("*")))
  (setq xdata (assoc -3 edata)) ; obtenemos (-3 ...)

  (if xdata
    (progn
      (foreach app (cdr xdata)
        (if (= (car app) "ETIQUETA_PIEZA")
          (setq etiquetaFinal (cdr (assoc 1000 (cdr app))))
        )
      )
      (if etiquetaFinal
        (progn
          (alert (strcat "Etiqueta del objeto:\n" etiquetaFinal))
          (princ (strcat "\nEtiqueta del objeto: " etiquetaFinal))
        )
        (alert "Este objeto no tiene etiqueta guardada.")
      )
    )
    (alert "Este objeto no tiene etiqueta guardada.")
  )
  (princ)
)