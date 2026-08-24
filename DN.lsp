(defun c:DN ( / tipo dn sch color tipoCobre od id pt entry tabla espesor id40 id80 )

  ;; Selección de tipo de tubería
  (initget "Galvanizado Negro PVC CPVC Cobre Conduit")
  (setq tipo (getkword "\nSeleccione el tipo de tubería [Galvanizado/Negro/PVC/CPVC/Cobre/Conduit]: "))

  ;; Tablas completas
  
  ;;TABLA TUBO GALVANIZADO

  (setq tabla-glav40
    '(
      (0.125 10.29 6.83)
      (0.25 13.72 10.41)
      (0.375 17.15 12.73)
      (0.5 21.34 15.80)
      (0.625 21.34 15.80)
      (0.75 26.67 20.93)
      (1 33.40 26.64)
      (1.25 42.16 35.05)
      (1.5 48.26 40.94)
      (2 60.33 52.50)
      (2.5 73.03 62.68)
      (3 88.90 77.93)
      (3.5 101.60 90.12)
      (4 114.30 102.26)
      (5 141.30 128.18)
      (6 168.28 154.08)
      (8 219.08 202.74)
      (10 273.05 254.51)
      (12 323.85 304.83)
      (14 355.60 336.55)
      (16 406.40 387.36)
    )
  )

  (setq tabla-glav80
    '(
      (0.125 10.29 5.47)
      (0.25 13.72 9.12)
      (0.375 17.15 10.55)
      (0.5 21.34 13.84)
      (0.625 21.34 13.84)
      (0.75 26.67 18.80)
      (1 33.40 24.30)
      (1.25 42.16 32.46)
      (1.5 48.26 37.93)
      (2 60.33 49.25)
      (2.5 73.03 58.61)
      (3 88.90 73.66)
      (3.5 101.60 85.38)
      (4 114.30 97.18)
      (5 141.30 122.24)
      (6 168.28 146.28)
      (8 219.08 193.68)
      (10 273.05 242.87)
      (12 323.85 288.93)
      (14 355.60 317.50)
      (16 406.40 365.12)
    )
  )

  ;;TABLA TUBO PVC

  (setq tabla-pvc40
    '(
      (0.5 21.34 15.80)
      (0.75 26.67 20.93)
      (1 33.40 26.64)
      (1.25 42.16 35.05)
      (1.5 48.26 40.89)
      (2 60.33 52.53)
      (2.5 73.03 62.72)
      (3 88.90 77.96)
      (4 114.30 102.21)
      (5 141.30 128.19)
      (6 168.28 154.05)
      (8 219.08 202.74)
      (10 273.05 254.51)
      (12 323.85 303.23)
      (14 355.60 333.34)
      (16 406.40 381.00)
    )
  )

  (setq tabla-pvc80
    '(
      (0.5 21.34 13.87)
      (0.75 26.67 18.85)
      (1 33.40 24.31)
      (1.25 42.16 32.46)
      (1.5 48.26 38.10)
      (2 60.33 49.26)
      (2.5 73.03 59.00)
      (3 88.90 73.66)
      (4 114.30 97.18)
      (5 141.30 122.25)
      (6 168.28 146.38)
      (8 219.08 193.68)
      (10 273.05 242.92)
      (12 323.85 289.00)
      (14 355.60 317.50)
      (16 406.40 363.58)
    )
  )

  ;;TABLA TUBO NEGRO

  (setq tabla-negr10
    '(
      (1 33.40 30.20)
      (1.25 42.20 38.10)
      (1.5 48.30 44.20)
      (2 60.30 56.10)
      (2.5 73.00 68.80)
      (3 88.90 84.80)
      (4 114.30 109.50)
      (5 141.30 136.50)
      (6 168.30 163.50)
    )
  )

  (setq tabla-negr20
    '(
      (1 33.40 29.50)
      (1.25 42.20 38.10)
      (1.5 48.30 43.90)
      (2 60.30 55.90)
      (2.5 73.00 68.60)
      (3 88.90 84.60)
      (4 114.30 109.30)
      (5 141.30 136.30)
      (6 168.30 163.30)
    )
  )

  (setq tabla-negr30
    '(
      (1 33.40 28.90)
      (1.25 42.20 37.50)
      (1.5 48.30 43.30)
      (2 60.30 55.30)
      (2.5 73.00 68.00)
      (3 88.90 84.00)
      (4 114.30 108.70)
      (5 141.30 135.70)
      (6 168.30 162.70)
    )
  )

  (setq tabla-negr40
    '(
      (0.5 21.34 15.80)
      (0.75 26.67 20.93)
      (1 33.40 26.64)
      (1.25 42.16 35.05)
      (1.5 48.26 40.94)
      (2 60.33 52.50)
      (2.5 73.03 62.68)
      (3 88.90 77.93)
      (3.5 101.60 90.12)
      (4 114.30 102.26)
      (5 141.30 128.18)
      (6 168.28 154.08)
      (8 219.08 202.74)
      (10 273.05 254.51)
      (12 323.85 304.83)
      (14 355.60 336.55)
      (16 406.40 387.36)
    )
  )

  (setq tabla-negr80
    '(
      (0.5 21.34 13.84)
      (0.75 26.67 18.80)
      (1 33.40 24.30)
      (1.25 42.16 32.46)
      (1.5 48.26 37.93)
      (2 60.33 49.25)
      (2.5 73.03 58.61)
      (3 88.90 73.66)
      (3.5 101.60 85.38)
      (4 114.30 97.18)
      (5 141.30 122.24)
      (6 168.28 146.28)
      (8 219.08 193.68)
      (10 273.05 242.87)
      (12 323.85 288.93)
      (14 355.60 317.50)
      (16 406.40 365.12)
    )
  )

  (setq tabla-negr160
    '(
      (1 33.40 21.20)
      (1.25 42.20 28.60)
      (1.5 48.30 33.70)
      (2 60.30 43.90)
      (2.5 73.00 55.60)
      (3 88.90 69.00)
      (4 114.30 91.90)
      (5 141.30 116.80)
      (6 168.30 141.70)
    )
  )

  ;;TABLA TUBO COBRE

  (setq tabla-cobreM
    '(
      (0.25 9.525 0.635)
      (0.375 12.700 0.635)
      (0.5 15.875 0.711)
      (0.75 22.225 0.812)
      (1 28.575 0.889)
      (1.25 34.925 1.067)
      (1.5 41.275 1.245)
      (2 53.975 1.473)
      (2.5 66.675 1.651)
      (3 79.375 1.889)
      (4 104.775 2.413)
    )
  )

  (setq tabla-cobreL
    '(
      (0.25 9.525 0.889)
      (0.375 12.700 0.889)
      (0.5 15.875 1.016)
      (0.75 22.225 1.143)
      (1 28.575 1.270)
      (1.25 34.925 1.397)
      (1.5 41.275 1.524)
      (2 53.975 1.778)
      (2.5 66.675 2.032)
      (3 79.375 2.286)
      (4 104.775 2.794)
    )
  )

  (setq tabla-cobreK
    '(
      (0.5 15.875 1.651)
      (0.75 22.225 1.651)
      (1 28.575 1.651)
      (1.25 34.925 1.829)
      (1.5 41.275 2.108)
      (2 53.975 2.413)
    )
  )

  ;;TABLA TUBO CONDUIT 

  (setq tabla-condR0
    '(
      (0.5 17.9 15.9)
      (0.75 23.4 21.4)
      (1 29.5 27.1)
      (1.25 36.1 33.7)
      (1.5 44.2 41.2)
      (2 56.1 53.1)
      (2.5 73.0 70.0)
      (3 88.9 85.9)
      (4 114.3 111.3)
      (6 168.3 165.1)
    )
  )

  (setq tabla-condR1
    '(
      (0.5 18.2 15.0)
      (0.75 23.6 20.6)
      (1 30.3 27.3)
      (1.25 37.0 34.0)
      (1.5 44.5 40.7)
      (2 56.6 52.4)
      (2.5 73.0 67.6)
      (3 88.7 82.9)
      (4 114.3 107.5)
      (6 168.3 160.1)
    )
  )

  (setq tabla-condC40
    '(
      (0.5 21.4 15.8)
      (0.75 26.8 22.6)
      (1 33.5 27.7)
      (1.25 42.2 36.4)
      (1.5 48.3 42.3)
      (2 60.3 52.5)
      (2.5 76.2 65.8)
      (3 88.9 77.9)
      (4 114.3 102.3)
      (6 168.3 154.1)
    )
  )
  
;; Lógica por tipo
  (cond

;; Galvanizado o Negro
((member tipo '("Galvanizado" "Negro"))
  (initget "10 20 30 40 80 160")
  (setq sch (getkword "\nSeleccione el tipo de cédula [10/20/30/40/80/160]: "))

  ;; Asignar tabla según tipo y cédula
  (setq tabla
    (cond
      ;; Galvanizado
      ((and (= tipo "Galvanizado") (= sch "10")) tabla-negr10)
      ((and (= tipo "Galvanizado") (= sch "20")) tabla-negr20)
      ((and (= tipo "Galvanizado") (= sch "30")) tabla-negr30)
      ((and (= tipo "Galvanizado") (= sch "40")) tabla-glav40)
      ((and (= tipo "Galvanizado") (= sch "80")) tabla-glav80)
      ((and (= tipo "Galvanizado") (= sch "160")) tabla-negr160)

      ;; Negro
      ((and (= tipo "Negro") (= sch "10")) tabla-negr10)
      ((and (= tipo "Negro") (= sch "20")) tabla-negr20)
      ((and (= tipo "Negro") (= sch "30")) tabla-negr30)
      ((and (= tipo "Negro") (= sch "40")) tabla-negr40)
      ((and (= tipo "Negro") (= sch "80")) tabla-negr80)
      ((and (= tipo "Negro") (= sch "160")) tabla-negr160)
    )
  )

;; Generar lista de DN disponibles (solo números válidos)
(setq lista-dn (vl-remove-if-not 'numberp (mapcar 'car tabla)))
(setq opciones-dn (mapcar '(lambda (x) (rtos x 2 2)) lista-dn))

;; Construir prompt con todas las opciones visibles
(setq prompt (strcat "\nIngrese DN ["
                     (apply 'strcat (mapcar '(lambda (x) (strcat x "/")) opciones-dn))
                     "] o manual: "))

;; Entrada con getreal y validación
(setq valido nil)
(while (not valido)
  (setq dn (getreal prompt))
  (setq entry (assoc dn tabla))
  (if entry
    (setq valido T)
    (princ "\nValor no válido, intente nuevamente.")
  )
)

  ;; Validar y dibujar
  (if entry
    (progn
      (setq od (cadr entry) id (caddr entry))
      (if (> id 0)
        (progn
          (setq pt (getpoint "\nSeleccione punto de inserción: "))
          (command "_.circle" pt (/ od 2))
          (command "_.circle" pt (/ id 2))
          (princ (strcat
            "\nTipo: " tipo ", Cédula: " sch
            "\nDN: " (rtos dn 2 2) " pulgadas"
            "\nOD: " (rtos od 2 2) " mm"
            "\nID: " (rtos id 2 2) " mm"
          ))
        )
        (princ "\n⚠️ Error: el espesor excede el OD. No se puede dibujar el círculo interior.")
      )
    )
    (princ (strcat "\nEl DN " (rtos dn 2 2) " no está en la tabla para " tipo " Cédula " sch "."))
  )
)

;; PVC
((= tipo "PVC")
  (initget "40 80")
  (setq sch (getkword "\nSeleccione el tipo de cédula [40/80]: "))

  ;; Asignar tabla con cond
  (setq tabla
    (cond
      ((= sch "40") tabla-pvc40)
      ((= sch "80") tabla-pvc80)
      (T nil)
    )
  )

  ;; Validar tabla
  (if tabla
    (progn
      ;; Generar lista de DN disponibles
      (setq lista-dn (mapcar 'car tabla))
      (setq opciones-dn
        (apply 'strcat
          (mapcar '(lambda (x) (strcat (rtos x 2 2) " ")) lista-dn)
        )
      )

      ;; Selección de DN
      (initget opciones-dn)
      (setq dn (getkword (strcat "\nSeleccione DN [" opciones-dn "]: ")))
      (setq dn (atof dn))

      ;; Buscar entrada
      (setq entry (assoc dn tabla))

      ;; Validar y dibujar
      (if entry
        (progn
          (setq od (cadr entry) id (caddr entry))
          (if (> id 0)
            (progn
              (setq pt (getpoint "\nSeleccione punto de inserción: "))
              (command "_.circle" pt (/ od 2))
              (command "_.circle" pt (/ id 2))
              (princ (strcat
                "\nTipo: PVC " sch
                "\nDN: " (rtos dn 2 2) " pulgadas"
                "\nOD: " (rtos od 2 2) " mm"
                "\nID: " (rtos id 2 2) " mm"
              ))
            )
            (princ "\n⚠️ Error: el espesor excede el OD. No se puede dibujar el círculo interior.")
          )
        )
        (princ (strcat "\nEl DN " (rtos dn 2 2) " no está en la tabla para PVC Cédula " sch "."))
      )
    )
    (princ "\n⚠️ Cédula no válida. Solo se permite 40 u 80.")
  )
)

;; CPVC
((= tipo "CPVC")
  (initget "40 80")
  (setq sch (getkword "\nSeleccione el tipo de cédula [40/80]: "))

  ;; Asignar tabla con cond
  (setq tabla
    (cond
      ((= sch "40") tabla-cpvc40)
      ((= sch "80") tabla-cpvc80)
      (T nil)
    )
  )

  ;; Validar tabla
  (if tabla
    (progn
      ;; Generar lista de DN disponibles
      (setq lista-dn (mapcar 'car tabla))
      (setq opciones-dn
        (apply 'strcat
          (mapcar '(lambda (x) (strcat (rtos x 2 2) " ")) lista-dn)
        )
      )

      ;; Selección de DN
      (initget opciones-dn)
      (setq dn (getkword (strcat "\nSeleccione DN [" opciones-dn "]: ")))
      (setq dn (atof dn))

      ;; Buscar entrada
      (setq entry (assoc dn tabla))

      ;; Validar y dibujar
      (if entry
        (progn
          (setq od (cadr entry) id (caddr entry))
          (if (> id 0)
            (progn
              (setq pt (getpoint "\nSeleccione punto de inserción: "))
              (command "_.circle" pt (/ od 2))
              (command "_.circle" pt (/ id 2))
              (princ (strcat
                "\nTipo: CPVC " sch
                "\nDN: " (rtos dn 2 2) " pulgadas"
                "\nOD: " (rtos od 2 2) " mm"
                "\nID: " (rtos id 2 2) " mm"
              ))
            )
            (princ "\n⚠️ Error: el espesor excede el OD. No se puede dibujar el círculo interior.")
          )
        )
        (princ (strcat "\nEl DN " (rtos dn 2 2) " no está en la tabla para CPVC Cédula " sch "."))
      )
    )
    (princ "\n⚠️ Cédula no válida. Solo se permite 40 u 80.")
  )
)

;; Cobre
((= tipo "Cobre")
  (initget "Rojo Azul Verde")
  (setq color (getkword "\nSeleccione el color del tubo [Rojo/Azul/Verde]: "))

  ;; Asignar tipo según color
  (setq tipoCobre
    (cond
      ((= color "Rojo") "M")
      ((= color "Azul") "L")
      ((= color "Verde") "K")
      (T nil)
    )
  )

  ;; Asignar tabla según tipo
  (setq tabla
    (cond
      ((= tipoCobre "M") tabla-cobreM)
      ((= tipoCobre "L") tabla-cobreL)
      ((= tipoCobre "K") tabla-cobreK)
      (T nil)
    )
  )

  ;; Validar tabla
  (if tabla
    (progn
      ;; Generar lista de DN disponibles
      (setq lista-dn (mapcar 'car tabla))
      (setq opciones-dn
        (apply 'strcat
          (mapcar '(lambda (x) (strcat (rtos x 2 2) " ")) lista-dn)
        )
      )

      ;; Selección de DN
      (initget opciones-dn)
      (setq dn (getkword (strcat "\nSeleccione DN [" opciones-dn "]: ")))
      (setq dn (atof dn))

      ;; Buscar entrada
      (setq entry (assoc dn tabla))

      ;; Validar y dibujar
      (if entry
        (progn
          (setq od (cadr entry) espesor (caddr entry))
          (setq id (- od (* 2 espesor)))
          (if (> id 0)
            (progn
              (setq pt (getpoint "\nSeleccione punto de inserción: "))
              (command "_.circle" pt (/ od 2))
              (command "_.circle" pt (/ id 2))
              (princ (strcat
                "\nTipo: Cobre " tipoCobre
                "\nDN: " (rtos dn 2 2) " pulgadas"
                "\nOD: " (rtos od 2 2) " mm"
                "\nEspesor: " (rtos espesor 2 2) " mm"
                "\nID: " (rtos id 2 2) " mm"
              ))
            )
            (princ "\n⚠️ Error: el espesor excede el OD. No se puede dibujar el círculo interior.")
          )
        )
        (princ (strcat "\nEl DN " (rtos dn 2 2) " no está en la tabla para Cobre tipo " tipoCobre "."))
      )
    )
    (princ "\n⚠️ Tipo de cobre no válido.")
  )
)
    
;; Conduit
((= tipo "Conduit")
  (initget "R0 R1 C40")
  (setq sch (getkword "\nSeleccione el tipo de Conduit [R0/R1/C40]: "))

  ;; Asignar tabla con cond
  (setq tabla
    (cond
      ((= sch "R0") tabla-conduitR0)
      ((= sch "R1") tabla-conduitR1)
      ((= sch "C40") tabla-conduitC40)
      (T nil)
    )
  )

  ;; Validar tabla
  (if tabla
    (progn
      ;; Generar lista de DN disponibles
      (setq lista-dn (mapcar 'car tabla))
      (setq opciones-dn
        (apply 'strcat
          (mapcar '(lambda (x) (strcat (rtos x 2 2) " ")) lista-dn)
        )
      )

      ;; Selección de DN
      (initget opciones-dn)
      (setq dn (getkword (strcat "\nSeleccione DN [" opciones-dn "]: ")))
      (setq dn (atof dn))

      ;; Buscar entrada
      (setq entry (assoc dn tabla))

      ;; Validar y dibujar
      (if entry
        (progn
          (setq od (cadr entry) id (caddr entry))
          (if (> id 0)
            (progn
              (setq pt (getpoint "\nSeleccione punto de inserción: "))
              (command "_.circle" pt (/ od 2))
              (command "_.circle" pt (/ id 2))
              (princ (strcat
                "\nTipo: Conduit " sch
                "\nDN: " (rtos dn 2 2) " pulgadas"
                "\nOD: " (rtos od 2 2) " mm"
                "\nID: " (rtos id 2 2) " mm"
              ))
            )
            (princ "\n⚠️ Error: el espesor excede el OD. No se puede dibujar el círculo interior.")
          )
        )
        (princ (strcat "\nEl DN " (rtos dn 2 2) " no está en la tabla de Conduit tipo " sch "."))
      )
    )
    (princ "\n⚠️ Tipo de Conduit no válido.")
  )
)
  )

  (princ)
)
