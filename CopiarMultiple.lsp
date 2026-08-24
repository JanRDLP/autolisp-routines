(defun c:CopiarMultiple (/ ss offset numCopies mode ang plane i dirVector displacement)
  (setq ss (ssget))
  (if ss
    (progn
      ;; Parámetros comunes
      (setq offset (getreal "\nDistancia entre copias: "))
      (setq numCopies (getint "\nNúmero de copias: "))
      
      ;; Selección de modo
      (initget "X Y Z Vector")
      (setq mode (getkword "\nDirección de copia [X/Y/Z/Vector]: "))
      
      ;; Definir vector de dirección
      (cond
        ((= mode "X") (setq dirVector (list 1 0 0)))
        ((= mode "Y") (setq dirVector (list 0 1 0)))
        ((= mode "Z") (setq dirVector (list 0 0 1)))
        ((= mode "Vector")
          (setq ang (getreal "\nÁngulo de copia (grados): "))
          (setq ang (* pi (/ ang 180.0)))
          (initget "XY XZ YZ")
          (setq plane (getkword "\nPlano de referencia [XY/XZ/YZ]: "))
          (cond
            ((= plane "XY") (setq dirVector (list (cos ang) (sin ang) 0)))
            ((= plane "XZ") (setq dirVector (list (cos ang) 0 (sin ang))))
            ((= plane "YZ") (setq dirVector (list 0 (cos ang) (sin ang))))
          )
        )
      )
      
      ;; Normalizar vector
      (defun normalize (v)
        (setq mag (sqrt (+ (* (car v) (car v))
                           (* (cadr v) (cadr v))
                           (* (caddr v) (caddr v)))))
        (if (/= mag 0)
          (list (/ (car v) mag) (/ (cadr v) mag) (/ (caddr v) mag))
          v
        )
      )
      (setq dirVector (normalize dirVector))
      
      ;; Bucle de copias
      (setq i 1)
      (while (<= i numCopies)
        (setq displacement
          (list
            (* (car dirVector) offset i)
            (* (cadr dirVector) offset i)
            (* (caddr dirVector) offset i)
          )
        )
        ;; Usar coordenada relativa con @
        (command "_.COPY" ss "" "0,0,0"
                 (strcat "@" (rtos (car displacement) 2 4) ","
                           (rtos (cadr displacement) 2 4) ","
                           (rtos (caddr displacement) 2 4)))
        (setq i (1+ i))
      )
      
      ;; Resumen
      (princ (strcat
        "\nResumen: " (itoa numCopies) " copias, distancia "
        (rtos offset 2 2) ", dirección " mode
      ))
    )
    (princ "\nNo se seleccionó ningún objeto.")
  )
  (princ)
)
