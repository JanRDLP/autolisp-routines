(defun c:VerEtiquetados ( / ss ename edata etiqueta lista sel )
  (setq lista '())

  ;; Recorremos todos los 3DSOLID y bloques
  (setq ss (ssget "_X" '((0 . "3DSOLID,INSERT"))))
  (if ss
    (progn
      (repeat (setq i (sslength ss))
        (setq ename (ssname ss (setq i (1- i))))
        ;; obtenemos con filtro de aplicación
        (setq edata (entget ename '("ETIQUETA_PIEZA")))
        (if (assoc -3 edata)
          (progn
            ;; buscar etiqueta dentro del XDATA
            (foreach app (cdr (assoc -3 edata))
              (if (= (car app) "ETIQUETA_PIEZA")
                (setq etiqueta (cdr (assoc 1000 (cdr app))))
              )
            )
            (if etiqueta
              (progn
                (setq lista (cons ename lista))
                (princ (strcat "\nObjeto etiquetado encontrado: " etiqueta))
              )
            )
          )
        )
      )
    )
  )

  ;; Si encontró algo, los seleccionamos
  (if lista
    (progn
      (setq sel (ssadd))
      (foreach e lista (ssadd e sel))
      (sssetfirst nil sel) ;; resaltado
      (princ (strcat "\nTotal de objetos etiquetados: " (itoa (length lista))))
    )
    (princ "\nNo se encontraron objetos etiquetados.")
  )
  (princ)
)
