(defun c:MostrarEtiqueta ( / sel ename edata xdata etiquetaFinal )
  (setq sel (entsel "\nSeleccione el objeto para mostrar etiqueta: "))
  (if (not sel)
    (progn (princ "\nNo se seleccionó ningún objeto.") (exit))
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
