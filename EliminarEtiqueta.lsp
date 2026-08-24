(defun c:EliminarEtiqueta ( / oldpick sel ename edata xdata etiqueta app cont respuesta )
  ;; guardar estado actual de PICKSTYLE y desactivar selección de grupos
  (setq oldpick (getvar "PICKSTYLE"))
  (setvar "PICKSTYLE" 0)

  ;; selección manual
  (setq sel (ssget '((0 . "*"))))

  ;; restaurar valor original
  (setvar "PICKSTYLE" oldpick)

  (if sel
    (progn
      ;; confirmación general
      (initget "SI NO")
      (setq respuesta
        (getkword "\n¿Desea eliminar TODAS las etiquetas ETIQUETA_PIEZA de los objetos seleccionados? [SI/NO]: ")
      )

      (if (= respuesta "SI")
        (progn
          (setq cont 0)
          (repeat (setq i (sslength sel))
            (setq ename (ssname sel (setq i (1- i))))
            (setq edata (entget ename '("*")))
            (setq xdata (assoc -3 edata))
            (setq etiqueta nil)

            ;; buscar si tiene XDATA "ETIQUETA_PIEZA"
            (if xdata
              (foreach app (cdr xdata)
                (if (= (car app) "ETIQUETA_PIEZA")
                  (setq etiqueta (cdr (assoc 1000 (cdr app))))
                )
              )
            )

            ;; si tiene etiqueta, eliminarla
            (if etiqueta
              (progn
                (entmod
                  (list
                    (cons -1 ename)
                    (list -3 (list "ETIQUETA_PIEZA"))
                  )
                )
                (entupd ename)
                (setq cont (1+ cont))
              )
            )
          )
          (alert (strcat "Proceso completado.\nTotal de etiquetas eliminadas: " (itoa cont)))
        )
        (princ "\nOperación cancelada por el usuario.")
      )
    )
    (princ "\nNo se seleccionaron objetos.")
  )
  (princ)
)
