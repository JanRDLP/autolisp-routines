(defun c:PULG ( / p1 eje long_pulg long_mm p2)
  (prompt "\nRutina: Dibujar linea de tuberia en mm a partir de pulgadas")

  ;; pedir punto inicial
  (setq p1 (getpoint "\nSeleccione punto inicial: "))

  ;; elegir eje
  (initget "X Y Z")
  (setq eje (getkword "\nEje al que se dirige la tuberia [X/Y/Z]: "))

  ;; pedir longitud en pulgadas (acepta decimales)
  (setq long_pulg (getreal "\nIngrese longitud en PULGADAS (decimales permitidos): "))

  ;; convertir a mm
  (setq long_mm (* long_pulg 25.4))

  ;; calcular punto final segun eje
  (cond
    ((= eje "X") (setq p2 (list (+ (car p1) long_mm) (cadr p1) (caddr p1))))
    ((= eje "Y") (setq p2 (list (car p1) (+ (cadr p1) long_mm) (caddr p1))))
    ((= eje "Z") (setq p2 (list (car p1) (cadr p1) (+ (caddr p1) long_mm))))
  )

  ;; trazar la linea
  (command "_.LINE" p1 p2 "")

  (princ)
)
