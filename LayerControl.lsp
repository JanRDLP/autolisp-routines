;;; ============================================================
;;;  LAYERCONTROL.LSP
;;;  Panel unico para controlar visibilidad de capas por CAPAS
;;;  o "requisitos": Fase 1, Fase 2, Tubo, 2D, 3D, Electrico,
;;;  Banda Tomatillo, Banda Nachos.
;;;
;;;  LOGICA: cada capa puede coincidir con VARIOS checkboxes a la
;;;  vez (ej. una tuberia 3D de Fase 1 coincide con "Fase 1", "Tubo"
;;;  y "3D"). Esa capa se muestra UNICAMENTE si TODOS los checkboxes
;;;  que le aplican estan marcados (logica AND). Si una capa no
;;;  coincide con ningun grupo, no se toca.
;;;
;;;  Caso especial: "FASE 1 SOLO" se trata como una version de
;;;  "Fase 1" que ademas exige que "Fase 2" este APAGADO.
;;;
;;;  Comando: LAYERCONTROL
;;; ============================================================

;; -------- Definicion de grupos: (id include exclude label) --------
(setq *lc-groups*
  (list
    (list "fase1"     "*FASE 1*"     "*FASE 1 SOLO*"  "Fase 1")
    (list "fase2"     "*FASE 2*"     nil              "Fase 2")
    (list "tubo"      "*TUBO*"       nil              "Tubo")
    (list "d2"        "*2D*"         nil              "2D")
    (list "d3"        "*3D*"         nil              "3D")
    (list "electrico" "*ELECTRICO*"  nil              "Electrico")
    (list "tomatillo" "*TOMATILLO*"  nil              "Banda Tomatillo")
    (list "nachos"    "*NACHOS*"     nil              "Banda Nachos")
  )
)

;; -------- Utilidades --------

(defun lc:matches (layerName include exclude)
  (and
    (wcmatch layerName include)
    (or (null exclude) (not (wcmatch layerName exclude)))
  )
)

;; T si AL MENOS UNA capa del grupo esta visible (para inicializar checkboxes)
(defun lc:group-visible-p (layMan include exclude / layObj layName visible)
  (setq visible nil)
  (vlax-for layObj layMan
    (setq layName (strcase (vla-get-Name layObj)))
    (if (lc:matches layName include exclude)
      (if (= (vla-get-Freeze layObj) :vlax-false)
        (setq visible T)
      )
    )
  )
  visible
)

;; Cuenta cuantas capas coinciden con un grupo (para mostrar en el label)
(defun lc:count-group (layMan include exclude / layObj layName n)
  (setq n 0)
  (vlax-for layObj layMan
    (setq layName (strcase (vla-get-Name layObj)))
    (if (lc:matches layName include exclude) (setq n (1+ n)))
  )
  n
)

;; Pone la capa "0" como activa antes de aplicar cambios, para evitar que
;; falle el congelamiento por intentar congelar la capa actual.
(defun lc:make-safe-current (doc / res)
  (setq res (vl-catch-all-apply 'vla-put-ActiveLayer
              (list doc (vla-Item (vla-get-Layers doc) "0"))))
  (vl-catch-all-error-p res)
)

;; -------- MOTOR PRINCIPAL: logica AND --------
;; Para cada capa: reune los checkboxes que le aplican (incluyendo el
;; caso especial FASE 1 SOLO) y la muestra solo si TODOS estan marcados.
;; Si no le aplica ningun checkbox, no se toca la capa.
(defun lc:apply-all (layMan / layObj layName grp reqs allTrue res)
  (vlax-for layObj layMan
    (setq layName (strcase (vla-get-Name layObj)))
    (setq reqs nil)

    (foreach grp *lc-groups*
      (if (lc:matches layName (nth 1 grp) (nth 2 grp))
        (setq reqs (cons (= (get_tile (nth 0 grp)) "1") reqs))
      )
    )

    ;; Caso especial: FASE 1 SOLO -> requiere Fase1 ON y Fase2 OFF
    (if (wcmatch layName "*FASE 1 SOLO*")
      (setq reqs
        (cons
          (and (= (get_tile "fase1") "1") (not (= (get_tile "fase2") "1")))
          reqs
        )
      )
    )

    (if reqs
      (progn
        (setq allTrue (not (member nil reqs)))
        (if allTrue
          (progn
            (vla-put-Freeze layObj :vlax-false)
            (vla-put-LayerOn layObj :vlax-true)
          )
          (progn
            (setq res (vl-catch-all-apply 'vla-put-Freeze (list layObj :vlax-true)))
            (if (vl-catch-all-error-p res)
              (setq *lc-errors* (cons layName *lc-errors*))
            )
          )
        )
      )
      ;; si reqs es nil, la capa no pertenece a ningun grupo: no se toca
    )
  )
)

;; -------- Generacion del DCL en un archivo temporal --------
(defun lc:write-dcl (path layMan / f grp cnt)
  (setq f (open path "w"))
  (write-line "layercontrol : dialog {" f)
  (write-line "  label = \"Control de Capas\";" f)
  (write-line "  : column {" f)
  (foreach grp *lc-groups*
    (setq cnt (lc:count-group layMan (nth 1 grp) (nth 2 grp)))
    (write-line
      (strcat
        "    : toggle { key = \"" (nth 0 grp) "\"; label = \""
        (nth 3 grp) " (" (itoa cnt) " capas)"
        "\"; }"
      )
      f
    )
  )
  (write-line "  }" f)
  (write-line "  : text { label = \"Fase 1 Solo: visible solo si Fase 1 esta encendido y Fase 2 apagado\"; }" f)
  (write-line "  spacer;" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"allon\"; label = \"Todo visible\"; }" f)
  (write-line "    : button { key = \"alloff\"; label = \"Todo oculto\"; }" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
)

;; -------- Comando principal --------
(defun c:LAYERCONTROL ( / doc layMan dclFile dcl_id result grp)
  (setq doc    (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq layMan (vla-get-Layers doc))
  (setq *lc-errors* nil)

  (setq dclFile (strcat (getenv "TEMP") "\\layercontrol.dcl"))
  (lc:write-dcl dclFile layMan)

  (setq dcl_id (load_dialog dclFile))
  (if (not (new_dialog "layercontrol" dcl_id))
    (progn
      (princ "\nLAYERCONTROL: no se pudo cargar el dialogo.")
      (exit)
    )
  )

  ;; Inicializar cada checkbox con el estado actual del dibujo
  (foreach grp *lc-groups*
    (set_tile (nth 0 grp)
      (if (lc:group-visible-p layMan (nth 1 grp) (nth 2 grp)) "1" "0")
    )
  )

  (action_tile "allon"
    "(foreach grp *lc-groups* (set_tile (nth 0 grp) \"1\"))"
  )
  (action_tile "alloff"
    "(foreach grp *lc-groups* (set_tile (nth 0 grp) \"0\"))"
  )

  (action_tile "accept"
    (strcat
      "(lc:make-safe-current doc)"
      "(lc:apply-all layMan)"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq result (start_dialog))
  (unload_dialog dcl_id)

  (if (= result 1)
    (progn
      (command "_.REGEN")
      (princ "\nLAYERCONTROL: capas actualizadas.")
      (if *lc-errors*
        (progn
          (princ "\nAVISO: no se pudieron congelar estas capas (posiblemente son la capa activa):")
          (foreach n *lc-errors* (princ (strcat "\n  - " n)))
        )
      )
    )
    (princ "\nLAYERCONTROL: cancelado, no se hicieron cambios.")
  )
  (princ)
)

(princ "\nLAYERCONTROL cargado. Escribe LAYERCONTROL para abrir el panel.")
(princ)