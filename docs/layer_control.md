# LayerControl.lsp

## 📌 Description
AutoLISP routine that provides a **single panel to control layer visibility** in AutoCAD.  
It uses checkboxes to toggle groups such as *Phase 1, Phase 2, Pipe, 2D, 3D, Electrical, Banda Tomatillo, Banda Nachos*.  
A layer is visible only if **all applicable checkboxes are enabled (AND logic)**.

## ▶️ Usage
1. Load the routine in AutoCAD with `APPLOAD`.
2. Run the command `LAYERCONTROL`.
3. A dialog window will appear with checkboxes for each group.
4. Toggle visibility as needed. Special case: *Phase 1 Only* requires Phase 1 ON and Phase 2 OFF.

## 📸 Example
![LayerControl routine example](images/layer_control_change.png)

## 🔹 Code snippet
```lisp
(defun c:LAYERCONTROL ( / doc layMan dclFile dcl_id result )
  ;; Main command to open the dialog
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  ;; ... rest of logic ...
)
```

## 🎯 Benefits
- Centralized control of layer visibility.
- Logical grouping of layers for complex projects.
- Improves productivity by reducing manual layer management.