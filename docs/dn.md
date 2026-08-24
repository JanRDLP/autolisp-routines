# DN.lsp

## 📌 Description
AutoLISP routine that automates the drawing of pipes in AutoCAD using **Nominal Diameter (DN)** notation.  
It supports multiple pipe types (Galvanized, Black, PVC, CPVC, Copper, Conduit) and schedules (SCH 10, 20, 30, 40, 80, 160).

## ▶️ Usage
1. Load the routine in AutoCAD with `APPLOAD`.
2. Run the command `DN`.
3. Select the pipe type (Galvanized, Black, PVC, CPVC, Copper, Conduit).
4. Choose the schedule (SCH) and DN size.
5. Pick an insertion point. The routine will draw two concentric circles representing **OD (Outer Diameter)** and **ID (Inner Diameter)**.

## 📸 Example
![DN result](../images/dn_inserted.png)

## 🔹 Code snippet
```lisp
(defun c:DN ( / tipo dn sch color tipoCobre od id pt entry tabla )
  ;; Pipe type selection
  (initget "Galvanizado Negro PVC CPVC Cobre Conduit")
  (setq tipo (getkword "\nSelect pipe type [Galvanizado/Negro/PVC/CPVC/Cobre/Conduit]: "))
  ;; ... rest of logic ...
)
```

## 🎯 Benefits
- Automates pipe drawing with accurate dimensions.
- Supports multiple materials and schedules.
- Reduces manual errors in technical drawings.

