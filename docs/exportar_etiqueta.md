# ExportarEtiqueta.lsp

## 📌 Description
AutoLISP routine that allows you to **export all piece labels** from an AutoCAD drawing into a CSV file.  
The generated file includes structured information such as layer, piece type, elbow type, reduction type, pipe type, size, flange type, length, and final label.

## ▶️ Usage
1. Load the routine in AutoCAD using the `APPLOAD` command.
2. Run the command `ExportarEtiquetas`.
3. Select the labeled objects in the drawing.
4. Save the CSV file to the desired path.
5. Review the generated inventory with all fields organized.

## 📸 Example
The result is a CSV file with columns like:
```bash
LAYER | PIECE TYPE | ELBOW TYPE | REDUCTION TYPE | PIPE TYPE | SIZE | FLANGE TYPE | LENGTH | LABEL
```

![Export Labels in action](images/export_tags_document_example.png)


## 🔹 Code snippet
```lisp
(defun c:ExportarEtiquetas ( / ss ename edata xdata etiqueta ruta ultRuta line i
                               tmp tmp-pairs assoc1000 csv-line campos)

  ;; safe-str: converts to safe string
  (defun safe-str (v)
    (cond
      ((null v) "")
      ((numberp v) (rtos v 2 2))
      ((eq (type v) 'STRING) v)
      (T (strcase (strcat v "")))
    )
  )
  ;; ... rest of the export logic ...
)
```

🎯 Benefits
- Automatic generation of piece inventories.
- Direct export to CSV for analysis in Excel or other systems.
- Saves time and reduces manual errors.

