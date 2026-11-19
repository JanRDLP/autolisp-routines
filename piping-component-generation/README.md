# Piping Component Generation

`CREARPIEZA` is an AutoLISP command for configuring and placing 3D piping components in AutoCAD. The dialog previews the component label, block name, and layer before insertion.

## Run in AutoCAD

1. Download this folder and keep the five `.lsp` files together.
2. In AutoCAD for Windows, run `APPLOAD` and load `CrearPieza.lsp` from a trusted location.
3. Run `CREARPIEZA`. If prompted for a dependency, select `PiezaDatos.lsp` in the same folder. The command loads the remaining modules and generates its dialog at runtime.

## Example

1. Select an elbow for weldable black pipe with Schedule 40, a 90° angle, a long radius, and a nominal size of 2 inches.
2. Select the labeled-solid-in-block output and review the generated label, block name, and destination layer.
3. Click **Crear** and pick the intersection of the elbow’s connection centerlines in the drawing.
4. Press Enter to place another elbow with the same settings, or choose **Salir**.

The example creates simplified geometry without threads or bevels.

## Files

- `CrearPieza.lsp` provides the command and configuration dialog.
- `PiezaDatos.lsp` handles component labels and stored data.
- `GalvanizadosCifunsa.lsp`, `PiezasTuboNegro.lsp`, and `AccesoriosTuberia.lsp` provide the component catalogs and geometry used by the command.
