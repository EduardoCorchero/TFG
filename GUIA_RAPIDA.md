# Guía Rápida - Validación de Tabla

## 🎯 Objetivo

Validar contratos en la tabla de control de perímetro SIMA IC y verificar que las refasociaciones existen tanto en `intermedia_BDR` como en `intermedia_CREAM`.

## 📋 Query Principal

Para ver las refasociaciones que están en `intermedia_BDR` y también en `intermedia_CREAM`:

```sql
SELECT DISTINCT 
    a.contra1,
    b.refasociacion,
    c.refasociacion AS refasociacion_cream
FROM control_perimetro_SIMA_IC_PROD a
LEFT JOIN intermedia_BDR b
    ON a.contra1 = b.contra1
INNER JOIN intermedia_CREAM c
    ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL;
```

> **Nota:** Reemplazar `PROD` con tu entorno (`TEST`, `DEV`, etc.)

## 🐍 Uso del Script Python

### Generar el query para tu entorno

```bash
python3 validacion_tabla.py --bdr PROD --tipo solo_coincidencias
```

### Ver estadísticas

```bash
python3 validacion_tabla.py --bdr PROD --tipo analisis
```

### Guardar todos los queries

```bash
python3 validacion_tabla.py --bdr PROD --tipo todos --output mis_queries.sql
```

## 📊 Interpretación de Resultados

| Resultado | Significado |
|-----------|-------------|
| **Existe en CREAM** | ✅ La refasociación está sincronizada en ambas vistas |
| **No existe en CREAM** | ⚠️ La refasociación solo existe en BDR, requiere investigación |

## 📁 Archivos Disponibles

- **`validacion_tabla.sql`** - Queries SQL listos para usar
- **`validacion_tabla.py`** - Script Python para generar queries
- **`README_VALIDACION.md`** - Documentación completa
- **`ejemplo_uso_validacion.md`** - Ejemplos de uso detallados

## 🚀 Empezar Ahora

1. **Opción 1 - SQL Directo:**
   - Abrir `validacion_tabla.sql`
   - Reemplazar `${bdr}` con tu entorno
   - Ejecutar el query deseado

2. **Opción 2 - Script Python:**
   ```bash
   python3 validacion_tabla.py --bdr TU_ENTORNO --tipo refasociacion
   ```

## 💡 Queries Disponibles

1. **Principal** - Lista contratos con clasificaciones específicas
2. **Refasociación** - Muestra refasociaciones y su estado en CREAM
3. **Solo Coincidencias** - Filtra solo las que existen en ambas vistas
4. **Análisis** - Estadísticas de sincronización

## ⚙️ Personalización

Para añadir nuevas clasificaciones, editar el array en `validacion_tabla.py`:

```python
self.clasificaciones = [
    "01. El contrato no está ni en Solvencia ni en traza de SIMA",
    "03. El contrato está y cruza en Solvencia pero no está en traza de SIMA",
    # Añadir más aquí
]
```

## 🔍 Troubleshooting Rápido

- **Tabla no encontrada:** Verificar el nombre del entorno en la tabla
- **Sin resultados:** Verificar que existen datos con las clasificaciones
- **Error de permisos:** Contactar al administrador de base de datos

## 📚 Más Información

Ver `README_VALIDACION.md` para documentación completa y `ejemplo_uso_validacion.md` para casos de uso detallados.
