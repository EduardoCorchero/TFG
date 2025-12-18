# Ejemplos de Uso - Validación de Tabla

## 1. Uso del Script Python

### Generar todos los queries para el entorno PROD

```bash
python3 validacion_tabla.py --bdr PROD
```

### Generar solo el query de refasociación

```bash
python3 validacion_tabla.py --bdr PROD --tipo refasociacion
```

### Generar solo coincidencias (refasociaciones en ambas vistas)

```bash
python3 validacion_tabla.py --bdr PROD --tipo solo_coincidencias
```

### Guardar queries en un archivo

```bash
python3 validacion_tabla.py --bdr PROD --tipo todos --output queries_prod.sql
```

### Generar query de análisis para TEST

```bash
python3 validacion_tabla.py --bdr TEST --tipo analisis
```

## 2. Ejecución Directa de SQL

### Query para ver refasociaciones que existen en ambas vistas

```sql
-- Reemplazar ${bdr} con el entorno apropiado (ej: PROD)
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

## 3. Verificación de Resultados

### Paso 1: Ejecutar query de análisis
Primero ejecuta el query de análisis para obtener estadísticas:

```sql
SELECT 
    COUNT(DISTINCT b.refasociacion) AS total_refasociacion_BDR,
    COUNT(DISTINCT CASE WHEN c.refasociacion IS NOT NULL THEN b.refasociacion END) AS refasociacion_en_ambas,
    COUNT(DISTINCT CASE WHEN c.refasociacion IS NULL THEN b.refasociacion END) AS refasociacion_solo_BDR
FROM control_perimetro_SIMA_IC_PROD a
LEFT JOIN intermedia_BDR b ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL;
```

### Paso 2: Revisar detalle
Si encuentras refasociaciones solo en BDR, ejecuta el query extendido para ver el detalle:

```sql
SELECT DISTINCT 
    a.contra1,
    b.refasociacion,
    CASE 
        WHEN c.refasociacion IS NOT NULL THEN 'Existe en CREAM'
        ELSE 'No existe en CREAM'
    END AS estado_cream
FROM control_perimetro_SIMA_IC_PROD a
LEFT JOIN intermedia_BDR b ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL;
```

## 4. Casos de Uso Comunes

### Caso 1: Encontrar inconsistencias
Para encontrar refasociaciones que existen en BDR pero no en CREAM:

```sql
SELECT DISTINCT 
    a.contra1,
    b.refasociacion
FROM control_perimetro_SIMA_IC_PROD a
LEFT JOIN intermedia_BDR b ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL
AND c.refasociacion IS NULL;
```

### Caso 2: Validar sincronización completa
Para verificar que todas las refasociaciones están sincronizadas:

```sql
SELECT 
    CASE 
        WHEN COUNT(DISTINCT CASE WHEN c.refasociacion IS NULL THEN b.refasociacion END) = 0 
        THEN 'SINCRONIZADO'
        ELSE 'PENDIENTE'
    END AS estado_sincronizacion,
    COUNT(DISTINCT b.refasociacion) AS total_refasociaciones
FROM control_perimetro_SIMA_IC_PROD a
LEFT JOIN intermedia_BDR b ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL;
```

## 5. Notas Importantes

- **Variable ${bdr}**: Siempre reemplazar con el entorno correcto (PROD, TEST, DEV)
- **Performance**: Los queries pueden tardar dependiendo del volumen de datos
- **Permisos**: Asegurar que se tienen los permisos necesarios para acceder a las vistas
- **Actualización**: Revisar periódicamente si las clasificaciones han cambiado

## 6. Troubleshooting

### Error: Tabla no encontrada
Verificar que el nombre de la tabla incluye el entorno correcto:
- ✓ `control_perimetro_SIMA_IC_PROD`
- ✗ `control_perimetro_SIMA_IC_${bdr}`

### Error: Permisos insuficientes
Contactar al administrador de base de datos para obtener acceso a:
- `control_perimetro_SIMA_IC_*`
- `intermedia_BDR`
- `intermedia_CREAM`

### Resultados vacíos
Verificar:
1. Que existan datos con las clasificaciones especificadas
2. Que la condición `b.contra1 IS NULL` sea correcta para tu caso de uso
3. Que las tablas estén actualizadas
