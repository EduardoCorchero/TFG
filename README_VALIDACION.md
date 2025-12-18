# Validación de Tabla - Control de Perímetro SIMA IC

## Descripción General

Este documento describe el proceso de validación de tablas para el control de perímetro SIMA IC, específicamente para verificar la presencia de refasociaciones en múltiples vistas.

## Contexto

Se necesita validar contratos que cumplan con criterios específicos de clasificación y verificar la existencia de sus refasociaciones en diferentes vistas del sistema.

## Tablas/Vistas Involucradas

1. **control_perimetro_SIMA_IC_${bdr}**: Vista principal de control de perímetro
   - Campo clave: `contra1`
   - Campo de clasificación: `clasificacion`

2. **intermedia_BDR**: Vista intermedia de BDR
   - Campo clave: `contra1`
   - Campo de referencia: `refasociacion`

3. **intermedia_CREAM**: Vista intermedia de CREAM
   - Campo de referencia: `refasociacion`

## Clasificaciones Objetivo

Los contratos que deben ser validados tienen las siguientes clasificaciones:

1. `"01. El contrato no está ni en Solvencia ni en traza de SIMA"`
2. `"03. El contrato está y cruza en Solvencia pero no está en traza de SIMA"`

## Queries de Validación

El archivo `validacion_tabla.sql` contiene varios queries:

### Query 1: Query Principal
Obtiene todos los contratos con las clasificaciones especificadas que no tienen correspondencia en intermedia_BDR.

### Query 2: Query Extendido con Estado
Muestra los refasociacion de intermedia_BDR y verifica si existen en intermedia_CREAM, indicando su estado.

### Query 3: Query Filtrado
Muestra únicamente las refasociaciones que existen tanto en intermedia_BDR como en intermedia_CREAM.

### Query 4: Query de Análisis
Proporciona estadísticas sobre el número de refasociaciones encontradas en cada vista.

## Uso

### Requisitos Previos
- Reemplazar la variable `${bdr}` con el valor correspondiente al entorno
- Asegurar acceso a las vistas: control_perimetro_SIMA_IC, intermedia_BDR, intermedia_CREAM

### Ejecución

```sql
-- Ejemplo de uso reemplazando la variable
-- Si ${bdr} = 'PROD', entonces:
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
AND b.refasociacion IS NOT NULL;
```

## Interpretación de Resultados

### Estado "Existe en CREAM"
Los contratos con este estado tienen sus refasociaciones presentes en ambas vistas, lo cual indica consistencia entre los sistemas.

### Estado "No existe en CREAM"
Los contratos con este estado tienen refasociaciones en intermedia_BDR pero no en intermedia_CREAM, lo cual puede indicar:
- Datos faltantes en CREAM
- Procesos de sincronización pendientes
- Inconsistencias que requieren investigación

## Notas Adicionales

- El query utiliza `LEFT JOIN` para incluir contratos que no tienen correspondencia
- La condición `b.contra1 IS NULL` filtra específicamente los casos donde no hay match en intermedia_BDR
- Se recomienda ejecutar el query de análisis primero para obtener una visión general de los datos

## Mantenimiento

Para actualizar los criterios de clasificación, modificar la cláusula `WHERE` en el archivo `validacion_tabla.sql`.

## Contacto

Para preguntas o sugerencias sobre esta validación, contactar al equipo responsable del proyecto TFG.
