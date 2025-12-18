-- ================================================================
-- Validación de Tabla: Control de Perimetro SIMA IC
-- ================================================================
-- Descripción: Este query valida los contratos que cumplen con las 
-- condiciones especificadas y verifica la existencia de refasociacion
-- en ambas vistas (intermedia_BDR e intermedia_CREAM)
-- ================================================================

-- Query principal: Obtener contratos con clasificaciones específicas
-- y que no tienen correspondencia en intermedia_BDR
SELECT DISTINCT 
    a.contra1, 
    b.*
FROM control_perimetro_SIMA_IC_${bdr} a
LEFT JOIN intermedia_BDR b
    ON a.contra1 = b.contra1
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.contra1 IS NULL;

-- ================================================================
-- Query extendido: Verificar refasociaciones en ambas vistas
-- ================================================================
-- Este query muestra las refasociaciones de intermedia_BDR que también
-- existen en intermedia_CREAM

SELECT DISTINCT 
    a.contra1,
    b.refasociacion,
    CASE 
        WHEN c.refasociacion IS NOT NULL THEN 'Existe en CREAM'
        ELSE 'No existe en CREAM'
    END AS estado_cream
FROM control_perimetro_SIMA_IC_${bdr} a
LEFT JOIN intermedia_BDR b
    ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c
    ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.refasociacion IS NOT NULL;

-- ================================================================
-- Query alternativo: Solo refasociaciones que existen en ambas vistas
-- ================================================================
-- Filtra solo las refasociaciones que están presentes tanto en 
-- intermedia_BDR como en intermedia_CREAM

SELECT DISTINCT 
    a.contra1,
    b.refasociacion,
    c.refasociacion AS refasociacion_cream
FROM control_perimetro_SIMA_IC_${bdr} a
LEFT JOIN intermedia_BDR b
    ON a.contra1 = b.contra1
INNER JOIN intermedia_CREAM c
    ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.refasociacion IS NOT NULL;

-- ================================================================
-- Query de análisis: Contar refasociaciones por estado
-- ================================================================

SELECT 
    COUNT(DISTINCT b.refasociacion) AS total_refasociacion_BDR,
    COUNT(DISTINCT CASE WHEN c.refasociacion IS NOT NULL THEN b.refasociacion END) AS refasociacion_en_ambas,
    COUNT(DISTINCT CASE WHEN c.refasociacion IS NULL THEN b.refasociacion END) AS refasociacion_solo_BDR
FROM control_perimetro_SIMA_IC_${bdr} a
LEFT JOIN intermedia_BDR b
    ON a.contra1 = b.contra1
LEFT JOIN intermedia_CREAM c
    ON b.refasociacion = c.refasociacion
WHERE a.clasificacion IN (
    '01. El contrato no está ni en Solvencia ni en traza de SIMA',
    '03. El contrato está y cruza en Solvencia pero no está en traza de SIMA'
)
AND b.refasociacion IS NOT NULL;
