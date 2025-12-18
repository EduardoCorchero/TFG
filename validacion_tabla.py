#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script de Validación de Tabla - Control de Perímetro SIMA IC

Este script facilita la ejecución de queries de validación para verificar
la presencia de refasociaciones en las vistas intermedia_BDR e intermedia_CREAM.

Autor: TFG Project
Fecha: 2025
"""

import sys
import argparse
from typing import Optional


class ValidadorTabla:
    """
    Clase para gestionar las validaciones de tabla del control de perímetro.
    """
    
    def __init__(self, bdr_env: str):
        """
        Inicializa el validador con el entorno BDR.
        
        Args:
            bdr_env: Entorno BDR (ej: 'PROD', 'TEST', 'DEV')
        """
        self.bdr_env = bdr_env
        self.clasificaciones = [
            "01. El contrato no está ni en Solvencia ni en traza de SIMA",
            "03. El contrato está y cruza en Solvencia pero no está en traza de SIMA"
        ]
    
    def generar_query_principal(self) -> str:
        """
        Genera el query principal de validación.
        
        Returns:
            str: Query SQL formateado
        """
        query = f"""
        SELECT DISTINCT 
            a.contra1, 
            b.*
        FROM control_perimetro_SIMA_IC_{self.bdr_env} a
        LEFT JOIN intermedia_BDR b
            ON a.contra1 = b.contra1
        WHERE a.clasificacion IN (
            '{self.clasificaciones[0]}',
            '{self.clasificaciones[1]}'
        )
        AND b.contra1 IS NULL;
        """
        return query.strip()
    
    def generar_query_refasociacion(self) -> str:
        """
        Genera el query para verificar refasociaciones en ambas vistas.
        
        Returns:
            str: Query SQL formateado
        """
        query = f"""
        SELECT DISTINCT 
            a.contra1,
            b.refasociacion,
            CASE 
                WHEN c.refasociacion IS NOT NULL THEN 'Existe en CREAM'
                ELSE 'No existe en CREAM'
            END AS estado_cream
        FROM control_perimetro_SIMA_IC_{self.bdr_env} a
        LEFT JOIN intermedia_BDR b
            ON a.contra1 = b.contra1
        LEFT JOIN intermedia_CREAM c
            ON b.refasociacion = c.refasociacion
        WHERE a.clasificacion IN (
            '{self.clasificaciones[0]}',
            '{self.clasificaciones[1]}'
        )
        AND b.refasociacion IS NOT NULL;
        """
        return query.strip()
    
    def generar_query_solo_coincidencias(self) -> str:
        """
        Genera el query que muestra solo refasociaciones presentes en ambas vistas.
        
        Returns:
            str: Query SQL formateado
        """
        query = f"""
        SELECT DISTINCT 
            a.contra1,
            b.refasociacion,
            c.refasociacion AS refasociacion_cream
        FROM control_perimetro_SIMA_IC_{self.bdr_env} a
        LEFT JOIN intermedia_BDR b
            ON a.contra1 = b.contra1
        INNER JOIN intermedia_CREAM c
            ON b.refasociacion = c.refasociacion
        WHERE a.clasificacion IN (
            '{self.clasificaciones[0]}',
            '{self.clasificaciones[1]}'
        )
        AND b.refasociacion IS NOT NULL;
        """
        return query.strip()
    
    def generar_query_analisis(self) -> str:
        """
        Genera el query de análisis estadístico.
        
        Returns:
            str: Query SQL formateado
        """
        query = f"""
        SELECT 
            COUNT(DISTINCT b.refasociacion) AS total_refasociacion_BDR,
            COUNT(DISTINCT CASE WHEN c.refasociacion IS NOT NULL THEN b.refasociacion END) AS refasociacion_en_ambas,
            COUNT(DISTINCT CASE WHEN c.refasociacion IS NULL THEN b.refasociacion END) AS refasociacion_solo_BDR
        FROM control_perimetro_SIMA_IC_{self.bdr_env} a
        LEFT JOIN intermedia_BDR b
            ON a.contra1 = b.contra1
        LEFT JOIN intermedia_CREAM c
            ON b.refasociacion = c.refasociacion
        WHERE a.clasificacion IN (
            '{self.clasificaciones[0]}',
            '{self.clasificaciones[1]}'
        )
        AND b.refasociacion IS NOT NULL;
        """
        return query.strip()
    
    def generar_todos_los_queries(self) -> dict:
        """
        Genera todos los queries disponibles.
        
        Returns:
            dict: Diccionario con todos los queries
        """
        return {
            'principal': self.generar_query_principal(),
            'refasociacion': self.generar_query_refasociacion(),
            'coincidencias': self.generar_query_solo_coincidencias(),
            'analisis': self.generar_query_analisis()
        }


def main():
    """
    Función principal del script.
    """
    parser = argparse.ArgumentParser(
        description='Generador de queries de validación de tabla'
    )
    parser.add_argument(
        '--bdr',
        type=str,
        required=True,
        help='Entorno BDR (ej: PROD, TEST, DEV)'
    )
    parser.add_argument(
        '--tipo',
        type=str,
        choices=['principal', 'refasociacion', 'solo_coincidencias', 'analisis', 'todos'],
        default='todos',
        help='Tipo de query a generar'
    )
    parser.add_argument(
        '--output',
        type=str,
        help='Archivo de salida para guardar los queries'
    )
    
    args = parser.parse_args()
    
    # Crear validador
    validador = ValidadorTabla(args.bdr)
    
    # Generar queries
    if args.tipo == 'todos':
        queries = validador.generar_todos_los_queries()
        output = "\n\n-- " + "="*70 + "\n\n".join([
            f"-- Query {nombre.upper()}\n{query}"
            for nombre, query in queries.items()
        ])
    else:
        # Mapear nombres de argumentos a métodos
        method_name = f'generar_query_{args.tipo}'
        if not hasattr(validador, method_name):
            print(f"Error: Tipo de query '{args.tipo}' no reconocido", file=sys.stderr)
            sys.exit(1)
        metodo = getattr(validador, method_name)
        output = metodo()
    
    # Mostrar o guardar resultado
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(output)
        print(f"Queries guardados en: {args.output}")
    else:
        print(output)


if __name__ == '__main__':
    main()
