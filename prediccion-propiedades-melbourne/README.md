Análisis estadístico, imputación de datos (MICE), PCA y regresión lineal múltiple en R para estimar precios de venta inmobiliarios en Melbourne, Australia.

# Resumen del Proyecto
* **Problema:** Justificación y comparación de modelos predictivos para estimar precios de venta de propiedades con variaciones estructurales y de ubicación.
* **Dataset:** *Melbourne Housing Snapshot* (13,580 observaciones y 21 variables).
* **Técnicas Clave:** Imputación MICE (PMM), Análisis de Componentes Principales (PCA), diagnóstico de multicolinealidad (VIF) y selección por AIC (Backward).

# Resultados Predictivos en Prueba (Test)
| Modelo | N° Variables | $R^2$ (Test) | RMSE (AUD) | MAE (AUD) |
| :--- | :---: | :---: | :---: | :---: |
| **Regresión Inicial** | 8 | 0.419 | $473,771 | $334,449 |
| **Regresión Reducida (AIC)** | 6 | 0.419 | $473,832 | $334,589 |

* **Conclusión:** El modelo reducido por AIC logra un desempeño equivalente con mayor parsimonia al eliminar `Landsize` y `Propertycount`.

# Documentación
* [Informe Completo del Proyecto (PDF)](docs/Melbourne_Housing_Analysis_Report.pdf)

# Coautores del Proyecto
* Andrés Peña
* Maximiliano Gallardo
* Patricio Frez
* Alejandro Martínez
