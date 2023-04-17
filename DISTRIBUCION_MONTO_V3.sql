CREATE OR REPLACE PROCEDURE DISTRIBUCION_MONTO_V3 (PEMPR      IN   NUMBER,
                                                   PBASE         IN   VARCHAR2,
                                                   PSTATUS       IN   VARCHAR2,
                                                   FECHA_D       IN   DATE,
                                                   FECHA_H       IN   DATE,
                                                   PPROCESO      IN   VARCHAR2,
                                                   PSESION       IN   VARCHAR2,
                                                   PTIPO_REPORTE IN VARCHAR2 DEFAULT NULL) IS --esto es para determinar si tipo planilla o acreedores

   MFECHA_D DATE := NVL(FECHA_D, FECHA_H);
   MFECHA_H DATE := NVL(FECHA_H, FECHA_D);
   MCOD_BASE    BASES_VALORACION.COD_BASE%TYPE;
   --
   --
   CURSOR D IS SELECT NOMI.COD_TRABAJADOR,
                      TRAB.COD_CONTRATO,
                      TRAB.FECHA_INGRESO,
                      TRAB.TIPO_PAGO,
                      SUM(DECODE (BAVA.DEFINIBLE, 'S', NVL (NOMI.MONTO, 0)
                                                           , DECODE(CONC.TIPO, 'A', NOMI.MONTO, -NOMI.MONTO))
                          ) MONTO_NOM,
                      BAVA.COD_BASE COD_BASE    
                 FROM NOMINAS NOMI,
                      TRABAJADORES TRAB,
                      CONCEPTOS CONC,
                      BASES_VALORACION BAVA               
                WHERE NOMI.COD_EMPRESA    = PEMPR
                  AND (NOMI.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                  AND NOMI.STATUS         = PSTATUS
                  --AND DECODE(PTIPO_REP_PDF, 'PDF', DECODE(NOMI.STATUS, 'I', 'N', NOMI.STATUS ), NOMI.STATUS) = PSTATUS --PARA SER UTILIZADO SIN IMPORTAR ESTATUS REPORTE RELACION DE PAGO--
                  AND TRAB.COD_EMPRESA    = NOMI.COD_EMPRESA
                  AND TRAB.COD_TRABAJADOR = NOMI.COD_TRABAJADOR
                  AND (TRAB.FECHA_EGRESO  IS NULL OR TRAB.FECHA_EGRESO >= MFECHA_D)
                  AND CONC.COD_CONCEPTO   = NOMI.COD_CONCEPTO
                  AND ((BAVA.DEFINIBLE    = 'N' AND CONC.CLASE = 'N')
                   OR (BAVA.DEFINIBLE     = 'S' AND NOMI.COD_CONCEPTO IN (SELECT BASE.COD_CONCEPTO
                                                                          FROM CONCEPTO_BASES BASE
                                                                          WHERE BASE.COD_BASE = PBASE)))
                  AND BAVA.COD_BASE       = PBASE
                  AND ((PPROCESO = 'V' AND NOMI.FECHA BETWEEN MFECHA_D AND MFECHA_H) OR NOMI.FECHA = MFECHA_H)
                  AND (NOMI.COD_EMPRESA, NOMI.COD_TRABAJADOR) IN (SELECT BANCO_TRAB.COD_EMPRESA, BANCO_TRAB.COD_TRABAJADOR
                                                                  FROM BANCOS_TRABAJADORES BANCO_TRAB
                                                                  WHERE BANCO_TRAB.COD_EMPRESA  = NOMI.COD_EMPRESA
                                                                  AND BANCO_TRAB.COD_TRABAJADOR = NOMI.COD_TRABAJADOR
                                                                  AND BANCO_TRAB.COD_BASE       = PBASE
                                                                  AND BANCO_TRAB.FECHA_INICIAL  = (SELECT MAX(BT.FECHA_INICIAL)
                                                                                                   FROM BANCOS_TRABAJADORES BT
                                                                                                   WHERE BT.COD_EMPRESA  = BANCO_TRAB.COD_EMPRESA
                                                                                                   AND BT.COD_TRABAJADOR = BANCO_TRAB.COD_TRABAJADOR
                                                                                                   AND BT.TIPO_CUENTA    = BANCO_TRAB.TIPO_CUENTA
                                                                                                   AND BT.COD_BASE       = BANCO_TRAB.COD_BASE
                                                                                                   AND BT.COD_BANCO      = BANCO_TRAB.COD_BANCO
                                                                                                   AND BT.FECHA_INICIAL  <= MFECHA_H))
                  AND (TO_CHAR(TRAB.COD_CONTRATO) IN (SELECT TR.PARAMETRO
                                                      FROM TEMPORAL_REPORTES TR
                                                      WHERE TR.COLUMNA = 'COD_CONTRATO'
                                                      AND TR.SESION    = TO_NUMBER(PSESION))
                                                      OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                                                    FROM TEMPORAL_REPORTES TR2
                                                                                    WHERE TR2.COLUMNA = 'COD_CONTRATO'))
                  AND (TO_CHAR(TRAB.COD_TRABAJADOR) IN (SELECT TR.PARAMETRO
                                                        FROM TEMPORAL_REPORTES TR
                                                        WHERE TR.COLUMNA = 'COD_TRABAJADOR'
                                                        AND TR.SESION    = TO_NUMBER(PSESION))
                                                        OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                                                      FROM TEMPORAL_REPORTES TR2
                                                                                      WHERE TR2.COLUMNA = 'COD_TRABAJADOR'))
                  AND PTIPO_REPORTE = 'N'
             GROUP BY NOMI.COD_TRABAJADOR,
                      TRAB.COD_CONTRATO,
                      TRAB.FECHA_INGRESO,
                      TRAB.TIPO_PAGO,
                      BAVA.COD_BASE
UNION ALL
SELECT NOMI.COD_TRABAJADOR,
                      TRAB.COD_CONTRATO,
                      TRAB.FECHA_INGRESO,
                      TRAB.TIPO_PAGO,
                      SUM(DECODE (BAVA.DEFINIBLE, 'S', NVL (NOMI.MONTO, 0)
                                                           , DECODE(CONC.TIPO, 'A', NOMI.MONTO, -NOMI.MONTO))
                          ) MONTO_NOM,
                      BAVA.COD_BASE COD_BASE    
                 FROM NOMINAS NOMI,
                      TRABAJADORES TRAB,
                      CONCEPTOS CONC,
                      BASES_VALORACION BAVA               
                WHERE NOMI.COD_EMPRESA  = PEMPR
                AND (NOMI.PROCESO       = PPROCESO OR PPROCESO IS NULL)                 
                AND NOMI.STATUS         IN ('N', 'I')
                AND TRAB.COD_EMPRESA    = NOMI.COD_EMPRESA
                AND TRAB.COD_TRABAJADOR = NOMI.COD_TRABAJADOR
                AND (TRAB.FECHA_EGRESO  IS NULL OR TRAB.FECHA_EGRESO >= MFECHA_D)
                AND CONC.COD_CONCEPTO = NOMI.COD_CONCEPTO
                AND  (BAVA.DEFINIBLE = 'S' AND NOMI.COD_CONCEPTO IN (SELECT BASE.COD_CONCEPTO
                                                                     FROM CONCEPTO_BASES BASE
                                                                    WHERE BASE.COD_BASE = BAVA.COD_BASE))
                AND ((PPROCESO IN ('V', 'N') AND NOMI.FECHA BETWEEN MFECHA_D AND MFECHA_H)) --OR NOMI.FECHA = MFECHA_H)
                /*AND ((BAVA.DEFINIBLE    = 'N' AND CONC.CLASE = 'N')
                   OR (BAVA.DEFINIBLE     = 'S' AND NOMI.COD_CONCEPTO IN (SELECT BASE.COD_CONCEPTO
                                                                          FROM CONCEPTO_BASES BASE
                                                                          WHERE BASE.COD_BASE = PBASE)))*/
                --AND BAVA.COD_BASE       = PBASE                  
                AND (NOMI.COD_EMPRESA, NOMI.COD_TRABAJADOR) IN (SELECT BANCO_TRAB.COD_EMPRESA, BANCO_TRAB.COD_TRABAJADOR
                                                                    FROM BANCOS_TRABAJADORES BANCO_TRAB
                                                                   WHERE BANCO_TRAB.COD_EMPRESA    = NOMI.COD_EMPRESA
                                                                     AND BANCO_TRAB.COD_TRABAJADOR = NOMI.COD_TRABAJADOR
                                                                     AND BANCO_TRAB.COD_BASE = BAVA.COD_BASE
                                                                     AND BANCO_TRAB.FECHA_INICIAL = (SELECT MAX(BT.FECHA_INICIAL)
                                                                                                       FROM BANCOS_TRABAJADORES BT
                                                                                                      WHERE BT.COD_EMPRESA = BANCO_TRAB.COD_EMPRESA
                                                                                                        AND BT.COD_TRABAJADOR = BANCO_TRAB.COD_TRABAJADOR
                                                                                                        AND BT.TIPO_CUENTA = BANCO_TRAB.TIPO_CUENTA
                                                                                                        AND BT.COD_BASE = BANCO_TRAB.COD_BASE
                                                                                                        AND BT.COD_BANCO = BANCO_TRAB.COD_BANCO
                                                                                                        AND BT.FECHA_INICIAL <= MFECHA_H))
                AND (TO_CHAR(TRAB.COD_CONTRATO) IN (SELECT TR.PARAMETRO
                                                      FROM TEMPORAL_REPORTES TR
                                                      WHERE TR.COLUMNA = 'COD_CONTRATO'
                                                      AND TR.SESION = TO_NUMBER(PSESION))
                                                      OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                                                    FROM TEMPORAL_REPORTES TR2
                                                                                    WHERE TR2.COLUMNA = 'COD_CONTRATO'))
                AND (TO_CHAR(TRAB.COD_TRABAJADOR) IN (SELECT TR.PARAMETRO
                                                        FROM TEMPORAL_REPORTES TR
                                                        WHERE TR.COLUMNA = 'COD_TRABAJADOR'
                                                        AND TR.SESION = TO_NUMBER(PSESION))
                                                        OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                                                      FROM TEMPORAL_REPORTES TR2
                                                                                      WHERE TR2.COLUMNA = 'COD_TRABAJADOR'))
                AND (BAVA.COD_BASE IN (SELECT TR.PARAMETRO
                                                        FROM TEMPORAL_REPORTES TR
                                                        WHERE TR.COLUMNA = 'COD_BASE'
                                                        AND TR.SESION = TO_NUMBER(PSESION))
                                                        OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                                                      FROM TEMPORAL_REPORTES TR2
                                                                                      WHERE TR2.COLUMNA = 'COD_BASE'))
                AND PTIPO_REPORTE = 'A'
             GROUP BY NOMI.COD_TRABAJADOR,
                      TRAB.COD_CONTRATO,
                      TRAB.FECHA_INGRESO,
                      TRAB.TIPO_PAGO,
                      BAVA.COD_BASE;
                      --
 CURSOR C (TRABAJADOR NUMBER) IS
      SELECT   BANCO.TIPO_CUENTA TIPO,
               BANCO.FECHA_INICIAL,
               BANCO.NU_CUENTA,
               BANCO.PRIORIDAD,
               BANCO.MONTO,
               BANCO.BALANCE,
               BANCO.COD_TRABAJADOR,
               BANCO.PORCENTAJE,
               BANCO.COD_BANCO
          FROM BANCOS_TRABAJADORES BANCO
         WHERE BANCO.COD_EMPRESA    = PEMPR
           AND BANCO.COD_TRABAJADOR = TRABAJADOR
           AND BANCO.COD_BASE       = MCOD_BASE 
           AND BANCO.FECHA_INICIAL  = (SELECT MAX(BT.FECHA_INICIAL)
                                       FROM BANCOS_TRABAJADORES BT
                                       WHERE BT.COD_EMPRESA  = BANCO.COD_EMPRESA
                                       AND BT.COD_TRABAJADOR = BANCO.COD_TRABAJADOR
                                       AND BT.COD_BASE       = BANCO.COD_BASE
                                       AND BT.COD_BANCO      = BANCO.COD_BANCO
                                       AND BT.TIPO_CUENTA    = BANCO.TIPO_CUENTA
                                       AND BT.FECHA_INICIAL  <= MFECHA_H)
      ORDER BY BANCO.COD_TRABAJADOR, 
               BANCO.PRIORIDAD;
               MONTO_TRAB   NUMBER;
               MONTO_RES    NUMBER;
               FECHA        DATE;
               TRABAJADOR   TRABAJADORES1.COD_TRABAJADOR%TYPE;
               NRO_REG      NUMBER := 0;
--
BEGIN
   ROLLBACK;
   FOR D1 IN D LOOP
       MONTO_RES  := D1.MONTO_NOM;      
       TRABAJADOR := D1.COD_TRABAJADOR;
       MCOD_BASE  := D1.COD_BASE;
       --raise_application_error(-20003, 'loop '||D1.MONTO_NOM);
       
       FOR C1 IN C (D1.COD_TRABAJADOR) LOOP

           MONTO_TRAB := 0;
           --raise_application_error(-20003, 'if 3 MONTO_RES '||MONTO_RES||' C1.PORCENTAJE '||C1.PORCENTAJE);
           IF C1.BALANCE = 'S' AND MONTO_RES > 0 THEN
              MONTO_TRAB := MONTO_RES;
              MONTO_RES := 0;
           ELSIF NVL (C1.MONTO, 0) > 0 AND MONTO_RES > 0 THEN
              --raise_application_error(-20003, 'if');
              IF MONTO_RES > C1.MONTO THEN
                 MONTO_TRAB := C1.MONTO;
              ELSE
                 MONTO_TRAB := MONTO_RES;
                 MONTO_RES := 0;
              END IF;
           ELSIF NVL (C1.PORCENTAJE, 0) > 0 AND MONTO_RES > 0 THEN
              MONTO_TRAB := ROUND (MONTO_RES * (C1.PORCENTAJE / 100), 2);
              --raise_application_error(-20003, 'if 3 ');
              IF MONTO_TRAB > MONTO_RES THEN
                 MONTO_TRAB := MONTO_RES;
                 MONTO_RES := 0;
              END IF;
           ELSIF MONTO_RES > 0 THEN
              --raise_application_error(-20003, 'if 4 ');
              MONTO_RES := MONTO_RES - MONTO_TRAB;
           ELSE
             --raise_application_error(-20003, 'else ');
             MONTO_TRAB := 0;
           END IF;
           MONTO_RES := MONTO_RES - MONTO_TRAB;
           IF MONTO_TRAB > 0 THEN
              NRO_REG := NRO_REG + 1;
              INSERT INTO TEMP_BANCOS
                 (COD_EMPRESA, COD_TRABAJADOR, COD_BANCO, NU_CUENTA, TIPO_CUENTA, SESION, MONTO, NRO_REGISTRO,COD_BASE)--DPERAZA
              VALUES
                 (PEMPR, D1.COD_TRABAJADOR, C1.COD_BANCO, C1.NU_CUENTA, C1.TIPO, PSESION, MONTO_TRAB, NRO_REG,D1.COD_BASE);
                 --raise_application_error(-20003, 'inserto');
           END IF;
       END LOOP;
   END LOOP;
END;
/