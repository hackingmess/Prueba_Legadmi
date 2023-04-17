CREATE OR REPLACE PROCEDURE ACUM_BASE_DIAS_EFEC_LAB_CR (PCOD_EMPRESA         IN NUMBER,
                                                        PCOD_TRABAJADOR      IN NUMBER,
                                                        PSUCURSAL            IN NUMBER,
                                                        PCOD_CONCEPTO        IN NUMBER,
                                                        PCOD_CONTRATO        IN NUMBER,
                                                        PFECHA_NOMINA        IN DATE,
                                                        PUNIDAD_PROM_MOVIL   IN VARCHAR2,  
                                                        PTIEMPO_PROM_MOVIL   IN NUMBER,
                                                        PCANT                IN NUMBER,
                                                        PVNLUA               IN VARCHAR2,
                                                        PFECHA_INGRESO       IN DATE,
                                                        PSTATUS              IN VARCHAR2,
                                                        PVALOR               OUT NUMBER,    --MONTO_FINAL
                                                        PVALOR1              OUT NUMBER) IS --CANTIDAD_FINAL
                                                        
VFECHA_INICIAL          DATE;
VDIAS_A_PROCESAR        NUMBER := 0;
VACUM_DIAS_PROCESADOS   NUMBER := 0;
VDIAS_INCAP             NUMBER := 0;
VMONTO_ACUM_MES         NUMBER := 0;
VMONTO_ACUM_POS_LIQ     NUMBER := 0;
VMONTO_PARCIAL          NUMBER := 0;
VACUM_DIAS_PARCIAL      NUMBER := 0;
VACUM_DEL_MES           NUMBER := 0;
FECHA_CONCEP_CONT       DATE;
CANT_CONCEP_CONT        NUMBER := 0;
VCANT                   NUMBER := PCANT;

BEGIN
--
    BEGIN
        SELECT MAX (CC1.FECHA_INICIAL),CANTIDAD
        INTO FECHA_CONCEP_CONT,CANT_CONCEP_CONT
        FROM CONCEPTOS_CONTRATOS1 CC1
        WHERE CC1.COD_CONTRATO = PCOD_CONTRATO
        AND CC1.COD_CONCEPTO   = PCOD_CONCEPTO
        AND (CC1.FECHA_FINAL   >= PFECHA_NOMINA OR CC1.FECHA_FINAL IS NULL)
        AND CC1.FECHA_INICIAL  <= PFECHA_NOMINA
        GROUP BY CANTIDAD;
    EXCEPTION WHEN OTHERS THEN
        CANT_CONCEP_CONT := VCANT;
    END;
    --
    IF VCANT > CANT_CONCEP_CONT THEN
        CANT_CONCEP_CONT := VCANT - CANT_CONCEP_CONT;
        VCANT            := VCANT - CANT_CONCEP_CONT;
    ELSIF  VCANT < CANT_CONCEP_CONT THEN 
        CANT_CONCEP_CONT := VCANT - CANT_CONCEP_CONT;
        VCANT            := VCANT + ABS(CANT_CONCEP_CONT);  
    ELSE
        CANT_CONCEP_CONT := 0;
    END IF;
    --
    IF PVNLUA <> 'L' THEN
        --DBMS_OUTPUT.PUT_LINE('**********INICIO DE FORMULA DE ACUM CONCEPTO '||PCOD_CONCEPTO);
        --DBMS_OUTPUT.PUT_LINE('PARAMETROS DE ENTRADA: ');
        --DBMS_OUTPUT.PUT_LINE('PCOD_EMPRESA '||PCOD_EMPRESA);
        --DBMS_OUTPUT.PUT_LINE('PCOD_TRABAJADOR '||PCOD_TRABAJADOR);
        --DBMS_OUTPUT.PUT_LINE('PSUCURSAL '||PSUCURSAL);
        --DBMS_OUTPUT.PUT_LINE('PCOD_CONCEPTO '||PCOD_CONCEPTO);
        --DBMS_OUTPUT.PUT_LINE('PCOD_CONTRATO '||PCOD_CONTRATO);
        --DBMS_OUTPUT.PUT_LINE('PFECHA_NOMINA '||PFECHA_NOMINA);
        --DBMS_OUTPUT.PUT_LINE('PUNIDAD_PROM_MOVIL '||PUNIDAD_PROM_MOVIL);
        --DBMS_OUTPUT.PUT_LINE('PTIEMPO_PROM_MOVIL '||PTIEMPO_PROM_MOVIL);
        --DBMS_OUTPUT.PUT_LINE('VCANT '||VCANT);
        --DBMS_OUTPUT.PUT_LINE('PVNLUA '||PVNLUA);
        --DBMS_OUTPUT.PUT_LINE('PFECHA_INGRESO '||PFECHA_INGRESO);
        --ULTIMO DIA DEL MES CALENDARIO CERRADO ANTERIOR A LA NÓMINA EN PROCESO.
        VFECHA_INICIAL :=  FIRST_DAY(PFECHA_NOMINA)-1;
        --DBMS_OUTPUT.PUT_LINE('Fecha inicial VFECHA_INICIAL '||VFECHA_INICIAL);
        --OBTIENE LOS DIAS A PROCESAR SEGUN LA CONVERSION DE UPM Y TPM PARAMETRIZADO A NIVEL DE CONCEPTOS CONTRATOS
        VDIAS_A_PROCESAR := CONVIERTE(PCOD_EMPRESA, PSUCURSAL, PTIEMPO_PROM_MOVIL, PUNIDAD_PROM_MOVIL, 'D', PFECHA_NOMINA);
        --DBMS_OUTPUT.PUT_LINE('Dias a procesar VDIAS_A_PROCESAR '||VDIAS_A_PROCESAR);
        --
        IF PFECHA_INGRESO > VFECHA_INICIAL THEN
            PVALOR  := 0;
            PVALOR1 := 0; 
            --DBMS_OUTPUT.PUT_LINE('Si la fecha de ingreso es mayor a la VFECHA_INICIAL termina aqui y coloca 0 para los dos valores de pvalores ');
        ELSIF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN     
             --DBMS_OUTPUT.PUT_LINE('Si la fecha de ingreso esta entre el primer dia de la fecha inicial y la fecha  VFECHA_INICIAL entra aqui');
            BEGIN
                SELECT NVL(SUM(NOM.CANTIDAD),0) CANT
                 INTO VDIAS_INCAP
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.PROCESO        IN ( PVNLUA,'M')
                AND NOM.STATUS         != 'P'
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCED
                                         FROM PROCEDENCIAS_COMBINADAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO;
            EXCEPTION 
                WHEN OTHERS THEN
                VDIAS_INCAP := 0;    
            END;
            --DBMS_OUTPUT.PUT_LINE('Busca los dias de capacidad (Cantidad) de las procedencias combinadas del concepto desde '||FIRST_DAY(VFECHA_INICIAL)||' hasta VFECHA_INICIAL '||VFECHA_INICIAL||' dias obtenidos VDIAS_INCAP: '||VDIAS_INCAP);
        --
            BEGIN 
                SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
                 INTO VMONTO_ACUM_MES
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.PROCESO        IN ( PVNLUA,'M')
                AND NOM.STATUS         != 'P'
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                         FROM PROCEDENCIAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
                GROUP BY C.TIPO;
            EXCEPTION 
                WHEN OTHERS THEN
                VMONTO_ACUM_MES := 0;    
            END;
            --DBMS_OUTPUT.PUT_LINE('Busca el monto del mes de las procedencias  del concepto desde '||FIRST_DAY(VFECHA_INICIAL)||' hasta VFECHA_INICIAL '||VFECHA_INICIAL||' dias obtenidos VDIAS_INCAP: '||VDIAS_INCAP);
            --
            IF NVL(VCANT,0) - VDIAS_INCAP <= 0 THEN
                PVALOR1 := 0;
                PVALOR  := 0;
                --DBMS_OUTPUT.PUT_LINE('Si la cantidad de  VDIAS_INCAP es mayor a la cantidad no paga nada');
            ELSE 
                PVALOR1 := LEAST(PFECHA_INGRESO - FIRST_DAY(VFECHA_INICIAL),30);
                --jj CASO 21517 validacion del 28 o 29 de febrero. EN NOMINA NO SE OCUPA ESTO PORQUE SACA LA CANT DESDE LA CANTIDAD DE CONCEPTOS 
               /*IF TO_CHAR(VFECHA_INICIAL,'MM') = '02' AND PVALOR1 IN (28,29) THEN
                    PVALOR1 := NVL(VCANT,30);
                END IF;*/
                PVALOR1 := NVL(VCANT,0) - NVL(VDIAS_INCAP,0) - PVALOR1;
                --DBMS_OUTPUT.PUT_LINE('PVALOR1  '||PVALOR1); 
                IF PVALOR1 > 0 THEN  
                    PVALOR  := VMONTO_ACUM_MES;  
                ELSE 
                    PVALOR  := 0;
                    PVALOR1 := 0;
                END IF;
                --DBMS_OUTPUT.PUT_LINE('PVALOR  := VMONTO_ACUM_MES '||PVALOR);
            END IF; 
            --
        ELSE
        --VDIAS_A_PROCESAR: Cantidad de dias totales que debe procesar 
        --VACUM_DIAS_PROCESADOS: Variable que va almacenando la cantidad de dias procesados.  
        --VDIAS_INCAP: Obtiene la cantidad del mes procesando segun parametrizacion de procedencia combinadas (Dias de incapacidad). 
        --VMONTO_ACUM_MES: Obtiene el monto del mes procesando segun parametrizacion de las procedencias.. 
        --VFECHA_INICIAL: Variable que va almaceando la fecha del mes que se esta procesando. (FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL)(Inicio y fin de mes).
            --DBMS_OUTPUT.PUT_LINE('ENTRA AL ELSE VDIAS_A_PROCESAR '||VDIAS_A_PROCESAR||'    VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
            WHILE VDIAS_A_PROCESAR > VACUM_DIAS_PROCESADOS 
            LOOP
                BEGIN
                    SELECT  NVL(SUM(NOM.CANTIDAD),0) CANT
                     INTO VDIAS_INCAP
                    FROM NOMINAS NOM,
                         CONCEPTOS C
                    WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                    AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                    AND NOM.PROCESO        IN ( PVNLUA,'M')
                    AND NOM.STATUS         != 'P'
                    AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCED
                                             FROM PROCEDENCIAS_COMBINADAS P
                                             WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                             AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                    AND C.COD_CONCEPTO = NOM.COD_CONCEPTO;
                EXCEPTION 
                    WHEN OTHERS THEN
                    VDIAS_INCAP := 0;    
                END;
                --DBMS_OUTPUT.PUT_LINE('PERIODO DESDE:  '||FIRST_DAY(VFECHA_INICIAL) ||'    HASTA'||VFECHA_INICIAL);
                --DBMS_OUTPUT.PUT_LINE('VDIAS_INCAP: '||VDIAS_INCAP);
                --
                BEGIN 
                    SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
                     INTO VMONTO_ACUM_MES
                    FROM NOMINAS NOM,
                         CONCEPTOS C
                    WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                    AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                    AND NOM.PROCESO        IN ( PVNLUA,'M')
                    AND NOM.STATUS         != 'P'
                    AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                             FROM PROCEDENCIAS P
                                             WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                             AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                    AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
                    GROUP BY C.TIPO;
                EXCEPTION 
                    WHEN OTHERS THEN
                    VMONTO_ACUM_MES := 0;    
                END;
                --DBMS_OUTPUT.PUT_LINE('VMONTO_ACUM_MES: '||VMONTO_ACUM_MES);
               --
               --Valida si el acumulado da menor a 0 ya que esto significa que va acumular dias en negativo y no esta correcto (PREGUNTARLE A ITZA) 
               --
                VACUM_DEL_MES :=  NVL(VCANT,0) - NVL(VDIAS_INCAP,0);  
                --DBMS_OUTPUT.PUT_LINE('VACUM_DEL_MES :=  NVL(VCANT,0) - NVL(VDIAS_INCAP,0)  '||VACUM_DEL_MES );
                -- 
                IF VACUM_DEL_MES <= 0 THEN
                --DBMS_OUTPUT.PUT_LINE('ENTRA 1');
                    IF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN
                        PVALOR1 := NVL(PVALOR1,0) + 0; 
                        PVALOR  := NVL(PVALOR,0)  + 0;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 1.1');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        EXIT;
                    ELSE--ENTRA POR AQUI PARA CONTINUAR CON EL LOOP YA QUE LA FECHA DE INGRESO DEL TRABAJADOR NO ESTA ENTRE EL MES PROCESANDO Y EL TRABAJADOR TIENE DIAS ADICIONALES PARA PROCESAR SEGUN LA ACUMULACION DE DIAS (VARIABLE: VACUM_DIAS_PROCESADOS) 
                        --Coloca la cantidad de dias a procesar 
                        VACUM_DIAS_PROCESADOS := NVL(VACUM_DIAS_PROCESADOS,0) + 0;
                        PVALOR  := NVL(PVALOR,0)  + 0;
                        PVALOR1 := NVL(PVALOR1,0) + 0;
                         --DBMS_OUTPUT.PUT_LINE('ENTRA 1.2');
                         --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                    END IF; 
                ELSIF VDIAS_A_PROCESAR <= (VACUM_DIAS_PROCESADOS + NVL(VCANT,0) - NVL(VDIAS_INCAP,0)) AND TO_CHAR(PFECHA_NOMINA ,'MM') <> TO_CHAR(PFECHA_INGRESO ,'MM') THEN
                --AQUI VA IR UNA SALIDA FINAL 
                --AQUI TAMBIEN HAY QUE VALIDA LA FECHA DE INGRESO DEL TRABAJADOR POR AQUELLO. y lo de 31 
                    VACUM_DIAS_PARCIAL := (VACUM_DIAS_PROCESADOS + NVL(VCANT,0) - NVL(VDIAS_INCAP,0)) - VDIAS_A_PROCESAR; 
                    VACUM_DIAS_PARCIAL := NVL(VCANT,0) - VACUM_DIAS_PARCIAL;
                    VMONTO_PARCIAL     := (VMONTO_ACUM_MES/NVL(VCANT,1))*VACUM_DIAS_PARCIAL;
                    --
                    PVALOR  := NVL(PVALOR,0)  + VMONTO_PARCIAL;
                    PVALOR1 := VACUM_DIAS_PROCESADOS + VACUM_DIAS_PARCIAL;
                    --DBMS_OUTPUT.PUT_LINE('ENTRA 2');
                    --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                    --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                    --DBMS_OUTPUT.PUT_LINE('VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
                    EXIT;
                ELSE
                --DBMS_OUTPUT.PUT_LINE('ENTRA 3');
                --AQUI VA IR LO QUE DEBERIA HACER EL LOOP DEBERIA VALIDA INICIALMENTE LA FECHA DE INGRESO DEL TRABAJADOR YA QUE SINO DEBE HACER ALGO DIFERENTE.
                --AQUI TAMBIEN HAY QUE VALIDA LA FECHA DE INGRESO DEL TRABAJADOR POR AQUELLO. y lo de 31 
                    IF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN--
                        VACUM_DIAS_PARCIAL := LEAST(PFECHA_INGRESO - FIRST_DAY(VFECHA_INICIAL),30);
                        ----jj CASO 21517 validacion del 28 o 29 de febrero. EN NOMINA NO SE OCUPA ESTO PORQUE SACA LA CANT DESDE LA CANTIDAD DE CONCEPTOS
                        /*IF TO_CHAR(VFECHA_INICIAL,'MM') = '02' AND VACUM_DIAS_PARCIAL IN (28,29) THEN
                            VACUM_DIAS_PARCIAL := NVL(VCANT,30);
                        END IF;*/
                        --
                        VACUM_DIAS_PARCIAL := NVL(VCANT,0) - VACUM_DIAS_PARCIAL - NVL(VDIAS_INCAP,0);
                        IF VACUM_DIAS_PARCIAL <= 0 THEN 
                            VACUM_DIAS_PARCIAL := 0;
                            VMONTO_ACUM_MES    := 0;
                        END IF;
                        PVALOR1 := VACUM_DIAS_PROCESADOS + VACUM_DIAS_PARCIAL; 
                        PVALOR  := NVL(PVALOR,0) + VMONTO_ACUM_MES;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 3.1');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        EXIT;
                 ELSE--ENTRA POR AQUI PARA CONTINUAR CON EL LOOP YA QUE LA FECHA DE INGRESO DEL TRABAJADOR NO ESTA ENTRE EL MES PROCESANDO Y EL TRABAJADOR TIENE DIAS ADICIONALES PARA PROCESAR SEGUN LA ACUMULACION DE DIAS (VARIABLE: VACUM_DIAS_PROCESADOS) 
                        --Coloca la cantidad de dias a procesar 
                        VACUM_DIAS_PROCESADOS := NVL(VACUM_DIAS_PROCESADOS,0) + NVL(VCANT,0) - NVL(VDIAS_INCAP,0);
                        PVALOR  := NVL(PVALOR,0)  + VMONTO_ACUM_MES;
                        PVALOR1 := VACUM_DIAS_PROCESADOS;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 3.2');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        --DBMS_OUTPUT.PUT_LINE('VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
                    END IF; 
                END IF; 
                --
                VFECHA_INICIAL := ADD_MONTHS(VFECHA_INICIAL,-1);
                 --DBMS_OUTPUT.PUT_LINE('VFECHA_INICIAL := ADD_MONTHS(VFECHA_INICIAL,-1)   '||VFECHA_INICIAL);
            END LOOP;
        END IF;
        PVALOR1 := PVALOR1 - NVL(VCANT,0);
    ELSE --@@@@@@@@@@@@@@@@@@@@@@@@@@@@AQUI INICIA LA LOGICA DE LIQUIDACION@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         --@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@. 
        --DBMS_OUTPUT.PUT_LINE('**********INICIO DE FORMULA DE ACUM CONCEPTO '||PCOD_CONCEPTO);
        --DBMS_OUTPUT.PUT_LINE('PARAMETROS DE ENTRADA: ');
        --DBMS_OUTPUT.PUT_LINE('PCOD_EMPRESA '||PCOD_EMPRESA);
        --DBMS_OUTPUT.PUT_LINE('PCOD_TRABAJADOR '||PCOD_TRABAJADOR);
        --DBMS_OUTPUT.PUT_LINE('PSUCURSAL '||PSUCURSAL);
        --DBMS_OUTPUT.PUT_LINE('PCOD_CONCEPTO '||PCOD_CONCEPTO);
        --DBMS_OUTPUT.PUT_LINE('PCOD_CONTRATO '||PCOD_CONTRATO);
        --DBMS_OUTPUT.PUT_LINE('PFECHA_NOMINA '||PFECHA_NOMINA);
        --DBMS_OUTPUT.PUT_LINE('PUNIDAD_PROM_MOVIL '||PUNIDAD_PROM_MOVIL);
        --DBMS_OUTPUT.PUT_LINE('PTIEMPO_PROM_MOVIL '||PTIEMPO_PROM_MOVIL);
        --DBMS_OUTPUT.PUT_LINE('VCANT '||VCANT);
        --DBMS_OUTPUT.PUT_LINE('PVNLUA '||PVNLUA);
        --DBMS_OUTPUT.PUT_LINE('PFECHA_INGRESO '||PFECHA_INGRESO);
        --ULTIMO DIA DEL MES CALENDARIO CERRADO ANTERIOR A LA NÓMINA EN PROCESO.
        VFECHA_INICIAL :=  PFECHA_NOMINA;
        --DBMS_OUTPUT.PUT_LINE('Fecha inicial VFECHA_INICIAL '||VFECHA_INICIAL);
        --OBTIENE LOS DIAS A PROCESAR SEGUN LA CONVERSION DE UPM Y TPM PARAMETRIZADO A NIVEL DE CONCEPTOS CONTRATOS
        VDIAS_A_PROCESAR := CONVIERTE(PCOD_EMPRESA, PSUCURSAL, PTIEMPO_PROM_MOVIL, PUNIDAD_PROM_MOVIL, 'D', PFECHA_NOMINA);
        --DBMS_OUTPUT.PUT_LINE('Dias a procesar VDIAS_A_PROCESAR '||VDIAS_A_PROCESAR);
        --
        IF PFECHA_INGRESO > VFECHA_INICIAL THEN
            PVALOR  := 0;
            PVALOR1 := 0; 
            --DBMS_OUTPUT.PUT_LINE('Si la fecha de ingreso es mayor a la VFECHA_INICIAL termina aqui y coloca 0 para los dos valores de pvalores ');
        ELSIF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN     
             --DBMS_OUTPUT.PUT_LINE('Si la fecha de ingreso esta entre el primer dia de la fecha inicial y la fecha  VFECHA_INICIAL entra aqui');
            BEGIN
                SELECT  NVL(SUM(NOM.CANTIDAD),0) CANT
                 INTO VDIAS_INCAP
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCED
                                         FROM PROCEDENCIAS_COMBINADAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO;
            EXCEPTION 
                WHEN OTHERS THEN
                VDIAS_INCAP := 0;    
            END;
            --DBMS_OUTPUT.PUT_LINE('Busca los dias de capacidad (Cantidad) de las procedencias combinadas del concepto desde '||FIRST_DAY(VFECHA_INICIAL)||' hasta VFECHA_INICIAL '||VFECHA_INICIAL||' dias obtenidos VDIAS_INCAP: '||VDIAS_INCAP);
        --
            BEGIN 
                SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
                 INTO VMONTO_ACUM_MES
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                         FROM PROCEDENCIAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
                GROUP BY C.TIPO;
            EXCEPTION 
                WHEN OTHERS THEN
                VMONTO_ACUM_MES := 0;    
            END;
            --DBMS_OUTPUT.PUT_LINE('Busca el monto del mes de las procedencias  del concepto desde '||FIRST_DAY(VFECHA_INICIAL)||' hasta VFECHA_INICIAL '||VFECHA_INICIAL||' dias obtenidos VDIAS_INCAP: '||VDIAS_INCAP);
            --
            IF NVL(VCANT,0) - VDIAS_INCAP <= 0 THEN
                PVALOR1 := 0;
                PVALOR  := 0;
                --DBMS_OUTPUT.PUT_LINE('Si la cantidad de  VDIAS_INCAP es mayor a la cantidad no paga nada');
            ELSE 
                PVALOR1 := LEAST(VFECHA_INICIAL+1 - PFECHA_INGRESO,NVL(VCANT,0));
                PVALOR1 := PVALOR1 - NVL(VDIAS_INCAP,0);
                --DBMS_OUTPUT.PUT_LINE('PVALOR1  '||PVALOR1); 
                IF PVALOR1 > 0 THEN  
                    PVALOR  := VMONTO_ACUM_MES;  
                ELSE 
                    PVALOR  := 0;
                    PVALOR1 := 0;
                END IF;
                --DBMS_OUTPUT.PUT_LINE('PVALOR  := VMONTO_ACUM_MES '||PVALOR);
            END IF; 
            --
        ELSE---AQUI INICIAL LA LOGICA PARA IR ACUMULANDO 
             --DBMS_OUTPUT.PUT_LINE('AQUI INICIAL LA LOGICA PARA IR ACUMULANDO ');
            BEGIN
                SELECT  NVL(SUM(NOM.CANTIDAD),0) CANT
                 INTO VDIAS_INCAP
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCED
                                         FROM PROCEDENCIAS_COMBINADAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO;
            EXCEPTION 
                WHEN OTHERS THEN
                VDIAS_INCAP := 0;    
            END;
        --
            BEGIN 
                SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
                 INTO VMONTO_ACUM_MES
                FROM NOMINAS NOM,
                     CONCEPTOS C
                WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                         FROM PROCEDENCIAS P
                                         WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                         AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
                GROUP BY C.TIPO;
            EXCEPTION 
                WHEN OTHERS THEN
                VMONTO_ACUM_MES := 0;    
            END;
            --DBMS_OUTPUT.PUT_LINE('Busca desde '||FIRST_DAY(VFECHA_INICIAL)||' hasta VFECHA_INICIAL '||VFECHA_INICIAL);
            --DBMS_OUTPUT.PUT_LINE('VMONTO_ACUM_MES  '||VMONTO_ACUM_MES);
            --DBMS_OUTPUT.PUT_LINE('VDIAS_INCAP  '||VDIAS_INCAP);
            --
            IF NVL(VCANT,0) - VDIAS_INCAP <= 0 THEN
             --DBMS_OUTPUT.PUT_LINE('ENTRO A'); 
                PVALOR1 := 0;
                PVALOR  := 0;
                VACUM_DIAS_PROCESADOS := VDIAS_A_PROCESAR;--se hace esto para que no entre acumular 
                --DBMS_OUTPUT.PUT_LINE('Si la cantidad de  VDIAS_INCAP es mayor a la cantidad no paga nada');
            ELSE 
               --DBMS_OUTPUT.PUT_LINE('ENTRO B'); 
                VACUM_DIAS_PROCESADOS := LEAST(VFECHA_INICIAL+1 - FIRST_DAY(VFECHA_INICIAL),NVL(VCANT,0));
                --jj CASO 21517 validacion del 28 o 29 de febrero.
                IF TO_CHAR(VFECHA_INICIAL,'MM') = '02' AND VACUM_DIAS_PROCESADOS IN (28,29) THEN
                    VACUM_DIAS_PROCESADOS := NVL(VCANT,30);
                END IF;
                --
                VACUM_DIAS_PROCESADOS := VACUM_DIAS_PROCESADOS - NVL(VDIAS_INCAP,0);
                --DBMS_OUTPUT.PUT_LINE('VACUM_DIAS_PROCESADOS  '||VACUM_DIAS_PROCESADOS); 
                IF VACUM_DIAS_PROCESADOS > 0 THEN  
                   --DBMS_OUTPUT.PUT_LINE('ENTRO B.1'); 
                    PVALOR  := VMONTO_ACUM_MES; 
                    --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR); 
                ELSE 
                   --DBMS_OUTPUT.PUT_LINE('ENTRO B.2');
                    PVALOR  := 0;
                    PVALOR1 := 0;
                    VACUM_DIAS_PROCESADOS := 0;
                    --VACUM_DIAS_PROCESADOS := VDIAS_A_PROCESAR;--se hace esto para que no entre acumular 
                END IF;
                --
                IF VDIAS_A_PROCESAR <= VACUM_DIAS_PROCESADOS THEN
                    PVALOR1 := VACUM_DIAS_PROCESADOS; --Setea la cantidad ya que no va entrar al WHILE
                END IF; 
            END IF;         
            VFECHA_INICIAL :=  FIRST_DAY(PFECHA_NOMINA)-1; 
        --VDIAS_A_PROCESAR: Cantidad de dias totales que debe procesar 
        --VACUM_DIAS_PROCESADOS: Variable que va almacenando la cantidad de dias procesados.  
        --VDIAS_INCAP: Obtiene la cantidad del mes procesando segun parametrizacion de procedencia combinadas (Dias de incapacidad). 
        --VMONTO_ACUM_MES: Obtiene el monto del mes procesando segun parametrizacion de las procedencias.. 
        --VFECHA_INICIAL: Variable que va almaceando la fecha del mes que se esta procesando. (FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL)(Inicio y fin de mes).
            --DBMS_OUTPUT.PUT_LINE('ENTRA AL ELSE VDIAS_A_PROCESAR '||VDIAS_A_PROCESAR||'    VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
            WHILE VDIAS_A_PROCESAR > VACUM_DIAS_PROCESADOS 
            LOOP
                BEGIN
                    SELECT  NVL(SUM(NOM.CANTIDAD),0) CANT
                     INTO VDIAS_INCAP
                    FROM NOMINAS NOM,
                         CONCEPTOS C
                    WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                    AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                    AND NOM.STATUS         != 'P'
                    AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCED
                                             FROM PROCEDENCIAS_COMBINADAS P
                                             WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                             AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                    AND C.COD_CONCEPTO = NOM.COD_CONCEPTO;
                EXCEPTION 
                    WHEN OTHERS THEN
                    VDIAS_INCAP := 0;    
                END;
                --DBMS_OUTPUT.PUT_LINE('PERIODO DESDE:  '||FIRST_DAY(VFECHA_INICIAL) ||'    HASTA'||VFECHA_INICIAL);
                --DBMS_OUTPUT.PUT_LINE('VDIAS_INCAP: '||VDIAS_INCAP);
                --
                BEGIN 
                    SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
                     INTO VMONTO_ACUM_MES
                    FROM NOMINAS NOM,
                         CONCEPTOS C
                    WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
                    AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND NOM.FECHA          BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL
                    AND NOM.STATUS         != 'P'
                    AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                             FROM PROCEDENCIAS P
                                             WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                             AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
                    AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
                    GROUP BY C.TIPO;
                EXCEPTION 
                    WHEN OTHERS THEN
                    VMONTO_ACUM_MES := 0;    
                END;
                --DBMS_OUTPUT.PUT_LINE('VMONTO_ACUM_MES: '||VMONTO_ACUM_MES);
               --
               --Valida si el acumulado da menor a 0 ya que esto significa que va acumular dias en negativo y no esta correcto (PREGUNTARLE A ITZA) 
               --
                VACUM_DEL_MES :=  NVL(VCANT,0) - NVL(VDIAS_INCAP,0);  
                --DBMS_OUTPUT.PUT_LINE('VACUM_DEL_MES :=  NVL(VCANT,0) - NVL(VDIAS_INCAP,0)  '||VACUM_DEL_MES );
                -- 
                IF VACUM_DEL_MES <= 0 THEN
                --DBMS_OUTPUT.PUT_LINE('ENTRA 1');
                    IF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN
                        PVALOR1 := NVL(PVALOR1,0) + 0; 
                        PVALOR  := NVL(PVALOR,0)  + 0;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 1.1');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        EXIT;
                    ELSE--ENTRA POR AQUI PARA CONTINUAR CON EL LOOP YA QUE LA FECHA DE INGRESO DEL TRABAJADOR NO ESTA ENTRE EL MES PROCESANDO Y EL TRABAJADOR TIENE DIAS ADICIONALES PARA PROCESAR SEGUN LA ACUMULACION DE DIAS (VARIABLE: VACUM_DIAS_PROCESADOS) 
                        --Coloca la cantidad de dias a procesar 
                        VACUM_DIAS_PROCESADOS := NVL(VACUM_DIAS_PROCESADOS,0) + 0;
                        PVALOR  := NVL(PVALOR,0)  + 0;
                        PVALOR1 := NVL(PVALOR1,0) + 0;
                         --DBMS_OUTPUT.PUT_LINE('ENTRA 1.2');
                         --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                    END IF; 
                ELSIF VDIAS_A_PROCESAR <= (VACUM_DIAS_PROCESADOS + NVL(VCANT,0) - NVL(VDIAS_INCAP,0)) THEN
                --AQUI VA IR UNA SALIDA FINAL 
                --AQUI TAMBIEN HAY QUE VALIDA LA FECHA DE INGRESO DEL TRABAJADOR POR AQUELLO. y lo de 31 
                    VACUM_DIAS_PARCIAL := (VACUM_DIAS_PROCESADOS + NVL(VCANT,0) - NVL(VDIAS_INCAP,0)) - VDIAS_A_PROCESAR; 
                    VACUM_DIAS_PARCIAL := NVL(VCANT,0) - VACUM_DIAS_PARCIAL;
                    VMONTO_PARCIAL     := (VMONTO_ACUM_MES/NVL(VCANT,1))*VACUM_DIAS_PARCIAL;
                    --
                    PVALOR  := NVL(PVALOR,0)  + VMONTO_PARCIAL;
                    PVALOR1 := VACUM_DIAS_PROCESADOS + VACUM_DIAS_PARCIAL;
                    --DBMS_OUTPUT.PUT_LINE('ENTRA 2');
                    --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                    --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                    --DBMS_OUTPUT.PUT_LINE('VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
                    EXIT;
                ELSE
                --DBMS_OUTPUT.PUT_LINE('ENTRA 3');
                --AQUI VA IR LO QUE DEBERIA HACER EL LOOP DEBERIA VALIDA INICIALMENTE LA FECHA DE INGRESO DEL TRABAJADOR YA QUE SINO DEBE HACER ALGO DIFERENTE.
                --AQUI TAMBIEN HAY QUE VALIDA LA FECHA DE INGRESO DEL TRABAJADOR POR AQUELLO. y lo de 31 
                    IF PFECHA_INGRESO BETWEEN FIRST_DAY(VFECHA_INICIAL) AND VFECHA_INICIAL THEN--jj aqui hay que analizar 
                        VACUM_DIAS_PARCIAL := LEAST(PFECHA_INGRESO - FIRST_DAY(VFECHA_INICIAL),30);
                        --jj CASO 21517 validacion del 28 o 29 de febrero.
                        IF TO_CHAR(VFECHA_INICIAL,'MM') = '02' AND VACUM_DIAS_PARCIAL IN (28,29) THEN
                            VACUM_DIAS_PARCIAL := NVL(VCANT,30);
                        END IF;
                        --
                        VACUM_DIAS_PARCIAL := NVL(VCANT,0) - VACUM_DIAS_PARCIAL - NVL(VDIAS_INCAP,0);
                        IF VACUM_DIAS_PARCIAL <= 0 THEN 
                            VACUM_DIAS_PARCIAL := 0;
                            VMONTO_ACUM_MES    := 0;
                        END IF;
                        PVALOR1 := VACUM_DIAS_PROCESADOS + VACUM_DIAS_PARCIAL; 
                        PVALOR  := NVL(PVALOR,0) + VMONTO_ACUM_MES;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 3.1');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        EXIT;
                 ELSE--ENTRA POR AQUI PARA CONTINUAR CON EL LOOP YA QUE LA FECHA DE INGRESO DEL TRABAJADOR NO ESTA ENTRE EL MES PROCESANDO Y EL TRABAJADOR TIENE DIAS ADICIONALES PARA PROCESAR SEGUN LA ACUMULACION DE DIAS (VARIABLE: VACUM_DIAS_PROCESADOS) 
                        --Coloca la cantidad de dias a procesar 
                        VACUM_DIAS_PROCESADOS := NVL(VACUM_DIAS_PROCESADOS,0) + NVL(VCANT,0) - NVL(VDIAS_INCAP,0);
                        PVALOR  := NVL(PVALOR,0)  + VMONTO_ACUM_MES;
                        PVALOR1 := VACUM_DIAS_PROCESADOS;
                        --DBMS_OUTPUT.PUT_LINE('ENTRA 3.2');
                        --DBMS_OUTPUT.PUT_LINE('PVALOR '||PVALOR);
                        --DBMS_OUTPUT.PUT_LINE('PVALOR1 '||PVALOR1);
                        --DBMS_OUTPUT.PUT_LINE('VACUM_DIAS_PROCESADOS '||VACUM_DIAS_PROCESADOS);
                    END IF; 
                END IF; 
                --
                VFECHA_INICIAL := ADD_MONTHS(VFECHA_INICIAL,-1);
                 --DBMS_OUTPUT.PUT_LINE('VFECHA_INICIAL := ADD_MONTHS(VFECHA_INICIAL,-1)   '||VFECHA_INICIAL);
            END LOOP;
        END IF;
        --
        BEGIN--en porceso de liq hay que buscar el acum de la procedencias con fecha superior a la liq y sumarlas al resultado final.  
            SELECT  NVL(SUM(DECODE(C.TIPO,'A',NOM.MONTO,-NOM.MONTO)),0) MONTO
             INTO VMONTO_ACUM_POS_LIQ
            FROM NOMINAS NOM,
                 CONCEPTOS C
            WHERE NOM.COD_EMPRESA  = PCOD_EMPRESA
            AND NOM.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND NOM.FECHA          > PFECHA_NOMINA
            AND NOM.STATUS         != 'P'
            AND NOM.COD_CONCEPTO IN (SELECT P.COD_CONCEPTO_PROCEDE
                                     FROM PROCEDENCIAS P
                                     WHERE P.COD_CONTRATO = PCOD_CONTRATO
                                     AND P.COD_CONCEPTO   = PCOD_CONCEPTO)
            AND C.COD_CONCEPTO = NOM.COD_CONCEPTO
            GROUP BY C.TIPO;
        EXCEPTION 
            WHEN OTHERS THEN
            VMONTO_ACUM_POS_LIQ := 0;    
        END;
        --
        IF PVALOR1 > 0 THEN
            PVALOR := PVALOR + NVL(VMONTO_ACUM_POS_LIQ,0);
        END IF;  
        --
        PVALOR1 := PVALOR1 - NVL(VCANT,0);  --Se hace esto porque cuando salga de aqui el proceso de nomina le agrega el entero de la cantidad
    END IF;        
END;
/
