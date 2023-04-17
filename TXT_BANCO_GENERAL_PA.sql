CREATE OR REPLACE PROCEDURE TXT_BANCO_GENERAL_PA  (PCOD_EMPRESA            IN  NUMBER,
                                                   PCOD_BANCO              IN  NUMBER,
                                                   PFECHA_D                IN  DATE,
                                                   PFECHA_H                IN  DATE,
                                                   PCOD_BASE               IN  VARCHAR2, 
                                                   PPROCESO                IN  VARCHAR2,
                                                   PSTATUS                 IN  VARCHAR2,
                                                   PDESCRIPCION            IN  VARCHAR2,
                                                   PSEPARADOR              IN  VARCHAR2,
                                                   PTXT_DIR                IN  VARCHAR2,   
                                                   PARCHIVO                IN  VARCHAR2,
                                                   PSESION                 IN  NUMBER,
                                                   PTIPO_REPORTE           IN  VARCHAR2,
                                                   MG_ERROR                OUT VARCHAR2) IS 
--            
MFILE        UTL_FILE.FILE_TYPE;
RUTA         VARCHAR2(150)   := PTXT_DIR;
MLINEA       VARCHAR2(32767);
VCONTADOR    NUMBER := 0;
-- CURSOR
 CURSOR GER IS
            (SELECT DECODE(PTIPO_REPORTE, 'N', SUBSTR(DECODE(OT.EXTRANJERO,'S',OT.ID_FISCAL,OT.ID_FISCAL), 1, 15)
                                             , TRANSLATE(SUBSTR(T.PRIMER_NOMBRE||' '||T.PRIMER_APELLIDO, 1, 15), 'Ò·ÈÌÛ˙‡ËÏÚ˘„ı‚ÍÓÙÙ‰ÎÔˆ¸Á—¡…Õ”⁄¿»Ã“Ÿ√’¬ Œ‘€ƒÀœ÷‹«', 'naeiouaeiouaoaeiooaeioucNAEIOUAEIOUAOAEIOOAEIOUC')) ID_BENEFICIARIO, --SI ES ACREEDORES--
                    DECODE(PTIPO_REPORTE, 'N', TRANSLATE(SUBSTR(T.PRIMER_NOMBRE    ||' '||T.SEGUNDO_NOMBRE||' '||T.PRIMER_APELLIDO||' '||T.SEGUNDO_APELLIDO, 1, 22)
                                        ,'Ò·ÈÌÛ˙‡ËÏÚ˘„ı‚ÍÓÙÙ‰ÎÔˆ¸Á—¡…Õ”⁄¿»Ã“Ÿ√’¬ Œ‘€ƒÀœ÷‹«', 'naeiouaeiouaoaeiooaeioucNAEIOUAEIOUAOAEIOOAEIOUC') 
                                             , NB_BANCO)  NOMBRE_BENEFICIARIO, --SI ES ACREEDORES--
                    SUBSTR(NVL(B.ID_BANCARIO,B.COD_BANCO),1,9) RUTA_DESTINO,
                    SUBSTR(TB.NU_CUENTA,1,17) CUENTA_DESTINO,  
                    --         
                    SUBSTR(DECODE(BT.TIPO_CUENTA, 'A', '04', 'C', '03', '07'), 1, 2) PRODUCTO_DESTINO, --SI ES ACREEDORES--
                    --                         
                    SUBSTR(TRIM(TO_CHAR(TB.MONTO,'999999990.00')),1,11) MONTO,
                    'C' TIPO_PAGO,
                    'REF*TXT**'||SUBSTR(PDESCRIPCION,1,80)||'\' REFERENCIA_TEXTO,
                     DECODE(PSEPARADOR,'PC',';','C',',','A','@','T',CHR(9),'P','|') SEPARADOR
            FROM BANCOS_TRABAJADORES BT,
                 TRABAJADORES1 T,
                 TEMP_BANCOS TB,
                 BANCOS B,
                 ORIGEN_TRABAJADORES OT    
            WHERE T.COD_EMPRESA    = PCOD_EMPRESA
            AND   T.COD_EMPRESA    = BT.COD_EMPRESA
            AND   T.COD_TRABAJADOR = BT.COD_TRABAJADOR
            AND   T.COD_EMPRESA    = TB.COD_EMPRESA
            AND   T.COD_TRABAJADOR = TB.COD_TRABAJADOR
            AND   BT.COD_BANCO     = TB.COD_BANCO
            AND   BT.NU_CUENTA     = TB.NU_CUENTA
            AND   BT.COD_BASE      = TB.COD_BASE --DPERAZA
            AND   TB.SESION        = PSESION
            AND   BT.COD_BANCO     = B.COD_BANCO
            AND  T.COD_EMPRESA     = OT.COD_EMPRESA
            AND   T.COD_TRABAJADOR = OT.COD_TRABAJADOR)
            ORDER BY SUBSTR(TB.NU_CUENTA,1,17),T.COD_TRABAJADOR;--DPERAZA
BEGIN
    MFILE := UTL_FILE.FOPEN(PTXT_DIR, PARCHIVO,'w');
    --ESTA VERSION DE DISTRIBUCION_MONTO REALIZA SUMA DE CONCEPTOS DE BASE DEFINIBLE SIN IMPORTAR EL TIPO DE CONCEPTO--
    DISTRIBUCION_MONTO_V3 (PCOD_EMPRESA,PCOD_BASE,PSTATUS,PFECHA_D,PFECHA_H,PPROCESO,PSESION, PTIPO_REPORTE);
    --  
    FOR X IN GER
    LOOP
        IF VCONTADOR <> 0 THEN
            UTL_FILE.PUT_LINE(MFILE,MLINEA||CHR(13));
        END IF;
        --
        VCONTADOR := VCONTADOR + 1;
        --
        MLINEA :=
                X.ID_BENEFICIARIO||X.SEPARADOR||           
                X.NOMBRE_BENEFICIARIO||X.SEPARADOR|| 
                X.RUTA_DESTINO||X.SEPARADOR|| 
                X.CUENTA_DESTINO||X.SEPARADOR|| 
                X.PRODUCTO_DESTINO||X.SEPARADOR|| 
                X.MONTO||X.SEPARADOR|| 
                X.TIPO_PAGO||X.SEPARADOR|| 
                X.REFERENCIA_TEXTO;
    END LOOP;
    --
    UTL_FILE.PUT_LINE(MFILE,MLINEA||CHR(13));
    --
    IF (utl_file.is_open(MFILE)) THEN 
        UTL_FILE.FCLOSE(MFILE); 
    END IF;  
EXCEPTION 
       WHEN OTHERS THEN
            MG_ERROR := SQLERRM;
            IF (utl_file.is_open(MFILE)) THEN 
                UTL_FILE.FCLOSE(MFILE); 
            END IF; 
END;
/