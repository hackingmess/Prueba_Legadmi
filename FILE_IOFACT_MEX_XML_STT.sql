CREATE OR REPLACE PACKAGE FILE_IOFACT_MEX_XML_STT AS
--*******************************************************      SONOCO          ************
FUNCTION COD_CONCEPTO_CATALOGO_IOPAC (PCOD_EMPRESA               IN   NUMBER,
                                      PCOD_TRABAJADOR            IN   NUMBER,
                                      PFECHA_D                   IN   DATE, 
                                      PFECHA_H                   IN   DATE, 
                                      PPROCESO                   IN   VARCHAR2,
                                      PREGISTRO                  IN   VARCHAR2,
                                      PTIPO_PAGO                 IN   VARCHAR2,
                                      PCOD_CONTRATO              IN   NUMBER,
                                      PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2; 
--   
FUNCTION COD_CONCEPTO_LEGADMI (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   NUMBER,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER,
                               PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2; 
--        
FUNCTION MONTO_CONCEPTO_CODI_FAC (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PCONCEPTO_IMPUESTO         IN   NUMBER,
                                  PEXENTO_GRAVADO            IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER,
                                  PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2;   
--  
FUNCTION DESCRIPCION_CONCEPTOS_IOFAC (PCOD_EMPRESA               IN   NUMBER,
                                      PCOD_TRABAJADOR            IN   NUMBER,
                                      PFECHA_D                   IN   DATE, 
                                      PFECHA_H                   IN   DATE, 
                                      PPROCESO                   IN   VARCHAR2,
                                      PREGISTRO                  IN   VARCHAR2,
                                      PCOLUM_CONCEPTO            IN   VARCHAR2,
                                      PULT_NOMBRE_DEL_CONCEP     IN   VARCHAR2,
                                      PCOD_CONTRATO              IN   NUMBER,
                                      PTIPO_PAGO                 IN   VARCHAR2,
                                      PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2;
-- 
FUNCTION CANT_DIAS_HORA_EXTRA    (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PDIA_HORA_MONTO_DESCRIP    IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2;   
--   
FUNCTION TIPO_HORAS           (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   VARCHAR2,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2;
--   
FUNCTION MONTO_CONCEPTO_DESCUENTO(PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2; 
--    
FUNCTION CANT_DIAS_MONTO_INCAP   (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PDIA_HORA_MONTO_DESCRIP    IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2;
-- 
FUNCTION TIPO_INCAP           (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   VARCHAR2,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2;
--     
FUNCTION MONTO_POR_GRUPO      (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PGRUPO                     IN   VARCHAR2)
   RETURN VARCHAR2; 
--
FUNCTION MONTO_POR_GRUPO_REGIMEN      (PCOD_EMPRESA               IN   NUMBER,
                                       PCOD_TRABAJADOR            IN   NUMBER,
                                       PFECHA_D                   IN   DATE, 
                                       PFECHA_H                   IN   DATE, 
                                       PPROCESO                   IN   VARCHAR2,
                                       PGRUPO                     IN   VARCHAR2,
                                       PREGIMEN                   IN   VARCHAR2,
                                       PCONTRATO                  IN   NUMBER,
                                       PTIPO_PAGO                 IN   VARCHAR2,
                                       PTIPO_PAGO2                IN   VARCHAR2 DEFAULT NULL,
                                       PTIPO_PAGO3                IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2; 
--  
FUNCTION CANT_POR_GRUPO       (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PGRUPO                     IN   VARCHAR2)
   RETURN NUMBER;    
--  
FUNCTION ANTIGUEDAD   (PCOD_EMPRESA               IN   NUMBER,
                       PCOD_TRABAJADOR            IN   NUMBER,
                       PFECHA_FINAL               IN   DATE, 
                       PFECHA_INGRESO             IN   DATE,
                       PFORMATO_ANTI              IN   VARCHAR2)
  RETURN VARCHAR2;
--  
FUNCTION MONTO_CONCEPTO_OTROS_PAGOS (PCOD_EMPRESA               IN   NUMBER,
                                     PCOD_TRABAJADOR            IN   NUMBER,
                                     PFECHA_D                   IN   DATE, 
                                     PFECHA_H                   IN   DATE, 
                                     PPROCESO                   IN   VARCHAR2,
                                     PREGISTRO                  IN   VARCHAR2,
                                     PCONTRATO                  IN   NUMBER,
                                     PAPLICA_TABLA_PORC         IN   VARCHAR2,
                                     PTIPO_PAGO                 IN   VARCHAR2,
                                     PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2;
--
FUNCTION MONTO_CONCEPTO_OTROS_PAGOS_REGIMEN (PCOD_EMPRESA               IN   NUMBER,
                                             PCOD_TRABAJADOR            IN   NUMBER,
                                             PFECHA_D                   IN   DATE, 
                                             PFECHA_H                   IN   DATE, 
                                             PPROCESO                   IN   VARCHAR2,
                                             PREGISTRO                  IN   VARCHAR2,
                                             PCONTRATO                  IN   NUMBER,
                                             PAPLICA_TABLA_PORC         IN   VARCHAR2,
                                             PTIPO_PAGO                 IN   VARCHAR2,
                                             PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL,
                                             PREGIMEN                   IN   VARCHAR2)
   RETURN VARCHAR2;
--                                  
PROCEDURE ANTIGUEDAD_TOTAL       (PFECHA_EMISION           IN DATE,
                                  PFECHA_INGRESO           IN DATE,
                                  PYEARS                  OUT NUMBER,                               
                                  PMONTHS                 OUT NUMBER,
                                  PDAYS                   OUT NUMBER); 
--
PROCEDURE GENERAR_IOFACT_MEX_XML (PEMPRESA                 IN NUMBER,
                                  PFECHA_D                 IN DATE,
                                  PFECHA_H                 IN DATE,
                                  PFORMATO_ANTI            IN VARCHAR2,                                
                                  PPROCESO                 IN VARCHAR2,
                                  PSESION                  IN NUMBER,
                                  PARCHIVO                 IN VARCHAR2,
                                  PTXT_DIR                 IN VARCHAR2,
                                  PREGIMEN                 IN VARCHAR2,
                                  PSERIE                   IN VARCHAR2,
                                  PFOLIO                   IN VARCHAR2,
                                  PFORMA_PAGO              IN VARCHAR2,
                                  PPERIODICIDAD            IN VARCHAR2,
                                  PNO_CERTIFICADO          IN VARCHAR2,
                                  PSUBTOTAL                IN NUMBER,
                                  PDESCUENTO               IN NUMBER,
                                  PTOTAL                   IN NUMBER,
                                  PUSO_CFDI                IN VARCHAR2,
                                  PUUID_1                  IN VARCHAR2,
                                  PUUID_2                  IN VARCHAR2,
                                  PCONCEPT_CLAVE_PROD_SERV IN VARCHAR2,
                                  PCONCEPT_NO_IDENTIFICADO IN VARCHAR2,
                                  PCONCEPTO_CANTIDAD       IN VARCHAR2,
                                  PCONCEPTO_CLAVE_UNIDAD   IN VARCHAR2,
                                  PCONCEPTO_DESCRIPCION    IN VARCHAR2,
                                  PCONCEPTO_VALOR_UNITARIO IN NUMBER,
                                  PCONCEPTO_IMPORTE        IN NUMBER,
                                  PCONCEPTO_DESCUENTO      IN NUMBER,
                                  PTIPO_NOMINA             IN VARCHAR2,
                                  PFECHA_PAGO              IN DATE,
                                  PNUM_DIAS_PAGADOS        IN NUMBER,
                                  PTOTAL_PERCEPCIONES      IN NUMBER,
                                  PTOTAL_DEDUCCIONES       IN NUMBER,
                                  PTOTAL_OTROS_PAGOS       IN NUMBER,
                                  PCVE_REGISTRO_FISCAL     IN VARCHAR2,
                                  PE_CURP                  IN VARCHAR2,
                                  PREGISTRO_PATRONAL       IN VARCHAR2,
                                  PRFC_PATRON_ORIGEN       IN VARCHAR2,
                                  PORIGEN_RECURSO          IN VARCHAR2,
                                  PRECURSO_PROPIO          IN NUMBER,
                                  PSALARIO_DIARIO_INTEGRADO IN NUMBER,          --SE LE CAMBIÓ EL NOMBRE AL PARÁMETRO DE ENTRADA
                                  PSALARIO_DIARIO           IN NUMBER,          --SE AGREGÓ UN NUEVO PARÁMERO
                                  PTOTAL_SUELDOS           IN NUMBER,
                                  PTOTAL_SEPARACION        IN NUMBER,
                                  PTOTAL_JUBILACION        IN NUMBER,
                                  PTOTAL_GRAVADO           IN NUMBER,
                                  PTOTAL_EXENTO            IN NUMBER,
                                  PCONCEPTO_IMPUESTO       IN NUMBER,
                                  PJPR_TOTAL_UNA_EXHIB     IN NUMBER,
                                  PJPR_TOTAL_PARCIALIDAD   IN NUMBER,
                                  PJPR_MONTO_DIARIO        IN NUMBER,
                                  PJPR_INGRESO_ACUM        IN NUMBER,
                                  PJPR_INGRESO_NO_ACUM     IN NUMBER,
                                  PSI_TOTAL_PAGADO         IN NUMBER,
                                  PSI_NUM_ANOS             IN NUMBER,
                                  PSI_INGRESO_ACUM         IN NUMBER,
                                  PSI_INGRESO_NO_ACUM      IN NUMBER,
                                  PTOTAL_OTRAS_DEDUCCIONES IN NUMBER,
                                  PTOTAL_IMP_RETENIDO      IN NUMBER,
                                  PBASE                    IN VARCHAR2,
                                  PPAIS                    IN VARCHAR2,
                                  PULT_NOMBRE_CONCEP       IN VARCHAR2 DEFAULT 'N',
                                  PLUGAR_EXP               IN VARCHAR2 DEFAULT NULL,
                                  PNO_ERROR               OUT NUMBER,
                                  PMG_ERROR               OUT VARCHAR2,
                                  PFECHA_INI_TIMB          IN   DATE,
                                  PFECHA_FIN_TIMB          IN   DATE,
                                  PSALARIO_BASE_COT        IN   VARCHAR2);                                         
END FILE_IOFACT_MEX_XML_STT;
--
/
CREATE OR REPLACE PACKAGE BODY FILE_IOFACT_MEX_XML_STT IS
--SONOCO
FUNCTION COD_CONCEPTO_CATALOGO_IOPAC (PCOD_EMPRESA               IN   NUMBER,
                                      PCOD_TRABAJADOR            IN   NUMBER,
                                      PFECHA_D                   IN   DATE, 
                                      PFECHA_H                   IN   DATE, 
                                      PPROCESO                   IN   VARCHAR2,
                                      PREGISTRO                  IN   VARCHAR2,
                                      PTIPO_PAGO                 IN   VARCHAR2,
                                      PCOD_CONTRATO              IN   NUMBER,
                                      PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS
--Funcion que generar el codigo segun Catalogo del IOPAC (Catalogo C_tipoPercepcion)
VCOD_CONCEPTO VARCHAR2(20); 
--   
  BEGIN
    --
    IF PDIF_PAGO = 'S' THEN
    --
      SELECT CL.CLASIFICACION
             INTO VCOD_CONCEPTO 
      FROM CODIFICACION_CONC_GOSOCKET  CL
      WHERE CL.COD_REGISTRO  = PREGISTRO
      AND   CL.COD_CONTRATO  = PCOD_CONTRATO
      AND   CL.TIPO_PAGO     = PTIPO_PAGO;
    --  
    ELSE 
    --
      SELECT CL.CLASIFICACION
             INTO VCOD_CONCEPTO 
      FROM NOMINAS N,
           CONCEPTOS C,
           CODIFICACION_CONC_GOSOCKET  CL
      WHERE N.COD_EMPRESA    = PCOD_EMPRESA
        AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
        AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
        AND N.STATUS         IN ('N','I')-- 'P'
        AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
        AND N.COD_CONCEPTO   = C.COD_CONCEPTO
        AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
        AND CL.COD_REGISTRO  = PREGISTRO
        AND CL.COD_CONTRATO  = PCOD_CONTRATO
        AND CL.TIPO_PAGO     = PTIPO_PAGO 
        GROUP BY CL.CLASIFICACION, 
               C.COD_CONCEPTO, 
               C.NB_CONCEPTO; 
    --           
    END IF;                       
    RETURN VCOD_CONCEPTO; 
    --   
    EXCEPTION
    WHEN TOO_MANY_ROWS THEN
    VCOD_CONCEPTO := 'Existen Multiples filas para el registro '||PREGISTRO||' en la tabla CODIFICACION_CONC_GOSOCKET, solo debe existir un solo registro.';
    RETURN VCOD_CONCEPTO;
    WHEN OTHERS THEN 
    --
     BEGIN
     --
     IF PTIPO_PAGO = 'P' THEN 
     --
          SELECT CL.CLASIFICACION
                 INTO VCOD_CONCEPTO 
          FROM NOMINAS N,
               CONCEPTOS C,
               CODIFICACION_CONC_GOSOCKET  CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CL.COD_CONCEPTO2
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO
            AND CL.TIPO_PAGO     = PTIPO_PAGO 
            GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO;
         RETURN VCOD_CONCEPTO;
     --
     ELSE 
         RETURN NULL;     
     END IF;    
     EXCEPTION WHEN OTHERS THEN
     RETURN VCOD_CONCEPTO;  
     END;
        END;

FUNCTION COD_CONCEPTO_LEGADMI (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   NUMBER,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER,
                               PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS
--Funcion que generar el codigo segun Catalogo del IOPAC (Catalogo C_tipoPercepcion)
--Campos del 93 - 144 (P_CLAVE)  
--Campos del 217 - 264  
VCOD_CONCEPTO VARCHAR2(250); 
--    
    BEGIN
            IF PDIF_PAGO = 'S' THEN
                  --
                  SELECT NVL(CL.COD_CONCEPTO,CL.COD_CONCEPTO2)
                         INTO VCOD_CONCEPTO
                  FROM CODIFICACION_CONC_GOSOCKET CL
                  WHERE CL.COD_REGISTRO  = PREGISTRO
                  AND   CL.TIPO_PAGO     = PTIPO_PAGO
                  AND   CL.COD_CONTRATO  = PCOD_CONTRATO; 
                  --
            ELSE 
                  SELECT C.COD_CONCEPTO
                         INTO VCOD_CONCEPTO
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,NVL(CL.COD_CONCEPTO,CL.COD_CONCEPTO2))
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO 
                    GROUP BY CL.CLASIFICACION, 
                           C.COD_CONCEPTO, 
                           C.NB_CONCEPTO;
                   --
            END IF;   
            --                         
            IF PTIPO_PAGO = 'P' AND LENGTH(VCOD_CONCEPTO) <=3 THEN                     
             VCOD_CONCEPTO :=  'P'||LPAD(VCOD_CONCEPTO,3,'0'); 
            ELSIF PTIPO_PAGO = 'O' AND LENGTH(VCOD_CONCEPTO) <=3 THEN  
             VCOD_CONCEPTO :=  'P'||LPAD(VCOD_CONCEPTO,3,'0'); 
            END IF;  
            --                      
            RETURN VCOD_CONCEPTO;    
    EXCEPTION
    WHEN OTHERS THEN 
            BEGIN
            IF PTIPO_PAGO = 'P' THEN
                  SELECT NVL(CL.COD_CONCEPTO,CL.COD_CONCEPTO2)
                         INTO VCOD_CONCEPTO
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,(CL.COD_CONCEPTO2))
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO 
                    GROUP BY CL.CLASIFICACION, 
                             CL.COD_CONCEPTO,
                             C.NB_CONCEPTO;
                           
                IF PTIPO_PAGO = 'P' AND LENGTH(VCOD_CONCEPTO) <=3 THEN                                        
                 VCOD_CONCEPTO :=  'P'||LPAD(VCOD_CONCEPTO,3,'0'); 
                ELSIF PTIPO_PAGO = 'O' AND LENGTH(VCOD_CONCEPTO) <=3 THEN
                 VCOD_CONCEPTO :=  'P'||LPAD(VCOD_CONCEPTO,3,'0'); 
                END IF;                    
                RETURN VCOD_CONCEPTO;
            ELSE 
                RETURN NULL;     
            END IF; 
            EXCEPTION WHEN OTHERS THEN
                RETURN VCOD_CONCEPTO;   
            END;
    END;
FUNCTION MONTO_CONCEPTO_CODI_FAC (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PCONCEPTO_IMPUESTO         IN   NUMBER,
                                  PEXENTO_GRAVADO            IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER,
                                  PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS
--Funcion que generar el codigo segun el concepto que existe en Legadmi (Catalogo C_tipoPercepcion)
--Campos del 93 - 144 (P_CLAVE)  
VMONTO_CONCEPTO_GRA VARCHAR2(60); 
VCOD_CONCEPTO_GRA   NUMBER; 
VMONTO_CONCEPTO_EX  VARCHAR2(60); 
VCOD_CONCEPTO_EX    NUMBER; 
    BEGIN
    --
            BEGIN
                  SELECT SUM(N.MONTO),
                         C.COD_CONCEPTO
                         INTO VMONTO_CONCEPTO_GRA,
                              VCOD_CONCEPTO_GRA 
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND N.COD_CONCEPTO   = (CL.COD_CONCEPTO)
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO  
                  GROUP BY CL.CLASIFICACION, 
                           C.COD_CONCEPTO, 
                           C.NB_CONCEPTO; 
            EXCEPTION WHEN OTHERS THEN 
            VMONTO_CONCEPTO_GRA := NULL;                   
            END;
    --                
            BEGIN
                  SELECT SUM(N.MONTO),
                         C.COD_CONCEPTO
                         INTO VMONTO_CONCEPTO_EX,
                              VCOD_CONCEPTO_EX 
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND N.COD_CONCEPTO   = CL.COD_CONCEPTO2
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO  
                  GROUP BY CL.CLASIFICACION, 
                           C.COD_CONCEPTO, 
                           C.NB_CONCEPTO; 
            EXCEPTION WHEN OTHERS THEN 
            VMONTO_CONCEPTO_EX := NULL;               
            END;                                         
    --     
          IF PEXENTO_GRAVADO = 'G' AND VMONTO_CONCEPTO_GRA IS NOT NULL THEN
             RETURN VMONTO_CONCEPTO_GRA;
          ELSIF PEXENTO_GRAVADO = 'G' AND VMONTO_CONCEPTO_GRA IS NULL  AND VMONTO_CONCEPTO_EX IS NOT NULL THEN
           VMONTO_CONCEPTO_GRA := 0;    
           RETURN VMONTO_CONCEPTO_GRA;  
          ELSIF PEXENTO_GRAVADO = 'E' AND VMONTO_CONCEPTO_EX IS NOT NULL THEN 
             RETURN VMONTO_CONCEPTO_EX; 
          ELSIF PEXENTO_GRAVADO = 'E' AND VMONTO_CONCEPTO_EX IS NULL  AND VMONTO_CONCEPTO_GRA IS NOT NULL THEN 
           VMONTO_CONCEPTO_EX := 0;    
           RETURN VMONTO_CONCEPTO_EX; 
          ELSE
          RETURN NULL;     
          END IF; 
    END;
--
FUNCTION DESCRIPCION_CONCEPTOS_IOFAC (PCOD_EMPRESA               IN   NUMBER,
                                      PCOD_TRABAJADOR            IN   NUMBER,
                                      PFECHA_D                   IN   DATE, 
                                      PFECHA_H                   IN   DATE, 
                                      PPROCESO                   IN   VARCHAR2,
                                      PREGISTRO                  IN   VARCHAR2,
                                      PCOLUM_CONCEPTO            IN   VARCHAR2,
                                      PULT_NOMBRE_DEL_CONCEP     IN   VARCHAR2,
                                      PCOD_CONTRATO              IN   NUMBER,
                                      PTIPO_PAGO                 IN   VARCHAR2,
                                      PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS
VDESCRIPTION VARCHAR2(250);
BEGIN
--
    IF PCOLUM_CONCEPTO = 1 THEN 
    --
          SELECT DECODE (PULT_NOMBRE_DEL_CONCEP,'S',NVL(CC.NB_ALTERNO,C.NB_CONCEPTO), 
                 DECODE(TRIM(CL.CLASIFICACION)  ,'001','Sueldos, Salarios  Rayas y Jornales'
                                                ,'002','Gratificación Anual (Aguinaldo)'
                                                ,'003','Participación de los Trabajadores en las Utilidades PTU'
                                                ,'004','Reembolso de Gastos Médicos Dentales y Hospitalarios'
                                                ,'005','Fondo de Ahorro'
                                                ,'006','Caja de ahorro'
                                                ,'009','Contribuciones a Cargo del Trabajador Pagadas por el Patrón'
                                                ,'010','Premios por puntualidad'
                                                ,'011','Prima de Seguro de vida'
                                                ,'012','Seguro de Gastos Médicos Mayores'
                                                ,'013','Cuotas Sindicales Pagadas por el Patrón'
                                                ,'014','Subsidios por incapacidad'
                                                ,'015','Becas para trabajadores y/o hijos'
                                                ,'019','Horas extra'
                                                ,'020','Prima dominical'
                                                ,'021','Prima vacacional'
                                                ,'022','Prima por antigüedad'
                                                ,'023','Pagos por separación'
                                                ,'024','Seguro de retiro'
                                                ,'025','Indemnizaciones'
                                                ,'026','Reembolso por funeral'
                                                ,'027','Cuotas de seguridad social pagadas por el patrón'
                                                ,'028','Comisiones'
                                                ,'029','Vales de despensa'
                                                ,'030','Vales de restaurante'
                                                ,'031','Vales de gasolina'
                                                ,'032','Vales de ropa'
                                                ,'033','Ayuda para renta'
                                                ,'034','Ayuda para artículos escolares'
                                                ,'035','Ayuda para anteojos'
                                                ,'036','Ayuda para transporte'
                                                ,'037','Ayuda para gastos de funeral'
                                                ,'038','Otros ingresos por salarios'
                                                ,'039','Jubilaciones, pensiones o haberes de retiro'
                                                ,'044','Jubilaciones, pensiones o haberes de retiro en parcialidades'
                                                ,'045','Ingresos en acciones o títulos valor que representan bienes'
                                                ,'046','Ingresos asimilados a salarios'
                                                ,'047','Alimentación'
                                                ,'048','Habitación'
                                                ,'049','Premios por asistencia'
                                                ,'050','Viáticos',' '))
          INTO VDESCRIPTION 
          FROM NOMINAS N,
               CONCEPTOS C,
               CONCEPTOS_CONTRATOS1 CC,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CC.COD_CONCEPTO
            AND CC.COD_CONTRATO  = PCOD_CONTRATO
            AND CC.FECHA_INICIAL = (SELECT MAX(CC1.FECHA_INICIAL)
                                     FROM CONCEPTOS_CONTRATOS1 CC1
                                     WHERE CC1.COD_CONTRATO = CC.COD_CONTRATO
                                     AND   CC1.COD_CONCEPTO = CC.COD_CONCEPTO
                                     AND   CC1.FECHA_INICIAL <= PFECHA_H)
            AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO)
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO                         
            GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO,
                   CC.NB_ALTERNO;
    ELSIF PCOLUM_CONCEPTO = 2 THEN 
          SELECT DECODE (PULT_NOMBRE_DEL_CONCEP,'S',NVL(CC.NB_ALTERNO,C.NB_CONCEPTO),
                 DECODE(TRIM(CL.CLASIFICACION) ,'001','Seguridad social',
                                                '002','ISR',
                                                '003','Aportaciones a retiro, cesantía en edad avanzada y vejez.',
                                                '004','Otros',
                                                '005','Aportaciones a Fondo de vivienda',
                                                '006','Descuento por incapacidad',
                                                '007','Pensión alimenticia',
                                                '008','Renta',
                                                '009','Préstamos provenientes del Fondo Nacional de la Vivienda para los Trabajadores',
                                                '010','Pago por crédito de vivienda',
                                                '011','Pago de abonos INFONACOT',
                                                '012','Anticipo de salarios',
                                                '013','Pagos hechos con exceso al trabajador',
                                                '014','Errores',
                                                '015','Pérdidas',
                                                '016','Averías',
                                                '017','Adquisición de artículos producidos por la empresa o establecimiento',
                                                '018','Cuotas para la constitución y fomento de sociedades cooperativas y de cajas de ahorro',
                                                '019','Cuotas sindicales',
                                                '020','Ausencia (Ausentismo)',
                                                '021','Cuotas obrero patronales',
                                                '022','Impuestos Locales',
                                                '023','Aportaciones voluntarias',
                                                '024','Ajuste en Gratificación Anual (Aguinaldo) Exento',
                                                '025','Ajuste en Gratificación Anual (Aguinaldo) Gravado',
                                                '026','Ajuste en Participación de los Trabajadores en las Utilidades PTU Exento',
                                                '027','Ajuste en Participación de los Trabajadores en las Utilidades PTU Gravado',
                                                '028','Ajuste en Reembolso de Gastos Médicos Dentales y Hospitalarios Exento',
                                                '029','Ajuste en Fondo de ahorro Exento',
                                                '030','Ajuste en Caja de ahorro Exento',
                                                '031','Ajuste en Contribuciones a Cargo del Trabajador Pagadas por el Patrón Exento',
                                                '032','Ajuste en Premios por puntualidad Gravado',
                                                '033','Ajuste en Prima de Seguro de vida Exento',
                                                '034','Ajuste en Seguro de Gastos Médicos Mayores Exento',
                                                '035','Ajuste en Cuotas Sindicales Pagadas por el Patrón Exento',
                                                '036','Ajuste en Subsidios por incapacidad Exento',
                                                '037','Ajuste en Becas para trabajadores y/o hijos Exento',
                                                '038','Ajuste en Horas extra Exento',
                                                '039','Ajuste en Horas extra Gravado',
                                                '040','Ajuste en Prima dominical Exento',
                                                '041','Ajuste en Prima dominical Gravado',
                                                '042','Ajuste en Prima vacacional Exento',
                                                '043','Ajuste en Prima vacacional Gravado',
                                                '044','Ajuste en Prima por antigüedad Exento',
                                                '045','Ajuste en Prima por antigüedad Gravado',
                                                '046','Ajuste en Pagos por separación Exento',
                                                '047','Ajuste en Pagos por separación Gravado',
                                                '048','Ajuste en Seguro de retiro Exento',
                                                '049','Ajuste en Indemnizaciones Exento',
                                                '050','Ajuste en Indemnizaciones Gravado',
                                                '051','Ajuste en Reembolso por funeral Exento',
                                                '052','Ajuste en Cuotas de seguridad social pagadas por el patrón Exento',
                                                '053','Ajuste en Comisiones Gravado',
                                                '054','Ajuste en Vales de despensa Exento',
                                                '055','Ajuste en Vales de restaurante Exento',
                                                '056','Ajuste en Vales de gasolina Exento',
                                                '057','Ajuste en Vales de ropa Exento',
                                                '058','Ajuste en Ayuda para renta Exento',
                                                '059','Ajuste en Ayuda para artículos escolares Exento',
                                                '060','Ajuste en Ayuda para anteojos Exento',
                                                '061','Ajuste en Ayuda para transporte Exento',
                                                '062','Ajuste en Ayuda para gastos de funeral Exento',
                                                '063','Ajuste en Otros ingresos por salarios Exento',
                                                '064','Ajuste en Otros ingresos por salarios Gravado',
                                                '065','Ajuste en Jubilaciones, pensiones o haberes de retiro Exento',
                                                '066','Ajuste en Jubilaciones, pensiones o haberes de retiro Gravado',
                                                '067','Ajuste en Pagos por separación Acumulable',
                                                '068','Ajuste en Pagos por separación No acumulable',
                                                '069','Ajuste en Jubilaciones, pensiones o haberes de retiro Acumulable',
                                                '070','Ajuste en Jubilaciones, pensiones o haberes de retiro No acumulable',
                                                '071','Ajuste en Subsidio para el empleo (efectivamente entregado al trabajador)',
                                                '072','Ajuste en Ingresos en acciones o títulos valor que representan bienes Exento',
                                                '073','Ajuste en Ingresos en acciones o títulos valor que representan bienes Gravado',
                                                '074','Ajuste en Alimentación Exento',
                                                '075','Ajuste en Alimentación Gravado',
                                                '076','Ajuste en Habitación Exento',
                                                '077','Ajuste en Habitación Gravado',
                                                '078','Ajuste en Premios por asistencia',
                                                '079','Ajuste en Pagos distintos a los listados y que no deben considerarse como ingreso por sueldos, salarios o ingresos asimilados.',
                                                '080','Ajuste en Viáticos gravados',
                                                '081','Ajuste en Viáticos (entregados al trabajador)',
                                                '082','Ajuste en Fondo de ahorro Gravado',
                                                '083','Ajuste en Caja de ahorro Gravado',
                                                '084','Ajuste en Prima de Seguro de vida Gravado',
                                                '085','Ajuste en Seguro de Gastos Médicos Mayores Gravado',
                                                '086','Ajuste en Subsidios por incapacidad Gravado',
                                                '087','Ajuste en Becas para trabajadores y/o hijos Gravado',
                                                '088','Ajuste en Seguro de retiro Gravado',
                                                '089','Ajuste en Vales de despensa Gravado',
                                                '090','Ajuste en Vales de restaurante Gravado',
                                                '091','Ajuste en Vales de gasolina Gravado',
                                                '092','Ajuste en Vales de ropa Gravado',
                                                '093','Ajuste en Ayuda para renta Gravado',
                                                '094','Ajuste en Ayuda para artículos escolares Gravado',
                                                '095','Ajuste en Ayuda para anteojos Gravado',
                                                '096','Ajuste en Ayuda para transporte Gravado',
                                                '097','Ajuste en Ayuda para gastos de funeral Gravado',
                                                '098','Ajuste a ingresos asimilados a salarios gravados',
                                                '099','Ajuste a ingresos por sueldos y salarios gravados',
                                                '100','Ajuste en Viáticos exentos',
                                                '101','ISR Retenido de ejercicio anterior', ' '))
          INTO VDESCRIPTION 
          FROM NOMINAS N,
               CONCEPTOS C,
               CONCEPTOS_CONTRATOS1 CC,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CC.COD_CONCEPTO
            AND CC.COD_CONTRATO  = PCOD_CONTRATO
            AND CC.FECHA_INICIAL = (SELECT MAX(CC1.FECHA_INICIAL)
                                     FROM CONCEPTOS_CONTRATOS1 CC1
                                     WHERE CC1.COD_CONTRATO = CC.COD_CONTRATO
                                     AND   CC1.COD_CONCEPTO = CC.COD_CONCEPTO
                                     AND   CC1.FECHA_INICIAL <= PFECHA_H)
            AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO)
            AND CL.COD_REGISTRO  = PREGISTRO 
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO                         
            GROUP BY CL.CLASIFICACION, 
                     C.COD_CONCEPTO, 
                     C.NB_CONCEPTO,
                     CC.NB_ALTERNO;
    ELSE
    --
        IF PDIF_PAGO = 'S' THEN 
        --
          SELECT DECODE(TRIM(CL.CLASIFICACION)  ,'001','Reintegro de ISR pagado en exceso'
                                                ,'002','Subsidio para el empleo'
                                                ,'003','Viáticos'
                                                ,'004','Aplicación de saldo a favor por compensación anual'
                                                ,'005','Reintegro de ISR retenido en exceso de ejercicio anterior'
                                                ,'999','Pagos distintos a los listados y que no deben considerarse como ingreso por sueldos, salarios o ingresos asimilados')
          INTO VDESCRIPTION 
          FROM CODIFICACION_CONC_GOSOCKET CL
          WHERE CL.COD_REGISTRO  = PREGISTRO
          AND   CL.TIPO_PAGO     = PTIPO_PAGO
          AND   CL.COD_CONTRATO  = PCOD_CONTRATO;   
        ELSE --
         --   raise_application_error (-20001,'Entro '||PDIF_PAGO);
         SELECT DECODE (PULT_NOMBRE_DEL_CONCEP,'S',NVL(CC.NB_ALTERNO,C.NB_CONCEPTO),DECODE(TRIM(CL.CLASIFICACION)  ,'001','Reintegro de ISR pagado en exceso'
                                                ,'002','Subsidio para el empleo'
                                                ,'003','Viáticos'
                                                ,'004','Aplicación de saldo a favor por compensación anual'
                                                ,'005','Reintegro de ISR retenido en exceso de ejercicio anterior'
                                                ,'999','Pagos distintos a los listados y que no deben considerarse como ingreso por sueldos, salarios o ingresos asimilados'))--DPERAZA
          INTO VDESCRIPTION 
          FROM NOMINAS N,
               CONCEPTOS C,
               CONCEPTOS_CONTRATOS1 CC,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO       
            AND N.COD_CONCEPTO   = CC.COD_CONCEPTO
            AND CC.COD_CONTRATO  = PCOD_CONTRATO
            AND CC.FECHA_INICIAL = (SELECT MAX(CC1.FECHA_INICIAL)
                                     FROM CONCEPTOS_CONTRATOS1 CC1
                                     WHERE CC1.COD_CONTRATO = CC.COD_CONTRATO
                                     AND   CC1.COD_CONCEPTO = CC.COD_CONCEPTO
                                     AND   CC1.FECHA_INICIAL <= PFECHA_H)
            AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO)
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO                         
            GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO,
                   CC.NB_ALTERNO;     
        END IF;                       
    END IF;        
  RETURN REPLACE(VDESCRIPTION,'.',NULL); 
  --     
EXCEPTION
    WHEN TOO_MANY_ROWS THEN
    VDESCRIPTION := 'Existen Multiples filas para el registro '||PREGISTRO||' en la tabla CODIFICACION_CONC_GOSOCKET, solo debe existir un solo registro.';
    RETURN VDESCRIPTION;
    WHEN OTHERS THEN
        BEGIN
          IF PTIPO_PAGO = 'P' THEN
            IF PCOLUM_CONCEPTO = 1 THEN 
                  SELECT DECODE (PULT_NOMBRE_DEL_CONCEP,'S',NVL(CC.NB_ALTERNO,C.NB_CONCEPTO),
                         DECODE(TRIM(CL.CLASIFICACION)  ,'001','Sueldos, Salarios  Rayas y Jornales'
                                                        ,'002','Gratificación Anual (Aguinaldo)'
                                                        ,'003','Participación de los Trabajadores en las Utilidades PTU'
                                                        ,'004','Reembolso de Gastos Médicos Dentales y Hospitalarios'
                                                        ,'005','Fondo de Ahorro'
                                                        ,'006','Caja de ahorro'
                                                        ,'009','Contribuciones a Cargo del Trabajador Pagadas por el Patrón'
                                                        ,'010','Premios por puntualidad'
                                                        ,'011','Prima de Seguro de vida'
                                                        ,'012','Seguro de Gastos Médicos Mayores'
                                                        ,'013','Cuotas Sindicales Pagadas por el Patrón'
                                                        ,'014','Subsidios por incapacidad'
                                                        ,'015','Becas para trabajadores y/o hijos'
                                                        ,'019','Horas extra'
                                                        ,'020','Prima dominical'
                                                        ,'021','Prima vacacional'
                                                        ,'022','Prima por antigüedad'
                                                        ,'023','Pagos por separación'
                                                        ,'024','Seguro de retiro'
                                                        ,'025','Indemnizaciones'
                                                        ,'026','Reembolso por funeral'
                                                        ,'027','Cuotas de seguridad social pagadas por el patrón'
                                                        ,'028','Comisiones'
                                                        ,'029','Vales de despensa'
                                                        ,'030','Vales de restaurante'
                                                        ,'031','Vales de gasolina'
                                                        ,'032','Vales de ropa'
                                                        ,'033','Ayuda para renta'
                                                        ,'034','Ayuda para artículos escolares'
                                                        ,'035','Ayuda para anteojos'
                                                        ,'036','Ayuda para transporte'
                                                        ,'037','Ayuda para gastos de funeral'
                                                        ,'038','Otros ingresos por salarios'
                                                        ,'039','Jubilaciones, pensiones o haberes de retiro'
                                                        ,'044','Jubilaciones, pensiones o haberes de retiro en parcialidades'
                                                        ,'045','Ingresos en acciones o títulos valor que representan bienes'
                                                        ,'046','Ingresos asimilados a salarios'
                                                        ,'047','Alimentación'
                                                        ,'048','Habitación'
                                                        ,'049','Premios por asistencia'
                                                        ,'050','Viáticos'))
                  INTO VDESCRIPTION 
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CONCEPTOS_CONTRATOS1 CC,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND CL.COD_CONCEPTO  = CC.COD_CONCEPTO
                    AND CC.COD_CONTRATO  = PCOD_CONTRATO
                    AND CC.FECHA_INICIAL = (SELECT MAX(CC1.FECHA_INICIAL)
                                             FROM CONCEPTOS_CONTRATOS1 CC1
                                             WHERE CC1.COD_CONTRATO = CC.COD_CONTRATO
                                             AND   CC1.COD_CONCEPTO = CC.COD_CONCEPTO
                                             AND   CC1.FECHA_INICIAL <= PFECHA_H)
                    AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO2)
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO                          
                    GROUP BY CL.CLASIFICACION, 
                             C.COD_CONCEPTO, 
                             C.NB_CONCEPTO,
                             CC.NB_ALTERNO;
            ELSIF PCOLUM_CONCEPTO = 2 THEN 
                  SELECT DECODE (PULT_NOMBRE_DEL_CONCEP,'S',NVL(CC.NB_ALTERNO,C.NB_CONCEPTO),
                         DECODE(TRIM(CL.CLASIFICACION) ,'001','Seguridad social',
                                                        '002','ISR',
                                                        '003','Aportaciones a retiro, cesantía en edad avanzada y vejez.',
                                                        '004','Otros',
                                                        '005','Aportaciones a Fondo de vivienda',
                                                        '006','Descuento por incapacidad',
                                                        '007','Pensión alimenticia',
                                                        '008','Renta',
                                                        '009','Préstamos provenientes del Fondo Nacional de la Vivienda para los Trabajadores',
                                                        '010','Pago por crédito de vivienda',
                                                        '011','Pago de abonos INFONACOT',
                                                        '012','Anticipo de salarios',
                                                        '013','Pagos hechos con exceso al trabajador',
                                                        '014','Errores',
                                                        '015','Pérdidas',
                                                        '016','Averías',
                                                        '017','Adquisición de artículos producidos por la empresa o establecimiento',
                                                        '018','Cuotas para la constitución y fomento de sociedades cooperativas y de cajas de ahorro',
                                                        '019','Cuotas sindicales',
                                                        '020','Ausencia (Ausentismo)',
                                                        '021','Cuotas obrero patronales',
                                                        '022','Impuestos Locales',
                                                        '023','Aportaciones voluntarias',
                                                        '024','Ajuste en Gratificación Anual (Aguinaldo) Exento',
                                                        '025','Ajuste en Gratificación Anual (Aguinaldo) Gravado',
                                                        '026','Ajuste en Participación de los Trabajadores en las Utilidades PTU Exento',
                                                        '027','Ajuste en Participación de los Trabajadores en las Utilidades PTU Gravado',
                                                        '028','Ajuste en Reembolso de Gastos Médicos Dentales y Hospitalarios Exento',
                                                        '029','Ajuste en Fondo de ahorro Exento',
                                                        '030','Ajuste en Caja de ahorro Exento',
                                                        '031','Ajuste en Contribuciones a Cargo del Trabajador Pagadas por el Patrón Exento',
                                                        '032','Ajuste en Premios por puntualidad Gravado',
                                                        '033','Ajuste en Prima de Seguro de vida Exento',
                                                        '034','Ajuste en Seguro de Gastos Médicos Mayores Exento',
                                                        '035','Ajuste en Cuotas Sindicales Pagadas por el Patrón Exento',
                                                        '036','Ajuste en Subsidios por incapacidad Exento',
                                                        '037','Ajuste en Becas para trabajadores y/o hijos Exento',
                                                        '038','Ajuste en Horas extra Exento',
                                                        '039','Ajuste en Horas extra Gravado',
                                                        '040','Ajuste en Prima dominical Exento',
                                                        '041','Ajuste en Prima dominical Gravado',
                                                        '042','Ajuste en Prima vacacional Exento',
                                                        '043','Ajuste en Prima vacacional Gravado',
                                                        '044','Ajuste en Prima por antigüedad Exento',
                                                        '045','Ajuste en Prima por antigüedad Gravado',
                                                        '046','Ajuste en Pagos por separación Exento',
                                                        '047','Ajuste en Pagos por separación Gravado',
                                                        '048','Ajuste en Seguro de retiro Exento',
                                                        '049','Ajuste en Indemnizaciones Exento',
                                                        '050','Ajuste en Indemnizaciones Gravado',
                                                        '051','Ajuste en Reembolso por funeral Exento',
                                                        '052','Ajuste en Cuotas de seguridad social pagadas por el patrón Exento',
                                                        '053','Ajuste en Comisiones Gravado',
                                                        '054','Ajuste en Vales de despensa Exento',
                                                        '055','Ajuste en Vales de restaurante Exento',
                                                        '056','Ajuste en Vales de gasolina Exento',
                                                        '057','Ajuste en Vales de ropa Exento',
                                                        '058','Ajuste en Ayuda para renta Exento',
                                                        '059','Ajuste en Ayuda para artículos escolares Exento',
                                                        '060','Ajuste en Ayuda para anteojos Exento',
                                                        '061','Ajuste en Ayuda para transporte Exento',
                                                        '062','Ajuste en Ayuda para gastos de funeral Exento',
                                                        '063','Ajuste en Otros ingresos por salarios Exento',
                                                        '064','Ajuste en Otros ingresos por salarios Gravado',
                                                        '065','Ajuste en Jubilaciones, pensiones o haberes de retiro Exento',
                                                        '066','Ajuste en Jubilaciones, pensiones o haberes de retiro Gravado',
                                                        '067','Ajuste en Pagos por separación Acumulable',
                                                        '068','Ajuste en Pagos por separación No acumulable',
                                                        '069','Ajuste en Jubilaciones, pensiones o haberes de retiro Acumulable',
                                                        '070','Ajuste en Jubilaciones, pensiones o haberes de retiro No acumulable',
                                                        '071','Ajuste en Subsidio para el empleo (efectivamente entregado al trabajador)',
                                                        '072','Ajuste en Ingresos en acciones o títulos valor que representan bienes Exento',
                                                        '073','Ajuste en Ingresos en acciones o títulos valor que representan bienes Gravado',
                                                        '074','Ajuste en Alimentación Exento',
                                                        '075','Ajuste en Alimentación Gravado',
                                                        '076','Ajuste en Habitación Exento',
                                                        '077','Ajuste en Habitación Gravado',
                                                        '078','Ajuste en Premios por asistencia',
                                                        '079','Ajuste en Pagos distintos a los listados y que no deben considerarse como ingreso por sueldos, salarios o ingresos asimilados.',
                                                        '080','Ajuste en Viáticos gravados',
                                                        '081','Ajuste en Viáticos (entregados al trabajador)',
                                                        '082','Ajuste en Fondo de ahorro Gravado',
                                                        '083','Ajuste en Caja de ahorro Gravado',
                                                        '084','Ajuste en Prima de Seguro de vida Gravado',
                                                        '085','Ajuste en Seguro de Gastos Médicos Mayores Gravado',
                                                        '086','Ajuste en Subsidios por incapacidad Gravado',
                                                        '087','Ajuste en Becas para trabajadores y/o hijos Gravado',
                                                        '088','Ajuste en Seguro de retiro Gravado',
                                                        '089','Ajuste en Vales de despensa Gravado',
                                                        '090','Ajuste en Vales de restaurante Gravado',
                                                        '091','Ajuste en Vales de gasolina Gravado',
                                                        '092','Ajuste en Vales de ropa Gravado',
                                                        '093','Ajuste en Ayuda para renta Gravado',
                                                        '094','Ajuste en Ayuda para artículos escolares Gravado',
                                                        '095','Ajuste en Ayuda para anteojos Gravado',
                                                        '096','Ajuste en Ayuda para transporte Gravado',
                                                        '097','Ajuste en Ayuda para gastos de funeral Gravado',
                                                        '098','Ajuste a ingresos asimilados a salarios gravados',
                                                        '099','Ajuste a ingresos por sueldos y salarios gravados',
                                                        '100','Ajuste en Viáticos exentos',
                                                        '101','ISR Retenido de ejercicio anterior'))
                  INTO VDESCRIPTION 
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CONCEPTOS_CONTRATOS1 CC,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO 
                    AND CL.COD_CONCEPTO  = CC.COD_CONCEPTO
                    AND CC.COD_CONTRATO  = PCOD_CONTRATO
                    AND CC.FECHA_INICIAL = (SELECT MAX(CC1.FECHA_INICIAL)
                                             FROM CONCEPTOS_CONTRATOS1 CC1
                                             WHERE CC1.COD_CONTRATO = CC.COD_CONTRATO
                                             AND   CC1.COD_CONCEPTO = CC.COD_CONCEPTO
                                             AND   CC1.FECHA_INICIAL <= PFECHA_H)
                    AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO2)
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO                          
                    GROUP BY CL.CLASIFICACION, 
                           C.COD_CONCEPTO, 
                           C.NB_CONCEPTO,
                           CC.NB_ALTERNO;
            ELSE 
          --  raise_application_error (-20001,'Entro 2 '||PDIF_PAGO);
                  SELECT DECODE(TRIM(CL.CLASIFICACION)  ,'001','Reintegro de ISR pagado en exceso'
                                                        ,'002','Subsidio para el empleo'
                                                        ,'003','Viáticos'
                                                        ,'004','Aplicación de saldo a favor por compensación anual'
                                                        ,'005','Reintegro de ISR retenido en exceso de ejercicio anterior'
                                                        ,'999','Pagos distintos a los listados y que no deben considerarse como ingreso por sueldos, salarios o ingresos asimilados')
                  INTO VDESCRIPTION 
                  FROM NOMINAS N,
                       CONCEPTOS C,
                       CODIFICACION_CONC_GOSOCKET CL
                  WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                    AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                    AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                    AND N.STATUS         IN ('N','I')
                    AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                    AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                    AND N.COD_CONCEPTO   = NVL(CL.COD_CONCEPTO3,CL.COD_CONCEPTO2)
                    AND CL.COD_REGISTRO  = PREGISTRO
                    AND CL.TIPO_PAGO     = PTIPO_PAGO
                    AND CL.COD_CONTRATO  = PCOD_CONTRATO 
                    GROUP BY CL.CLASIFICACION, 
                           C.COD_CONCEPTO, 
                           C.NB_CONCEPTO;              
            END IF;        
              RETURN REPLACE(VDESCRIPTION,'.',NULL);
          ELSE 
            RETURN NULL;     
          END IF;           
        EXCEPTION WHEN OTHERS THEN 
            RETURN REPLACE(VDESCRIPTION,'.',NULL);
        END;    
END;
--***************************************************FIN DE DATOS/MONTOS DE PERCEPCIONES******************************************************
--                                                   **************************************
--***************************************************INICIO DE DATOS/MONTOS DE HORAS EXTRA****************************************************
FUNCTION CANT_DIAS_HORA_EXTRA    (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PDIA_HORA_MONTO_DESCRIP    IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2 IS
--Funcion que genera la cantidad de dias segun las horas de la asistencias
--Campos del 145 - 184 (PHE_DIAS)  
VDIA_HORA_MONTO_DESCRIP VARCHAR2(60);
-- 
BEGIN
        IF PDIA_HORA_MONTO_DESCRIP = 'D' THEN 
        --
              SELECT  --GREATEST(NVL(TRUNC((SUM(N.CANTIDAD)/8)),0),1) CANT_DIAS
                      SUM(NVL(DECODE(CT.FECHA_INICIAL,CT.FECHA_FINAL,((CT.FECHA_FINAL+1) - CT.FECHA_INICIAL),(CT.FECHA_FINAL - CT.FECHA_INICIAL)),0)) +
                      SUM(NVL(DECODE(A.FECHA_INICIAL,A.FECHA_FINAL,((A.FECHA_FINAL+1) - A.FECHA_INICIAL),(A.FECHA_FINAL - A.FECHA_INICIAL)),0))  CANT_DIAS --DEPERAZA29092022
                      INTO VDIA_HORA_MONTO_DESCRIP
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL,
                   CONCEPTOS_TRABAJADORES CT,
                   ASISTENCIAS A
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCOD_CONTRATO
                AND N.COD_EMPRESA    = CT.COD_EMPRESA(+)
                AND N.COD_TRABAJADOR = CT.COD_TRABAJADOR(+)
                AND N.COD_CONCEPTO   = CT.COD_CONCEPTO(+)
                AND (CT.FECHA_INICIAL(+) BETWEEN PFECHA_D AND PFECHA_H OR  CT.FECHA_FINAL(+)   BETWEEN PFECHA_D AND PFECHA_H)
                AND N.COD_EMPRESA    = A.COD_EMPRESA(+)
                AND N.COD_TRABAJADOR = A.COD_TRABAJADOR(+)
                AND N.COD_CONCEPTO   = A.COD_CONCEPTO(+)
                AND (N.FECHA         = A.FECHA_PAGADO(+))                                       
            GROUP BY CL.CLASIFICACION, 
                      C.COD_CONCEPTO, 
                      C.NB_CONCEPTO;
               IF VDIA_HORA_MONTO_DESCRIP < 0 THEN
                   VDIA_HORA_MONTO_DESCRIP := 1;
               END IF;    
               --    
               IF (VDIA_HORA_MONTO_DESCRIP - TRUNC(VDIA_HORA_MONTO_DESCRIP)) <> 0 THEN        
                   VDIA_HORA_MONTO_DESCRIP  := TRUNC(VDIA_HORA_MONTO_DESCRIP) + 1;          
               END IF;        
        ELSIF PDIA_HORA_MONTO_DESCRIP = 'H' THEN 
        -- 
              SELECT SUM(N.CANTIDAD) CANT_HORAS
                     INTO VDIA_HORA_MONTO_DESCRIP
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCOD_CONTRATO 
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;               
        ELSIF PDIA_HORA_MONTO_DESCRIP = 'M' THEN 
        -- 
              SELECT SUM(N.MONTO) MONTO
                     INTO VDIA_HORA_MONTO_DESCRIP
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCOD_CONTRATO 
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO; 
           --SELECT REPLACE(VDIA_HORA_MONTO_DESCRIP,'.',',') INTO VDIA_HORA_MONTO_DESCRIP FROM DUAL;                                                  
        END IF;                          
   RETURN VDIA_HORA_MONTO_DESCRIP;
   --
 EXCEPTION WHEN OTHERS THEN 
    RETURN VDIA_HORA_MONTO_DESCRIP;              
END;  
--
FUNCTION TIPO_HORAS           (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   VARCHAR2,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2 IS
--Funcion que genera la cantidad de dias segun las horas de la asistencias
--Campos del 145 - 184 (PHE_TIPOHORAS)
VTIPO_HORA VARCHAR2(20);
BEGIN
      SELECT CL.CLASIFICACION 
             INTO VTIPO_HORA 
      FROM NOMINAS N,
           CONCEPTOS C,
           CODIFICACION_CONC_GOSOCKET CL
      WHERE N.COD_EMPRESA    = PCOD_EMPRESA
        AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
        AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
        AND N.STATUS         IN ('N','I')
        AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
        AND N.COD_CONCEPTO   = C.COD_CONCEPTO
        AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
        AND CL.COD_REGISTRO  = PREGISTRO 
        AND CL.TIPO_PAGO     = PTIPO_PAGO
        AND CL.COD_CONTRATO  = PCOD_CONTRATO
        GROUP BY CL.CLASIFICACION, 
               C.COD_CONCEPTO, 
               C.NB_CONCEPTO;
      RETURN VTIPO_HORA;       
EXCEPTION
    WHEN TOO_MANY_ROWS THEN
    VTIPO_HORA := 'Existen Multiples filas para el registro '||PREGISTRO||' en la tabla CODIFICACION_CONC_GOSOCKET, solo debe existir un solo registro.';
    RETURN VTIPO_HORA;
    WHEN OTHERS THEN 
    RETURN VTIPO_HORA;

END;           
--***************************************************FIN DE DATOS/MONTOS DE HORAS EXTRA****************************************************
--                                                  **************************************

--***************************************************FIN DE DATOS/MONTOS DE Descuento****************************************************

FUNCTION MONTO_CONCEPTO_DESCUENTO(PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2 IS
--Campos del 197 - 216 (D_IMPORTE)  
VMONTO_CONCEPTO VARCHAR2(60); 
VCOD_CONCEPTO   NUMBER; 
VGRAVADO        VARCHAR2(1);
BEGIN
      SELECT ABS(SUM(N.MONTO)),
             C.COD_CONCEPTO
             INTO VMONTO_CONCEPTO,
                  VCOD_CONCEPTO 
      FROM NOMINAS N,
           CONCEPTOS C,
           CODIFICACION_CONC_GOSOCKET CL
      WHERE N.COD_EMPRESA    = PCOD_EMPRESA
        AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
        AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
        AND N.STATUS         IN ('N','I')
        AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
        AND N.COD_CONCEPTO   = C.COD_CONCEPTO
        AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
        AND CL.COD_REGISTRO  = PREGISTRO
        AND CL.TIPO_PAGO     = PTIPO_PAGO
        AND CL.COD_CONTRATO  = PCOD_CONTRATO 
      GROUP BY CL.CLASIFICACION, 
               C.COD_CONCEPTO, 
               C.NB_CONCEPTO;                           
  --SELECT REPLACE(VMONTO_CONCEPTO,'.',',') INTO VMONTO_CONCEPTO FROM DUAL;  
  RETURN VMONTO_CONCEPTO;
                                    
EXCEPTION
    WHEN OTHERS THEN 
     RETURN VMONTO_CONCEPTO;
END;
--***************************************************FIN DE DATOS/MONTOS DE DESCUENTOA****************************************************
--                                                  **************************************

--***************************************************INICIO DE DATOS/MONTOS DE INCAPACIDAD****************************************************

FUNCTION CANT_DIAS_MONTO_INCAP   (PCOD_EMPRESA               IN   NUMBER,
                                  PCOD_TRABAJADOR            IN   NUMBER,
                                  PFECHA_D                   IN   DATE, 
                                  PFECHA_H                   IN   DATE, 
                                  PPROCESO                   IN   VARCHAR2,
                                  PREGISTRO                  IN   VARCHAR2,
                                  PDIA_HORA_MONTO_DESCRIP    IN   VARCHAR2,
                                  PTIPO_PAGO                 IN   VARCHAR2,
                                  PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2 IS
--Campos del 265 - 294 
VDIA_HORA_MONTO_DESCRIP VARCHAR2(60); 
BEGIN
    IF PDIA_HORA_MONTO_DESCRIP = 'C' THEN 
    -- 
          SELECT ABS(SUM(N.CANTIDAD)) CANT
                 INTO VDIA_HORA_MONTO_DESCRIP
          FROM NOMINAS N,
               CONCEPTOS C,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = N.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO 
          GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO;              
    ELSIF PDIA_HORA_MONTO_DESCRIP = 'M' THEN 
    -- 
          SELECT ABS(SUM(N.MONTO)) MONTO
                 INTO VDIA_HORA_MONTO_DESCRIP
          FROM NOMINAS N,
               CONCEPTOS C,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCOD_CONTRATO 
          GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO;
        --SELECT REPLACE(VDIA_HORA_MONTO_DESCRIP,'.',',') INTO VDIA_HORA_MONTO_DESCRIP FROM DUAL;                                                 
    END IF;                          
   RETURN VDIA_HORA_MONTO_DESCRIP; 
EXCEPTION WHEN OTHERS THEN 
    RETURN VDIA_HORA_MONTO_DESCRIP;              
END;   

FUNCTION TIPO_INCAP           (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PREGISTRO                  IN   VARCHAR2,
                               PTIPO_PAGO                 IN   VARCHAR2,
                               PCOD_CONTRATO              IN   NUMBER)
   RETURN VARCHAR2 IS
--Campos del 265 - 294   
VTIPO_HORA VARCHAR2(20);
BEGIN

      SELECT CL.CLASIFICACION 
             INTO VTIPO_HORA 
      FROM NOMINAS N,
           CONCEPTOS C,
           CODIFICACION_CONC_GOSOCKET CL
      WHERE N.COD_EMPRESA    = PCOD_EMPRESA
        AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
        AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
        AND N.STATUS         IN ('N','I')
        AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
        AND N.COD_CONCEPTO   = C.COD_CONCEPTO
        AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
        AND CL.COD_REGISTRO  = PREGISTRO
        AND CL.TIPO_PAGO     = PTIPO_PAGO
        AND CL.COD_CONTRATO  = PCOD_CONTRATO 
        GROUP BY CL.CLASIFICACION, 
               C.COD_CONCEPTO, 
               C.NB_CONCEPTO;
      RETURN VTIPO_HORA;       
EXCEPTION
    WHEN TOO_MANY_ROWS THEN
    VTIPO_HORA := 'Existen Multiples filas para el registro '||PREGISTRO||' en la tabla CODIFICACION_CONC_GOSOCKET, solo debe existir un solo registro.';
    RETURN VTIPO_HORA;
    WHEN OTHERS THEN 
    RETURN VTIPO_HORA;

END;           

--***************************************************FIN DE DATOS/MONTOS DE INCAPACIDADA****************************************************
--                                                  **************************************

--***************************************************SUM MONTO/CANT GRUPO****************************************************

FUNCTION MONTO_POR_GRUPO      (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PGRUPO                     IN   VARCHAR2)
   RETURN VARCHAR2 IS   
VMONTO VARCHAR(60);
BEGIN
       SELECT ABS(NVL(SUM (DECODE (C.TIPO, 'A', MONTO, -MONTO)), 0))
        INTO VMONTO
        FROM NOMINAS N,
             CONCEPTOS C
       WHERE N.COD_EMPRESA    = PCOD_EMPRESA
         AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
         AND N.PROCESO = NVL (PPROCESO, N.PROCESO)
         AND N.FECHA BETWEEN PFECHA_D AND PFECHA_H
         AND N.STATUS         IN ('N','I')
         AND N.COD_CONCEPTO = C.COD_CONCEPTO
         AND C.COD_CONCEPTO IN (SELECT COD_CONCEPTO
                                  FROM GRUPOS_CONCEPTOS
                                 WHERE COD_GRUPO = PGRUPO);      
   -- SELECT REPLACE(VMONTO,'.',',') INTO VMONTO FROM DUAL;    
    RETURN VMONTO;      
EXCEPTION
    WHEN OTHERS THEN 
    RETURN VMONTO;
END;
--
FUNCTION MONTO_POR_GRUPO_REGIMEN      (PCOD_EMPRESA               IN   NUMBER,
                                       PCOD_TRABAJADOR            IN   NUMBER,
                                       PFECHA_D                   IN   DATE, 
                                       PFECHA_H                   IN   DATE, 
                                       PPROCESO                   IN   VARCHAR2,
                                       PGRUPO                     IN   VARCHAR2,
                                       PREGIMEN                   IN   VARCHAR2,
                                       PCONTRATO                  IN   NUMBER,
                                       PTIPO_PAGO                 IN   VARCHAR2,
                                       PTIPO_PAGO2                IN   VARCHAR2 DEFAULT NULL,
                                       PTIPO_PAGO3                IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS   
VMONTO VARCHAR(60);
BEGIN
       SELECT ABS(NVL(SUM (DECODE (C.TIPO, 'A', MONTO, -MONTO)), 0))
        INTO VMONTO
        FROM NOMINAS N,
             CONCEPTOS C,
             CODIFICACION_CONC_GOSOCKET  CCG1
       WHERE N.COD_EMPRESA    = PCOD_EMPRESA
         AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
         AND N.PROCESO = NVL (PPROCESO, N.PROCESO)
         AND N.FECHA BETWEEN PFECHA_D AND PFECHA_H
         AND N.STATUS         IN ('N','I')
         AND N.COD_CONCEPTO = C.COD_CONCEPTO
         AND C.COD_CONCEPTO IN (SELECT COD_CONCEPTO
                                  FROM GRUPOS_CONCEPTOS
                                 WHERE COD_GRUPO = PGRUPO)
         AND (N.COD_CONCEPTO  = CCG1.COD_CONCEPTO
             OR N.COD_CONCEPTO  = CCG1.COD_CONCEPTO2
             OR N.COD_CONCEPTO  = CCG1.COD_CONCEPTO3)
         AND CCG1.COD_CONTRATO  = PCONTRATO  
         AND  CCG1.TIPO_PAGO    IN (PTIPO_PAGO,PTIPO_PAGO2,PTIPO_PAGO3)       --DPERAZA                                                    
         AND ((CCG1.COD_REGIMEN IS NULL AND PREGIMEN IS NULL)
               OR CCG1.COD_REGIMEN = PREGIMEN);    
    --            
    RETURN VMONTO; 
    --     
EXCEPTION
    WHEN OTHERS THEN 
    RETURN VMONTO;
END;
--
FUNCTION CANT_POR_GRUPO       (PCOD_EMPRESA               IN   NUMBER,
                               PCOD_TRABAJADOR            IN   NUMBER,
                               PFECHA_D                   IN   DATE, 
                               PFECHA_H                   IN   DATE, 
                               PPROCESO                   IN   VARCHAR2,
                               PGRUPO                     IN   VARCHAR2)
   RETURN NUMBER IS   
VCANT NUMBER;
BEGIN
       SELECT NVL (SUM (DECODE (C.TIPO, 'A', N.CANTIDAD, -N.CANTIDAD)), 0)
        INTO VCANT
        FROM NOMINAS N,
             CONCEPTOS C
       WHERE N.COD_EMPRESA    = PCOD_EMPRESA
         AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
         AND N.PROCESO = NVL (PPROCESO, N.PROCESO)
         AND N.FECHA BETWEEN PFECHA_D AND PFECHA_H
         AND N.STATUS         IN ('N','I')
         AND N.COD_CONCEPTO   = C.COD_CONCEPTO
         AND C.COD_CONCEPTO IN (SELECT COD_CONCEPTO
                                  FROM GRUPOS_CONCEPTOS
                                 WHERE COD_GRUPO = PGRUPO);      
    RETURN VCANT;       
EXCEPTION
    WHEN OTHERS THEN 
    RETURN VCANT;

END;
--***************************************************SUM MONTO GRUPO****************************************************
--                                                   ***************
              
FUNCTION ANTIGUEDAD   (PCOD_EMPRESA               IN   NUMBER,
                       PCOD_TRABAJADOR            IN   NUMBER,
                       PFECHA_FINAL               IN   DATE, 
                       PFECHA_INGRESO             IN   DATE,
                       PFORMATO_ANTI              IN   VARCHAR2)
    RETURN VARCHAR2 IS                       
VANTIGUEDAD VARCHAR2(60);
VANO        NUMBER;
VMES        NUMBER;
VDIA        NUMBER;
VCANTWEEKS  NUMBER := 0;
BEGIN
--
      --ANTIG_TOTAL(PCOD_EMPRESA,PCOD_TRABAJADOR,PFECHA_FINAL,PFECHA_INGRESO,VANO,VMES,VDIA);
--
    IF PFORMATO_ANTI = 'DIA' THEN
        VCANTWEEKS := ((PFECHA_FINAL - PFECHA_INGRESO)+1);
        VANTIGUEDAD := 'P'||VCANTWEEKS||'D';
    ELSIF PFORMATO_ANTI = 'SEM' THEN
        VCANTWEEKS := TRUNC((PFECHA_FINAL - PFECHA_INGRESO)/7);
        VANTIGUEDAD := 'P'||VCANTWEEKS||'W';
    END IF;
--           
    RETURN VANTIGUEDAD;
EXCEPTION 
    WHEN OTHERS THEN 
      VANTIGUEDAD := ' ';
      RETURN VANTIGUEDAD;
END;
--***************************************************OTROS PAGOS****************************************************
-- 
FUNCTION MONTO_CONCEPTO_OTROS_PAGOS (PCOD_EMPRESA               IN   NUMBER,
                                     PCOD_TRABAJADOR            IN   NUMBER,
                                     PFECHA_D                   IN   DATE, 
                                     PFECHA_H                   IN   DATE, 
                                     PPROCESO                   IN   VARCHAR2,
                                     PREGISTRO                  IN   VARCHAR2,
                                     PCONTRATO                  IN   NUMBER,
                                     PAPLICA_TABLA_PORC         IN   VARCHAR2,
                                     PTIPO_PAGO                 IN   VARCHAR2,
                                     PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL)
   RETURN VARCHAR2 IS
--Campos del 217 - 264 (P_CLAVE)  
VMONTO_CONCEPTO VARCHAR2(60); 
VCOD_CONCEPTO   NUMBER; 
VSUBSIDIO       NUMBER; 
BEGIN
    IF PDIF_PAGO IS NULL THEN 
          SELECT SUM(N.MONTO),
                 C.COD_CONCEPTO
          INTO VMONTO_CONCEPTO,
                VCOD_CONCEPTO 
          FROM NOMINAS N,
               CONCEPTOS C,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCONTRATO 
          GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO;  
        IF PAPLICA_TABLA_PORC = 'S' AND VMONTO_CONCEPTO IS NOT NULL AND VMONTO_CONCEPTO <> 0 THEN 
              SELECT SUM(N.MONTO)
              INTO VSUBSIDIO
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = (CL.COD_CONCEPTO2)
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO 
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;     
            RETURN VSUBSIDIO;
        ELSE 
            RETURN VMONTO_CONCEPTO; 
        END IF;

    ELSE  --ESTE CAMBIO VIENE DE LA MANO DEL NUEVO REGIMEN SOLO PARA LA POSICION 217
        BEGIN
              SELECT SUM(N.MONTO)
              INTO VSUBSIDIO
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = (CL.COD_CONCEPTO2)
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO  
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;
        EXCEPTION WHEN NO_DATA_FOUND THEN
            VSUBSIDIO := 0;
        END;
        --
        BEGIN
              SELECT SUM(N.MONTO),
                   C.COD_CONCEPTO
              INTO VMONTO_CONCEPTO,
                   VCOD_CONCEPTO 
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO  
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;  
        EXCEPTION WHEN NO_DATA_FOUND THEN
            VMONTO_CONCEPTO := 0;
        END; 
        IF PAPLICA_TABLA_PORC = 'S' THEN 
            IF VMONTO_CONCEPTO = 0 AND VSUBSIDIO <> 0 THEN
                RETURN VSUBSIDIO;
            ELSIF VSUBSIDIO = 0 THEN
                VSUBSIDIO := .00;
                RETURN VSUBSIDIO;
            ELSE 
                RETURN VSUBSIDIO;
            END IF;
        ELSE
            IF VMONTO_CONCEPTO = 0 AND VSUBSIDIO <> 0 THEN
                VMONTO_CONCEPTO := .01;
                RETURN VMONTO_CONCEPTO;
            ELSIF VSUBSIDIO = 0 THEN
                VMONTO_CONCEPTO := .00;
                RETURN VMONTO_CONCEPTO;
            ELSE 
                RETURN VMONTO_CONCEPTO;        
            END IF;
        END IF;              
    END IF;                                                            
EXCEPTION
    WHEN NO_DATA_FOUND THEN
            VMONTO_CONCEPTO := NULL;
        RETURN VMONTO_CONCEPTO;
    WHEN OTHERS THEN 
        RETURN VMONTO_CONCEPTO;
END;
--
--
-- NUEVA FUNCION JC
FUNCTION MONTO_CONCEPTO_OTROS_PAGOS_REGIMEN (PCOD_EMPRESA               IN   NUMBER,
                                             PCOD_TRABAJADOR            IN   NUMBER,
                                             PFECHA_D                   IN   DATE, 
                                             PFECHA_H                   IN   DATE, 
                                             PPROCESO                   IN   VARCHAR2,
                                             PREGISTRO                  IN   VARCHAR2,
                                             PCONTRATO                  IN   NUMBER,
                                             PAPLICA_TABLA_PORC         IN   VARCHAR2,
                                             PTIPO_PAGO                 IN   VARCHAR2,
                                             PDIF_PAGO                  IN   VARCHAR2 DEFAULT NULL,
                                             PREGIMEN                   IN   VARCHAR2)
   RETURN VARCHAR2 IS
--Campos del 217 - 264 (P_CLAVE)  
VMONTO_CONCEPTO VARCHAR2(60); 
VCOD_CONCEPTO   NUMBER; 
VSUBSIDIO       NUMBER; 
BEGIN
    IF PDIF_PAGO IS NULL THEN 
          SELECT SUM(N.MONTO),
                 C.COD_CONCEPTO
          INTO VMONTO_CONCEPTO,
               VCOD_CONCEPTO 
          FROM NOMINAS N,
               CONCEPTOS C,
               CODIFICACION_CONC_GOSOCKET CL
          WHERE N.COD_EMPRESA    = PCOD_EMPRESA
            AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
            AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
            AND N.STATUS         IN ('N','I')
            AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
            AND N.COD_CONCEPTO   = C.COD_CONCEPTO
            AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
            AND CL.COD_REGISTRO  = PREGISTRO
            AND CL.TIPO_PAGO     = PTIPO_PAGO
            AND CL.COD_CONTRATO  = PCONTRATO 
            AND ((CL.COD_REGIMEN IS NULL AND PREGIMEN IS NULL)
                   OR CL.COD_REGIMEN = PREGIMEN)
          GROUP BY CL.CLASIFICACION, 
                   C.COD_CONCEPTO, 
                   C.NB_CONCEPTO;  
        IF PAPLICA_TABLA_PORC = 'S' AND VMONTO_CONCEPTO IS NOT NULL AND VMONTO_CONCEPTO <> 0 THEN 
              SELECT SUM(N.MONTO)
                     INTO VSUBSIDIO
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = (CL.COD_CONCEPTO2)
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO 
                AND ((CL.COD_REGIMEN IS NULL AND PREGIMEN IS NULL)
                   OR CL.COD_REGIMEN = PREGIMEN)
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;     
            RETURN VSUBSIDIO;
        ELSE 
            RETURN VMONTO_CONCEPTO; 
        END IF;

    ELSE  --ESTE CAMBIO VIENE DE LA MANO DEL NUEVO REGIMEN SOLO PARA LA POSICION 217
        BEGIN
              SELECT SUM(N.MONTO)
              INTO VSUBSIDIO
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = (CL.COD_CONCEPTO2)
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO 
                AND ((CL.COD_REGIMEN IS NULL AND PREGIMEN IS NULL)
                   OR CL.COD_REGIMEN = PREGIMEN) 
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;
        EXCEPTION WHEN NO_DATA_FOUND THEN
            VSUBSIDIO := 0;
        END;
        --
        BEGIN
              SELECT SUM(N.MONTO),
                     C.COD_CONCEPTO
              INTO VMONTO_CONCEPTO,
                   VCOD_CONCEPTO 
              FROM NOMINAS N,
                   CONCEPTOS C,
                   CODIFICACION_CONC_GOSOCKET CL
              WHERE N.COD_EMPRESA    = PCOD_EMPRESA
                AND N.COD_TRABAJADOR = PCOD_TRABAJADOR
                AND N.FECHA          BETWEEN PFECHA_D AND PFECHA_H
                AND N.STATUS         IN ('N','I')
                AND (N.PROCESO       = PPROCESO OR PPROCESO IS NULL)
                AND N.COD_CONCEPTO   = C.COD_CONCEPTO
                AND N.COD_CONCEPTO   = CL.COD_CONCEPTO
                AND CL.COD_REGISTRO  = PREGISTRO
                AND CL.TIPO_PAGO     = PTIPO_PAGO
                AND CL.COD_CONTRATO  = PCONTRATO  
                AND ((CL.COD_REGIMEN IS NULL AND PREGIMEN IS NULL)
                   OR CL.COD_REGIMEN = PREGIMEN)
              GROUP BY CL.CLASIFICACION, 
                       C.COD_CONCEPTO, 
                       C.NB_CONCEPTO;  
        EXCEPTION WHEN NO_DATA_FOUND THEN
            VMONTO_CONCEPTO := 0;
        END; 
        IF PAPLICA_TABLA_PORC = 'S' THEN 
            IF VMONTO_CONCEPTO = 0 AND VSUBSIDIO <> 0 THEN
                RETURN VSUBSIDIO;
            ELSIF VSUBSIDIO = 0 THEN
                VSUBSIDIO := .00;
                RETURN VSUBSIDIO;
            ELSE 
                RETURN VSUBSIDIO;
            END IF;
        ELSE
            IF VMONTO_CONCEPTO = 0 AND VSUBSIDIO <> 0 THEN
                VMONTO_CONCEPTO := .01;
                RETURN VMONTO_CONCEPTO;
            ELSIF VSUBSIDIO = 0 THEN
                VMONTO_CONCEPTO := .00;
                RETURN VMONTO_CONCEPTO;
            ELSE 
                RETURN VMONTO_CONCEPTO;        
            END IF;
        END IF;              
    END IF;                                                            
EXCEPTION
    WHEN NO_DATA_FOUND THEN
            VMONTO_CONCEPTO := NULL;
        RETURN VMONTO_CONCEPTO;
    WHEN OTHERS THEN 
        RETURN VMONTO_CONCEPTO;
END;
--
/*
*************************************************ANTIGUEDAD_TOTAL********************************************
                                        CREADO 16/02/2023 POR DPERAZA
***************************************SUTITUYE AL PROCEDIMIENTO ANTIG_TOTAL*******************************************
*/
PROCEDURE ANTIGUEDAD_TOTAL       (PFECHA_EMISION           IN DATE,
                                  PFECHA_INGRESO           IN DATE,
                                  PYEARS                  OUT NUMBER,                               
                                  PMONTHS                 OUT NUMBER,
                                  PDAYS                   OUT NUMBER
                                  )
                                  IS
--
  START_DATE DATE := PFECHA_INGRESO;
  END_DATE   DATE := PFECHA_EMISION;
  DIFF       NUMBER;
  VALIDA31   NUMBER;
--
    BEGIN
      --
      DIFF    := MONTHS_BETWEEN(END_DATE, START_DATE);
      --  
      PYEARS  := FLOOR(DIFF/12); 
      PMONTHS := TRUNC(MOD(DIFF,12));
      PDAYS   := TRUNC(END_DATE - ADD_MONTHS(START_DATE, TRUNC(MONTHS_BETWEEN(END_DATE,START_DATE))));
     --
    EXCEPTION 
        WHEN OTHERS THEN 
              PYEARS  :=0;
              PMONTHS :=0;
              PDAYS   :=0; 
    END; 
-- 
--***************************************************FIN DE OTROS PAGOS****************************************************
--                                        **************************************
--
--***************************************************IOFACTURO FORMATO XML****************************************************
PROCEDURE GENERAR_IOFACT_MEX_XML (PEMPRESA                 IN   NUMBER,
                                  PFECHA_D                 IN   DATE,
                                  PFECHA_H                 IN   DATE,
                                  PFORMATO_ANTI            IN   VARCHAR2,
                                  PPROCESO                 IN   VARCHAR2,
                                  PSESION                  IN   NUMBER,
                                  PARCHIVO                 IN   VARCHAR2,
                                  PTXT_DIR                 IN   VARCHAR2,
                                  PREGIMEN                 IN   VARCHAR2,
                                  PSERIE                   IN   VARCHAR2,
                                  PFOLIO                   IN   VARCHAR2,
                                  PFORMA_PAGO              IN   VARCHAR2,
                                  PPERIODICIDAD            IN   VARCHAR2,
                                  PNO_CERTIFICADO          IN   VARCHAR2,
                                  PSUBTOTAL                IN   NUMBER,
                                  PDESCUENTO               IN   NUMBER,
                                  PTOTAL                   IN   NUMBER,
                                  PUSO_CFDI                IN   VARCHAR2,
                                  PUUID_1                  IN   VARCHAR2,
                                  PUUID_2                  IN   VARCHAR2,
                                  PCONCEPT_CLAVE_PROD_SERV IN   VARCHAR2,
                                  PCONCEPT_NO_IDENTIFICADO IN   VARCHAR2,
                                  PCONCEPTO_CANTIDAD       IN   VARCHAR2,
                                  PCONCEPTO_CLAVE_UNIDAD   IN   VARCHAR2,
                                  PCONCEPTO_DESCRIPCION    IN   VARCHAR2,
                                  PCONCEPTO_VALOR_UNITARIO IN   NUMBER,
                                  PCONCEPTO_IMPORTE        IN   NUMBER,
                                  PCONCEPTO_DESCUENTO      IN   NUMBER,
                                  PTIPO_NOMINA             IN   VARCHAR2,
                                  PFECHA_PAGO              IN   DATE,
                                  PNUM_DIAS_PAGADOS        IN   NUMBER,
                                  PTOTAL_PERCEPCIONES      IN   NUMBER,
                                  PTOTAL_DEDUCCIONES       IN   NUMBER,
                                  PTOTAL_OTROS_PAGOS       IN   NUMBER,
                                  PCVE_REGISTRO_FISCAL     IN   VARCHAR2,
                                  PE_CURP                  IN   VARCHAR2,
                                  PREGISTRO_PATRONAL       IN   VARCHAR2,
                                  PRFC_PATRON_ORIGEN       IN   VARCHAR2,
                                  PORIGEN_RECURSO          IN   VARCHAR2,
                                  PRECURSO_PROPIO          IN   NUMBER,
                                  PSALARIO_DIARIO_INTEGRADO IN NUMBER,          --SE LE CAMBIÓ EL NOMBRE AL PARÁMETRO DE ENTRADA
                                  PSALARIO_DIARIO           IN NUMBER,          --SE AGREGÓ UN NUEVO PARÁMERO
                                  PTOTAL_SUELDOS           IN   NUMBER,
                                  PTOTAL_SEPARACION        IN   NUMBER,
                                  PTOTAL_JUBILACION        IN   NUMBER,
                                  PTOTAL_GRAVADO           IN   NUMBER,
                                  PTOTAL_EXENTO            IN   NUMBER,
                                  PCONCEPTO_IMPUESTO       IN   NUMBER,
                                  PJPR_TOTAL_UNA_EXHIB     IN   NUMBER,
                                  PJPR_TOTAL_PARCIALIDAD   IN   NUMBER,
                                  PJPR_MONTO_DIARIO        IN   NUMBER,
                                  PJPR_INGRESO_ACUM        IN   NUMBER,
                                  PJPR_INGRESO_NO_ACUM     IN   NUMBER,
                                  PSI_TOTAL_PAGADO         IN   NUMBER,
                                  PSI_NUM_ANOS             IN   NUMBER,
                                  PSI_INGRESO_ACUM         IN   NUMBER,
                                  PSI_INGRESO_NO_ACUM      IN   NUMBER,
                                  PTOTAL_OTRAS_DEDUCCIONES IN   NUMBER,
                                  PTOTAL_IMP_RETENIDO      IN   NUMBER,
                                  PBASE                    IN   VARCHAR2,
                                  PPAIS                    IN   VARCHAR2,
                                  PULT_NOMBRE_CONCEP       IN   VARCHAR2 DEFAULT 'N',
                                  PLUGAR_EXP               IN   VARCHAR2 DEFAULT NULL,
                                  PNO_ERROR                OUT  NUMBER,
                                  PMG_ERROR                OUT  VARCHAR2,
                                  PFECHA_INI_TIMB          IN   DATE,
                                  PFECHA_FIN_TIMB          IN   DATE,
                                  PSALARIO_BASE_COT        IN   VARCHAR2) IS 
ASESION      VARCHAR2(1000) := USERENV('SESSIONID');
MLINEA       VARCHAR2(32767);
MNB_ARCHIVO  VARCHAR2(20000) := PARCHIVO;
MFILE        UTL_FILE.FILE_TYPE;
RUTA         VARCHAR2(500) := PTXT_DIR;
MINGRESOS    NUMBER;
MIMPUESTO    NUMBER;
MEGRESOS     NUMBER;
MCANTIDAD    NUMBER;
MNETO        NUMBER := 0;
MCONTADOR    NUMBER := 0;
MFOLIO_CONT  NUMBER := NVL(PFOLIO,0)-1;
NB_DEPARTAMENTO DEPARTAMENTOS.NB_DEPARTAMENTO%TYPE;
VPERCEP_EXISTE   VARCHAR2(1):= 'N';
VHORA_EXISTE     VARCHAR2(1):= 'N';
VDEDU_EXISTE     VARCHAR2(1):= 'N';
VOTROS_EXISTE    VARCHAR2(1):= 'N';
VINCAP_EXISTE    VARCHAR2(1):= 'N'; 
VDIF_PAGO        VARCHAR2(1):= NULL;
VANO             NUMBER := 0;
VMES             NUMBER := 0;
VDIA             NUMBER := 0;
VNUM_ANTIGUEDAD  NUMBER := 0;
--
--Se agregó un parametro de entrada: PREGIMEN
--
        CURSOR CODIFI(PCOD_CONTRATO IN NUMBER,
                      PTIPO_PAGO    IN VARCHAR2,
                      PREGISTRO     IN NUMBER DEFAULT NULL,
                      PREGIMEN      IN VARCHAR2) IS
        SELECT CODI.COD_REGISTRO,
               CODI.CLASIFICACION,
               CODI.COD_CONCEPTO,
               CODI.COD_CONCEPTO2,
               CODI.COD_CONCEPTO3
        FROM CODIFICACION_CONC_GOSOCKET CODI
        WHERE CODI.COD_CONTRATO = PCOD_CONTRATO
        AND   CODI.TIPO_PAGO    = PTIPO_PAGO
        AND   CODI.COD_REGISTRO = NVL(PREGISTRO,CODI.COD_REGISTRO)
        AND ((CODI.COD_REGIMEN  =  PREGIMEN) OR (PREGIMEN IS NULL AND COD_REGIMEN IS NULL))
        ORDER BY COD_REGISTRO; 
--
--Se agregó un parametro de entrada: PREGIMEN
--
        CURSOR CODIFI_H(PCOD_CONTRATO IN NUMBER,
                        PTIPO_PAGO    IN VARCHAR2,
                        PREGISTRO     IN NUMBER DEFAULT NULL,
                        PREGIMEN      IN VARCHAR2) IS
        SELECT CODI.COD_REGISTRO,
               CODI.CLASIFICACION,
               CODI.COD_CONCEPTO,
               CODI.COD_CONCEPTO2,
               CODI.COD_CONCEPTO3
        FROM CODIFICACION_CONC_GOSOCKET CODI
        WHERE CODI.COD_CONTRATO = PCOD_CONTRATO
        AND   CODI.TIPO_PAGO    = PTIPO_PAGO
        AND   CODI.COD_REGISTRO = NVL(PREGISTRO,CODI.COD_REGISTRO)
        AND ((CODI.COD_REGIMEN  =  PREGIMEN) OR (PREGIMEN IS NULL AND COD_REGIMEN IS NULL))
        ORDER BY COD_REGISTRO; 
--                                          
BEGIN
    MFILE := UTL_FILE.FOPEN(RUTA, MNB_ARCHIVO, 'w', 32767);   
    BEGIN
        DBMS_LOB.FILECLOSEALL;     
    EXCEPTION WHEN OTHERS THEN
        NULL;
    END;
   --
   /*ENCABEZADO XML1*/           
   MLINEA := '<?xml version="1.0" encoding="UTF-8"?>'||CHR(13);
   --MLINEA := '<?xml version="1.0" encoding="iso-8859-1"?>'||CHR(13);
   MLINEA := MLINEA||'<DTE version="3.3">'||CHR(13);
   UTL_FILE.PUT_LINE(MFILE, MLINEA);                        
   /*REGISTROS1*/ 
   --
   --SE AÑADIÓ EL SELECT ANTERIOR COMO UNA TABLA Y UNA TABLA: CODIFICACION_CONC_GOSOCKET
   --
   FOR CLIE IN (SELECT   QUERY1.VERSION_CFDI
                        ,QUERY1.SERIE
                        ,QUERY1.FORMA_PAGO
                        ,TRIM(TO_CHAR(MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSUBTOTAL,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P','O'),'999999999999999999.99')) SUBTOTAL
                        ,TRIM(TO_CHAR(MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PDESCUENTO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'D'),'999999999999999999.99')) DESCUENTO
                        ,QUERY1.MONEDA
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P','D','O') TOTAL
                        ,QUERY1.TIPO_COMPROBANTE
                        ,QUERY1.METODO_PAGO
                        ,QUERY1.LUGAR_EXPEDICION
                        ,QUERY1.RFC_RECEPTOR
                        ,QUERY1.MAIL_NAME
                        ,QUERY1.NOMBRE
                        ,QUERY1.RESIDENCIA_FISCAL
                        ,QUERY1.NUM_REG_ID_TRIB
                        ,QUERY1.USO_CFDI
                        ,QUERY1.CFDI_RELACIONADO_UUID_1
                        ,QUERY1.CFDI_RELACIONADO_UUID_2
                        ,QUERY1.CONCEPTO_CLAVE_PROD_SERV
                        ,QUERY1.CONCEPTO_NO_IDENTIFICACION
                        ,QUERY1.CONCEPTO_CANTIDAD
                        ,QUERY1.CONCEPTO_CLAVE_UNIDAD
                        ,QUERY1.CONCEPTO_DESCRIPCION
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_VALOR_UNITARIO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P','O') CONCEPTO_VALOR_UNITARIO
                        ,TRIM(TO_CHAR(MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_IMPORTE,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P','D','O'),'999999999999999999.99')) CONCEPTO_IMPORTE
                        ,TRIM(TO_CHAR(MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_DESCUENTO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'D'),'999999999999999999.99')) CONCEPTO_DESCUENTO
                        ,QUERY1.TIPO_NOMINA
                        ,QUERY1.FECHA_PAGO
                        ,QUERY1.FECHA_INICIAL_PAGO
                        ,QUERY1.FECHA_FINAL_PAGO
                        ,QUERY1.NUM_DIAS_PAGADOS
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_PERCEPCIONES,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_PERCEPCIONES                                 
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_DEDUCCIONES,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'D') TOTAL_DEDUCCIONES                                    
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_OTROS_PAGOS,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'O') TOTAL_OTROS_PAGOS 
                        ,QUERY1.E_CVE_REGISTRO_FISCAL
                        ,QUERY1.E_CURP
                        ,QUERY1.E_REGISTRO_PATRONAL
                        ,QUERY1.E_RFC_PATRON_ORIGEN
                        ,QUERY1.ENT_ORIGEN_RECURSO
                        ,QUERY1.ENT_RECURSO_PROPIO
                        ,QUERY1.R_CURP
                        ,QUERY1.R_NUM_SEGURIDAD_SOCIAL
                        ,QUERY1.R_FECHA_INICIO_LABORAL
                        ,QUERY1.R_ANTIGUEDAD
                        ,QUERY1.R_TIPO_CONTRATO
                        ,QUERY1.R_SINDICALIZADO
                        ,QUERY1.R_TIPO_JORNADA
                        ,CG.COD_REGIMEN             --QUERY1.R_TIPO_REGIMEN
                        ,QUERY1.R_TIPO_REGIMEN      --TipoRegimen anterior
                        ,QUERY1.R_NUM_EMPLEADO
                        ,QUERY1.R_DEPARTAMENTO
                        ,QUERY1.R_PUESTO
                        ,QUERY1.R_RIEGO_PUESTO
                        ,QUERY1.R_PERIODICIDAD_PAGO
                        ,QUERY1.R_BANCO
                        ,QUERY1.R_CUENTA_BANCARIA
                        ,DECODE(PSALARIO_BASE_COT,NULL, QUERY1.SUELDO,MONTO_POR_GRUPO(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_BASE_COT)) R_SALARIO_BASE_COT
                        ,MONTO_POR_GRUPO(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_DIARIO_INTEGRADO) R_SALARIO_DIARIO_INTEGRADO
                        ,MONTO_POR_GRUPO(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_DIARIO) R_SALARIO_DIARIO
                        ,QUERY1.R_CLAVE_ENTFED
                        ,QUERY1.SUB_RFC_LABORA_1
                        ,QUERY1.SUB_PORCENTAJE_TIEMPO_1
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_SUELDOS,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_SUELDOS                                            -- Campo 88
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_SEPARACION,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_SEPARACION                                      -- Campo 89
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_JUBILACION,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_JUBILACION                                      -- Campo 90
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_GRAVADO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_GRAVADO                                            -- Campo 91
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_EXENTO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') TOTAL_EXENTO
                        ,QUERY1.PAT_VALOR_MERCADO
                        ,QUERY1.PAT_PRECIO_AL_OTORG
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_UNA_EXHIB,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') JPR_TOTAL_UNA_EXHIB                                -- Campo 185
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_PARCIALIDAD,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') JPR_TOTAL_PARCIALIDAD                            -- Campo 186
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_MONTO_DIARIO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') JPR_MONTO_DIARIO                             -- Campo 187
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_ACUM,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') JPR_INGRESO_ACUM                                      -- Campo 188
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_NO_ACUM,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') JPR_INGRESO_NO_ACUM                                -- Campo 189
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_TOTAL_PAGADO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') SI_TOTAL_PAGADO                                        -- Campo 190
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_NUM_ANOS,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') SI_NUM_ANOS
                        ,QUERY1.SI_ULTIMO_SUELDO
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_ACUM,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') SI_INGRESO_ACUM
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_NO_ACUM,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'P') SI_INGRESO_NO_ACUM                                  -- Campo 194
                        ,MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_OTRAS_DEDUCCIONES,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'D') D_TOTAL_OTRAS_DEDUCCIONES
                        ,NULLIF(MONTO_POR_GRUPO_REGIMEN(QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_IMP_RETENIDO,CG.COD_REGIMEN,QUERY1.COD_CONTRATO,'D'),'0') D_TOTAL_IMPUESTOS_RETENIDOS
                        ,QUERY1.NB_EMPRESA
                        ,QUERY1.UUID
                        ,QUERY1.RELACION
                        ,QUERY1.COD_CONTRATO
                        ,QUERY1.COD_EMPRESA
                        ,QUERY1.COD_TRABAJADOR
                        ,MONTO_CONCEPTO_OTROS_PAGOS_REGIMEN (QUERY1.cod_empresa,QUERY1.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,1,QUERY1.COD_CONTRATO,'N','O','S',CG.COD_REGIMEN) OP_IMPORTE_1
                        ,QUERY1.FECHA_ANTI_FINAL
                        ,QUERY1.FECHA_ANTI_INICIAL
                        ,QUERY1.NOMBRE_SUCURSAL 
                FROM CODIFICACION_CONC_GOSOCKET CG,
                      --SELECT ANTERIOR
                     (SELECT   '3.3' VERSION_CFDI,       --1                                                                                                                   -- Campo 1
                                PSERIE SERIE,                                                                                                                                       -- Campo 2
                                PFORMA_PAGO FORMA_PAGO,                                                                                                                             -- Campo 4
                                MONE.SIMBOLO MONEDA,                                                                                                                                -- Campo 10     
                                'N' TIPO_COMPROBANTE,                                                                                                                               -- Campo 12
                                'PUE' METODO_PAGO,                                                                                                                                  -- Campo 13 
                                PLUGAR_EXP LUGAR_EXPEDICION,                                                                                                                        -- Campo 14 
                                O.ID_FISCAL RFC_RECEPTOR,                                                                                                                           -- Campo 15
                                R.MAIL_NAME,
                                REPLACE(REPLACE(T.PRIMER_NOMBRE||DECODE(T.SEGUNDO_NOMBRE, NULL, '', ' '||T.SEGUNDO_NOMBRE)||DECODE(T.SIGUIENTES_NOMBRES, NULL, '', ' '||T.SIGUIENTES_NOMBRES)||' '||T.PRIMER_APELLIDO||DECODE(T.SEGUNDO_APELLIDO, NULL, '', ' '||T.SEGUNDO_APELLIDO)||DECODE(T.SIGUIENTES_APELLIDOS, NULL, '', ' '||T.SIGUIENTES_APELLIDOS),'/',''),'-','') NOMBRE,                    -- Campo 16
                                '' RESIDENCIA_FISCAL,                                                                                                                               -- Campo 17 
                                '' NUM_REG_ID_TRIB,                                                                                                                                 -- Campo 18
                                PUSO_CFDI USO_CFDI,                                                                                                                                 -- Campo 19 
                                PUUID_1 CFDI_RELACIONADO_UUID_1,                                                                                                                    -- Campo 19
                                PUUID_2 CFDI_RELACIONADO_UUID_2,                                                                                                                    -- Campo 20
                                PCONCEPT_CLAVE_PROD_SERV CONCEPTO_CLAVE_PROD_SERV,                                                                                                  -- Campo 22
                                PCONCEPT_NO_IDENTIFICADO CONCEPTO_NO_IDENTIFICACION,                                                                                                -- Campo 23
                                PCONCEPTO_CANTIDAD CONCEPTO_CANTIDAD,                                                                                                               -- Campo 24 
                                PCONCEPTO_CLAVE_UNIDAD CONCEPTO_CLAVE_UNIDAD,                                                                                                       -- Campo 25
                                PCONCEPTO_DESCRIPCION CONCEPTO_DESCRIPCION,                                                                                                         -- Campo 27
                                --MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_VALOR_UNITARIO) CONCEPTO_VALOR_UNITARIO,                        -- Campo 28
                                --TRIM(TO_CHAR(MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_IMPORTE),'999999999999999999.99')) CONCEPTO_IMPORTE,     -- Campo 29
                                --TRIM(TO_CHAR(MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_DESCUENTO),'999999999999999999.99')) CONCEPTO_DESCUENTO, -- Campo 30
                                PTIPO_NOMINA TIPO_NOMINA,                                                                                                                           -- Campo 50
                                TO_CHAR(PFECHA_PAGO,'YYYY-MM-DD') FECHA_PAGO,                                                                                                       -- Campo 51
                                TO_CHAR(PFECHA_INI_TIMB,'YYYY-MM-DD')  FECHA_INICIAL_PAGO, --SERA LOS ITEMS DE FECHAS TIMBRADO                                                                                                -- Campo 52
                                TO_CHAR(PFECHA_FIN_TIMB,'YYYY-MM-DD')  FECHA_FINAL_PAGO,   --SERA LOS ITEMS DE FECHAS TIMBRADO                                                                                                -- Campo 53
                                ROUND(CANT_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PNUM_DIAS_PAGADOS)) NUM_DIAS_PAGADOS,                                -- Campo 54
                                PCVE_REGISTRO_FISCAL E_CVE_REGISTRO_FISCAL,                                                                                                         -- Campo 58
                                PE_CURP E_CURP,                                                                                                                                     -- Campo 59
                                DM.REG_PATRONAL E_REGISTRO_PATRONAL,                                                                                                             -- Campo 60
                                PRFC_PATRON_ORIGEN E_RFC_PATRON_ORIGEN,                                                                                                             -- Campo 61
                                PORIGEN_RECURSO ENT_ORIGEN_RECURSO,                                                                                                                 -- Campo 62
                                PRECURSO_PROPIO ENT_RECURSO_PROPIO,                                                                                                                 -- Campo 63
                                O.ID_FISCAL1 R_CURP,                                                                                                                                -- Campo 64
                                NUMERO_SSO R_NUM_SEGURIDAD_SOCIAL, --  O.CEDULA R_NUM_SEGURIDAD_SOCIAL,                                                                                                                    -- Campo 65
                                TO_CHAR(T.FECHA_INGRESO,'YYYY-MM-DD') R_FECHA_INICIO_LABORAL,                                                                                        -- Campo 66
                                DECODE(PFORMATO_ANTI, 'DIA', ANTIGUEDAD(t.cod_empresa,t.cod_trabajador,PFECHA_H,T.FECHA_INGRESO, PFORMATO_ANTI),
                                                      'SEM', ANTIGUEDAD(t.cod_empresa,t.cod_trabajador,PFECHA_H,T.FECHA_INGRESO, PFORMATO_ANTI)) R_ANTIGUEDAD,                      -- Campo 67,                                                                   -- Campo 67
                                DM.TIPO_CONTRATACION_SAT R_TIPO_CONTRATO,                                                                                                           -- Campo 68
                                DM.SINDICALIZADO_SAT R_SINDICALIZADO,                                                                                                               -- Campo 69
                                DM.TIPO_JORNADA_SAT R_TIPO_JORNADA,                                                                                                                 -- Campo 70
                                DECODE(PREGIMEN,'13',PREGIMEN,DM.TIPO_REGIMEN_SAT) R_TIPO_REGIMEN,                                                                                                                 -- Campo 71
                                T.COD_TRABAJADOR R_NUM_EMPLEADO,                                                                                                                    -- Campo 72
                                NVL(DEPA.NB_ALTERNO,DEPA.NB_DEPARTAMENTO) R_DEPARTAMENTO,                                                                                           -- Campo 73
                                NVL(CARGO.NB_ALTERNO,CARGO.NB_CARGO) R_PUESTO,                                                                                                      -- Campo 74
                                DM.RIESGO_PUESTO_SAT R_RIEGO_PUESTO,                                                                                                                -- Campo 75
                                /* DECODE(CO.FREC_NOMINA,'Q','04','M','05','S','02','B','03')  R_PERIODICIDAD_PAGO,  */
                                PPERIODICIDAD  R_PERIODICIDAD_PAGO,-- Campo 76                                                                    -- Campo 76
                                DECODE (length(BANCOS.NU_CUENTA),18,NULL,BAN.ID_BANCARIO) R_BANCO,                                                                                                                            -- Campo 77
                                BANCOS.NU_CUENTA R_CUENTA_BANCARIA,                                                                                                                 -- Campo 78
                                --DECODE(PSALARIO_BASE_COT,NULL, SUEL.SUELDO,MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_BASE_COT)) R_SALARIO_BASE_COT,                                                                                                                     -- Campo 79
                                --MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_DIARIO_INTEGRADO) R_SALARIO_DIARIO,                                        -- Campo 80
                                DM.ENT_FED_SAT R_CLAVE_ENTFED,                                                                                                                        -- Campo 81
                                '' SUB_RFC_LABORA_1,                                                                                                                               -- Campo 82
                                '' SUB_PORCENTAJE_TIEMPO_1,                                                                                                                        -- Campo 83                                                                                                                                         --Campo 49
                                '' PAT_VALOR_MERCADO,                                                                                                                              -- Campo 143
                                '' PAT_PRECIO_AL_OTORG,                                                                                                                            -- Campo 144                               
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_UNA_EXHIB) JPR_TOTAL_UNA_EXHIB,                                -- Campo 185
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_PARCIALIDAD) JPR_TOTAL_PARCIALIDAD,                            -- Campo 186
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_MONTO_DIARIO) JPR_MONTO_DIARIO,                                      -- Campo 187
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_ACUM) JPR_INGRESO_ACUM,                                      -- Campo 188
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_NO_ACUM) JPR_INGRESO_NO_ACUM,                                -- Campo 189
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_TOTAL_PAGADO) SI_TOTAL_PAGADO,                                        -- Campo 190
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_NUM_ANOS) SI_NUM_ANOS,                                                -- Campo 191
                                SUEL.SUELDO SI_ULTIMO_SUELDO,                                                                                                                       -- Campo 192
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_ACUM) SI_INGRESO_ACUM,                                        -- Campo 193
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_NO_ACUM) SI_INGRESO_NO_ACUM,                                  -- Campo 194
--                                MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_OTRAS_DEDUCCIONES) D_TOTAL_OTRAS_DEDUCCIONES,                      -- Campo 195
                                E.NB_EMPRESA,
                                (SELECT NVL(RE.UUID2,RE.UUID1) FROM REMISION_IOFACTURO RE WHERE RE.COD_EMPRESA = T.COD_EMPRESA AND RE.COD_TRABAJADOR = T.COD_TRABAJADOR AND RE.FECHA = PFECHA_H) UUID, 
                                (SELECT RE.RELACION FROM REMISION_IOFACTURO RE WHERE RE.COD_EMPRESA = T.COD_EMPRESA AND RE.COD_TRABAJADOR = T.COD_TRABAJADOR AND RE.FECHA = PFECHA_H) RELACION,                         
                               T.COD_CONTRATO,
                               T.COD_EMPRESA,
                               T.COD_TRABAJADOR,
                               LEAST(NVL(T.FECHA_EGRESO,PFECHA_H),PFECHA_H)  FECHA_ANTI_FINAL,
                               T.FECHA_INGRESO FECHA_ANTI_INICIAL,
                               SUC.NB_SUCURSAL NOMBRE_SUCURSAL,
                               SUEL.SUELDO
               FROM  TRABAJADORES T,
                     DATOS_EMPRESAS1 DE,
                     ORIGEN_TRABAJADORES O,
                     DOMICILIO_TRABAJADORES D,
                     PAISES P,
                     CIUDADES C,
                     CIUDADES C2,
                     DATOS_TRABAJADORES_MEX DM,
                     EMPRESAS E,
                     RR_HH_TRABAJADORES R, 
                     CONTRATOS CO,
                     EMPRESAS1 EMP,
                     MONEDAS1 MONE,
                     HISTORICO_DEPARTAMENTOS HD,
                     DEPARTAMENTOS DEPA,
                     HISTORICOS_CARGOS HC,
                     CARGOS CARGO,
                     SUELDOS SUEL, 
                     BANCOS_TRABAJADORES BANCOS,
                     BANCOS_PAIS1 BAN,
                     SUCURSALES SUC
                WHERE T.COD_EMPRESA        = O.COD_EMPRESA
                  AND T.COD_TRABAJADOR     = O.COD_TRABAJADOR
                  AND T.COD_EMPRESA        = D.COD_EMPRESA
                  AND T.COD_TRABAJADOR     = D.COD_TRABAJADOR                  
                  AND T.COD_EMPRESA        = DE.COD_EMPRESA
                  AND T.COD_EMPRESA        = E.COD_EMPRESA
                  AND T.COD_SUCURSAL       = SUC.COD_SUCURSAL
                  AND T.COD_EMPRESA        = SUC.COD_EMPRESA
                  AND DE.COD_PAIS          = P.COD_PAIS
                  AND DE.COD_PAIS          = C.COD_PAIS
                  AND DE.COD_CIUDAD        = C.COD_CIUDAD
                  AND O.COD_PAIS           = C2.COD_PAIS
                  AND O.COD_CIUDAD         = C2.COD_CIUDAD                  
                  AND T.COD_EMPRESA        = DM.COD_EMPRESA
                  AND T.COD_TRABAJADOR     = DM.COD_TRABAJADOR
                  AND R.COD_EMPRESA        = T.COD_EMPRESA
                  AND R.COD_TRABAJADOR     = T.COD_TRABAJADOR
                  AND T.COD_CONTRATO       = CO.COD_CONTRATO
                  AND T.COD_EMPRESA        = EMP.COD_EMPRESA
                  AND DE.COD_EMPRESA       = EMP.COD_EMPRESA
                  AND EMP.COD_MONEDA_LOCAL = MONE.COD_MONEDA
                  AND (T.COD_EMPRESA, T.COD_TRABAJADOR) IN (SELECT DISTINCT N.COD_EMPRESA, 
                                                                            N.COD_TRABAJADOR
                                                            FROM NOMINAS N
                                                            WHERE N.COD_EMPRESA = PEMPRESA
                                                            AND N.FECHA BETWEEN PFECHA_D AND PFECHA_H
                                                            AND N.PROCESO = NVL(PPROCESO,N.PROCESO)
                                                            AND N.STATUS != 'P')
                  AND T.COD_TRABAJADOR = HD.COD_TRABAJADOR
                  AND T.COD_EMPRESA    = HD.COD_EMPRESA 
                  AND HD.FECHA         = (SELECT MAX(HD2.FECHA) 
                                          FROM HISTORICO_DEPARTAMENTOS HD2
                                          WHERE T.COD_TRABAJADOR =  HD2.COD_TRABAJADOR
                                          AND   T.COD_EMPRESA    =  HD2.COD_EMPRESA 
                                          AND   HD2.FECHA        <= PFECHA_H)
                  AND HD.COD_EMPRESA      = DEPA.COD_EMPRESA 
                  AND HD.COD_GERENCIA     = DEPA.COD_GERENCIA
                  AND HD.COD_DIRECCION    = DEPA.COD_DIRECCION
                  AND HD.COD_DEPARTAMENTO = DEPA.COD_DEPARTAMENTO                         
                  AND T.COD_TRABAJADOR = HC.COD_TRABAJADOR
                  AND T.COD_EMPRESA    = HC.COD_EMPRESA 
                  AND HC.FECHA         = (SELECT MAX(HC2.FECHA) 
                                          FROM HISTORICOS_CARGOS HC2
                                          WHERE T.COD_TRABAJADOR =  HC2.COD_TRABAJADOR
                                          AND   T.COD_EMPRESA    =  HC2.COD_EMPRESA 
                                          AND   HC2.FECHA        <= PFECHA_H)
                  AND HC.COD_CARGO    = CARGO.COD_CARGO  
                  AND T.COD_TRABAJADOR = SUEL.COD_TRABAJADOR
                  AND T.COD_EMPRESA    = SUEL.COD_EMPRESA   
                  AND SUEL.FECHA        = (SELECT MAX(SUEL1.FECHA)
                                           FROM SUELDOS SUEL1 
                                           WHERE T.COD_EMPRESA    = SUEL1.COD_EMPRESA
                                           AND   T.COD_TRABAJADOR = SUEL1.COD_TRABAJADOR
                                           AND   SUEL1.FECHA       <= PFECHA_H)   
                  AND T.COD_TRABAJADOR = BANCOS.COD_TRABAJADOR
                  AND T.COD_EMPRESA    = BANCOS.COD_EMPRESA  
                  AND BANCOS.COD_BASE  = PBASE   
                  AND BANCOS.COD_BANCO = BAN.COD_BANCO
                  AND BAN.COD_PAIS     = PPAIS
                  AND T.TIPO_PAGO      = 'B'
                  AND (T.COD_CONTRATO IN (SELECT TR.PARAMETRO
                                          FROM TEMPORAL_REPORTES TR
                                          WHERE TR.COLUMNA = 'COD_CONTRATO'
                                          AND TR.SESION = TO_NUMBER(PSESION))
                       OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                      FROM TEMPORAL_REPORTES TR2
                                                      WHERE TR2.COLUMNA = 'COD_CONTRATO'))
                  AND (T.COD_TRABAJADOR IN (SELECT TR.PARAMETRO
                                            FROM TEMPORAL_REPORTES TR
                                            WHERE TR.COLUMNA = 'COD_TRABAJADOR'
                                            AND TR.SESION = TO_NUMBER(PSESION))
                       OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                      FROM TEMPORAL_REPORTES TR2
                                                      WHERE TR2.COLUMNA = 'COD_TRABAJADOR'))                                     
                 AND (T.COD_TRABAJADOR IN (SELECT REMI.COD_TRABAJADOR
                                           FROM REMISION_IOFACTURO REMI
                                           WHERE REMI.COD_EMPRESA = PEMPRESA
                                           AND   REMI.FECHA       = PFECHA_H)
                       OR NOT EXISTS (SELECT '1'
                                      FROM REMISION_IOFACTURO REMI
                                      WHERE REMI.COD_EMPRESA = PEMPRESA
                                      AND   REMI.FECHA       = PFECHA_H))                                    
                 UNION ALL   
                 SELECT  '3.3' VERSION_CFDI,       --2                                                                                                                   -- Campo 1
                         PSERIE SERIE,                                                                                                                                       -- Campo 2
                         PFORMA_PAGO FORMA_PAGO,                                                                                                                             -- Campo 4
                         MONE.SIMBOLO MONEDA,                                                                                                                                -- Campo 10     
                         'N' TIPO_COMPROBANTE,                                                                                                                               -- Campo 12
                         'PUE' METODO_PAGO,                                                                                                                                  -- Campo 13 
                         PLUGAR_EXP LUGAR_EXPEDICION,                                                                                                                        -- Campo 14 
                         O.ID_FISCAL RFC_RECEPTOR,                                                                                                                           -- Campo 15
                         R.MAIL_NAME,
                         REPLACE(REPLACE(T.PRIMER_NOMBRE||DECODE(T.SEGUNDO_NOMBRE, NULL, '', ' '||T.SEGUNDO_NOMBRE)||DECODE(T.SIGUIENTES_NOMBRES, NULL, '', ' '||T.SIGUIENTES_NOMBRES)||' '||T.PRIMER_APELLIDO||DECODE(T.SEGUNDO_APELLIDO, NULL, '', ' '||T.SEGUNDO_APELLIDO)||DECODE(T.SIGUIENTES_APELLIDOS, NULL, '', ' '||T.SIGUIENTES_APELLIDOS),'/',''),'-','') NOMBRE,                    -- Campo 16
                         '' RESIDENCIA_FISCAL,                                                                                                                               -- Campo 17 
                         '' NUM_REG_ID_TRIB,                                                                                                                                 -- Campo 18
                         PUSO_CFDI USO_CFDI,                                                                                                                                 -- Campo 19 
                         PUUID_1 CFDI_RELACIONADO_UUID_1,                                                                                                                    -- Campo 19
                         PUUID_2 CFDI_RELACIONADO_UUID_2,                                                                                                                    -- Campo 20
                         PCONCEPT_CLAVE_PROD_SERV CONCEPTO_CLAVE_PROD_SERV,                                                                                                  -- Campo 22
                         PCONCEPT_NO_IDENTIFICADO CONCEPTO_NO_IDENTIFICACION,                                                                                                -- Campo 23
                         PCONCEPTO_CANTIDAD CONCEPTO_CANTIDAD,                                                                                                               -- Campo 24 
                         PCONCEPTO_CLAVE_UNIDAD CONCEPTO_CLAVE_UNIDAD,                                                                                                       -- Campo 25
                         PCONCEPTO_DESCRIPCION CONCEPTO_DESCRIPCION,                                                                                                         -- Campo 27
                         --MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_VALOR_UNITARIO) CONCEPTO_VALOR_UNITARIO,                        -- Campo 28
                         --TRIM(TO_CHAR(MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_IMPORTE),'999999999999999999.99')) CONCEPTO_IMPORTE,     -- Campo 29
                         --TRIM(TO_CHAR(MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PCONCEPTO_DESCUENTO),'999999999999999999.99')) CONCEPTO_DESCUENTO, -- Campo 30
                         PTIPO_NOMINA TIPO_NOMINA,                                                                                                                           -- Campo 50
                         TO_CHAR(PFECHA_PAGO,'YYYY-MM-DD') FECHA_PAGO,                                                                                                       -- Campo 51
                         TO_CHAR(PFECHA_INI_TIMB,'YYYY-MM-DD')  FECHA_INICIAL_PAGO,  --SERA LOS ITEMS DE FECHAS TIMBRADO                                                     -- Campo 52
                         TO_CHAR(PFECHA_FIN_TIMB,'YYYY-MM-DD')  FECHA_FINAL_PAGO,    --SERA LOS ITEMS DE FECHAS TIMBRADO                                                     -- Campo 53
                         ROUND(CANT_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PNUM_DIAS_PAGADOS)) NUM_DIAS_PAGADOS,                                -- Campo 54
                         PCVE_REGISTRO_FISCAL E_CVE_REGISTRO_FISCAL,                                                                                                         -- Campo 58
                         PE_CURP E_CURP,                                                                                                                                     -- Campo 59
                         DM.REG_PATRONAL E_REGISTRO_PATRONAL,                                                                                                                -- Campo 60
                         PRFC_PATRON_ORIGEN E_RFC_PATRON_ORIGEN,                                                                                                             -- Campo 61
                         PORIGEN_RECURSO ENT_ORIGEN_RECURSO,                                                                                                                 -- Campo 62
                         PRECURSO_PROPIO ENT_RECURSO_PROPIO,                                                                                                                 -- Campo 63
                         O.ID_FISCAL1 R_CURP,                                                                                                                                -- Campo 64
                         NUMERO_SSO R_NUM_SEGURIDAD_SOCIAL,  --O.CEDULA R_NUM_SEGURIDAD_SOCIAL,                                                                              -- Campo 65
                         TO_CHAR(T.FECHA_INGRESO,'YYYY-MM-DD') R_FECHA_INICIO_LABORAL,                                                                                       -- Campo 66
                         DECODE(PFORMATO_ANTI, 'DIA', ANTIGUEDAD(t.cod_empresa,t.cod_trabajador,PFECHA_H,T.FECHA_INGRESO, PFORMATO_ANTI),
                                               'SEM', ANTIGUEDAD(t.cod_empresa,t.cod_trabajador,PFECHA_H,T.FECHA_INGRESO, PFORMATO_ANTI)) R_ANTIGUEDAD,                      -- Campo 67
                         DM.TIPO_CONTRATACION_SAT R_TIPO_CONTRATO,                                                                                                           -- Campo 68
                         DM.SINDICALIZADO_SAT R_SINDICALIZADO,                                                                                                               -- Campo 69
                         DM.TIPO_JORNADA_SAT R_TIPO_JORNADA,                                                                                                                 -- Campo 70
                         DECODE(PREGIMEN,'13',PREGIMEN,DM.TIPO_REGIMEN_SAT) R_TIPO_REGIMEN,                                                                                  -- Campo 71
                         T.COD_TRABAJADOR R_NUM_EMPLEADO,                                                                                                                    -- Campo 72
                         NVL(DEPA.NB_ALTERNO,DEPA.NB_DEPARTAMENTO) R_DEPARTAMENTO,                                                                                           -- Campo 73
                         NVL(CARGO.NB_ALTERNO,CARGO.NB_CARGO) R_PUESTO,                                                                                                      -- Campo 74
                         DM.RIESGO_PUESTO_SAT R_RIEGO_PUESTO,                                                                                                                -- Campo 75
                         /* DECODE(CO.FREC_NOMINA,'Q','04','M','05','S','02','B','03')  R_PERIODICIDAD_PAGO,  */
                         PPERIODICIDAD  R_PERIODICIDAD_PAGO,-- Campo 76                                                                      -- Campo 76
                         '' R_BANCO,                                                                                                                            -- Campo 77
                         '' R_CUENTA_BANCARIA,                                                                                                                 -- Campo 78
                         --DECODE(PSALARIO_BASE_COT,NULL, SUEL.SUELDO,MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_BASE_COT)) R_SALARIO_BASE_COT,                                                                                                                     -- Campo 79
                         --MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSALARIO_DIARIO_INTEGRADO) R_SALARIO_DIARIO,                                        -- Campo 80
                         DM.ENT_FED_SAT R_CLAVE_ENTFED,                                                                                                                        -- Campo 81
                         '' SUB_RFC_LABORA_1,                                                                                                                               -- Campo 82
                         '' SUB_PORCENTAJE_TIEMPO_1,                                                                                                                        -- Campo 83                                                                                                                                         --Campo 49
                         '' PAT_VALOR_MERCADO,                                                                                                                              -- Campo 143
                         '' PAT_PRECIO_AL_OTORG,                                                                                                                            -- Campo 144
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_UNA_EXHIB) JPR_TOTAL_UNA_EXHIB,                                -- Campo 185
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_TOTAL_PARCIALIDAD) JPR_TOTAL_PARCIALIDAD,                            -- Campo 186
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_MONTO_DIARIO) JPR_MONTO_DIARIO,                                      -- Campo 187
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_ACUM) JPR_INGRESO_ACUM,                                      -- Campo 188
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PJPR_INGRESO_NO_ACUM) JPR_INGRESO_NO_ACUM,                                -- Campo 189
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_TOTAL_PAGADO) SI_TOTAL_PAGADO,                                        -- Campo 190
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_NUM_ANOS) SI_NUM_ANOS,                                                -- Campo 191
                         SUEL.SUELDO SI_ULTIMO_SUELDO,                                                                                                                       -- Campo 192
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_ACUM) SI_INGRESO_ACUM,                                       -- Campo 193
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PSI_INGRESO_NO_ACUM) SI_INGRESO_NO_ACUM,                                  -- Campo 194
--                         MONTO_POR_GRUPO(t.cod_empresa,t.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PTOTAL_OTRAS_DEDUCCIONES) D_TOTAL_OTRAS_DEDUCCIONES,                      -- Campo 195
                         E.NB_EMPRESA,
                         (SELECT NVL(RE.UUID2,RE.UUID1) FROM REMISION_IOFACTURO RE WHERE RE.COD_EMPRESA = T.COD_EMPRESA AND RE.COD_TRABAJADOR = T.COD_TRABAJADOR AND RE.FECHA = PFECHA_H) UUID, 
                         (SELECT RE.RELACION FROM REMISION_IOFACTURO RE WHERE RE.COD_EMPRESA = T.COD_EMPRESA AND RE.COD_TRABAJADOR = T.COD_TRABAJADOR AND RE.FECHA = PFECHA_H) RELACION,
                         T.COD_CONTRATO,
                         T.COD_EMPRESA,
                         T.COD_TRABAJADOR,
                         LEAST(NVL(T.FECHA_EGRESO,PFECHA_H),PFECHA_H)  FECHA_ANTI_FINAL,
                         T.FECHA_INGRESO FECHA_ANTI_INICIAL,
                         SUC.NB_SUCURSAL NOMBRE_SUCURSAL,
                         SUEL.SUELDO
                    FROM TRABAJADORES T,
                         DATOS_EMPRESAS1 DE,
                         ORIGEN_TRABAJADORES O,
                         DOMICILIO_TRABAJADORES D,
                         PAISES P,
                         CIUDADES C,
                         CIUDADES C2,
                         DATOS_TRABAJADORES_MEX DM,
                         EMPRESAS E,
                         RR_HH_TRABAJADORES R, 
                         CONTRATOS CO,
                         EMPRESAS1 EMP,
                         MONEDAS1 MONE,
                         HISTORICO_DEPARTAMENTOS HD,
                         DEPARTAMENTOS DEPA,
                         HISTORICOS_CARGOS HC,
                         CARGOS CARGO,
                         SUELDOS SUEL,
                         SUCURSALES SUC
                     WHERE T.COD_EMPRESA       = O.COD_EMPRESA
                      AND T.COD_TRABAJADOR     = O.COD_TRABAJADOR
                      AND T.COD_EMPRESA        = D.COD_EMPRESA
                      AND T.COD_TRABAJADOR     = D.COD_TRABAJADOR                  
                      AND T.COD_EMPRESA        = DE.COD_EMPRESA
                      AND T.COD_EMPRESA        = E.COD_EMPRESA
                      AND T.COD_SUCURSAL       = SUC.COD_SUCURSAL
                      AND T.COD_EMPRESA        = SUC.COD_EMPRESA
                      AND DE.COD_PAIS          = P.COD_PAIS
                      AND DE.COD_PAIS          = C.COD_PAIS
                      AND DE.COD_CIUDAD        = C.COD_CIUDAD
                      AND O.COD_PAIS           = C2.COD_PAIS
                      AND O.COD_CIUDAD         = C2.COD_CIUDAD                  
                      AND T.COD_EMPRESA        = DM.COD_EMPRESA
                      AND T.COD_TRABAJADOR     = DM.COD_TRABAJADOR
                      AND R.COD_EMPRESA        = T.COD_EMPRESA
                      AND R.COD_TRABAJADOR     = T.COD_TRABAJADOR
                      AND T.COD_CONTRATO       = CO.COD_CONTRATO
                      AND T.COD_EMPRESA        = EMP.COD_EMPRESA
                      AND DE.COD_EMPRESA       = EMP.COD_EMPRESA
                      AND EMP.COD_MONEDA_LOCAL = MONE.COD_MONEDA
                      AND (T.COD_EMPRESA, T.COD_TRABAJADOR) IN (SELECT DISTINCT N.COD_EMPRESA, 
                                                                                N.COD_TRABAJADOR
                                                                FROM NOMINAS N
                                                                WHERE N.COD_EMPRESA = PEMPRESA
                                                                AND N.FECHA BETWEEN PFECHA_D AND PFECHA_H
                                                                AND N.PROCESO = NVL(PPROCESO,N.PROCESO)
                                                                AND N.STATUS != 'P')
                      AND T.COD_TRABAJADOR = HD.COD_TRABAJADOR
                      AND T.COD_EMPRESA    = HD.COD_EMPRESA 
                      AND HD.FECHA         = (SELECT MAX(HD2.FECHA) 
                                              FROM HISTORICO_DEPARTAMENTOS HD2
                                              WHERE T.COD_TRABAJADOR =  HD2.COD_TRABAJADOR
                                              AND   T.COD_EMPRESA    =  HD2.COD_EMPRESA 
                                              AND   HD2.FECHA        <= PFECHA_H)
                      AND HD.COD_EMPRESA      = DEPA.COD_EMPRESA 
                      AND HD.COD_GERENCIA     = DEPA.COD_GERENCIA
                      AND HD.COD_DIRECCION    = DEPA.COD_DIRECCION
                      AND HD.COD_DEPARTAMENTO = DEPA.COD_DEPARTAMENTO                         
                      AND T.COD_TRABAJADOR = HC.COD_TRABAJADOR
                      AND T.COD_EMPRESA    = HC.COD_EMPRESA 
                      AND HC.FECHA         = (SELECT MAX(HC2.FECHA) 
                                              FROM HISTORICOS_CARGOS HC2
                                              WHERE T.COD_TRABAJADOR =  HC2.COD_TRABAJADOR
                                              AND   T.COD_EMPRESA    =  HC2.COD_EMPRESA 
                                              AND   HC2.FECHA        <= PFECHA_H)
                      AND HC.COD_CARGO     = CARGO.COD_CARGO  
                      AND T.COD_TRABAJADOR = SUEL.COD_TRABAJADOR
                      AND T.COD_EMPRESA    = SUEL.COD_EMPRESA   
                      AND SUEL.FECHA       = (SELECT MAX(SUEL1.FECHA)
                                              FROM SUELDOS SUEL1 
                                              WHERE T.COD_EMPRESA    = SUEL1.COD_EMPRESA
                                              AND   T.COD_TRABAJADOR = SUEL1.COD_TRABAJADOR
                                              AND   SUEL1.FECHA       <= PFECHA_H) 
                      AND T.TIPO_PAGO     IN ('C','E')                            
                      AND (T.COD_CONTRATO IN (SELECT TR.PARAMETRO
                                              FROM TEMPORAL_REPORTES TR
                                              WHERE TR.COLUMNA = 'COD_CONTRATO'
                                              AND TR.SESION = TO_NUMBER(PSESION))
                           OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                         FROM TEMPORAL_REPORTES TR2
                                                         WHERE TR2.COLUMNA = 'COD_CONTRATO'))
                      AND (T.COD_TRABAJADOR IN (SELECT TR.PARAMETRO
                                                FROM TEMPORAL_REPORTES TR
                                                WHERE TR.COLUMNA = 'COD_TRABAJADOR'
                                                AND TR.SESION = TO_NUMBER(PSESION))
                           OR TO_NUMBER(PSESION) NOT IN (SELECT TR2.SESION
                                                         FROM TEMPORAL_REPORTES TR2
                                                         WHERE TR2.COLUMNA = 'COD_TRABAJADOR'))                                      
                      AND (T.COD_TRABAJADOR IN (SELECT REMI.COD_TRABAJADOR
                                                FROM REMISION_IOFACTURO REMI
                                                WHERE REMI.COD_EMPRESA = PEMPRESA
                                                AND   REMI.FECHA       = PFECHA_H)
                           OR NOT EXISTS (SELECT '1'
                                          FROM REMISION_IOFACTURO REMI
                                          WHERE REMI.COD_EMPRESA = PEMPRESA
                                          AND   REMI.FECHA       = PFECHA_H))) QUERY1 
                       WHERE CG.COD_CONTRATO = QUERY1.COD_CONTRATO
                       GROUP BY  QUERY1.VERSION_CFDI
                                ,QUERY1.SERIE
                                ,QUERY1.FORMA_PAGO
                                ,QUERY1.MONEDA
                                ,QUERY1.TIPO_COMPROBANTE
                                ,QUERY1.METODO_PAGO
                                ,QUERY1.LUGAR_EXPEDICION
                                ,QUERY1.RFC_RECEPTOR
                                ,QUERY1.MAIL_NAME
                                ,QUERY1.NOMBRE
                                ,QUERY1.RESIDENCIA_FISCAL
                                ,QUERY1.NUM_REG_ID_TRIB
                                ,QUERY1.USO_CFDI
                                ,QUERY1.CFDI_RELACIONADO_UUID_1
                                ,QUERY1.CFDI_RELACIONADO_UUID_2
                                ,QUERY1.CONCEPTO_CLAVE_PROD_SERV
                                ,QUERY1.CONCEPTO_NO_IDENTIFICACION
                                ,QUERY1.CONCEPTO_CANTIDAD
                                ,QUERY1.CONCEPTO_CLAVE_UNIDAD
                                ,QUERY1.CONCEPTO_DESCRIPCION
                                ,QUERY1.TIPO_NOMINA
                                ,QUERY1.FECHA_PAGO
                                ,QUERY1.FECHA_INICIAL_PAGO
                                ,QUERY1.FECHA_FINAL_PAGO
                                ,QUERY1.NUM_DIAS_PAGADOS
                                ,QUERY1.E_CVE_REGISTRO_FISCAL
                                ,QUERY1.E_CURP
                                ,QUERY1.E_REGISTRO_PATRONAL
                                ,QUERY1.E_RFC_PATRON_ORIGEN
                                ,QUERY1.ENT_ORIGEN_RECURSO
                                ,QUERY1.ENT_RECURSO_PROPIO
                                ,QUERY1.R_CURP
                                ,QUERY1.R_NUM_SEGURIDAD_SOCIAL
                                ,QUERY1.R_FECHA_INICIO_LABORAL
                                ,QUERY1.R_ANTIGUEDAD
                                ,QUERY1.R_TIPO_CONTRATO
                                ,QUERY1.R_SINDICALIZADO
                                ,QUERY1.R_TIPO_JORNADA
                                ,CG.COD_REGIMEN                                         --QUERY1.R_TIPO_REGIMEN
                                ,QUERY1.R_TIPO_REGIMEN                                  --TipoRegimen anterior
                                ,QUERY1.R_NUM_EMPLEADO
                                ,QUERY1.R_DEPARTAMENTO
                                ,QUERY1.R_PUESTO
                                ,QUERY1.R_RIEGO_PUESTO
                                ,QUERY1.R_PERIODICIDAD_PAGO
                                ,QUERY1.R_BANCO
                                ,QUERY1.R_CUENTA_BANCARIA
                                ,QUERY1.R_CLAVE_ENTFED
                                ,QUERY1.SUB_RFC_LABORA_1
                                ,QUERY1.SUB_PORCENTAJE_TIEMPO_1
                                ,QUERY1.PAT_VALOR_MERCADO
                                ,QUERY1.PAT_PRECIO_AL_OTORG
                                ,QUERY1.SI_ULTIMO_SUELDO
                                ,QUERY1.NB_EMPRESA
                                ,QUERY1.UUID
                                ,QUERY1.RELACION
                                ,QUERY1.COD_CONTRATO
                                ,QUERY1.COD_EMPRESA
                                ,QUERY1.COD_TRABAJADOR
                                ,QUERY1.FECHA_ANTI_FINAL
                                ,QUERY1.FECHA_ANTI_INICIAL
                                ,QUERY1.NOMBRE_SUCURSAL,
                                 QUERY1.SUELDO
                        ORDER BY QUERY1.NOMBRE) 
    LOOP
    --
     IF PSI_INGRESO_ACUM IS NULL AND PSI_INGRESO_NO_ACUM  IS NULL 
        AND  PSI_TOTAL_PAGADO IS NULL AND  PSI_TOTAL_PAGADO IS NULL 
        AND NVL(CLIE.COD_REGIMEN,CLIE.R_TIPO_REGIMEN) != '13' 
        AND PREGIMEN = '02' AND CLIE.TOTAL_SEPARACION  ='0' THEN
        --NUEVO CASO 17553 
        VANO := 0;
        VMES := 0;
        VDIA := 0;
        VNUM_ANTIGUEDAD := 0;
        /*ANTIG_TOTAL(CLIE.COD_TRABAJADOR,
                    CLIE.COD_EMPRESA,
                    CLIE.FECHA_ANTI_FINAL,
                    CLIE.FECHA_ANTI_INICIAL,
                    VANO,
                    VMES,
                    VDIA);*/
        FILE_IOFACT_MEX_XML_STT.ANTIGUEDAD_TOTAL ( CLIE.FECHA_ANTI_FINAL,
                                                   CLIE.FECHA_ANTI_INICIAL,
                                                   VANO,
                                                   VMES,
                                                   VDIA);            
        VNUM_ANTIGUEDAD := NVL(VANO,0);
        IF NVL(VMES,0) >= 6 THEN
            VNUM_ANTIGUEDAD := VNUM_ANTIGUEDAD + 1;
        END IF;               
        --             
        MCONTADOR   := MCONTADOR + 1;
        MFOLIO_CONT := MFOLIO_CONT + 1;
        --
        MLINEA := '<Documento>'||CHR(13);                                                                                --INI DOCUMENTO
        --
        MLINEA := MLINEA||'<Encabezado';                                                                                 --INI ENCABEZADO
        MLINEA := MLINEA||' RegimenContableR=""';
        MLINEA := MLINEA||' NmbRecep="'||CLIE.NOMBRE||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --
        MLINEA := MLINEA||'<IdDoc';
        MLINEA := MLINEA||' ContenidoTC=""';
        MLINEA := MLINEA||' Tipo="'||CLIE.TIPO_COMPROBANTE||'"';
        MLINEA := MLINEA||' Serie="'||CLIE.SERIE||'"';
        MLINEA := MLINEA||' FechaEmis="'||TO_CHAR(LOCALTIMESTAMP,'DD-MM-YYYY HH24:MI:SS')||'"';
        MLINEA := MLINEA||' TipoServicio="'||CLIE.USO_CFDI||'"';
        MLINEA := MLINEA||' FormaPago="'||CLIE.FORMA_PAGO||'"';
        MLINEA := MLINEA||' MedioPago="'||CLIE.METODO_PAGO||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        MLINEA := MLINEA||'<OtrosDocumentos';
        IF CLIE.RELACION IS NOT NULL THEN
            MLINEA := MLINEA||' Relacion="'||CLIE.RELACION||'"';
        END IF;
        MLINEA := MLINEA||'>'||CHR(13); 
        MLINEA := MLINEA||'<Documentos>'||CHR(13);
        MLINEA := MLINEA||'<Documento';
        IF CLIE.UUID IS NOT NULL THEN 
            MLINEA := MLINEA||' IdDocumento="'||CLIE.UUID||'"';
        END IF;
        IF PFOLIO IS NOT NULL THEN 
            MLINEA := MLINEA||' Folio="'||MFOLIO_CONT||'"';
        END IF;
        MLINEA := MLINEA||'></Documento>'||CHR(13);            --FIN DOCUMENTO
        MLINEA := MLINEA||'</Documentos>'||CHR(13);            --FIN DOCUMENTOS
        MLINEA := MLINEA||'</OtrosDocumentos>'||CHR(13);       --FIN OTROSDOCUMENTOS  
        MLINEA := MLINEA||'</IdDoc>'||CHR(13);                 --FIN IDDOC
        MLINEA := MLINEA||'<Emisor';
        MLINEA := MLINEA||' RegimenContable="'||CLIE.E_CVE_REGISTRO_FISCAL||'"';
        MLINEA := MLINEA||' NmbEmisor="'||CLIE.E_RFC_PATRON_ORIGEN||'"';
        MLINEA := MLINEA||'>'||CHR(13); 
        MLINEA := MLINEA||'<NombreEmisor>'||CLIE.NB_EMPRESA||'</NombreEmisor>'||CHR(13); --FIN NOMBREEMISOR 
        MLINEA := MLINEA||'<LugarExped';
           MLINEA := MLINEA||' CodigoPostal="'||CLIE.LUGAR_EXPEDICION||'"';
        MLINEA := MLINEA||'></LugarExped>'||CHR(13);                                      --FIN LUGAREXPED
        MLINEA := MLINEA||'</Emisor>'||CHR(13);                                           --FIN EMISOR
        MLINEA := MLINEA||'<DocRecep';   
           MLINEA := MLINEA||' NroDocRecep="'||CLIE.RFC_RECEPTOR||'"';
           MLINEA := MLINEA||' eMail="'||CLIE.MAIL_NAME||'"'; --jj email 
        MLINEA := MLINEA||'></DocRecep>'||CHR(13);       --FIN DOCRECEP
        MLINEA := MLINEA||'<CodigoReceptor';
           MLINEA := MLINEA||' TpoCdgIntRecep="'||CLIE.RESIDENCIA_FISCAL||'"';
        MLINEA := MLINEA||'></CodigoReceptor>'||CHR(13); --FIN CodigoReceptor
        MLINEA := MLINEA||'<Totales';
        MLINEA := MLINEA||' Moneda="'||CLIE.MONEDA||'"';
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' SubTotal="'||TRIM(TO_CHAR((NVL(CLIE.SUBTOTAL,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' SubTotal="'||TRIM(TO_CHAR(CLIE.SUBTOTAL,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||' MntDcto="0.00"';--dperaza
        ELSE         
            MLINEA := MLINEA||' MntDcto="'||CLIE.DESCUENTO||'"';
        END IF;
        --
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' VlrPagar="'||TRIM(TO_CHAR((NVL(CLIE.TOTAL,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' VlrPagar="'||TRIM(TO_CHAR(CLIE.TOTAL,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'></Totales>'||CHR(13); 
        MLINEA := MLINEA||'</Encabezado>'||CHR(13);
        MLINEA := MLINEA||'<Detalle';
        MLINEA := MLINEA||' NroLinDet="'||MCONTADOR||'"';
        MLINEA := MLINEA||' DscItem="'||CLIE.CONCEPTO_DESCRIPCION||'"';
        MLINEA := MLINEA||' QtyItem="'||CLIE.CONCEPTO_CANTIDAD||'"';
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' PrcNetoItem="'||TRIM(TO_CHAR((NVL(CLIE.CONCEPTO_VALOR_UNITARIO,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' PrcNetoItem="'||TRIM(TO_CHAR(CLIE.CONCEPTO_VALOR_UNITARIO,'999999999999999990.00'))||'"'; 
        END IF;
        MLINEA := MLINEA||' DescuentoMonto="'||TRIM(TO_CHAR(CLIE.CONCEPTO_DESCUENTO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||' MontoNetoItem="'||TRIM(TO_CHAR(CLIE.CONCEPTO_IMPORTE,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
                    --Repite                           
        MLINEA := MLINEA||'<CdgItem';
        MLINEA := MLINEA||' TpoCodigo="CPS"';
        MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_CLAVE_PROD_SERV||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                    --
        MLINEA := MLINEA||'<CdgItem';
           MLINEA := MLINEA||' TpoCodigo="SKU"';
           MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_NO_IDENTIFICACION||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                    --
        MLINEA := MLINEA||'<CdgItem';
           MLINEA := MLINEA||' TpoCodigo="CU"';
           MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_CLAVE_UNIDAD||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                --
        MLINEA := MLINEA||'</Detalle>'||CHR(13);
        MLINEA := MLINEA||'<Complementos>'||CHR(13); 
        MLINEA := MLINEA||'<Payroll>'||CHR(13);
        MLINEA := MLINEA||'<Resumen';
        MLINEA := MLINEA||' TipoNomina="'||CLIE.TIPO_NOMINA||'"';
        MLINEA := MLINEA||' FechaPago="'||CLIE.FECHA_PAGO||'"';
        MLINEA := MLINEA||' FechaInicialPago="'||CLIE.FECHA_INICIAL_PAGO||'"';--SERA LOS ITEMS DE FECHAS TIMBRADO
        MLINEA := MLINEA||' FechaFinalPago="'||CLIE.FECHA_FINAL_PAGO||'"';--SERA LOS ITEMS DE FECHAS TIMBRADO
        --
        IF CLIE.NUM_DIAS_PAGADOS IS NULL OR CLIE.NUM_DIAS_PAGADOS = '0' THEN -- jj caso 17578
            MLINEA := MLINEA||' NumDiasPagados="1"';
        ELSE 
            MLINEA := MLINEA||' NumDiasPagados="'||CLIE.NUM_DIAS_PAGADOS||'"';
        END IF;
        --
        MLINEA := MLINEA||' TotalPercepciones="'||TRIM(TO_CHAR(CLIE.TOTAL_PERCEPCIONES,'999999999999999990.00'))||'"';
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||'';--dperaza
        ELSE 
            MLINEA := MLINEA||' TotalDeducciones="'||TRIM(TO_CHAR(CLIE.TOTAL_DEDUCCIONES,'999999999999999990.00'))||'"';
        END IF;
       ---
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jesus
            MLINEA := MLINEA||' TotalOtrosPagos="'||TRIM(TO_CHAR((NVL(CLIE.TOTAL_OTROS_PAGOS,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' TotalOtrosPagos="'||TRIM(TO_CHAR(CLIE.TOTAL_OTROS_PAGOS,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'></Resumen>'||CHR(13);
        MLINEA := MLINEA||'<NominaEmisor';
        MLINEA := MLINEA||' Curp="'||CLIE.E_CURP||'"';
        MLINEA := MLINEA||' RegistroPatronal="'||CLIE.E_REGISTRO_PATRONAL||'"';
        MLINEA := MLINEA||' IdPatronOrigen="'||CLIE.E_RFC_PATRON_ORIGEN||'"';
        MLINEA := MLINEA||' OrigenRecurso="'||CLIE.ENT_ORIGEN_RECURSO||'"';
        MLINEA := MLINEA||' MontoRecursoPropio="'||TRIM(TO_CHAR(CLIE.ENT_RECURSO_PROPIO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'></NominaEmisor>'||CHR(13);
        MLINEA := MLINEA||'<NominaReceptor';
        MLINEA := MLINEA||' Curp="'||CLIE.R_CURP||'"';
        MLINEA := MLINEA||' NumSeguridadSocial="'||CLIE.R_NUM_SEGURIDAD_SOCIAL||'"';
        MLINEA := MLINEA||' FechaInicioRelacion="'||CLIE.R_FECHA_INICIO_LABORAL||'"';
        --        
        IF PFORMATO_ANTI = 'AMD' THEN --GG CASO 23056
            IF VANO = '0' AND VMES ='0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VDIA||'D"';
            ELSIF VANO != '0' AND VMES ='0' AND VDIA != '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y'||VDIA||'D"';
            ELSIF VANO = '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VMES||'M'||VDIA||'D"';
            ELSIF VANO != '0' AND VMES ='0' AND VDIA = '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y"';    
            ELSE
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y'||VMES||'M'||VDIA||'D"';
            END IF;
        ELSE
            MLINEA := MLINEA||' Antigüedad="'||CLIE.R_ANTIGUEDAD||'"';
        END IF;
        --
        MLINEA := MLINEA||' Sindicalizado="'||CLIE.R_SINDICALIZADO||'"';
        MLINEA := MLINEA||' TipoContrato="'||CLIE.R_TIPO_CONTRATO||'"';
        MLINEA := MLINEA||' TipoJornada="'||CLIE.R_TIPO_JORNADA||'"';
        MLINEA := MLINEA||' TipoRegimen="'||NVL(CLIE.COD_REGIMEN,CLIE.R_TIPO_REGIMEN)||'"';     --SE CAMBIÓ LA LÓGICA DEL VALOR TipoRegimen
        MLINEA := MLINEA||' NumEmpleado="'||CLIE.R_NUM_EMPLEADO||'"';
        MLINEA := MLINEA||' Departamento="'||CLIE.R_DEPARTAMENTO||'"';
        MLINEA := MLINEA||' Puesto="'||CLIE.R_PUESTO||'"';
        MLINEA := MLINEA||' RiesgoPuesto="'||CLIE.R_RIEGO_PUESTO||'"';
        MLINEA := MLINEA||' PeriodicidadPago="'||CLIE.R_PERIODICIDAD_PAGO||'"';
        MLINEA := MLINEA||' Banco="'||CLIE.R_BANCO||'"';
        MLINEA := MLINEA||' CuentaBancaria="'||CLIE.R_CUENTA_BANCARIA||'"';
        MLINEA := MLINEA||' SalarioBase="'||TRIM(TO_CHAR(CLIE.R_SALARIO_BASE_COT,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||' SalarioDiarioIntegrado ="'||TRIM(TO_CHAR(CLIE.R_SALARIO_DIARIO_INTEGRADO,'999999999999999990.00'))||'"';--PSALARIO_DIARIO_INTEGRADO
        MLINEA := MLINEA||' SalarioDiario ="'||TRIM(TO_CHAR(CLIE.R_SALARIO_DIARIO,'999999999999999990.00'))||'"';                   --NUEVA ETIQUETA PARA EL NUEVO PARÁMETRO 
        MLINEA := MLINEA||' ClaveEntFed= "'||CLIE.R_CLAVE_ENTFED||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        MLINEA := MLINEA||'<SubContratacion';
           MLINEA := MLINEA||' PorcentajeTiempo="'||CLIE.SUB_PORCENTAJE_TIEMPO_1||'"';
           MLINEA := MLINEA||' IdLabora="'||CLIE.SUB_RFC_LABORA_1||'"';
        MLINEA := MLINEA||'/>'||CHR(13); 
        MLINEA := MLINEA||'</NominaReceptor>'||CHR(13); 
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --***************************************PERCEPCIONES/HORAS EXTR************************************************             
        MLINEA :=            '<Percepciones';
           MLINEA := MLINEA||' TotalSueldos="'||TRIM(TO_CHAR(CLIE.TOTAL_SUELDOS,'999999999999999990.00'))||'"';
           MLINEA := MLINEA||' TotalSeparacionIndemnizacion="'||TRIM(TO_CHAR(CLIE.TOTAL_SEPARACION,'999999999999999990.00'))||'"';
           IF TRIM(TO_CHAR(CLIE.TOTAL_JUBILACION,'999999999999999990.00')) != '0.00' THEN -- caso 23141
              MLINEA := MLINEA||' TotalJubilacionPensionRetiro="'||TRIM(TO_CHAR(CLIE.TOTAL_JUBILACION,'999999999999999990.00'))||'"';
           END IF;
           MLINEA := MLINEA||' TotalGravado="'||TRIM(TO_CHAR(CLIE.TOTAL_GRAVADO,'999999999999999990.00'))||'"';
           MLINEA := MLINEA||' TotalExento="'||TRIM(TO_CHAR(CLIE.TOTAL_EXENTO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --PERCEPCIONES/HORAS EXTRA
       --INICIO QUERY PERCEP
        VPERCEP_EXISTE := 'N';
        VHORA_EXISTE   := 'N';
        VDEDU_EXISTE   := 'N'; 
        VOTROS_EXISTE  := 'N';
        VINCAP_EXISTE  := 'N';          
        FOR PERC IN CODIFI(CLIE.COD_CONTRATO,'P',NULL,CLIE.COD_REGIMEN)--JM
        LOOP
            VPERCEP_EXISTE := 'S';
            VHORA_EXISTE   := 'N'; 
            IF TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'G','P',CLIE.COD_CONTRATO),'999999999999999990.00')) IS NULL 
            AND TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'E','P',CLIE.COD_CONTRATO),'999999999999999990.00')) IS NULL THEN
                MLINEA := MLINEA||'<Percepcion';
                   MLINEA := MLINEA||' TipoPercepcion=""';
                   MLINEA := MLINEA||' Clave=""';
                   MLINEA := MLINEA||' Concepto=""';
                   MLINEA := MLINEA||' ImporteGravado=""';
                   MLINEA := MLINEA||' ImporteExento=""';
                   MLINEA := MLINEA||' ValorMercado=""';
                   MLINEA := MLINEA||' PrecioAlOtorgarse=""';
                MLINEA := MLINEA||'>'||CHR(13);
                MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias=""';
                       MLINEA := MLINEA||' TipoHoras=""';
                       MLINEA := MLINEA||' HorasExtra=""';
                       MLINEA := MLINEA||' ImportePagado=""';
                MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                MLINEA := MLINEA||'</Percepcion>'||CHR(13);
            ELSE 
                MLINEA := MLINEA||'<Percepcion';    
                       MLINEA := MLINEA||' TipoPercepcion="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,'P',CLIE.COD_CONTRATO)||'"';
                           MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,'P',CLIE.COD_CONTRATO)||'"';
                           MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,1,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'P'),'/',''),'-','')||'"';
                       MLINEA := MLINEA||' ImporteGravado="'||TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'G','P',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
                       MLINEA := MLINEA||' ImporteExento="'||TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'E','P',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
                       MLINEA := MLINEA||' ValorMercado=""';
                       MLINEA := MLINEA||' PrecioAlOtorgarse=""';
                MLINEA := MLINEA||'>'||CHR(13);
            
                FOR HORA  IN CODIFI_H(CLIE.COD_CONTRATO,'H',PERC.COD_REGISTRO,CLIE.COD_REGIMEN) --JM
                LOOP
                   VHORA_EXISTE := 'S'; 
                   --INICIAL CURSOR HORA EXTRA
                   MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias="'||CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'D','H',CLIE.COD_CONTRATO)||'"';
                       MLINEA := MLINEA||' TipoHoras="'||TIPO_HORAS(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'H',CLIE.COD_CONTRATO)||'"';
                       MLINEA := MLINEA||' HorasExtra="'||ROUND(CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'H','H',CLIE.COD_CONTRATO))||'"';
                       MLINEA := MLINEA||' ImportePagado="'||TRIM(TO_CHAR(CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'M','H',CLIE.COD_CONTRATO) ,'999999999999999990.00'))||'"';
                   MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                   MLINEA := MLINEA||'</Percepcion>'||CHR(13);
                   --FIN DE CURSOR HORAS EXTRAS
                   --FIN DE CURSOR PERCEPCION
                END LOOP;
                --
                IF VHORA_EXISTE = 'N' THEN 
                   MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias=""';
                       MLINEA := MLINEA||' TipoHoras=""';
                       MLINEA := MLINEA||' HorasExtra=""';
                       MLINEA := MLINEA||' ImportePagado=""';
                   MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                   MLINEA := MLINEA||'</Percepcion>'||CHR(13);
                END IF;
            END IF;     
        END LOOP;
        --
        IF VPERCEP_EXISTE = 'N' THEN 
           MLINEA := MLINEA||'<Percepcion';
               MLINEA := MLINEA||' TipoPercepcion=""';
               MLINEA := MLINEA||' Clave=""';
               MLINEA := MLINEA||' Concepto=""';
               MLINEA := MLINEA||' ImporteGravado=""';
               MLINEA := MLINEA||' ImporteExento=""';
               MLINEA := MLINEA||' ValorMercado=""';
               MLINEA := MLINEA||' PrecioAlOtorgarse=""';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<HorasExtra';
               MLINEA := MLINEA||' Dias=""';
               MLINEA := MLINEA||' TipoHoras=""';
               MLINEA := MLINEA||' HorasExtra=""';
               MLINEA := MLINEA||' ImportePagado=""';
           MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
           MLINEA := MLINEA||'</Percepcion>'||CHR(13);
        END IF; 
        --
        MLINEA := MLINEA||'<JubilacionPensionRetiro';
        IF CLIE.JPR_INGRESO_ACUM IS NULL OR CLIE.JPR_INGRESO_ACUM = '0' THEN
            MLINEA := MLINEA||' IngresoAcumulable=""';
        ELSE
            MLINEA := MLINEA||' IngresoAcumulable="'||TRIM(TO_CHAR(CLIE.JPR_INGRESO_ACUM,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.JPR_TOTAL_PARCIALIDAD IS NULL OR CLIE.JPR_TOTAL_PARCIALIDAD = '0' THEN
        MLINEA := MLINEA||' TotalParcialidad=""';    
        ELSE
        MLINEA := MLINEA||' TotalParcialidad="'||TRIM(TO_CHAR(CLIE.JPR_TOTAL_PARCIALIDAD,'999999999999999990.00'))||'"';
        END IF;       
        --
        IF CLIE.JPR_MONTO_DIARIO IS NULL OR CLIE.JPR_MONTO_DIARIO = '0' THEN
        MLINEA := MLINEA||' MontoDiario=""';
        ELSE
        MLINEA := MLINEA||' MontoDiario="'||TRIM(TO_CHAR(CLIE.JPR_MONTO_DIARIO,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.JPR_TOTAL_UNA_EXHIB IS NULL OR CLIE.JPR_TOTAL_UNA_EXHIB = '0' THEN
        MLINEA := MLINEA||' TotalUnaExhibicion=""';
        ELSE
        MLINEA := MLINEA||' TotalUnaExhibicion="'||TRIM(TO_CHAR(CLIE.JPR_TOTAL_UNA_EXHIB,'999999999999999990.00'))||'"';
        END IF;
        -- 
        IF CLIE.JPR_INGRESO_NO_ACUM IS NULL OR CLIE.JPR_INGRESO_NO_ACUM = '0' THEN
        MLINEA := MLINEA||' IngresoNoAcumulable=""';
        ELSE
        MLINEA := MLINEA||' IngresoNoAcumulable="'||TRIM(TO_CHAR(CLIE.JPR_INGRESO_NO_ACUM,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'/>'||CHR(13);
        MLINEA := MLINEA||'<SeparacionIndemnizacion';
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' IngresoAcumulable=""';
       ELSE
       MLINEA := MLINEA||' IngresoAcumulable="'||TRIM(TO_CHAR(CLIE.SI_INGRESO_ACUM,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' TotalPagado=""';
       ELSE
       MLINEA := MLINEA||' TotalPagado="'||TRIM(TO_CHAR(CLIE.SI_TOTAL_PAGADO,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' UltimoSueldoMensOrd=""';
       ELSE
       MLINEA := MLINEA||' UltimoSueldoMensOrd="'||TRIM(TO_CHAR(CLIE.SI_ULTIMO_SUELDO,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' NumAñosServicio=""'; 
       ELSE 
       MLINEA := MLINEA||' NumAñosServicio="'||VNUM_ANTIGUEDAD||'"'; 
       END IF;             
       --
       IF CLIE.SI_INGRESO_NO_ACUM IS NULL OR CLIE.SI_INGRESO_NO_ACUM = '0' THEN
       MLINEA := MLINEA||' IngresoNoAcumulable=""';
       ELSE
       MLINEA := MLINEA||' IngresoNoAcumulable="'||TRIM(TO_CHAR(CLIE.SI_INGRESO_NO_ACUM,'999999999999999990.00'))||'"';
       END IF;
        MLINEA := MLINEA||'/>'||CHR(13);
        --
        MLINEA := MLINEA||'</Percepciones>'||CHR(13); 
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************
       --***************************************DEDUCCIONES ************************************************          
        MLINEA := '<Deducciones';
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||' TotalOtrasDeducciones=""';
        ELSE 
            MLINEA := MLINEA||' TotalOtrasDeducciones="'||TRIM(TO_CHAR(CLIE.D_TOTAL_OTRAS_DEDUCCIONES,'999999999999999990.00'))||'"';
        END IF; 
            MLINEA := MLINEA||' TotalImpuestosRetenidos="'||TRIM(TO_CHAR(CLIE.D_TOTAL_IMPUESTOS_RETENIDOS,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --INICIO DEL CURSOR
        FOR DEDU  IN CODIFI(CLIE.COD_CONTRATO,'D',NULL,CLIE.COD_REGIMEN)  --JASON
        LOOP
        VDEDU_EXISTE := 'S';
        MLINEA := MLINEA||'<Deduccion';
            MLINEA := MLINEA||' TipoDeduccion="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO)||'"';
            MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO)||'"';
            MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,2,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'D'),'/',''),'-','')||'"';
            MLINEA := MLINEA||' Importe="'||TRIM(TO_CHAR(MONTO_CONCEPTO_DESCUENTO(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'></Deduccion>'||CHR(13); 
        END LOOP;
       --
        IF VDEDU_EXISTE = 'N' THEN 
        MLINEA := MLINEA||'<Deduccion';
            MLINEA := MLINEA||' TipoDeduccion=""';
            MLINEA := MLINEA||' Clave=""';
            MLINEA := MLINEA||' Concepto=""';
            MLINEA := MLINEA||' Importe=""';
        MLINEA := MLINEA||'></Deduccion>'||CHR(13); 
        END IF;                                
        --FIN EL CURSOR
        MLINEA := MLINEA||'</Deducciones>'||CHR(13);
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************   
       --***************************************OTRO PAGO ************************************************             
        MLINEA := '<OtrosPagos>'||CHR(13); 
        --INICIO DEL CURSOR
        FOR OTRO IN CODIFI(CLIE.COD_CONTRATO,'O',NULL,CLIE.COD_REGIMEN)  --JASON
        LOOP
           VOTROS_EXISTE  := 'S';
           --
           IF OTRO.COD_REGISTRO = 1 THEN
             VDIF_PAGO := 'S';  
           ELSE 
             VDIF_PAGO := NULL;
           END IF;
           -- 
           MLINEA := MLINEA||'<OtroPago';--DPERAZAOTROPAGO
               MLINEA := MLINEA||' TipoOtroPago="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,'O',CLIE.COD_CONTRATO,VDIF_PAGO)||'"';
               MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,'O',CLIE.COD_CONTRATO,VDIF_PAGO)||'"';
               MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,3,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'O',VDIF_PAGO),'/',''),'-','')||'"';
               MLINEA := MLINEA||' Importe="'||TRIM(TO_CHAR(MONTO_CONCEPTO_OTROS_PAGOS (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,CLIE.COD_CONTRATO,'N','O',VDIF_PAGO),'999999999999999990.00'))||'"';
               MLINEA := MLINEA||' Subsidio="'||TRIM(TO_CHAR(MONTO_CONCEPTO_OTROS_PAGOS (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,CLIE.COD_CONTRATO,'S','O',VDIF_PAGO),'999999999999999990.00'))||'"';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<SaldosAFavor';
               MLINEA := MLINEA||' Monto=""';
               MLINEA := MLINEA||' Año=""';
               MLINEA := MLINEA||' Remanente=""';
           MLINEA := MLINEA||'></SaldosAFavor>'||CHR(13);
           MLINEA := MLINEA||'</OtroPago>'||CHR(13);                
        END LOOP;
        --
        IF VOTROS_EXISTE  = 'N' THEN
           MLINEA := MLINEA||'<OtroPago';
               MLINEA := MLINEA||' TipoOtroPago=""';
               MLINEA := MLINEA||' Clave=""';
               MLINEA := MLINEA||' Concepto=""';
               MLINEA := MLINEA||' Importe=""';
               MLINEA := MLINEA||' Subsidio=""';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<SaldosAFavor';
               MLINEA := MLINEA||' Monto=""';
               MLINEA := MLINEA||' Año=""';
               MLINEA := MLINEA||' Remanente=""';
           MLINEA := MLINEA||'></SaldosAFavor>'||CHR(13);
           MLINEA := MLINEA||'</OtroPago>'||CHR(13);   
        END IF;
        --FIN DEL CURSOR
        MLINEA := MLINEA||'</OtrosPagos>'||CHR(13);
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************

       --***************************************INCAP ****************************************************
        MLINEA := '<Incapacidades>'||CHR(13);
       --AQUI VA EL CURSOR 
        FOR INC IN CODIFI(CLIE.COD_CONTRATO,'I',NULL,CLIE.COD_REGIMEN) --JASON 
        LOOP
            VINCAP_EXISTE  := 'S';
            MLINEA := MLINEA||'<Incapacidad';
                MLINEA := MLINEA||' TipoIncapacidad="'||TIPO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'I',CLIE.COD_CONTRATO)||'"';
                MLINEA := MLINEA||' DiasIncapacidad="'||CANT_DIAS_MONTO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'C','I',CLIE.COD_CONTRATO)||'"';
                MLINEA := MLINEA||' ImporteMonetario="'||TRIM(TO_CHAR(CANT_DIAS_MONTO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'M','I',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
            MLINEA := MLINEA||'></Incapacidad>'||CHR(13);
        END LOOP;
       --
        IF VINCAP_EXISTE  = 'N' THEN
            MLINEA := MLINEA||'<Incapacidad';
                MLINEA := MLINEA||' TipoIncapacidad=""';
                MLINEA := MLINEA||' DiasIncapacidad=""';
                MLINEA := MLINEA||' ImporteMonetario=""';
            MLINEA := MLINEA||'></Incapacidad>'||CHR(13);
        END IF;
        --FINAL DEL CURSOR
        MLINEA := MLINEA || '</Incapacidades>'||CHR(13);
        MLINEA := MLINEA || '<Personalizados>' || CHR(13);
        MLINEA := MLINEA || '<Planta>' || '"' || CLIE.NOMBRE_SUCURSAL || '"' || '</Planta>' || CHR(13);
        MLINEA := MLINEA || '</Personalizados>' || CHR(13);
        --*************************************************************************************************
        --******************FINAL TO TODO 
        MLINEA := MLINEA||'</Payroll>'||CHR(13);
        MLINEA := MLINEA||'</Complementos>'||CHR(13);
        MLINEA := MLINEA||'</Documento>'||CHR(13);
        --
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
     --
     ELSIF PREGIMEN = '13' AND CLIE.TOTAL_SEPARACION  !='0' THEN -- ESTO APLICA UNICAMENTE PARA EL REGIMEN 13
     --
      /**********************************
      OJO NUEVO 06032023 HECHO POR DPERAZA 
      ***********************************/
        VANO := 0;
        VMES := 0;
        VDIA := 0;
        VNUM_ANTIGUEDAD := 0;
        /*ANTIG_TOTAL(CLIE.COD_TRABAJADOR,
                    CLIE.COD_EMPRESA,
                    CLIE.FECHA_ANTI_FINAL,
                    CLIE.FECHA_ANTI_INICIAL,
                    VANO,
                    VMES,
                    VDIA);*/
        FILE_IOFACT_MEX_XML_STT.ANTIGUEDAD_TOTAL ( CLIE.FECHA_ANTI_FINAL,
                                                   CLIE.FECHA_ANTI_INICIAL,
                                                   VANO,
                                                   VMES,
                                                   VDIA);            
        VNUM_ANTIGUEDAD := NVL(VANO,0);
        IF NVL(VMES,0) >= 6 THEN
            VNUM_ANTIGUEDAD := VNUM_ANTIGUEDAD + 1;
        END IF;               
        --             
        MCONTADOR   := MCONTADOR + 1;
        MFOLIO_CONT := MFOLIO_CONT + 1;
        --
        MLINEA := '<Documento>'||CHR(13);                                                                                --INI DOCUMENTO
        --
        MLINEA := MLINEA||'<Encabezado';                                                                                 --INI ENCABEZADO
        MLINEA := MLINEA||' RegimenContableR=""';
        MLINEA := MLINEA||' NmbRecep="'||CLIE.NOMBRE||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --
        MLINEA := MLINEA||'<IdDoc';
        MLINEA := MLINEA||' ContenidoTC=""';
        MLINEA := MLINEA||' Tipo="'||CLIE.TIPO_COMPROBANTE||'"';
        MLINEA := MLINEA||' Serie="'||CLIE.SERIE||'"';
        MLINEA := MLINEA||' FechaEmis="'||TO_CHAR(LOCALTIMESTAMP,'DD-MM-YYYY HH24:MI:SS')||'"';
        MLINEA := MLINEA||' TipoServicio="'||CLIE.USO_CFDI||'"';
        MLINEA := MLINEA||' FormaPago="'||CLIE.FORMA_PAGO||'"';
        MLINEA := MLINEA||' MedioPago="'||CLIE.METODO_PAGO||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        MLINEA := MLINEA||'<OtrosDocumentos';
        IF CLIE.RELACION IS NOT NULL THEN
            MLINEA := MLINEA||' Relacion="'||CLIE.RELACION||'"';
        END IF;
        MLINEA := MLINEA||'>'||CHR(13); 
        MLINEA := MLINEA||'<Documentos>'||CHR(13);
        MLINEA := MLINEA||'<Documento';
        IF CLIE.UUID IS NOT NULL THEN 
            MLINEA := MLINEA||' IdDocumento="'||CLIE.UUID||'"';
        END IF;
        IF PFOLIO IS NOT NULL THEN 
            MLINEA := MLINEA||' Folio="'||MFOLIO_CONT||'"';
        END IF;
        MLINEA := MLINEA||'></Documento>'||CHR(13);            --FIN DOCUMENTO
        MLINEA := MLINEA||'</Documentos>'||CHR(13);            --FIN DOCUMENTOS
        MLINEA := MLINEA||'</OtrosDocumentos>'||CHR(13);       --FIN OTROSDOCUMENTOS  
        MLINEA := MLINEA||'</IdDoc>'||CHR(13);                 --FIN IDDOC
        MLINEA := MLINEA||'<Emisor';
        MLINEA := MLINEA||' RegimenContable="'||CLIE.E_CVE_REGISTRO_FISCAL||'"';
        MLINEA := MLINEA||' NmbEmisor="'||CLIE.E_RFC_PATRON_ORIGEN||'"';
        MLINEA := MLINEA||'>'||CHR(13); 
        MLINEA := MLINEA||'<NombreEmisor>'||CLIE.NB_EMPRESA||'</NombreEmisor>'||CHR(13); --FIN NOMBREEMISOR 
        MLINEA := MLINEA||'<LugarExped';
           MLINEA := MLINEA||' CodigoPostal="'||CLIE.LUGAR_EXPEDICION||'"';
        MLINEA := MLINEA||'></LugarExped>'||CHR(13);                                      --FIN LUGAREXPED
        MLINEA := MLINEA||'</Emisor>'||CHR(13);                                           --FIN EMISOR
        MLINEA := MLINEA||'<DocRecep';   
           MLINEA := MLINEA||' NroDocRecep="'||CLIE.RFC_RECEPTOR||'"';
           MLINEA := MLINEA||' eMail="'||CLIE.MAIL_NAME||'"'; --jj email 
        MLINEA := MLINEA||'></DocRecep>'||CHR(13);       --FIN DOCRECEP
        MLINEA := MLINEA||'<CodigoReceptor';
           MLINEA := MLINEA||' TpoCdgIntRecep="'||CLIE.RESIDENCIA_FISCAL||'"';
        MLINEA := MLINEA||'></CodigoReceptor>'||CHR(13); --FIN CodigoReceptor
        MLINEA := MLINEA||'<Totales';
        MLINEA := MLINEA||' Moneda="'||CLIE.MONEDA||'"';
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' SubTotal="'||TRIM(TO_CHAR((NVL(CLIE.SUBTOTAL,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' SubTotal="'||TRIM(TO_CHAR(CLIE.SUBTOTAL,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||' MntDcto="0.00"';--dperaza
        ELSE         
            MLINEA := MLINEA||' MntDcto="'||CLIE.DESCUENTO||'"';
        END IF;
        --
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' VlrPagar="'||TRIM(TO_CHAR((NVL(CLIE.TOTAL,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' VlrPagar="'||TRIM(TO_CHAR(CLIE.TOTAL,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'></Totales>'||CHR(13); 
        MLINEA := MLINEA||'</Encabezado>'||CHR(13);
        MLINEA := MLINEA||'<Detalle';
        MLINEA := MLINEA||' NroLinDet="'||MCONTADOR||'"';
        MLINEA := MLINEA||' DscItem="'||CLIE.CONCEPTO_DESCRIPCION||'"';
        MLINEA := MLINEA||' QtyItem="'||CLIE.CONCEPTO_CANTIDAD||'"';
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jj Jesus
            MLINEA := MLINEA||' PrcNetoItem="'||TRIM(TO_CHAR((NVL(CLIE.CONCEPTO_VALOR_UNITARIO,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' PrcNetoItem="'||TRIM(TO_CHAR(CLIE.CONCEPTO_VALOR_UNITARIO,'999999999999999990.00'))||'"'; 
        END IF;
        MLINEA := MLINEA||' DescuentoMonto="'||TRIM(TO_CHAR(CLIE.CONCEPTO_DESCUENTO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||' MontoNetoItem="'||TRIM(TO_CHAR(CLIE.CONCEPTO_IMPORTE,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
                    --Repite                           
        MLINEA := MLINEA||'<CdgItem';
        MLINEA := MLINEA||' TpoCodigo="CPS"';
        MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_CLAVE_PROD_SERV||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                    --
        MLINEA := MLINEA||'<CdgItem';
           MLINEA := MLINEA||' TpoCodigo="SKU"';
           MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_NO_IDENTIFICACION||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                    --
        MLINEA := MLINEA||'<CdgItem';
           MLINEA := MLINEA||' TpoCodigo="CU"';
           MLINEA := MLINEA||' VlrCodigo="'||CLIE.CONCEPTO_CLAVE_UNIDAD||'"';
        MLINEA := MLINEA||'></CdgItem>'||CHR(13);
                --
        MLINEA := MLINEA||'</Detalle>'||CHR(13);
        MLINEA := MLINEA||'<Complementos>'||CHR(13); 
        MLINEA := MLINEA||'<Payroll>'||CHR(13);
        MLINEA := MLINEA||'<Resumen';
        MLINEA := MLINEA||' TipoNomina="'||CLIE.TIPO_NOMINA||'"';
        MLINEA := MLINEA||' FechaPago="'||CLIE.FECHA_PAGO||'"';
        MLINEA := MLINEA||' FechaInicialPago="'||CLIE.FECHA_INICIAL_PAGO||'"';--SERA LOS ITEMS DE FECHAS TIMBRADO
        MLINEA := MLINEA||' FechaFinalPago="'||CLIE.FECHA_FINAL_PAGO||'"';--SERA LOS ITEMS DE FECHAS TIMBRADO
        --
        IF CLIE.NUM_DIAS_PAGADOS IS NULL OR CLIE.NUM_DIAS_PAGADOS = '0' THEN -- jj caso 17578
            MLINEA := MLINEA||' NumDiasPagados="1"';
        ELSE 
            MLINEA := MLINEA||' NumDiasPagados="'||CLIE.NUM_DIAS_PAGADOS||'"';
        END IF;
        --
        MLINEA := MLINEA||' TotalPercepciones="'||TRIM(TO_CHAR(CLIE.TOTAL_PERCEPCIONES,'999999999999999990.00'))||'"';
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||'';--dperaza
        ELSE 
            MLINEA := MLINEA||' TotalDeducciones="'||TRIM(TO_CHAR(CLIE.TOTAL_DEDUCCIONES,'999999999999999990.00'))||'"';
        END IF;
       ---
        IF CLIE.OP_IMPORTE_1 = '.01' THEN -- jesus
            MLINEA := MLINEA||' TotalOtrosPagos="'||TRIM(TO_CHAR((NVL(CLIE.TOTAL_OTROS_PAGOS,0)+.01),'999999999999999990.00'))||'"';
        ELSE
            MLINEA := MLINEA||' TotalOtrosPagos="'||TRIM(TO_CHAR(CLIE.TOTAL_OTROS_PAGOS,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'></Resumen>'||CHR(13);
        MLINEA := MLINEA||'<NominaEmisor';
        MLINEA := MLINEA||' Curp="'||CLIE.E_CURP||'"';
        MLINEA := MLINEA||' RegistroPatronal="'||CLIE.E_REGISTRO_PATRONAL||'"';
        MLINEA := MLINEA||' IdPatronOrigen="'||CLIE.E_RFC_PATRON_ORIGEN||'"';
        MLINEA := MLINEA||' OrigenRecurso="'||CLIE.ENT_ORIGEN_RECURSO||'"';
        MLINEA := MLINEA||' MontoRecursoPropio="'||TRIM(TO_CHAR(CLIE.ENT_RECURSO_PROPIO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'></NominaEmisor>'||CHR(13);
        MLINEA := MLINEA||'<NominaReceptor';
        MLINEA := MLINEA||' Curp="'||CLIE.R_CURP||'"';
        MLINEA := MLINEA||' NumSeguridadSocial="'||CLIE.R_NUM_SEGURIDAD_SOCIAL||'"';
        MLINEA := MLINEA||' FechaInicioRelacion="'||CLIE.R_FECHA_INICIO_LABORAL||'"';
        --        
         IF PFORMATO_ANTI = 'AMD' THEN --GG CASO 23056
            IF VANO = '0' AND VMES ='0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VDIA||'D"';
            ELSIF VANO != '0' AND VMES ='0' AND VDIA != '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y'||VDIA||'D"';
            ELSIF VANO = '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VMES||'M'||VDIA||'D"';
            ELSIF VANO != '0' AND VMES ='0' AND VDIA = '0' THEN
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y"';    
            ELSE
                MLINEA := MLINEA||' Antigüedad="P'||VANO||'Y'||VMES||'M'||VDIA||'D"';
            END IF;
        ELSE
            MLINEA := MLINEA||' Antigüedad="'||CLIE.R_ANTIGUEDAD||'"';
        END IF;
        --
        MLINEA := MLINEA||' Sindicalizado="'||CLIE.R_SINDICALIZADO||'"';
        MLINEA := MLINEA||' TipoContrato="'||CLIE.R_TIPO_CONTRATO||'"';
        MLINEA := MLINEA||' TipoJornada="'||CLIE.R_TIPO_JORNADA||'"';
        MLINEA := MLINEA||' TipoRegimen="'||NVL(CLIE.COD_REGIMEN,CLIE.R_TIPO_REGIMEN)||'"';     --SE CAMBIÓ LA LÓGICA DEL VALOR TipoRegimen
        MLINEA := MLINEA||' NumEmpleado="'||CLIE.R_NUM_EMPLEADO||'"';
        MLINEA := MLINEA||' Departamento="'||CLIE.R_DEPARTAMENTO||'"';
        MLINEA := MLINEA||' Puesto="'||CLIE.R_PUESTO||'"';
        MLINEA := MLINEA||' RiesgoPuesto="'||CLIE.R_RIEGO_PUESTO||'"';
        MLINEA := MLINEA||' PeriodicidadPago="'||CLIE.R_PERIODICIDAD_PAGO||'"';
        MLINEA := MLINEA||' Banco="'||CLIE.R_BANCO||'"';
        MLINEA := MLINEA||' CuentaBancaria="'||CLIE.R_CUENTA_BANCARIA||'"';
        MLINEA := MLINEA||' SalarioBase="'||TRIM(TO_CHAR(CLIE.R_SALARIO_BASE_COT,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||' SalarioDiarioIntegrado ="'||TRIM(TO_CHAR(CLIE.R_SALARIO_DIARIO_INTEGRADO,'999999999999999990.00'))||'"';--PSALARIO_DIARIO_INTEGRADO
        MLINEA := MLINEA||' SalarioDiario ="'||TRIM(TO_CHAR(CLIE.R_SALARIO_DIARIO,'999999999999999990.00'))||'"';                   --NUEVA ETIQUETA PARA EL NUEVO PARÁMETRO 
        MLINEA := MLINEA||' ClaveEntFed= "'||CLIE.R_CLAVE_ENTFED||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        MLINEA := MLINEA||'<SubContratacion';
           MLINEA := MLINEA||' PorcentajeTiempo="'||CLIE.SUB_PORCENTAJE_TIEMPO_1||'"';
           MLINEA := MLINEA||' IdLabora="'||CLIE.SUB_RFC_LABORA_1||'"';
        MLINEA := MLINEA||'/>'||CHR(13); 
        MLINEA := MLINEA||'</NominaReceptor>'||CHR(13); 
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --***************************************PERCEPCIONES/HORAS EXTR************************************************             
        MLINEA :=            '<Percepciones';
           MLINEA := MLINEA||' TotalSueldos="'||TRIM(TO_CHAR(CLIE.TOTAL_SUELDOS,'999999999999999990.00'))||'"';
           MLINEA := MLINEA||' TotalSeparacionIndemnizacion="'||TRIM(TO_CHAR(CLIE.TOTAL_SEPARACION,'999999999999999990.00'))||'"';
           IF TRIM(TO_CHAR(CLIE.TOTAL_JUBILACION,'999999999999999990.00')) != '0.00' THEN -- caso 23141
              MLINEA := MLINEA||' TotalJubilacionPensionRetiro="'||TRIM(TO_CHAR(CLIE.TOTAL_JUBILACION,'999999999999999990.00'))||'"';
           END IF;
           MLINEA := MLINEA||' TotalGravado="'||TRIM(TO_CHAR(CLIE.TOTAL_GRAVADO,'999999999999999990.00'))||'"';
           MLINEA := MLINEA||' TotalExento="'||TRIM(TO_CHAR(CLIE.TOTAL_EXENTO,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --PERCEPCIONES/HORAS EXTRA
       --INICIO QUERY PERCEP
        VPERCEP_EXISTE := 'N';
        VHORA_EXISTE   := 'N';
        VDEDU_EXISTE   := 'N'; 
        VOTROS_EXISTE  := 'N';
        VINCAP_EXISTE  := 'N';          
        FOR PERC IN CODIFI(CLIE.COD_CONTRATO,'P',NULL,CLIE.COD_REGIMEN)--JM
        LOOP
            VPERCEP_EXISTE := 'S';
            VHORA_EXISTE   := 'N'; 
            IF TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'G','P',CLIE.COD_CONTRATO),'999999999999999990.00')) IS NULL 
            AND TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'E','P',CLIE.COD_CONTRATO),'999999999999999990.00')) IS NULL THEN
                MLINEA := MLINEA||'<Percepcion';
                   MLINEA := MLINEA||' TipoPercepcion=""';
                   MLINEA := MLINEA||' Clave=""';
                   MLINEA := MLINEA||' Concepto=""';
                   MLINEA := MLINEA||' ImporteGravado=""';
                   MLINEA := MLINEA||' ImporteExento=""';
                   MLINEA := MLINEA||' ValorMercado=""';
                   MLINEA := MLINEA||' PrecioAlOtorgarse=""';
                MLINEA := MLINEA||'>'||CHR(13);
                MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias=""';
                       MLINEA := MLINEA||' TipoHoras=""';
                       MLINEA := MLINEA||' HorasExtra=""';
                       MLINEA := MLINEA||' ImportePagado=""';
                MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                MLINEA := MLINEA||'</Percepcion>'||CHR(13);
            ELSE 
                MLINEA := MLINEA||'<Percepcion';    
                       MLINEA := MLINEA||' TipoPercepcion="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,'P',CLIE.COD_CONTRATO)||'"';
                           MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,'P',CLIE.COD_CONTRATO)||'"';
                           MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,1,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'P'),'/',''),'-','')||'"';
                       MLINEA := MLINEA||' ImporteGravado="'||TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'G','P',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
                       MLINEA := MLINEA||' ImporteExento="'||TRIM(TO_CHAR(MONTO_CONCEPTO_CODI_FAC (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,PERC.COD_REGISTRO,PCONCEPTO_IMPUESTO,'E','P',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
                       MLINEA := MLINEA||' ValorMercado=""';
                       MLINEA := MLINEA||' PrecioAlOtorgarse=""';
                MLINEA := MLINEA||'>'||CHR(13);
            
                FOR HORA  IN CODIFI_H(CLIE.COD_CONTRATO,'H',PERC.COD_REGISTRO,CLIE.COD_REGIMEN) --JM
                LOOP
                   VHORA_EXISTE := 'S'; 
                   --INICIAL CURSOR HORA EXTRA
                   MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias="'||CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'D','H',CLIE.COD_CONTRATO)||'"';
                       MLINEA := MLINEA||' TipoHoras="'||TIPO_HORAS(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'H',CLIE.COD_CONTRATO)||'"';
                       MLINEA := MLINEA||' HorasExtra="'||ROUND(CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'H','H',CLIE.COD_CONTRATO))||'"';
                       MLINEA := MLINEA||' ImportePagado="'||TRIM(TO_CHAR(CANT_DIAS_HORA_EXTRA(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,HORA.COD_REGISTRO,'M','H',CLIE.COD_CONTRATO) ,'999999999999999990.00'))||'"';
                   MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                   MLINEA := MLINEA||'</Percepcion>'||CHR(13);
                   --FIN DE CURSOR HORAS EXTRAS
                   --FIN DE CURSOR PERCEPCION
                END LOOP;
                --
                IF VHORA_EXISTE = 'N' THEN 
                   MLINEA := MLINEA||'<HorasExtra';
                       MLINEA := MLINEA||' Dias=""';
                       MLINEA := MLINEA||' TipoHoras=""';
                       MLINEA := MLINEA||' HorasExtra=""';
                       MLINEA := MLINEA||' ImportePagado=""';
                   MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
                   MLINEA := MLINEA||'</Percepcion>'||CHR(13);
                END IF;
            END IF;     
        END LOOP;
        --
        IF VPERCEP_EXISTE = 'N' THEN 
           MLINEA := MLINEA||'<Percepcion';
               MLINEA := MLINEA||' TipoPercepcion=""';
               MLINEA := MLINEA||' Clave=""';
               MLINEA := MLINEA||' Concepto=""';
               MLINEA := MLINEA||' ImporteGravado=""';
               MLINEA := MLINEA||' ImporteExento=""';
               MLINEA := MLINEA||' ValorMercado=""';
               MLINEA := MLINEA||' PrecioAlOtorgarse=""';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<HorasExtra';
               MLINEA := MLINEA||' Dias=""';
               MLINEA := MLINEA||' TipoHoras=""';
               MLINEA := MLINEA||' HorasExtra=""';
               MLINEA := MLINEA||' ImportePagado=""';
           MLINEA := MLINEA||'></HorasExtra>'||CHR(13);
           MLINEA := MLINEA||'</Percepcion>'||CHR(13);
        END IF; 
        --
        MLINEA := MLINEA||'<JubilacionPensionRetiro';
        IF CLIE.JPR_INGRESO_ACUM IS NULL OR CLIE.JPR_INGRESO_ACUM = '0' THEN
            MLINEA := MLINEA||' IngresoAcumulable=""';
        ELSE
            MLINEA := MLINEA||' IngresoAcumulable="'||TRIM(TO_CHAR(CLIE.JPR_INGRESO_ACUM,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.JPR_TOTAL_PARCIALIDAD IS NULL OR CLIE.JPR_TOTAL_PARCIALIDAD = '0' THEN
        MLINEA := MLINEA||' TotalParcialidad=""';    
        ELSE
        MLINEA := MLINEA||' TotalParcialidad="'||TRIM(TO_CHAR(CLIE.JPR_TOTAL_PARCIALIDAD,'999999999999999990.00'))||'"';
        END IF;       
        --
        IF CLIE.JPR_MONTO_DIARIO IS NULL OR CLIE.JPR_MONTO_DIARIO = '0' THEN
        MLINEA := MLINEA||' MontoDiario=""';
        ELSE
        MLINEA := MLINEA||' MontoDiario="'||TRIM(TO_CHAR(CLIE.JPR_MONTO_DIARIO,'999999999999999990.00'))||'"';
        END IF;
        --
        IF CLIE.JPR_TOTAL_UNA_EXHIB IS NULL OR CLIE.JPR_TOTAL_UNA_EXHIB = '0' THEN
        MLINEA := MLINEA||' TotalUnaExhibicion=""';
        ELSE
        MLINEA := MLINEA||' TotalUnaExhibicion="'||TRIM(TO_CHAR(CLIE.JPR_TOTAL_UNA_EXHIB,'999999999999999990.00'))||'"';
        END IF;
        -- 
        IF CLIE.JPR_INGRESO_NO_ACUM IS NULL OR CLIE.JPR_INGRESO_NO_ACUM = '0' THEN
        MLINEA := MLINEA||' IngresoNoAcumulable=""';
        ELSE
        MLINEA := MLINEA||' IngresoNoAcumulable="'||TRIM(TO_CHAR(CLIE.JPR_INGRESO_NO_ACUM,'999999999999999990.00'))||'"';
        END IF;
        MLINEA := MLINEA||'/>'||CHR(13);
        MLINEA := MLINEA||'<SeparacionIndemnizacion';
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' IngresoAcumulable=""';
       ELSE
       MLINEA := MLINEA||' IngresoAcumulable="'||TRIM(TO_CHAR(CLIE.SI_INGRESO_ACUM,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' TotalPagado=""';
       ELSE
       MLINEA := MLINEA||' TotalPagado="'||TRIM(TO_CHAR(CLIE.SI_TOTAL_PAGADO,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' UltimoSueldoMensOrd=""';
       ELSE
       MLINEA := MLINEA||' UltimoSueldoMensOrd="'||TRIM(TO_CHAR(CLIE.SI_ULTIMO_SUELDO,'999999999999999990.00'))||'"';
       END IF;
       --
       IF CLIE.SI_INGRESO_ACUM IS NULL OR CLIE.SI_INGRESO_ACUM = '0' THEN
       MLINEA := MLINEA||' NumAñosServicio=""'; 
       ELSE 
       MLINEA := MLINEA||' NumAñosServicio="'||VNUM_ANTIGUEDAD||'"'; 
       END IF;             
       --
       IF CLIE.SI_INGRESO_NO_ACUM IS NULL OR CLIE.SI_INGRESO_NO_ACUM = '0' THEN
       MLINEA := MLINEA||' IngresoNoAcumulable=""';
       ELSE
       MLINEA := MLINEA||' IngresoNoAcumulable="'||TRIM(TO_CHAR(CLIE.SI_INGRESO_NO_ACUM,'999999999999999990.00'))||'"';
       END IF;
        MLINEA := MLINEA||'/>'||CHR(13);
        --
        MLINEA := MLINEA||'</Percepciones>'||CHR(13); 
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************
       --***************************************DEDUCCIONES ************************************************          
        MLINEA := '<Deducciones';
        IF CLIE.TOTAL_DEDUCCIONES IS NULL OR CLIE.TOTAL_DEDUCCIONES = '0' THEN -- jj caso 17504
            MLINEA := MLINEA||' TotalOtrasDeducciones=""';
        ELSE 
            MLINEA := MLINEA||' TotalOtrasDeducciones="'||TRIM(TO_CHAR(CLIE.D_TOTAL_OTRAS_DEDUCCIONES,'999999999999999990.00'))||'"';
        END IF; 
            MLINEA := MLINEA||' TotalImpuestosRetenidos="'||TRIM(TO_CHAR(CLIE.D_TOTAL_IMPUESTOS_RETENIDOS,'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'>'||CHR(13);
        --INICIO DEL CURSOR
        FOR DEDU  IN CODIFI(CLIE.COD_CONTRATO,'D',NULL,CLIE.COD_REGIMEN)  --JASON
        LOOP
        VDEDU_EXISTE := 'S';
        MLINEA := MLINEA||'<Deduccion';
            MLINEA := MLINEA||' TipoDeduccion="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO)||'"';
            MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO)||'"';
            MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,2,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'D'),'/',''),'-','')||'"';
            MLINEA := MLINEA||' Importe="'||TRIM(TO_CHAR(MONTO_CONCEPTO_DESCUENTO(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,DEDU.COD_REGISTRO,'D',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
        MLINEA := MLINEA||'></Deduccion>'||CHR(13); 
        END LOOP;
       --
        IF VDEDU_EXISTE = 'N' THEN 
        MLINEA := MLINEA||'<Deduccion';
            MLINEA := MLINEA||' TipoDeduccion=""';
            MLINEA := MLINEA||' Clave=""';
            MLINEA := MLINEA||' Concepto=""';
            MLINEA := MLINEA||' Importe=""';
        MLINEA := MLINEA||'></Deduccion>'||CHR(13); 
        END IF;                                
        --FIN EL CURSOR
        MLINEA := MLINEA||'</Deducciones>'||CHR(13);
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************   
       --***************************************OTRO PAGO ************************************************             
        MLINEA := '<OtrosPagos>'||CHR(13); 
        --INICIO DEL CURSOR
        FOR OTRO IN CODIFI(CLIE.COD_CONTRATO,'O',NULL,CLIE.COD_REGIMEN)  --JASON
        LOOP
           VOTROS_EXISTE  := 'S';
           --
           IF OTRO.COD_REGISTRO = 1 THEN
             VDIF_PAGO := 'S';  
           ELSE 
             VDIF_PAGO := NULL;
           END IF;
           -- 
           MLINEA := MLINEA||'<OtroPago';--DPERAZAOTROPAGO
               MLINEA := MLINEA||' TipoOtroPago="'||COD_CONCEPTO_CATALOGO_IOPAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,'O',CLIE.COD_CONTRATO,VDIF_PAGO)||'"';
               MLINEA := MLINEA||' Clave="'||COD_CONCEPTO_LEGADMI(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,'O',CLIE.COD_CONTRATO,VDIF_PAGO)||'"';
               MLINEA := MLINEA||' Concepto="'||REPLACE(REPLACE(DESCRIPCION_CONCEPTOS_IOFAC(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,3,PULT_NOMBRE_CONCEP,CLIE.COD_CONTRATO,'O',VDIF_PAGO),'/',''),'-','')||'"';
               MLINEA := MLINEA||' Importe="'||TRIM(TO_CHAR(MONTO_CONCEPTO_OTROS_PAGOS (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,CLIE.COD_CONTRATO,'N','O',VDIF_PAGO),'999999999999999990.00'))||'"';
               MLINEA := MLINEA||' Subsidio="'||TRIM(TO_CHAR(MONTO_CONCEPTO_OTROS_PAGOS (CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,OTRO.COD_REGISTRO,CLIE.COD_CONTRATO,'S','O',VDIF_PAGO),'999999999999999990.00'))||'"';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<SaldosAFavor';
               MLINEA := MLINEA||' Monto=""';
               MLINEA := MLINEA||' Año=""';
               MLINEA := MLINEA||' Remanente=""';
           MLINEA := MLINEA||'></SaldosAFavor>'||CHR(13);
           MLINEA := MLINEA||'</OtroPago>'||CHR(13);                
        END LOOP;
        --
        IF VOTROS_EXISTE  = 'N' THEN
           MLINEA := MLINEA||'<OtroPago';
               MLINEA := MLINEA||' TipoOtroPago=""';
               MLINEA := MLINEA||' Clave=""';
               MLINEA := MLINEA||' Concepto=""';
               MLINEA := MLINEA||' Importe=""';
               MLINEA := MLINEA||' Subsidio=""';
           MLINEA := MLINEA||'>'||CHR(13);
           MLINEA := MLINEA||'<SaldosAFavor';
               MLINEA := MLINEA||' Monto=""';
               MLINEA := MLINEA||' Año=""';
               MLINEA := MLINEA||' Remanente=""';
           MLINEA := MLINEA||'></SaldosAFavor>'||CHR(13);
           MLINEA := MLINEA||'</OtroPago>'||CHR(13);   
        END IF;
        --FIN DEL CURSOR
        MLINEA := MLINEA||'</OtrosPagos>'||CHR(13);
        --NEW PRINT
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);
       --*************************************************************************************************

       --***************************************INCAP ****************************************************
        MLINEA := '<Incapacidades>'||CHR(13);
       --AQUI VA EL CURSOR 
        FOR INC IN CODIFI(CLIE.COD_CONTRATO,'I',NULL,CLIE.COD_REGIMEN) --JASON 
        LOOP
            VINCAP_EXISTE  := 'S';
            MLINEA := MLINEA||'<Incapacidad';
                MLINEA := MLINEA||' TipoIncapacidad="'||TIPO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'I',CLIE.COD_CONTRATO)||'"';
                MLINEA := MLINEA||' DiasIncapacidad="'||CANT_DIAS_MONTO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'C','I',CLIE.COD_CONTRATO)||'"';
                MLINEA := MLINEA||' ImporteMonetario="'||TRIM(TO_CHAR(CANT_DIAS_MONTO_INCAP(CLIE.cod_empresa,CLIE.cod_trabajador,PFECHA_D,PFECHA_H,PPROCESO,INC.COD_REGISTRO,'M','I',CLIE.COD_CONTRATO),'999999999999999990.00'))||'"';
            MLINEA := MLINEA||'></Incapacidad>'||CHR(13);
        END LOOP;
       --
        IF VINCAP_EXISTE  = 'N' THEN
            MLINEA := MLINEA||'<Incapacidad';
                MLINEA := MLINEA||' TipoIncapacidad=""';
                MLINEA := MLINEA||' DiasIncapacidad=""';
                MLINEA := MLINEA||' ImporteMonetario=""';
            MLINEA := MLINEA||'></Incapacidad>'||CHR(13);
        END IF;
        --FINAL DEL CURSOR
        MLINEA := MLINEA || '</Incapacidades>'||CHR(13);
        MLINEA := MLINEA || '<Personalizados>' || CHR(13);
        MLINEA := MLINEA || '<Planta>' || '"' || CLIE.NOMBRE_SUCURSAL || '"' || '</Planta>' || CHR(13);
        MLINEA := MLINEA || '</Personalizados>' || CHR(13);
        --*************************************************************************************************
        --******************FINAL TO TODO 
        MLINEA := MLINEA||'</Payroll>'||CHR(13);
        MLINEA := MLINEA||'</Complementos>'||CHR(13);
        MLINEA := MLINEA||'</Documento>'||CHR(13);
        --
        MLINEA := convert(REPLACE(MLINEA,chr(38),chr(38)||'amp;'),'utf8','WE8MSWIN1252');
        UTL_FILE.PUT_LINE(MFILE, MLINEA);       
     END IF;                         
    END LOOP;
    MLINEA := '</DTE>'||CHR(13);
    UTL_FILE.PUT_LINE(MFILE, MLINEA);
    UTL_FILE.FCLOSE(MFILE);                    
EXCEPTION
    WHEN OTHERS THEN 
        BEGIN 
            IF SQLCODE = -302000 THEN
              PMG_ERROR := 'No se puede crear el archivo: Ruta inválida o archivo en uso.';                       
            ELSE
              PMG_ERROR := 'Error al generar el archivo, verifique la ruta e intente nuevamente:'||SQLERRM;
            END IF;                
        UTL_FILE.FCLOSE(MFILE);
        END;        
END;
END FILE_IOFACT_MEX_XML_STT;
/