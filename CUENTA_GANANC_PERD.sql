CREATE OR REPLACE PROCEDURE CUENTA_GANANC_PERD(  EMPR    IN     NUMBER, 
                                                           VALOR   IN     VARCHAR2, 
                                                           FEC     IN     DATE,
                                                           PCUENTA IN     NUMBER,
                                                           PSUCURSAL IN NUMBER,
                                                           CTA     OUT    NUMBER, 
                                                           CO      OUT    VARCHAR2, 
                                                           NAT     OUT    VARCHAR2,
                                                           NB_OP   OUT    VARCHAR2,
                                                           MSG     OUT    VARCHAR2) IS
/******************************************************************************
   NAME:     CUENTA_GANANC_PERD
   PURPOSE:  CUENTA_GANANC_PERD
   
   REVISIONS:
   Ver        Date        Updated by         Description
   ---------  ----------  ---------------  ------------------------------------
   1.0        14/06/2015  Jason Murillo  Created this package.
******************************************************************************/     

BEGIN
                                                      
SELECT DC.COD_CUENTA_CONTRAPARTIDA,
       DC.COD_OPER_NO_MASTER,
       DECODE(DC.GANANCIA_PERDIDA, 'G', CSO.NATURALEZA, 'P', DECODE(CSO.NATURALEZA, 'C', 'D', 'D', 'C')),
       CO.NB_OPERACION
INTO CTA, CO, NAT, NB_OP
FROM FI_DEFINICION_DIF_POR_CAMBIO DC
INNER JOIN 
  FI_EMPRESAS_CONTROL EC ON EC.COD_PLAN = DC.COD_PLAN
INNER JOIN 
  FI_CODIGOS_OPERACION CO ON DC.COD_OPER_NO_MASTER = CO.COD_OPERACION
INNER JOIN
  FI_CTAS_SUC_OPERACION CSO ON CSO.COD_EMPRESA = EMPR
  AND CSO.COD_SUCURSAL = PSUCURSAL
  AND CSO.COD_PLAN = EC.COD_PLAN
  AND CSO.COD_CUENTA = PCUENTA
  AND CSO.COD_OPERACION = DC.COD_OPERACION_MASTER
  AND (CSO.FECHA_FINAL >= FEC OR CSO.FECHA_FINAL IS NULL) 
WHERE 
  EC.COD_EMPRESA = EMPR
  AND DC.GANANCIA_PERDIDA = VALOR
  AND DC.FECHA_INICIAL <= FEC
  AND (DC.FECHA_FINAL >= FEC OR DC.FECHA_FINAL IS NULL);
EXCEPTION
    WHEN NO_DATA_FOUND THEN
    MSG := 'ERROR: No existe Cuenta de Ganacia o Perdida por diferencia en cambio de moneda.';
END;
/
