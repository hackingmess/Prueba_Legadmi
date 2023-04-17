CREATE OR REPLACE PROCEDURE MAIL_ANIVERSARIOS_TRABAJADORES IS

CURSOR ANIVS IS

                    SELECT TR.PRIMER_NOMBRE NOMBRES,
                            TR.FECHA_INGRESO FING,
                            TO_NUMBER(TO_CHAR(SYSDATE,'yyyy')) - TO_NUMBER(TO_CHAR(TR.FECHA_INGRESO, 'yyyy')) ANIOS,
                            NVL(RH.MAIL_NAME, DOM.E_MAIL) TOEMAIL,
                            TR.COD_EMPRESA EMPRESATO,
                            TR.COD_TRABAJADOR TRABAJADORTO
                    FROM  TRABAJADORES1 TR, 
                          RR_HH_TRABAJADORES RH, 
                          DOMICILIO_TRABAJADORES DOM
                    WHERE TO_CHAR(TR.FECHA_INGRESO,'dd/mm') = TO_CHAR(SYSDATE,'dd/mm')
                    AND RH.COD_TRABAJADOR                   = TR.COD_TRABAJADOR
                    AND RH.COD_EMPRESA                      = TR.COD_EMPRESA
                    AND TR.FECHA_EGRESO                     IS NULL
                    AND (RH.MAIL_NAME IS NOT NULL OR DOM.E_MAIL IS NOT NULL)
                    AND TR.COD_TRABAJADOR                   = DOM.COD_TRABAJADOR
                    AND TR.COD_EMPRESA                      = DOM.COD_EMPRESA
                    AND TR.COD_TRABAJADOR NOT IN (SELECT ANT.COD_TRABAJADOR 
                                                   FROM ANIVERSARIOS_TRABAJADORES ANT,
                                                        TRABAJADORES1 TR2
                                                   WHERE ANT.COD_TRABAJADOR = TR2.COD_TRABAJADOR
                                                   AND ANT.COD_EMPRESA      = TR2.COD_EMPRESA
                                                   AND ANT.ANO              = TO_CHAR(SYSDATE,'yyyy')
                                                   AND ANT.ENVIO_ANIV       = 'S');

CURSOR CUMPLES IS
                 SELECT ORI.FECHA_NACIMIENTO FNAC,
                        TO_NUMBER(TO_CHAR(SYSDATE,'yyyy')) - TO_NUMBER(TO_CHAR(ORI.FECHA_NACIMIENTO,'yyyy')) CUANTOS,
                        TR.PRIMER_NOMBRE NOMBRESC,
                        NVL(RH.MAIL_NAME, DOM.E_MAIL) TOEMAILC,
                        TR.COD_EMPRESA EMPRESATOC,
                        TR.COD_TRABAJADOR TRABAJADORTOC
                 FROM ORIGEN_TRABAJADORES ORI, 
                      TRABAJADORES1 TR,
                      DOMICILIO_TRABAJADORES DOM, 
                      RR_HH_TRABAJADORES RH
                 WHERE TO_CHAR(ORI.FECHA_NACIMIENTO,'dd/mm') = TO_CHAR(SYSDATE,'dd/mm')
                 AND ORI.COD_TRABAJADOR = TR.COD_TRABAJADOR
                 AND ORI.COD_EMPRESA = TR.COD_EMPRESA
                 AND TR.FECHA_EGRESO IS NULL
                 AND TR.COD_TRABAJADOR = DOM.COD_TRABAJADOR
                 AND TR.COD_EMPRESA = DOM.COD_EMPRESA
                 AND RH.COD_TRABAJADOR = TR.COD_TRABAJADOR
                 AND RH.COD_EMPRESA = TR.COD_EMPRESA
                 AND (RH.MAIL_NAME IS NOT NULL OR DOM.E_MAIL IS NOT NULL)
                 AND TR.COD_TRABAJADOR NOT IN (SELECT ANT.COD_TRABAJADOR 
                                               FROM ANIVERSARIOS_TRABAJADORES ANT,
                                                    TRABAJADORES1 TR2
                                               WHERE ANT.COD_TRABAJADOR = TR2.COD_TRABAJADOR
                                               AND ANT.COD_EMPRESA      = TR2.COD_EMPRESA
                                               AND ANT.ANO              = TO_CHAR(SYSDATE,'yyyy')
                                               AND ANT.ENVIO_CUMPLE     = 'S');

   MSERVIDOR            VARCHAR2(300);
   MPUERTO              VARCHAR2(300);
   MCORREO_FROM         VARCHAR2(300);
   MASUNTO_ANIV         VARCHAR2(300);
   MASUNTO_CUMPLE       VARCHAR2(300);
   MCUERPO_ANIV         VARCHAR2(500);
   MCUERPO_CUMPLE       VARCHAR2(500);
   MIMG_ANIV            VARCHAR2(300); 
   MVIDEO_ANIV          VARCHAR2(300);
   MIMG_CUMPLE          VARCHAR2(300); 
   MVIDEO_CUMPLE        VARCHAR2(300);
   MFIRMA               VARCHAR2(300);
   MCORREO_TO           VARCHAR2(100);
   MEMPRESATO           NUMBER(4);
   MTRABAJADORTO        TRABAJADORES1.COD_TRABAJADOR%TYPE;
   MANOTO               VARCHAR2(4);
   MHTML_ANIV           VARCHAR2(32767);
   MHTML_CUMPLE         VARCHAR2(32767);
   MACTIVAR_ENVIO       NUMBER(1);  
   MACTIVAR_JOB_CUMPLEANOS VARCHAR2(1);
   mnb_aplicacion       varchar2(200);
   musuario_email       varchar2(200);
   mclave_email         varchar2(200);

BEGIN 
    BEGIN
        SELECT pon.nb_aplicacion,
               pon.usuario_email,
               pon.clave_email,
               PON.SERVIDOR_CORREO, 
               PON.PUERTO, 
               PON.CORREO_APLICACION,
               PON.ASUNTO_ANIV,
               PON.ASUNTO_CUMPLE,
               PON.CUERPO_ANIV,
               PON.CUERPO_CUMPLE, 
               PON.IMG_ANIV,
               DECODE(PON.VIDEO_ANIV, NULL, '<br>','<a href="'||PON.VIDEO_ANIV||'" target="_blank">Mira el video!</a>') VIDANIV, 
               PON.IMG_CUMPLE,DECODE(PON.VIDEO_CUMPLE, 
               NULL, 
               '<br>','<a href="'||PON.VIDEO_CUMPLE||'" target="_blank">Mira el video!</a>') VID,
               PON.FIRMA, 
               PON.ACTIVAR_ENVIO, --ACTIVAR JOB DE ANIVERSARIO
               PON.ACTIVAR_JOB_CUMPLEANOS
        INTO mnb_aplicacion, 
             musuario_email, 
             mclave_email, 
             MSERVIDOR,
             MPUERTO,
             MCORREO_FROM, 
             MASUNTO_ANIV, 
             MASUNTO_CUMPLE,
             MCUERPO_ANIV, 
             MCUERPO_CUMPLE, 
             MIMG_ANIV, 
             MVIDEO_ANIV, 
             MIMG_CUMPLE, 
             MVIDEO_CUMPLE,
             MFIRMA,
             MACTIVAR_ENVIO,
             MACTIVAR_JOB_CUMPLEANOS
        FROM PORTAL_OPC_NOTIFICACIONES_MAIL PON;
    EXCEPTION 
          WHEN OTHERS THEN
               NULL;
    END;
   --    
   FOR CUMPLE IN CUMPLES 
   LOOP
       MANOTO           := TO_CHAR(SYSDATE,'yyyy');
       MCORREO_TO       := CUMPLE.TOEMAILC;
       MEMPRESATO       := CUMPLE.EMPRESATOC;
       MTRABAJADORTO    := CUMPLE.TRABAJADORTOC;
       DBMS_OUTPUT.PUT_LINE('trab:'||MTRABAJADORTO|| ', empresa:'||MEMPRESATO||' ,email'||MCORREO_TO);
       MHTML_CUMPLE     := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
                        <html>
                        <head>
                        <title>Legadmi Notificaciones</title>
                        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                        </head>
                        <center>
                        <br><br>
                        <h1>¡Hola, '||CUMPLE.NOMBRESC||'!</h1>
                        <br><br><br>     
                        <img src="'||MIMG_CUMPLE||'">
                        <br><br>
                        '||MVIDEO_CUMPLE||'
                        <br><br>
                        <h3>
                        '||MCUERPO_CUMPLE||'
                        </h3><br><br><br>
                        <h4>'||MFIRMA||'</h4>
                        </center>
                        </body>
                        </html>';
       IF (MACTIVAR_JOB_CUMPLEANOS = 'S') THEN
           BEGIN
                --SEND_EMAIL_HTML (MCORREO_TO,MCORREO_FROM,MASUNTO_CUMPLE, 'null',MHTML_CUMPLE, MSERVIDOR,MPUERTO);
                SEND_EMAIL_HTML2(MCORREO_TO,
                                 CUMPLE.NOMBRESC,
                                 MCORREO_FROM,
                                 mnb_aplicacion,
                                 CONVERT(MASUNTO_CUMPLE, 'utf8', 'WE8ISO8859P1'),
                                 ' ',
                                 MHTML_CUMPLE,
                                 MSERVIDOR,
                                 MPUERTO,
                                 MUSUARIO_EMAIL, 
                                 MCLAVE_EMAIL,
                                 NULL,
                                 NULL);                
           EXCEPTION 
              WHEN OTHERS THEN
                   DBMS_OUTPUT.PUT_LINE('error:'||SQLERRM);
           END;
           BEGIN
            INSERT INTO ANIVERSARIOS_TRABAJADORES(COD_EMPRESA, 
                                                  COD_TRABAJADOR, 
                                                  ENVIO_CUMPLE, 
                                                  ANO)
                                           VALUES(MEMPRESATO, 
                                                  MTRABAJADORTO, 
                                                  'S', 
                                                  MANOTO);
            COMMIT;
           EXCEPTION 
               WHEN DUP_VAL_ON_INDEX THEN
                    UPDATE ANIVERSARIOS_TRABAJADORES 
                    SET ENVIO_CUMPLE ='S'
                    WHERE COD_EMPRESA  = MEMPRESATO
                    AND COD_TRABAJADOR = MTRABAJADORTO
                    AND ANO            = MANOTO;
                    COMMIT;
            WHEN OTHERS THEN
                 DBMS_OUTPUT.PUT_LINE('error:'||SQLERRM);
           END;
       END IF;
    END LOOP;
   FOR ANIV IN ANIVS 
   LOOP
       MANOTO           := TO_CHAR(SYSDATE,'yyyy');
       MCORREO_TO       := ANIV.TOEMAIL;
       MEMPRESATO       := ANIV.EMPRESATO;
       MTRABAJADORTO    := ANIV.TRABAJADORTO;        
       DBMS_OUTPUT.PUT_LINE('trab:'||MTRABAJADORTO|| ', empresa:'||MEMPRESATO||' ,email'||MCORREO_TO);
       MHTML_ANIV       := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
                        <html>
                        <head>
                        <title>Legadmi Notificaciones</title>
                        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                        </head>
                        <center>
                        <br><br>
                        <h1> ¡Hola, '||ANIV.NOMBRES||'!</h1>
                        <br><br><br>     
                        <img src="'||MIMG_ANIV||'">
                        <br><br>
                        '||MVIDEO_ANIV||'
                        <br><br>
                        <h3>
                        '||MCUERPO_ANIV||'
                        </h3><br><br><br>
                        <h4>'||MFIRMA||'</h4>
                        </center>
                        </body>
                        </html>';
        IF (MACTIVAR_ENVIO = 1) THEN
           BEGIN
            --SEND_EMAIL_HTML (MCORREO_TO,MCORREO_FROM,MASUNTO_ANIV, 'null',MHTML_ANIV, MSERVIDOR,MPUERTO);
            SEND_EMAIL_HTML2(MCORREO_TO,
                             ANIV.NOMBRES,
                             MCORREO_FROM,
                             mnb_aplicacion,
                             CONVERT(MASUNTO_ANIV, 'utf8', 'WE8ISO8859P1'),
                             ' ',
                             MHTML_ANIV,
                             MSERVIDOR,
                             MPUERTO,
                             MUSUARIO_EMAIL, 
                             MCLAVE_EMAIL,
                             NULL,
                             NULL);                
           EXCEPTION 
               WHEN OTHERS THEN
                    DBMS_OUTPUT.PUT_LINE('error:'||SQLERRM);
           END;
           BEGIN
            INSERT INTO  ANIVERSARIOS_TRABAJADORES(COD_EMPRESA,
                                                   COD_TRABAJADOR,
                                                   ENVIO_ANIV,
                                                   ANO)
                                            VALUES(MEMPRESATO,
                                                   MTRABAJADORTO,
                                                   'S',
                                                   MANOTO);
            COMMIT;            
           EXCEPTION 
               WHEN DUP_VAL_ON_INDEX THEN
                    UPDATE ANIVERSARIOS_TRABAJADORES 
                    SET ENVIO_ANIV = 'S'
                    WHERE COD_EMPRESA  = MEMPRESATO
                    AND COD_TRABAJADOR = MTRABAJADORTO
                    AND ANO            = MANOTO;
                    COMMIT;
               WHEN OTHERS THEN
            DBMS_OUTPUT.PUT_LINE('error:'||SQLERRM);
           END;
        END IF;
    END LOOP;
END MAIL_ANIVERSARIOS_TRABAJADORES;
/