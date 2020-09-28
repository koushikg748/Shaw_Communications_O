create or replace PACKAGE BODY    SHAW_OM_POS_LGF_PKG
AS

  /* Header: SHAW_OM_POS_LGF_PKG.pkb 1.0 02-May-2020 Gudhi Koushik             */
  --+==========================================================================+
  --| Module/Object Name: SHAW_OM_POS_LGF_PKG.pkb                              |
  --|                                                                          |
  --| Description :  Interface LOG_INT_18 package spec                         |
  --|                                                                          |
  --| Creation Date: 02-MAY-2020                                               |
  --|                                                                          |
  --| Author      : Gudhi Koushik                                              |
  --|                                                                          |
  --| Calls/Called by:                                                         |
  --|                                                                          |
  --| Arguments   :                                                            |
  --|                                                                          |
  --+==========================================================================+
  --| Modification History                                                     |
  --+==========================================================================+
  --| Date        Who                Ver   Change Description                  |
  --| ----------- ----------         ---  -------------------------------------|
  --| 02-May-2020  Gudhi Koushik     1.0  POS program is splitted into 2 interfaces, 
  --|                                     First to fetch records from ERP and dump into DBCS (Scheduled Hourly). 
  --|                                     Second Process records from DBCS to WMS (Scheduled every 15 min). 
  --|                                     ERP Query has been removed from pacakge
  --|                                     Project issue transaction has been separated from erp call,
  --+============================================================================================================+

 PROCEDURE Log (p_message IN VARCHAR2) 
IS 
BEGIN 
  dbms_output.Put_line ( '  >> SHAW_OM_POS_LGF_PKG' 
  || '.' 
  || p_message); 
END log;
PROCEDURE DeleteRecords(p_status OUT VARCHAR2)
is
  CURSOR err_records IS 
    SELECT erp_transaction_id,ROWID row_id  from shaw_om_pos_logfire_stg
    where erp_transaction_id is not null
    and status='New';
TYPE cur_serial_tab 
IS 
  TABLE OF err_records%ROWTYPE INDEX BY BINARY_INTEGER; 
  lt_cur_serial_tab CUR_SERIAL_TAB; 
  ln_counter NUMBER := 0; 
  ln_errors  VARCHAR2 (4000); 
BEGIN 

  p_status:='Success'; 
  OPEN err_records; 
  FETCH err_records bulk collect 
  INTO  lt_cur_serial_tab; 

  CLOSE err_records; 
  BEGIN 
    forall indx1 IN lt_cur_serial_tab.first .. lt_cur_serial_tab.last 
    save EXCEPTIONS 
    Delete from shaw_om_pos_logfire_stg 
    WHERE  erp_transaction_id = Lt_cur_serial_tab (indx1).erp_transaction_id 
    and  ROWID = Lt_cur_serial_tab (indx1).row_id; 

  EXCEPTION 
  WHEN OTHERS THEN 
    p_status := 'Error Whlile Bulk delete : DeleteRecords procedure' 
    || SQLERRM; 
    ln_errors := SQL%bulk_exceptions.count; 
    FOR j IN 1 .. ln_errors 
    LOOP 
      Log('Error :'); 
      Log('Bulk Error For Record Number: ' 
      || SQL%Bulk_exceptions(j).error_index ); 
    END LOOP; 
  END; 
EXCEPTION 
WHEN OTHERS THEN 
  Log ( 'Error in DeleteRecords  Procedure Main Block :' 
  || SQLERRM); 
  p_status :='Error  in DeleteRecords Procedure Main Block  '; 
END DeleteRecords ; 

PROCEDURE Updatebatchid(p_batch_id OUT VARCHAR2, 
                        p_sub_count IN NUMBER, 
                        p_status OUT VARCHAR2) 
IS 
  CURSOR cur_serial_numbers IS 
    SELECT a.*, 
           Ceil(ROWNUM / p_sub_count) counter 
    FROM   ( 
                    SELECT   seq_nbr, 
                             ROWID row_id 
                    FROM     shaw_om_pos_logfire_stg a 
                    WHERE    1 = 1 
                    AND      status = 'New' 
                    AND      batch_id = p_batch_id 
                    ORDER BY seq_nbr DESC) a; 

TYPE cur_serial_tab 
IS 
  TABLE OF cur_serial_numbers%ROWTYPE INDEX BY BINARY_INTEGER; 
  lt_cur_serial_tab CUR_SERIAL_TAB; 
  ln_counter NUMBER := 0; 
  ln_errors  VARCHAR2 (4000); 
BEGIN 
  SELECT shaw_om_pos_logfire_stg_s.NEXTVAL 
  INTO   p_batch_id 
  FROM   dual; 

  UPDATE shaw_om_pos_logfire_stg 
  SET    batch_id=p_batch_id 
  WHERE  status='New'; 

  p_status:='Success'; 
  OPEN cur_serial_numbers; 
  FETCH cur_serial_numbers bulk collect 
  INTO  lt_cur_serial_tab; 

  CLOSE cur_serial_numbers; 
  BEGIN 
    forall indx1 IN lt_cur_serial_tab.first .. lt_cur_serial_tab.last 
    save EXCEPTIONS 
    UPDATE shaw_om_pos_logfire_stg 
    SET    invn_attr_g = Lt_cur_serial_tab (indx1).counter 
    WHERE  ROWID = Lt_cur_serial_tab (indx1).row_id; 

  EXCEPTION 
  WHEN OTHERS THEN 
    p_status := 'update_trxns :' 
    || SQLERRM; 
    ln_errors := SQL%bulk_exceptions.count; 
    FOR j IN 1 .. ln_errors 
    LOOP 
      Log('Error :'); 
      Log('Bulk Error For Record Number: ' 
      || SQL%Bulk_exceptions(j).error_index ); 
    END LOOP; 
  END; 
EXCEPTION 
WHEN OTHERS THEN 
  Log ( 'Error in UpdateBatchID Procedure Main Block :' 
  || SQLERRM); 
  p_status :='Error'; 
END updatebatchid ; 

PROCEDURE Insertprojectdata ( o_out_status OUT VARCHAR2 )
AS 
    CURSOR fetchprojectdata IS 
    
   SELECT
    creation_date,
    source_record_id,
    ora_source_header_id,
    ora_source_line_id,
    facility_code,
    location,
    item_alternate_code,
    serial_number,
    ROWID,
    1
    || ROW_NUMBER() OVER(
        PARTITION BY ITEM_ALTERNATE_CODE,SERIAL_NUMBER
        ORDER BY
            ora_source_header_id, ora_source_line_id
    ) seq_nbr,
    primary_quantity
FROM
    (
        SELECT
            creation_date,
            source_record_id,
            ora_source_header_id,
            ora_source_line_id,
            ora_organization_code   facility_code,
            attribute8              location,
            ora_item_number         item_alternate_code,
            ora_fm_serial_number    serial_number,
            ROWID,
            CASE
                WHEN ora_fm_serial_number IS NULL THEN
                    abs(ora_trxn_qty)
                ELSE
                    1
            END primary_quantity
        FROM
            shaw_inv_transactions_stg
        WHERE
            ora_trxn_type_name ='Shaw Project Issue'
            AND pos_status = 'New'
    );

ln_errors NUMBER := 0; 
TYPE shaw_projectissue_tab 
IS 
  TABLE OF fetchprojectdata%ROWTYPE; 
  lt_shaw_trxns_tab SHAW_PROJECTISSUE_TAB ; 
BEGIN 
  OPEN fetchprojectdata; 
  LOOP 
    FETCH fetchprojectdata bulk collect 
    INTO  lt_shaw_trxns_tab LIMIT 5000; 

    BEGIN 
      forall indx IN lt_shaw_trxns_tab.first..lt_shaw_trxns_tab.last save EXCEPTIONS 
      
      INSERT INTO shaw_om_pos_logfire_stg 
                  ( 
                              creation_date, 
                              source_record_id, 
                              status, 
                              facility_code, 
                              company_code, 
                              transaction_type, 
                              seq_nbr, 
                              location, 
                              serial_nbr, 
                              item_alternate_code, 
                              quantity, 
                              ref_transaction_id, 
                              ref_seq_nbr, 
                              error_code , 
                              error_message , 
                              created_by , 
                              last_update_date , 
                              last_updated_by , 
                              last_update_login 
                  ) 
                  VALUES 
                  ( 
                              Lt_shaw_trxns_tab(indx).creation_date, 
                              Lt_shaw_trxns_tab(indx).source_record_id, 
                              'New', 
                              Lt_shaw_trxns_tab(indx).facility_code, 
                              'SHAW', 
                              'SALE', 
                              nvl2(Lt_shaw_trxns_tab(indx).serial_number,Lt_shaw_trxns_tab(indx).seq_nbr,'11'),  
                              Lt_shaw_trxns_tab(indx).location, 
                              Lt_shaw_trxns_tab(indx).serial_number, 
                              Lt_shaw_trxns_tab(indx).item_alternate_code, 
                              Lt_shaw_trxns_tab(indx).primary_quantity, 
                              'POS001', 
                              1, 
                              NULL, 
                              NULL, 
                              Sys_context('USERENV','SESSION_USER') , 
                              SYSDATE, 
                              Sys_context('USERENV','SESSION_USER') , 
                              Sys_context('USERENV','SESSION_USER') 
                  ); 



    EXCEPTION 
    WHEN OTHERS THEN 
      o_out_status := 'Error in Bulk insert of data into shaw_om_pos_logfire_stg in insertprojectdata procedure :' 
      || SQLERRM; 
      Log('Exception of the insertprojectdata Procedure ' 
      || SQLERRM); 
      ln_errors := SQL%bulk_exceptions.count; 
      FOR j IN 1..ln_errors 
      LOOP 
        Log('Error :'); 
        Log('Bulk Error For Record Number: ' 
        || SQL%Bulk_exceptions(j).error_index 
        || 'Source Record ID: ' 
        || Lt_shaw_trxns_tab(SQL%Bulk_exceptions(j).error_index).source_record_id); 
      END LOOP; 
    END; 
    BEGIN 
      forall indx1 IN lt_shaw_trxns_tab.first .. lt_shaw_trxns_tab.last save EXCEPTIONS 
      UPDATE shaw_inv_transactions_stg 
      SET    pos_status = 'Processed', 
             last_update_date = SYSDATE 
      WHERE  pos_status ='New' 
      and  rowid=Lt_shaw_trxns_tab (indx1).rowid
      AND   ora_trxn_type_name='Shaw Project Issue';




    EXCEPTION 
    WHEN OTHERS THEN 
      o_out_status := 'Error in Bulk Update of pos_status in insertprojectdata  procedure:' 
      || SQLERRM; 
      FOR j IN 1..SQL%bulk_exceptions.count 
      LOOP 
        dbms_output.Put_line('Bulk Error For Record Number: ' 
        || SQL%Bulk_exceptions(j).error_index ); 
      END LOOP; 
    END; 
 EXIT  WHEN lt_shaw_trxns_tab.count = 0; 
  END LOOP; 
  CLOSE fetchprojectdata;
o_out_status :='Success';

EXCEPTION 
WHEN OTHERS THEN  
  Log ( 'Error in  Procedure Main Block :' 
  || SQLERRM); 
  o_out_status :='Error in  Insertprojectdata Procedure Main Block  Error'|| SQLERRM; 

END insertprojectdata;
PROCEDURE Update_errors ( p_batch_id         IN NUMBER, 
                         p_success           IN VARCHAR2, 
                         p_message           IN VARCHAR2, 
                         p_sub_batch_id      IN NUMBER, 
                         p_seq               IN NUMBER,
                         p_err_dtls_tbl_type IN XX_SHAW_CMMN_ERR_TAB ) 
IS 
  lc_error VARCHAR2(4000); 
  lv_stage VARCHAR2(60); 
  lt_err_dtls_tbl_type XX_SHAW_CMMN_ERR_TAB := Xx_shaw_cmmn_err_tab(); 
  CURSOR cur_pos_err IS 
    SELECT sil.transaction_id, 
           xet.error_message 
    FROM   shaw_om_pos_logfire_stg sil, 
           TABLE ( p_err_dtls_tbl_type ) xet 
    WHERE  sil.transaction_id = Ltrim(Rtrim(xet.key_value, '-'), '-') 
    AND    sil.batch_id = p_batch_id 
    AND    To_number(sil.INVN_ATTR_G) = p_sub_batch_id 
    AND    sil.SEQ_NBR = p_seq 
    AND    sil.status = 'New'; 
TYPE tbl_cur_pos_err 
IS 
  TABLE OF cur_pos_err%ROWTYPE INDEX BY BINARY_INTEGER; 
  lt_cur_pos_err TBL_CUR_POS_ERR; 
  l_error_status  VARCHAR2(10); 
  l_error_message VARCHAR2(4000); 
  l_count         NUMBER; 
BEGIN 
     
  lv_stage := 'error 1.0'; 
  OPEN cur_pos_err; 
  FETCH cur_pos_err bulk collect 
  INTO  lt_cur_pos_err; 
  CLOSE cur_pos_err; 
   
  IF Upper(p_success) = 'FALSE' 
    THEN 
    IF ( lt_cur_pos_err.count = 0 
      OR 
      p_message = 'Data Validation Error' ) 
      THEN 
      lv_stage := 'error 1.1'; 
      BEGIN 
        UPDATE shaw_om_pos_logfire_stg 
        SET    status = 'Error', 
               error_message = p_message, 
               last_update_date = SYSDATE, 
               last_updated_by = Sys_context('USERENV','SESSION_USER'), 
               last_update_login = Sys_context('USERENV','SESSION_USER') 
        WHERE  batch_id = p_batch_id
        AND    To_number(invn_attr_g) = p_sub_batch_id 
        AND    SEQ_NBR = p_seq ;
      EXCEPTION 
      WHEN OTHERS THEN 
        Log('Error while updating error batch : ' 
        || lv_stage 
        || ' ' 
        || SQLERRM); 
      END; 
    ELSIF ( lt_cur_pos_err.count > 0      AND   p_message != 'Data Validation Error' ) THEN 
      lv_stage := 'error 1.2'; 
      BEGIN 
        forall i IN lt_cur_pos_err.first..lt_cur_pos_err.last 
        UPDATE shaw_om_pos_logfire_stg 
        SET    status = 'Error', 
               error_message = Lt_cur_pos_err(i).error_message, 
               last_update_date = SYSDATE, 
               last_updated_by = Sys_context('USERENV','SESSION_USER'), 
               last_update_login = Sys_context('USERENV','SESSION_USER') 
        WHERE  batch_id = p_batch_id 
        AND    INVN_ATTR_G = p_sub_batch_id 
        AND    To_number(SEQ_NBR) = p_seq 
        AND    transaction_id = Lt_cur_pos_err(i).transaction_id; 
      EXCEPTION 
      WHEN OTHERS THEN 
        Log('Error while updating error batch : ' 
        || lv_stage 
        || ' ' 
        || SQLERRM); 
      END; 
    END IF; 
 END IF; 
   BEGIN 
      UPDATE shaw_om_pos_logfire_stg 
      SET    status = 'Success', 
             error_message = NULL, 
             error_code = NULL, 
             last_update_date = SYSDATE, 
             last_updated_by = Sys_context('USERENV','SESSION_USER') ,
             last_update_login = Sys_context('USERENV','SESSION_USER') 
      WHERE  1 = 1 
      AND    status = 'New' 
      AND    batch_id = p_batch_id
      AND    SEQ_NBR = p_seq 
      AND    To_number(invn_attr_g) = p_sub_batch_id; 
    EXCEPTION 
    WHEN OTHERS THEN 
      Log('Error while updating error batch : ' 
      || lv_stage 
      || ' ' 
      || SQLERRM); 
    END; 
EXCEPTION 
WHEN OTHERS THEN 
  Log('Error in xx_update_errors : ' 
  || lv_stage 
  || ' ' 
  || SQLERRM); 
END update_errors;
PROCEDURE Getlastrundate( p_last_successful_run_date OUT VARCHAR2, 
                         p_current_run_date OUT VARCHAR2, 
                         p_status OUT VARCHAR2 ) 
IS 
  fd VARCHAR2 (100); 
  td VARCHAR2 (100); 
  l_current_time TIMESTAMP; 
  l_last_successful_run TIMESTAMP; 
BEGIN 
  p_status :='Success'; 
  BEGIN 
    SELECT last_successful_run 
    INTO   l_last_successful_run 
    FROM   shaw_interface_run_details 
    WHERE  rice_id = 'LOG-INT-18'; 

    l_current_time:=systimestamp; 
    UPDATE shaw_interface_run_details 
    SET    current_run_date =l_current_time 
    WHERE  rice_id ='LOG-INT-18'; 

    fd := To_char (l_last_successful_run, 'YYYY-MM-DD HH24:MI:SS.FF');  
    td := To_char (l_current_time, 'YYYY-MM-DD HH24:MI:SS.FF');        
    p_last_successful_run_date:=fd; 
    p_current_run_date:=td; 
  EXCEPTION 
  WHEN no_data_found THEN 
    BEGIN 
      INSERT INTO shaw_interface_run_details 
                  ( 
                              rice_id , 
                              interface_name , 
                              interface_type , 
                              current_run_date , 
                              last_successful_run , 
                              creation_date , 
                              created_by , 
                              last_updated_date , 
                              last_updated_by 
                  ) 
                  VALUES 
                  ( 
                              'LOG-INT-18', 
                              'Oracle to Logfire - Point Of Sales', 
                              'OutBound', 
                              systimestamp, 
                              To_timestamp ('01-JAN-1900 00:00:00.000000', 'DD-MON-YYYY HH24:MI:SS.FF'), 
                              SYSDATE, 
                              Sys_context('USERENV','SESSION_USER'), 
                              NULL, 
                              ' ' 
                  ); 

      l_last_successful_run:=To_timestamp ('01-JAN-1900 00:00:00.000000', 'DD-MON-YYYY HH24:MI:SS.FF');  
    EXCEPTION 
    WHEN OTHERS THEN 
      Log ( 'Error:' 
      || SQLERRM); 
      p_status :='Error'; 
    END; 
  END; 
EXCEPTION 
WHEN OTHERS THEN 
  Log ( 'Error in GetLastRunDate Procedure Main Block :' 
  || SQLERRM); 
  p_status :='Error'; 
END getlastrundate;
PROCEDURE Insertposdata( p_cur_pos_type IN SHAW_OM_POS_INS_TAB, 
                        p_status OUT VARCHAR2 ) 
IS 
  l_iteration NUMBER; 
  l_error     NUMBER; 
  l_message   VARCHAR2(2000); 
  l_err_msg   VARCHAR(32760);  
  lc_error    VARCHAR2 (4000); 
BEGIN 
  p_status:='Success'; 
  BEGIN 
    forall i IN p_cur_pos_type.first..p_cur_pos_type.last save EXCEPTIONS 
    INSERT INTO shaw_om_pos_logfire_stg 
                ( 
                            facility_code , 
                            company_code , 
                            transaction_type , 
                            erp_transaction_id, 
                            transaction_nbr , 
                            vendor_code , 
                            action_code , 
                            delivery_date , 
                            ship_date , 
                            cancel_date , 
                            pre_pack_code , 
                            pre_pack_ratio , 
                            pre_pack_total_units , 
                            unit_cost , 
                            unit_retail , 
                            seq_nbr , 
                            ref_transaction_id , 
                            ref_seq_nbr , 
                            location , 
                            item_alternate_code , 
                            item_part_a , 
                            item_part_b , 
                            item_part_c , 
                            item_part_d , 
                            item_part_e , 
                            item_part_f , 
                            invn_attr_a , 
                            invn_attr_b , 
                            invn_attr_c , 
                            expiry_date , 
                            batch_nbr , 
                            serial_nbr , 
                            quantity , 
                            pos_user , 
                            invn_attr_d , 
                            invn_attr_e , 
                            invn_attr_f , 
                            invn_attr_g , 
                            status , 
                            record_id , 
                            batch_id , 
                            error_code , 
                            error_message , 
                            created_by , 
                            creation_date , 
                            last_update_date , 
                            last_updated_by , 
                            last_update_login 
                ) 
                VALUES 
                ( 
                            P_cur_pos_type(i).facility_code , 
                            P_cur_pos_type(i).company_code , 
                            P_cur_pos_type(i).transaction_type , 
                            P_cur_pos_type(i).erp_transaction_id, 
                            P_cur_pos_type(i).transaction_nbr , 
                            P_cur_pos_type(i).vendor_code , 
                            P_cur_pos_type(i).action_code , 
                            P_cur_pos_type(i).delivery_date , 
                            P_cur_pos_type(i).ship_date , 
                            P_cur_pos_type(i).cancel_date , 
                            P_cur_pos_type(i).pre_pack_code , 
                            P_cur_pos_type(i).pre_pack_ratio , 
                            P_cur_pos_type(i).pre_pack_total_units , 
                            P_cur_pos_type(i).unit_cost , 
                            P_cur_pos_type(i).unit_retail , 
                            P_cur_pos_type(i).seq_nbr ,  
                            P_cur_pos_type(i).ref_transaction_id , 
                            P_cur_pos_type(i).ref_seq_nbr , 
                            P_cur_pos_type(i).location , 
                            P_cur_pos_type(i).item_alternate_code , 
                            P_cur_pos_type(i).item_part_a , 
                            P_cur_pos_type(i).item_part_b , 
                            P_cur_pos_type(i).item_part_c , 
                            P_cur_pos_type(i).item_part_d , 
                            P_cur_pos_type(i).item_part_e , 
                            P_cur_pos_type(i).item_part_f , 
                            P_cur_pos_type(i).invn_attr_a , 
                            P_cur_pos_type(i).invn_attr_b , 
                            P_cur_pos_type(i).invn_attr_c , 
                            P_cur_pos_type(i).expiry_date , 
                            P_cur_pos_type(i).batch_nbr , 
                            P_cur_pos_type(i).serial_nbr , 
                            P_cur_pos_type(i).quantity , 
                            P_cur_pos_type(i).pos_user , 
                            P_cur_pos_type(i).invn_attr_d , 
                            P_cur_pos_type(i).invn_attr_e , 
                            P_cur_pos_type(i).invn_attr_f , 
                            P_cur_pos_type(i).invn_attr_g , 
                            'New', 
                            NULL, 
                            NULL, 
                            NULL, 
                            NULL, 
                            Sys_context('USERENV','SESSION_USER') , 
                            SYSDATE, 
                            SYSDATE, 
                            Sys_context('USERENV','SESSION_USER') , 
                            Sys_context('USERENV','SESSION_USER') 
                ); 

  EXCEPTION 
  WHEN OTHERS THEN 
    p_status:='Error while Bulk updation Insertion of data in Insertposdata Procedure '; 

    FOR ex IN 1..SQL%bulk_exceptions.count 
    LOOP 
      l_iteration := SQL%bulk_exceptions ( ex ) .error_index; 
      l_error := SQL%bulk_exceptions(ex).error_code; 
      l_message := SQLERRM(-l_error); 
      lc_error := Substr(l_err_msg, 1, 3500); 
      Log('Error :'); 
      Log(SQL%Bulk_exceptions(ex).error_index); 
    END LOOP; 
  END; 
EXCEPTION 
WHEN OTHERS THEN 
  p_status:='Exception in main block'; 
  lc_error := Substr(SQLERRM, 1, 4000); 
  Log ( 'Error in InsertPOSData Procedure Main Block :' 
  || SQLERRM); 
END insertposdata; 

PROCEDURE update_trnxs ( p_batch_id IN NUMBER,
                        p_status OUT VARCHAR2 ) 
IS 
  CURSOR cur_serial_numbers IS 
    SELECT   transaction_id, 
             serial_nbr, 
             ROWID                row_id, 
             ceil (ROWNUM / 5000) counter 
    FROM     shaw_om_pos_logfire_stg 
    WHERE    1 = 1 
    AND      status = 'New' 
    AND      batch_id = p_batch_id 
    ORDER BY transaction_id DESC; 

TYPE cur_serial_tab 
IS 
  TABLE OF cur_serial_numbers%ROWTYPE INDEX BY BINARY_INTEGER; 
  lt_cur_serial_tab CUR_SERIAL_TAB; 
  ln_counter NUMBER := 0; 
  ln_errors  VARCHAR2 (4000); 
BEGIN 
  p_status := 'Success'; 
  OPEN cur_serial_numbers; 
  FETCH cur_serial_numbers bulk collect 
  INTO  lt_cur_serial_tab; 

  CLOSE cur_serial_numbers; 
  BEGIN 
    forall indx1 IN lt_cur_serial_tab.first .. lt_cur_serial_tab.last 
    save EXCEPTIONS 
    UPDATE shaw_om_pos_logfire_stg 
    SET    invn_attr_g = lt_cur_serial_tab (indx1).counter 
    WHERE  ROWID = lt_cur_serial_tab (indx1).row_id; 

  EXCEPTION 
  WHEN OTHERS THEN 
    p_status := 'update_trxns :' 
    || SQLERRM; 
    ln_errors := SQL%bulk_exceptions.count; 
    FOR j IN 1 .. ln_errors 
    LOOP 
      Log ( 'Bulk Error For Record Number: ' 
      || SQL%Bulk_exceptions (j).error_index 
      || ' Group ID: ' 
      || Lt_cur_serial_tab ( SQL%Bulk_exceptions (j).error_index).transaction_id); 
    END LOOP; 
  END; 
EXCEPTION 
WHEN OTHERS THEN 
  Log ( 'Error in update_trxns Procedure Main Block :' 
  || SQLERRM); 
END update_trnxs; 
END shaw_om_pos_lgf_pkg;