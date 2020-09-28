create or replace PACKAGE SHAW_OM_POS_LGF_PKG AUTHID CURRENT_USER
AS
  /* Header: SHAW_OM_POS_LGF_PKG.pks 1.0 02-MAY-2020 Gudhi Koushik        */
  --+======================================================================+
  --| Module/Object Name: SHAW_OM_POS_LGF_PKG.pks                          |
  --|                                                                      |
  --| Description :  Interface LOG_INT_18 package spec                     | 
  --|                                                                      |
  --| Creation Date: 02-MAY-2020                                           |
  --|                                                                      |
  --| Author      : Gudhi Koushik                                          |
  --|                                                                      |
  --| Calls/Called by:                                                     |
  --|                                                                      |
  --| Arguments   :                                                        |
  --|                                                                      |
  --+======================================================================+
  --| Modification History                                                 |
  --+======================================================================+
  --| Date        Who                Ver   Change Description              |
  --| ----------- ----------         ---  ---------------------------------|
  --| 15-MAY-2020 Gudhi Koushik         1.0   Created                      |
  --|   --+===============================================================+

  PROCEDURE LOG (p_message IN VARCHAR2);


  PROCEDURE GetLastRunDate(
      P_last_successful_run_date OUT VARCHAR2,
      P_current_run_date OUT VARCHAR2,
      p_status OUT VARCHAR2
);

PROCEDURE DeleteRecords(p_status OUT VARCHAR2);

PROCEDURE InsertPOSData(
      P_CUR_POS_TYPE IN SHAW_OM_POS_INS_TAB , p_status OUT VARCHAR2);
PROCEDURE InsertProjectData(o_out_status OUT VARCHAR2);

PROCEDURE  UpdateBatchID(p_batch_id  OUT VARCHAR2,p_sub_count in number,p_status OUT VARCHAR2);


PROCEDURE update_errors (
        p_batch_id            IN   NUMBER,
        p_success             IN   VARCHAR2,
        p_message             IN   VARCHAR2,
        p_sub_batch_id        IN   NUMBER, 
        p_seq                 IN NUMBER,
        p_err_dtls_tbl_type   IN   xx_shaw_cmmn_err_tab
    );

PROCEDURE update_trnxs(
    p_batch_id   IN     NUMBER,
    p_status OUT VARCHAR2);

END SHAW_OM_POS_LGF_PKG;