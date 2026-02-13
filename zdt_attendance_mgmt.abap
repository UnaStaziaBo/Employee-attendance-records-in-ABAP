*&---------------------------------------------------------------------*
*& Report  ZDT_ATTENDANCE_MGMT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZDT_ATTENDANCE_MGMT.

DATA ok_code TYPE sy-ucomm.

TABLES: zdt_attendance.

"====================================================
" TYPES
"====================================================
TYPES: BEGIN OF ty_month_alv,
         mandt     TYPE zdt_attendance-mandt,
         pernr     TYPE zdt_attendance-pernr,
         job_date  TYPE zdt_attendance-job_date,
         time_in   TYPE zdt_attendance-time_in,
         time_out  TYPE zdt_attendance-time_out,
         job_hours TYPE zdt_attendance-job_hours,
         note      TYPE zdt_attendance-note,
         rowcolor  TYPE c LENGTH 4,
       END OF ty_month_alv.

"====================================================
" GLOBAL DATA
"====================================================
"Screen 0100 data
DATA: gv_pernr    TYPE zdt_attendance-pernr,
      gv_date     TYPE zdt_attendance-job_date,
      gv_time_in  TYPE zdt_attendance-time_in,
      gv_time_out TYPE zdt_attendance-time_out,
      gv_hours    TYPE zdt_attendance-job_hours,
      gv_note     TYPE zdt_attendance-note.

"ALV objects
DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_grid      TYPE REF TO cl_gui_alv_grid.

"ALV data
DATA: gt_month TYPE STANDARD TABLE OF ty_month_alv.

"ALV columns and layout
DATA: gt_fcat  TYPE lvc_t_fcat.
DATA: gs_layo  TYPE lvc_s_layo.

"SmartForm data
DATA: gv_name  TYPE string,
      gv_month TYPE int2,
      gv_year  TYPE int4.
   "   gv_sumh  TYPE zdt_attendance-job_hours.

CALL SCREEN 0090.
CALL SCREEN 0100.

"====================================================
" CONSTANTS
"====================================================
"Time count
CONSTANTS:
  c_sec_per_hour TYPE i VALUE 3600,
  c_sec_per_min  TYPE i VALUE 60.

"====================================================
" CLASSES
"====================================================
"Double-click handler
CLASS lcl_alv_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.                    "lcl_alv_handler DEFINITION

DATA go_handler TYPE REF TO lcl_alv_handler.

CLASS lcl_alv_handler IMPLEMENTATION.
  METHOD on_double_click.
    DATA ls_row TYPE ty_month_alv.
    DATA ls_db  TYPE zdt_attendance.

    READ TABLE gt_month INTO ls_row INDEX e_row-index.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING ls_row TO ls_db.

    gv_pernr    = ls_db-pernr.
    gv_date     = ls_db-job_date.
    gv_time_in  = ls_db-time_in.
    gv_time_out = ls_db-time_out.
    gv_hours    = ls_db-job_hours.
    gv_note     = ls_db-note.

    LEAVE TO SCREEN 0100.
  ENDMETHOD.
ENDCLASS.

MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.

    WHEN 'BUTTON_EXIT' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.

   WHEN 'BACK'.
      LEAVE TO SCREEN 0090.
   WHEN 'STEP'.
      LEAVE TO SCREEN 0100.

    WHEN 'BUTTON_SAVE'.
      PERFORM save_data.

    WHEN 'BUTTON_UPDATE'.
      PERFORM update_data.

    WHEN 'BUTTON_DELETE'.
      PERFORM delete_data.

    WHEN 'BUTTON_OVERVIEW'.
      PERFORM overview_data.
      CALL SCREEN 0200.

    WHEN 'BUTTON_REPORT'.
      PERFORM print_monthly_report.

  ENDCASE.

  CLEAR ok_code.
ENDMODULE.



"  MODULE status_0200 OUTPUT
MODULE status_0200 OUTPUT.

ENDMODULE.                    "status_0200 OUTPUT

"MODULE user_command_0200 INPUT
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BUTTON_CLOSE'.
      LEAVE TO SCREEN 0100.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.                    "user_command_0200 INPUT

"Module  USER_COMMAND_0090  INPUT
module USER_COMMAND_0090 input.

    CASE sy-ucomm.
    WHEN 'BUTTON_CONTINUE'.
      IF gv_name IS INITIAL.
        MESSAGE 'Zadajte svoje meno' TYPE 'E'.
      ELSE.
        CLEAR sy-ucomm.
        LEAVE TO SCREEN 0100.
      ENDIF.
  ENDCASE.

endmodule.                 " USER_COMMAND_0090  INPUT


"====================================================
" FORMS
"====================================================
"Save_data
FORM save_data.
  DATA: ls_att      TYPE zdt_attendance,
        ls_exist    TYPE zdt_attendance,
        lv_sec_in   TYPE i,
        lv_sec_out  TYPE i,
        lv_diff_sec TYPE i,
        lv_hours_p  TYPE p LENGTH 8 DECIMALS 2.

  IF gv_pernr IS INITIAL OR gv_date IS INITIAL.
    MESSAGE 'PERNR a DATE sú povinné!' TYPE 'E'.
    RETURN.
  ENDIF.

  IF gv_time_in IS INITIAL OR gv_time_out IS INITIAL.
    MESSAGE 'TIME_IN a TIME_OUT sú povinné!' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_sec_in  =
    gv_time_in+0(2) * c_sec_per_hour
  + gv_time_in+2(2) * c_sec_per_min
  + gv_time_in+4(2).

  lv_sec_out =
      gv_time_out+0(2) * c_sec_per_hour
    + gv_time_out+2(2) * c_sec_per_min
    + gv_time_out+4(2).

  IF lv_sec_out <= lv_sec_in.
    MESSAGE 'TIME_OUT musí byť väčší ako TIME_IN!' TYPE 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE * FROM zdt_attendance INTO ls_exist
    WHERE pernr = gv_pernr AND job_date = gv_date.

  IF sy-subrc = 0.
    MESSAGE 'Záznam už existuje (použi UPDATE).' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_diff_sec = lv_sec_out - lv_sec_in.
  lv_hours_p = lv_diff_sec / c_sec_per_hour.
  gv_hours    = lv_hours_p.

  CLEAR ls_att.
  ls_att-mandt     = sy-mandt.
  ls_att-pernr     = gv_pernr.
  ls_att-job_date  = gv_date.
  ls_att-time_in   = gv_time_in.
  ls_att-time_out  = gv_time_out.
  ls_att-job_hours = gv_hours.
  ls_att-note      = gv_note.

  INSERT zdt_attendance FROM ls_att.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE 'Záznam bol úspešne uložený.' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Chyba pri ukladaní (INSERT zlyhal).' TYPE 'E'.
  ENDIF.
ENDFORM.

"Form  update_data
FORM update_data.
  DATA: ls_exist    TYPE zdt_attendance,
        ls_upd      TYPE zdt_attendance,
        lv_sec_in   TYPE i,
        lv_sec_out  TYPE i,
        lv_diff_sec TYPE i,
        lv_hours_p  TYPE p LENGTH 8 DECIMALS 2.

  IF gv_pernr IS INITIAL OR gv_date IS INITIAL.
    MESSAGE 'PERNR a DATE sú povinné!' TYPE 'E'.
    RETURN.
  ENDIF.

  IF gv_time_in IS INITIAL OR gv_time_out IS INITIAL.
    MESSAGE 'TIME_IN a TIME_OUT sú povinné!' TYPE 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE * FROM zdt_attendance INTO ls_exist
    WHERE pernr = gv_pernr AND job_date = gv_date.

  IF sy-subrc <> 0.
    MESSAGE 'Záznam neexistuje - nie je čo aktualizovať.' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_sec_in  =
    gv_time_in+0(2) * c_sec_per_hour
  + gv_time_in+2(2) * c_sec_per_min
  + gv_time_in+4(2).

  lv_sec_out =
      gv_time_out+0(2) * c_sec_per_hour
    + gv_time_out+2(2) * c_sec_per_min
    + gv_time_out+4(2).

  IF lv_sec_out <= lv_sec_in.
    MESSAGE 'TIME_OUT musí byť väčší ako TIME_IN!' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_diff_sec = lv_sec_out - lv_sec_in.
  lv_hours_p = lv_diff_sec / c_sec_per_hour.
  gv_hours    = lv_hours_p.

  ls_upd = ls_exist.
  ls_upd-time_in   = gv_time_in.
  ls_upd-time_out  = gv_time_out.
  ls_upd-job_hours = gv_hours.
  ls_upd-note      = gv_note.

  MODIFY zdt_attendance FROM ls_upd.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE 'Záznam bol úspešne aktualizovaný.' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Chyba pri aktualizácii (UPDATE zlyhal).' TYPE 'E'.
  ENDIF.
ENDFORM.                    "update_data

"Form  delete_data
FORM delete_data.
  IF gv_pernr IS INITIAL OR gv_date IS INITIAL.
    MESSAGE 'PERNR a DATE sú povinné pre zmazanie!' TYPE 'E'.
    RETURN.
  ENDIF.

  DELETE FROM zdt_attendance WHERE pernr = gv_pernr AND job_date = gv_date.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE 'Záznam bol odstránený.' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Záznam nebol nájdený alebo sa nepodarilo zmazať.' TYPE 'E'.
  ENDIF.
ENDFORM.                    "delete_data

"Form  overview_data]
FORM overview_data.
  DATA: lv_first TYPE d,
        lv_last  TYPE d,
        lv_day TYPE c LENGTH 1.

  DATA: lt_db TYPE STANDARD TABLE OF zdt_attendance WITH DEFAULT KEY,
        ls_db TYPE zdt_attendance,
        ls_alv TYPE ty_month_alv.

  IF gv_pernr IS INITIAL OR gv_date IS INITIAL.
    MESSAGE 'Zadaj PERNR a DATE (mesiac sa vezme z DATE).' TYPE 'E'.
    RETURN.
  ENDIF.

  lv_first = gv_date.
  lv_first+6(2) = '01'.

  lv_last = lv_first.
  ADD 31 TO lv_last.
  lv_last+6(2) = '01'.
  lv_last = lv_last - 1.

  CLEAR: gt_month, lt_db.

  SELECT * FROM zdt_attendance
    INTO TABLE lt_db
   WHERE pernr    = gv_pernr
     AND job_date BETWEEN lv_first AND lv_last
   ORDER BY job_date.

  LOOP AT lt_db INTO ls_db.
    CLEAR ls_alv.
    MOVE-CORRESPONDING ls_db TO ls_alv.

    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = ls_alv-job_date
      IMPORTING
        day  = lv_day.

    IF lv_day = '6' OR lv_day = '7'.
      ls_alv-rowcolor = 'C511'.
    ENDIF.

    APPEND ls_alv TO gt_month.
  ENDLOOP.
ENDFORM.                    "overview_data

"Form  build_fieldcat
FORM build_fieldcat.
  DATA ls_fcat TYPE lvc_s_fcat.
  CLEAR gt_fcat.

  DEFINE add_col.
    clear ls_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-coltext   = &2.
    ls_fcat-do_sum    = &3.
    append ls_fcat to gt_fcat.
  END-OF-DEFINITION.

  add_col 'PERNR'     'Osobné číslo' abap_false.
  add_col 'JOB_DATE'  'Dátum'        abap_false.
  add_col 'TIME_IN'   'Čas príchodu' abap_false.
  add_col 'TIME_OUT'  'Čas odchodu'  abap_false.
  add_col 'JOB_HOURS' 'Hodiny'       abap_true.
  add_col 'NOTE'      'Poznámka'     abap_false.
ENDFORM.                    "build_fieldcat

"Form  build_layout
FORM build_layout.
  CLEAR gs_layo.
  gs_layo-zebra      = abap_true.
  gs_layo-cwidth_opt = abap_true.
  gs_layo-info_fname = 'ROWCOLOR'.
ENDFORM.                    "build_layout

"Form  print_monthly_report
FORM print_monthly_report.

  DATA it_attendance TYPE TABLE OF zdt_attendance.
  SELECT * FROM zdt_attendance INTO TABLE it_attendance.

    IF gv_date IS NOT INITIAL.
    gv_year  = gv_date(4).     "YYYY
    gv_month = gv_date+4(2).   "MM
    ELSE.
      gv_year  = sy-datum(4).
      gv_month = sy-datum+4(2).
    ENDIF.

  CALL FUNCTION '/1BCDWB/SF00000002'
    EXPORTING
*     ARCHIVE_INDEX              =
*     ARCHIVE_INDEX_TAB          =
*     ARCHIVE_PARAMETERS         =
*     CONTROL_PARAMETERS         =
*     MAIL_APPL_OBJ              =
*     MAIL_RECIPIENT             =
*     MAIL_SENDER                =
*     OUTPUT_OPTIONS             =
*     USER_SETTINGS              = 'X'
      pernr                      = gv_pernr
      meno                       = gv_name
      mesiac                     = gv_month
      rok                        = gv_year
*   IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO            =
*     JOB_OUTPUT_OPTIONS         =
    TABLES
      it_attendance              = it_attendance
*   EXCEPTIONS
*     FORMATTING_ERROR           = 1
*     INTERNAL_ERROR             = 2
*     SEND_ERROR                 = 3
*     USER_CANCELED              = 4
*     OTHERS                     = 5
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "print_monthly_report


"====================================================
" INCLUDES
"====================================================
"Screen 0200 ALV module
MODULE alv_0200 OUTPUT.
  IF go_container IS INITIAL.

    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.

    IF go_handler IS INITIAL.
      CREATE OBJECT go_handler.
      SET HANDLER go_handler->on_double_click FOR go_grid.
    ENDIF.

    PERFORM build_fieldcat.
    PERFORM build_layout.

    go_grid->set_table_for_first_display(
      EXPORTING
        is_layout       = gs_layo
      CHANGING
        it_outtab       = gt_month
        it_fieldcatalog = gt_fcat ).

  ELSE.
    go_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  SET PF-STATUS 'MENU1'.
*  SET TITLEBAR 'xxx'.

endmodule.                 " STATUS_0100  OUTPUT
