*&---------------------------------------------------------------------*
*&  Include           ZPMC008_FRM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE
*&---------------------------------------------------------------------*
FORM f_get_file.

  IF p_local IS NOT INITIAL.
    CLEAR p_file.

    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        mask     = ',Excel (xlsx),*.xlsx.'
        mode     = 'O'
        def_path = p_file " file path
      IMPORTING
        filename = p_file
      EXCEPTIONS
        OTHERS   = 99.
  ELSE.
    CLEAR: p_file.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = space
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_READ_FILE
*&---------------------------------------------------------------------*
FORM f_read_file.

  IF p_local IS NOT INITIAL.
    PERFORM f_read_local.
  ELSE.
    PERFORM f_read_sap.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_READ_LOCAL
*&---------------------------------------------------------------------*
FORM f_read_local.

  DATA: lv_filename      TYPE string,
        lt_records       TYPE solix_tab,
        lv_headerxstring TYPE xstring,
        lv_filelength    TYPE i.

  TRY.
      lv_filename = p_file.

      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename   = lv_filename
          filetype   = 'BIN'
        IMPORTING
          filelength = lv_filelength
          header     = lv_headerxstring
        TABLES
          data_tab   = lt_records.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_filelength
        IMPORTING
          buffer       = lv_headerxstring
        TABLES
          binary_tab   = lt_records.

    CATCH cx_root.
      MESSAGE text-004 TYPE gc_erro.
  ENDTRY.

  DATA: lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
      document_name = lv_filename
      xdocument     = lv_headerxstring ) .
  ENDTRY .

  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
  IMPORTING
  worksheet_names = DATA(lt_worksheets) ).

  IF NOT lt_worksheets IS INITIAL.
    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname ).

    ASSIGN lo_data_ref->* TO <gt_data>.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN
*&---------------------------------------------------------------------*
FORM f_screen.

  IF p_local IS NOT INITIAL AND sy-ucomm NE 'ONLI'.
    CLEAR p_file.
  ELSEIF p_sap IS NOT INITIAL AND sy-ucomm NE 'ONLI'.
    CLEAR p_file.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_TAB
*&---------------------------------------------------------------------*
FORM f_format_tab.

  LOOP AT <gt_data> ASSIGNING <gs_data>.
    IF sy-tabix <= 4.
      CONTINUE.
    ENDIF.

    PERFORM f_assign_comp.

    APPEND gs_data TO gt_data.
    CLEAR gs_data.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASSIGN_COMP
*&---------------------------------------------------------------------*
FORM f_assign_comp.

  TRY.
      ASSIGN COMPONENT 'A' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-wptxt = <fs_value>.
      ASSIGN COMPONENT 'B' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-sfakt = <fs_value>.
      ASSIGN COMPONENT 'C' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-begru = <fs_value>.
      ASSIGN COMPONENT 'D' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-mptyp = <fs_value>.
      ASSIGN COMPONENT 'E' OF STRUCTURE <gs_data> TO <fs_value>.
      IF <fs_value> EQ gc_mon_pt.
        gs_data-hunit = gc_mon_en.
      ELSE.
        gs_data-hunit = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'F' OF STRUCTURE <gs_data> TO <fs_value>.
      REPLACE ALL OCCURRENCES OF '-' IN <fs_value> WITH space.
      gs_data-stadt = <fs_value>.
      ASSIGN COMPONENT 'G' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-plan_sort = <fs_value>.
      ASSIGN COMPONENT 'H' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-pstxt = <fs_value>.
      ASSIGN COMPONENT 'I' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-plnty = <fs_value>.
      ASSIGN COMPONENT 'J' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-plnnr = <fs_value>.
      ASSIGN COMPONENT 'K' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-plnal = <fs_value>+2(2).
      ASSIGN COMPONENT 'L' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-gewerk = <fs_value>.
      ASSIGN COMPONENT 'M' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-iwerk = <fs_value>.
      ASSIGN COMPONENT 'N' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-auart = <fs_value>.
      ASSIGN COMPONENT 'O' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-ilart = <fs_value>.
      ASSIGN COMPONENT 'P' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-gsber = <fs_value>.
      ASSIGN COMPONENT 'Q' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-mptyp2 = <fs_value>.
      ASSIGN COMPONENT 'R' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-priok = <fs_value>.
      ASSIGN COMPONENT 'S' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-tplnr = <fs_value>.
      ASSIGN COMPONENT 'T' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-zykl1 = <fs_value>.
      ASSIGN COMPONENT 'U' OF STRUCTURE <gs_data> TO <fs_value>.
      IF <fs_value> EQ gc_mon_pt.
        gs_data-zeieh = gc_mon_en.
      ELSE.
        gs_data-zeieh = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'V' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-pak_text = <fs_value>.
      ASSIGN COMPONENT 'W' OF STRUCTURE <gs_data> TO <fs_value>.
      gs_data-offs1 = <fs_value>.

    CATCH cx_root.
      MESSAGE text-004 TYPE gc_erro.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CALL_BAPI
*&---------------------------------------------------------------------*
FORM f_call_bapi.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Criando Planos de Manutenção...'.

  SELECT *
    FROM crhd
    INTO TABLE gt_crhd
 FOR ALL ENTRIES IN gt_data
   WHERE arbpl EQ gt_data-gewerk
     AND werks EQ gt_data-iwerk.

  SORT gt_crhd BY arbpl werks.

  gt_data_aux[] = gt_data[].

  DELETE ADJACENT DUPLICATES FROM gt_data_aux COMPARING wptxt.

  LOOP AT gt_data_aux INTO gs_data_aux.

    CLEAR: gs_header, gv_number.

    REFRESH: gt_items,
             gt_cycles,
             gt_return.

    CHECK gv_erro IS INITIAL.

    "header
    gs_header-wptxt     = gs_data_aux-wptxt.
    gs_header-sfakt     = gs_data_aux-sfakt.
    gs_header-begru     = gs_data_aux-begru.
    gs_header-mptyp     = gs_data_aux-mptyp.
    gs_header-hunit     = gs_data_aux-hunit.
    gs_header-stadt     = gs_data_aux-stadt.
    gs_header-plan_sort = gs_data_aux-plan_sort.

    LOOP AT gt_data INTO gs_data WHERE wptxt = gs_data_aux-wptxt.

      "items
      gs_items-pstxt = gs_data-pstxt.
      gs_items-plnty = gs_data-plnty.
      gs_items-plnnr = gs_data-plnnr.
      gs_items-plnal = gs_data-plnal.

      READ TABLE gt_crhd INTO gs_crhd WITH KEY arbpl = gs_data-gewerk
                                               werks = gs_data-iwerk BINARY SEARCH.

      gs_items-gewrk = gs_crhd-objid.

      gs_items-iwerk = gs_data-iwerk.
      gs_items-auart = gs_data-auart.
      gs_items-ilart = gs_data-ilart.
      gs_items-gsber = gs_data-gsber.
      gs_items-mityp = gs_data-mptyp.
      gs_items-priok = gs_data-priok.
      gs_items-tplnr = gs_data-tplnr.
      APPEND gs_items TO gt_items.
      CLEAR gs_items.

    ENDLOOP.

    "cycles
    gs_cycles-zykl1    = gs_data_aux-zykl1.
    gs_cycles-zeieh    = gs_data_aux-zeieh.
    gs_cycles-pak_text = gs_data_aux-pak_text.
    gs_cycles-offset   = gs_data_aux-offs1.
    APPEND gs_cycles TO gt_cycles.
    CLEAR gs_cycles.

    CALL FUNCTION 'MPLAN_CREATE'
      EXPORTING
        header = gs_header
      IMPORTING
        number = gv_number
      TABLES
        items  = gt_items
        cycles = gt_cycles
        return = gt_return.

    DELETE gt_return WHERE type = gc_warning.

    gs_log-number = gv_number.

    LOOP AT gt_return INTO gs_return.

      CASE gs_return-type.
        WHEN gc_sucess.
          gs_log-status = gc_green.
        WHEN gc_erro.
          gs_log-status = gc_red.
          gv_erro = 'X'.
      ENDCASE.

      gs_log-message = gs_return-message.

      APPEND gs_log TO gt_log.
      CLEAR gs_log.

    ENDLOOP.
  ENDLOOP.

  IF gv_erro IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    MESSAGE text-002 TYPE gc_sucess.
  ELSE.
    MESSAGE text-003 TYPE gc_sucess DISPLAY LIKE gc_erro.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_LOG
*&---------------------------------------------------------------------*
FORM f_show_log.

  IF gt_log IS NOT INITIAL.

    CLEAR gt_fieldcat.
    gt_fieldcat-col_pos     = 0.
    gt_fieldcat-outputlen   = 8.
    gt_fieldcat-fieldname   = 'NUMBER'.
    gt_fieldcat-seltext_m   = 'Plano de Manutenção'.
    APPEND gt_fieldcat TO gt_fieldcat.

    CLEAR gt_fieldcat.
    gt_fieldcat-col_pos     = 1.
    gt_fieldcat-outputlen   = 7.
    gt_fieldcat-fieldname   = 'STATUS'.
    gt_fieldcat-seltext_m   = 'Status'.
    APPEND gt_fieldcat TO gt_fieldcat.

    CLEAR gt_fieldcat.
    gt_fieldcat-col_pos     = 2.
    gt_fieldcat-outputlen   = 60.
    gt_fieldcat-fieldname   = 'MESSAGE'.
    gt_fieldcat-seltext_m   = 'Mensagem'.
    APPEND gt_fieldcat TO gt_fieldcat.

    gs_layout-zebra             = 'X'.
    gs_layout-colwidth_optimize = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = gs_layout
        it_fieldcat        = gt_fieldcat[]
        i_save             = 'X'
      TABLES
        t_outtab           = gt_log
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_READ_SAP
*&---------------------------------------------------------------------*
FORM f_read_sap.

  DATA:
    lv_subrc            TYPE sy-subrc,
    lv_len              TYPE sy-tabix,

    lst_rcgrepfile      TYPE cps_x255,
    li_e_rcgrepfile_tab TYPE cpt_x255,
    li_xtab             TYPE cpt_x255,

    lv_file_size        TYPE i,
    lv_lines            TYPE i,
    lv_lines2           TYPE i,

    lv_file_content     TYPE xstring,
    lv_xstring          TYPE xstring.

  CLEAR lv_subrc.
  TRANSLATE p_file TO LOWER CASE.

  IF p_file CS '.xls'.
    OPEN DATASET p_file FOR INPUT IN BINARY MODE.

    lv_subrc = sy-subrc.

    IF sy-subrc <> 0 OR lv_subrc <> 0.
*        RAISE open_failed.
      MESSAGE text-004 TYPE gc_erro.
    ENDIF.

    DO.
      CLEAR lv_len.
      CLEAR lst_rcgrepfile.

      READ DATASET p_file INTO lst_rcgrepfile LENGTH lv_len.

      IF sy-subrc <> 0.
        IF lv_len > 0.
          lv_file_size = lv_file_size + lv_len.
          APPEND lst_rcgrepfile TO li_e_rcgrepfile_tab.
        ENDIF.
        EXIT.
      ENDIF.

      CONCATENATE lv_file_content lst_rcgrepfile INTO lv_file_content IN BYTE MODE.

      lv_file_size = lv_file_size + lv_len.

      APPEND lst_rcgrepfile TO li_e_rcgrepfile_tab.
    ENDDO.

    IF sy-subrc > 10.
*    RAISE read_error.
      MESSAGE text-004 TYPE gc_erro.
    ENDIF.

    DESCRIBE TABLE li_e_rcgrepfile_tab LINES lv_lines.
    CLOSE DATASET p_file.
  ELSE.
    MESSAGE text-004 TYPE gc_erro.
  ENDIF.


  IF li_e_rcgrepfile_tab[] IS NOT INITIAL.
    li_xtab[] = li_e_rcgrepfile_tab[].
  ENDIF.

  cl_scp_change_db=>xtab_to_xstr( EXPORTING im_xtab    = li_xtab
    im_size    = lv_lines2
  IMPORTING ex_xstring = lv_xstring ).

  DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
      document_name = p_file
      xdocument     = lv_xstring ) .
    CATCH cx_fdt_excel_core.
  ENDTRY .
  IF lo_excel_ref IS BOUND.

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
    worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.
      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
            lv_woksheetname ).

      ASSIGN lo_data_ref->* TO <gt_data>.
    ENDIF.
  ENDIF.

ENDFORM.
