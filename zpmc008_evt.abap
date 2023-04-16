*&---------------------------------------------------------------------*
*&  Include           ZPMC008_EVT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM f_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM f_get_file.

START-OF-SELECTION.

  PERFORM f_read_file.

  PERFORM f_format_tab.

  PERFORM f_call_bapi.

END-OF-SELECTION.

  PERFORM f_show_log.