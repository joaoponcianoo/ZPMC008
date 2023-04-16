*&---------------------------------------------------------------------*
*&  Include           ZPMC008_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS:
  p_file  TYPE string,
  p_local RADIOBUTTON GROUP rb DEFAULT 'X' USER-COMMAND ucomm,
  p_sap   RADIOBUTTON GROUP rb.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Field Symbols
*----------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gt_data>  TYPE STANDARD TABLE,
  <gs_data>  TYPE any,
  <fs_value> TYPE any.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_data,
    wptxt     TYPE wptxt,
    sfakt     TYPE sfakt,
    begru     TYPE begru,
    mptyp     TYPE mptyp,
    hunit     TYPE hunit,
    stadt     TYPE stadt,
    plan_sort TYPE plan_sort,
    pstxt     TYPE postxt,
    plnty     TYPE plnty,
    plnnr     TYPE plnnr,
    plnal     TYPE plnal,
    gewerk    TYPE gewrk,
    iwerk     TYPE iwerk,
    auart     TYPE auart,
    ilart     TYPE ila,
    gsber     TYPE gsber,
    mptyp2    TYPE mptyp,
    priok     TYPE priok,
    tplnr     TYPE tplnr,
    zykl1     TYPE wzykl1,
    zeieh     TYPE dzeieh,
    pak_text  TYPE txzyklus,
    offs1     TYPE woffs1,
  END OF ty_data,

  BEGIN OF ty_log,
    number  TYPE warpl,
    status  TYPE icon-id,
    message TYPE bapiret2-message,
  END OF ty_log.

*----------------------------------------------------------------------*
* Structures
*----------------------------------------------------------------------*
DATA:
  gs_data     TYPE ty_data,
  gs_data_aux TYPE ty_data,
  gs_crhd     TYPE crhd,
  gs_log      TYPE ty_log,

  "bapi
  gs_header   TYPE mplan_mpla,
  gs_items    TYPE mplan_mpos,
  gs_cycles   TYPE mplan_mmpt,
  gs_return   TYPE bapiret2,
  gs_layout   TYPE slis_layout_alv.

*----------------------------------------------------------------------*
* Internal Tables
*----------------------------------------------------------------------*
DATA:
  gt_data     TYPE TABLE OF ty_data,
  gt_data_aux TYPE TABLE OF ty_data,
  gt_crhd     TYPE TABLE OF crhd,
  gt_log      TYPE TABLE OF ty_log,

  "babi
  gt_items    TYPE TABLE OF mplan_mpos,
  gt_cycles   TYPE TABLE OF mplan_mmpt,
  gt_return   TYPE TABLE OF bapiret2,
  gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.


*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA:
  gv_number TYPE warpl,
  gv_erro   TYPE c.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:
  gc_red     TYPE icon-id VALUE '@0A@',
  gc_yellow  TYPE icon-id VALUE '@09@',
  gc_green   TYPE icon-id VALUE '@08@',

  gc_erro    TYPE c VALUE 'E',
  gc_warning TYPE c VALUE 'W',
  gc_sucess  TYPE c VALUE 'S',

  gc_mon_pt  TYPE c LENGTH 3 VALUE 'MES',
  gc_mon_en  TYPE c LENGTH 3 VALUE 'MON'.