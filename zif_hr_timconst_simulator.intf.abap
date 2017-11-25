INTERFACE zif_hr_timconst_simulator
  PUBLIC .

  TYPES: tt_key_fields TYPE TABLE OF name_komp.

  METHODS set_table
    IMPORTING it_table TYPE ANY TABLE
    RAISING   cx_dynamic_check.

  METHODS set_time_constraints
    IMPORTING iv_allow_concurent_records TYPE boole_d
              iv_allow_gaps              TYPE boole_d.

  METHODS set_key_fields
    IMPORTING it_key_fields TYPE tt_key_fields.

  METHODS check_collision_insert
    IMPORTING is_record                  TYPE any
    RETURNING VALUE(ro_colision_results) TYPE REF TO zif_hr_modify_colision_rslts.

  METHODS check_collision_delete
    IMPORTING is_record                  TYPE any
    RETURNING VALUE(ro_colision_results) TYPE REF TO zif_hr_delete_colision_rslts.

  METHODS check_collision_modify
    IMPORTING is_record                  TYPE any
              iv_new_begda               TYPE begda
              iv_new_endda               TYPE endda
    RETURNING VALUE(ro_colision_results) TYPE REF TO zif_hr_modify_colision_rslts.

  METHODS insert_record
    IMPORTING is_record TYPE any
    RAISING   cx_dynamic_check.

  METHODS delete_record
    IMPORTING is_record TYPE any
    RAISING   cx_dynamic_check.

  METHODS modify_record
    IMPORTING is_record    TYPE any
              iv_new_begda TYPE begda
              iv_new_endda TYPE endda
    RAISING   cx_dynamic_check.

  METHODS get_resolved_table
    RETURNING VALUE(rrt_resolved_table) TYPE REF TO data.

ENDINTERFACE.
