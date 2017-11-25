*"* use this source file for your ABAP unit test classes
CLASS lcl_assert_ref_utilities DEFINITION.
  PUBLIC SECTION.
    METHODS: assert_structure_ref_equal
      IMPORTING irs_act_record TYPE REF TO data
                is_exp_record  TYPE any
      RAISING   cx_static_check,
      assert_ref_table_contains
        IMPORTING irt_act_table TYPE REF TO data
                  is_exp_record TYPE any
        RAISING   cx_static_check,
      assert_ref_table_lines
        IMPORTING irt_act_table TYPE REF TO data
                  iv_exp_lines  TYPE i
        RAISING   cx_static_check.
ENDCLASS.

CLASS lcl_assert_ref_utilities IMPLEMENTATION.
  METHOD assert_structure_ref_equal.

    FIELD-SYMBOLS: <ls_act_record> TYPE any.

    ASSIGN irs_act_record->* TO <ls_act_record>.

    cl_abap_unit_assert=>assert_equals( exp = is_exp_record act = <ls_act_record> ).

  ENDMETHOD.

  METHOD assert_ref_table_contains.

    FIELD-SYMBOLS: <lt_act_table> TYPE ANY TABLE.

    ASSIGN irt_act_table->* TO <lt_act_table>.

    cl_abap_unit_assert=>assert_table_contains( line = is_exp_record table = <lt_act_table> ).

  ENDMETHOD.

  METHOD assert_ref_table_lines.

    FIELD-SYMBOLS: <lt_act_table> TYPE ANY TABLE.

    ASSIGN irt_act_table->* TO <lt_act_table>.

    cl_abap_unit_assert=>assert_equals( exp = iv_exp_lines act = lines( <lt_act_table> ) ).

  ENDMETHOD.
ENDCLASS.

CLASS ltcl_timconst_simulator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_pernr_dates,
             pernr  TYPE numc07,
             begda  TYPE begda,
             endda  TYPE endda,
             nonkey TYPE string,
           END OF ts_pernr_dates,

           tt_pernr_dates TYPE STANDARD TABLE OF ts_pernr_dates WITH DEFAULT KEY.

  PRIVATE SECTION.

    DATA: mo_simulator        TYPE REF TO zif_hr_timconst_simulator,
          mo_ref_assert_util  TYPE REF TO lcl_assert_ref_utilities,
          mv_lines_before_ops TYPE i.

    METHODS:
      setup,
      verify_modify_collision_types
        IMPORTING io_collision_results TYPE REF TO zif_hr_modify_colision_rslts
                  delim                TYPE boole_d
                  override             TYPE boole_d
                  postpond             TYPE boole_d
        RAISING   cx_static_check,
      ins_nogaps_delimitation FOR TESTING RAISING cx_static_check,
      ins_nogaps_overriden FOR TESTING RAISING cx_static_check,
      ins_nogaps_postpond FOR TESTING RAISING cx_static_check,
      ins_nogaps_diff_keyfield FOR TESTING RAISING cx_static_check,
      ins_nogaps_all_effects FOR TESTING RAISING cx_static_check,
      ins_nogaps_detect_gap FOR TESTING RAISING cx_static_check,
      del_nogaps_extended FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_timconst_simulator IMPLEMENTATION.

  METHOD setup.

    DATA: lt_pernr_dates TYPE me->tt_pernr_dates,
          lt_key_fields  TYPE zif_hr_timconst_simulator=>tt_key_fields.

    lt_key_fields = VALUE #( ( 'PERNR' ) ).

    lt_pernr_dates = VALUE #(
        ( pernr = '0000001' begda = '20170101' endda = '99991231' nonkey = 'A' )
        ( pernr = '0000001' begda = '20160101' endda = '20161231' nonkey = 'B' )
        ( pernr = '0000001' begda = '20150101' endda = '20151231' nonkey = 'C' )

        ( pernr = '0000002' begda = '20170101' endda = '99991231' nonkey = 'A' )
        ( pernr = '0000002' begda = '20160101' endda = '20161231' nonkey = 'B' )
        ( pernr = '0000002' begda = '20150101' endda = '20151231' nonkey = 'C' )
    ).

    mv_lines_before_ops = lines( lt_pernr_dates ).

    mo_simulator = NEW zcl_hr_timconst_simulator( ).

    mo_simulator->set_table( it_table = lt_pernr_dates ).

    mo_simulator->set_time_constraints(
      EXPORTING
        iv_allow_concurent_records = abap_false
        iv_allow_gaps              = abap_false
        iv_protect_initial_record  = abap_false
    ).

    mo_simulator->set_key_fields( lt_key_fields ).

    me->mo_ref_assert_util = NEW #(  ).

  ENDMETHOD.

  METHOD ins_nogaps_delimitation.

    DATA: ls_insert_record      TYPE me->ts_pernr_dates,
          ls_exp_delim          TYPE me->ts_pernr_dates,
          ls_exp_resolved_delim TYPE me->ts_pernr_dates,
          lo_collision_result   TYPE REF TO zif_hr_modify_colision_rslts,
          lrs_delimited_record  TYPE REF TO me->ts_pernr_dates,
          lrt_resolved_table    TYPE REF TO me->tt_pernr_dates.

    FIELD-SYMBOLS: <lt_resolved_table> TYPE me->tt_pernr_dates.

    ls_insert_record = VALUE #( pernr = '00000001' begda = '20170201' endda = '99991231' nonkey = 'L' ).
    ls_exp_delim = VALUE #( pernr = '00000001' begda = '20170101' endda = '99991231' nonkey = 'A' ).
    ls_exp_resolved_delim = VALUE #( pernr = '00000001' begda = '20170101' endda = '20170131' nonkey = 'A' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_record ).

    me->verify_modify_collision_types( io_collision_results = lo_collision_result
                                       delim = 'X' override = ' ' postpond = ' ' ).

    lrs_delimited_record ?= lo_collision_result->get_delimited_record( ).

    mo_ref_assert_util->assert_structure_ref_equal( irs_act_record = lrs_delimited_record is_exp_record = ls_exp_delim ).

    mo_simulator->insert_record( is_record = ls_insert_record ).

    lrt_resolved_table ?= mo_simulator->get_resolved_table( ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_exp_resolved_delim ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_insert_record ).
    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = me->mv_lines_before_ops + 1 ).

  ENDMETHOD.

  METHOD ins_nogaps_overriden.

    DATA: ls_insert_record      TYPE me->ts_pernr_dates,
          lo_collision_result   TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_overriden_records TYPE REF TO me->tt_pernr_dates,
          lrt_resolved_table    TYPE REF TO me->tt_pernr_dates.

    ls_insert_record = VALUE #( pernr = '00000001' begda = '20150101' endda = '20151231' nonkey = 'Z' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_record ).

    me->verify_modify_collision_types( io_collision_results = lo_collision_result
                                       delim = ' ' override = 'X' postpond = ' ' ).

    lrt_overriden_records ?= lo_collision_result->get_overriden_records(  ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_overriden_records is_exp_record = ls_insert_record ).
    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_overriden_records iv_exp_lines = 1 ).

    mo_simulator->insert_record( is_record = ls_insert_record ).
    lrt_resolved_table ?= mo_simulator->get_resolved_table( ).

    cl_abap_unit_assert=>assert_bound( act = lrt_resolved_table ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_insert_record ).
    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = me->mv_lines_before_ops ).

  ENDMETHOD.

  METHOD ins_nogaps_diff_keyfield.

    DATA: ls_insert_record    TYPE me->ts_pernr_dates,
          lo_collision_result TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_resolved_table  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_resolved_table> TYPE me->tt_pernr_dates.

    ls_insert_record = VALUE #( pernr = '0000003' begda = '20150101' endda = '20151231' nonkey = 'C' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_record ).

    me->verify_modify_collision_types( io_collision_results = lo_collision_result
                                       delim = ' ' override = ' ' postpond = ' ' ).

    mo_simulator->insert_record( is_record = ls_insert_record ).
    lrt_resolved_table = mo_simulator->get_resolved_table( ).

    cl_abap_unit_assert=>assert_bound( act = lrt_resolved_table ).

    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = me->mv_lines_before_ops + 1 ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_insert_record ).

  ENDMETHOD.

  METHOD ins_nogaps_postpond.

    DATA: ls_insert_record         TYPE me->ts_pernr_dates,
          ls_exp_delim             TYPE me->ts_pernr_dates,
          ls_exp_resolved_delim    TYPE me->ts_pernr_dates,
          ls_exp_postpond          TYPE me->ts_pernr_dates,
          ls_exp_resolved_postpond TYPE me->ts_pernr_dates,
          lo_collision_result      TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_resolved_table       TYPE REF TO data.

    ls_insert_record = VALUE #( pernr = '0000001' begda = '20161230' endda = '20170102' nonkey = 'L' ).
    ls_exp_delim = VALUE #( pernr = '0000001' begda = '20160101' endda = '20161231' nonkey = 'B' ).
    ls_exp_resolved_delim = VALUE #( pernr = '0000001' begda = '20160101' endda = '20161229' nonkey = 'B' ).
    ls_exp_postpond = VALUE #( pernr = '0000001' begda = '20170101' endda = '99991231' nonkey = 'A' ).
    ls_exp_resolved_postpond = VALUE #( pernr = '0000001' begda = '20170103' endda = '99991231' nonkey = 'A' ).

    lo_collision_result =  mo_simulator->check_collision_insert( is_record =  ls_insert_record ).

    me->verify_modify_collision_types( io_collision_results = lo_collision_result
                                       delim = 'X' override = ' ' postpond = 'X' ).

    mo_ref_assert_util->assert_structure_ref_equal( is_exp_record = ls_exp_delim
                                    irs_act_record = lo_collision_result->get_delimited_record( ) ).

    mo_ref_assert_util->assert_structure_ref_equal( is_exp_record = ls_exp_postpond
                                    irs_act_record = lo_collision_result->get_postponded_record( ) ).

    mo_simulator->insert_record( is_record = ls_insert_record ).
    lrt_resolved_table = mo_simulator->get_resolved_table( ).

    cl_abap_unit_assert=>assert_bound( act = lrt_resolved_table ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_exp_resolved_delim ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_exp_resolved_postpond ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_insert_record ).
    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = me->mv_lines_before_ops + 1 ).

  ENDMETHOD.

  METHOD verify_modify_collision_types.

    IF delim = abap_true.
      cl_abap_unit_assert=>assert_bound( act = io_collision_results->get_delimited_record( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( act = io_collision_results->get_delimited_record( ) ).
    ENDIF.

    IF override = abap_true.
      cl_abap_unit_assert=>assert_bound( act = io_collision_results->get_overriden_records( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( act = io_collision_results->get_overriden_records( ) ).
    ENDIF.

    IF postpond = abap_true.
      cl_abap_unit_assert=>assert_bound( act = io_collision_results->get_postponded_record( ) ).
    ELSE.
      cl_abap_unit_assert=>assert_not_bound( act = io_collision_results->get_postponded_record( ) ).
    ENDIF.

  ENDMETHOD.

  METHOD del_nogaps_extended.

    DATA: ls_delete_record             TYPE me->ts_pernr_dates,
          ls_exp_extnd_record          TYPE me->ts_pernr_dates,
          ls_exp_resolved_extnd_record TYPE me->ts_pernr_dates,
          lrt_resolved_table           TYPE REF TO me->tt_pernr_dates,
          lo_collision_result          TYPE REF TO zif_hr_delete_colision_rslts.

    ls_delete_record = VALUE #( pernr = '0000001' begda = '20160101' endda = '20161231' nonkey = 'B' ).
    ls_exp_extnd_record = VALUE #( pernr = '0000001' begda = '20150101' endda = '20151231' nonkey = 'C' ).
    ls_exp_resolved_extnd_record = VALUE #( pernr = '0000001' begda = '20150101' endda = '20161231' nonkey = 'C' ).
    lo_collision_result = mo_simulator->check_collision_delete( is_record = ls_delete_record ).

    cl_abap_unit_assert=>assert_bound( act = lo_collision_result ).
    cl_abap_unit_assert=>assert_bound( act = lo_collision_result->get_extended_record( ) ).
    mo_ref_assert_util->assert_structure_ref_equal( irs_act_record = lo_collision_result->get_extended_record( )
                                                    is_exp_record = ls_exp_extnd_record ).

    mo_simulator->delete_record( is_record = ls_delete_record ).
    lrt_resolved_table ?= mo_simulator->get_resolved_table(  ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table
                                                   is_exp_record = ls_exp_resolved_extnd_record ).

    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = mv_lines_before_ops - 1 ).

  ENDMETHOD.

  METHOD ins_nogaps_all_effects.

    DATA: ls_insert_record     TYPE me->ts_pernr_dates,
          ls_delim_record      TYPE me->ts_pernr_dates,
          ls_delim_resolved    TYPE me->ts_pernr_dates,
          ls_overriden_record  TYPE me->ts_pernr_dates,
          ls_postpond_record   TYPE me->ts_pernr_dates,
          ls_postpond_resolved TYPE me->ts_pernr_dates,
          lo_collison_results  TYPE REF TO zif_hr_modify_colision_rslts,
          lrt_resolved_table   TYPE REF TO me->tt_pernr_dates.

    ls_insert_record = VALUE #( pernr = '0000001' begda = '20151230' endda = '20170102' nonkey = 'L' ).
    ls_delim_record = VALUE #( pernr = '0000001' begda = '20150101' endda = '20151231' nonkey = 'C' ).
    ls_delim_resolved = VALUE #( pernr = '0000001' begda = '20150101' endda = '20151229' nonkey = 'C' ).
    ls_overriden_record = VALUE #( pernr = '0000001' begda = '20160101' endda = '20161231' nonkey = 'B' ).
    ls_postpond_record =  VALUE #( pernr = '0000001' begda = '20170101' endda = '99991231' nonkey = 'A' ).
    ls_postpond_resolved =  VALUE #( pernr = '0000001' begda = '20170103' endda = '99991231' nonkey = 'A' ).

    lo_collison_results = mo_simulator->check_collision_insert( ls_insert_record ).

    me->verify_modify_collision_types( io_collision_results = lo_collison_results
                                       delim = 'X' override = 'X' postpond = 'X' ).

    mo_ref_assert_util->assert_structure_ref_equal( irs_act_record = lo_collison_results->get_delimited_record(  )
                                                    is_exp_record = ls_delim_record ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lo_collison_results->get_overriden_records(  )
                                                   is_exp_record = ls_overriden_record ).

    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lo_collison_results->get_overriden_records(  )
                                                iv_exp_lines = 1 ).

    mo_ref_assert_util->assert_structure_ref_equal( irs_act_record = lo_collison_results->get_postponded_record(  )
                                                    is_exp_record = ls_postpond_record ).

    mo_simulator->insert_record( ls_insert_record ).

    lrt_resolved_table ?= mo_simulator->get_resolved_table(  ).

    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_delim_resolved ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_insert_record ).
    mo_ref_assert_util->assert_ref_table_contains( irt_act_table = lrt_resolved_table is_exp_record = ls_postpond_resolved ).
    mo_ref_assert_util->assert_ref_table_lines( irt_act_table = lrt_resolved_table iv_exp_lines = mv_lines_before_ops ).

  ENDMETHOD.

  METHOD ins_nogaps_detect_gap.

    DATA: ls_insert_record      TYPE me->ts_pernr_dates,
          lo_exp_exception_check TYPE REF TO zcx_timconst_check,
          lo_exp_exception_insrt TYPE REF TO zcx_timconst_check.

    ls_insert_record = VALUE #( pernr = '0000001' begda = '20180101' endda = '20190101' nonkey = 'L' ).

    TRY.

        mo_simulator->check_collision_insert( is_record = ls_insert_record ).

      CATCH zcx_timconst_check INTO lo_exp_exception_check.


    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lo_exp_exception_check ).

    TRY.

        mo_simulator->check_collision_insert( is_record = ls_insert_record ).

      CATCH zcx_timconst_check INTO lo_exp_exception_insrt.


    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = lo_exp_exception_insrt ).

  ENDMETHOD.

ENDCLASS.
